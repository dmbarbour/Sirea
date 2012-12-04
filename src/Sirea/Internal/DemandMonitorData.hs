
-- Utilities for tracking demands and monitors of demand.
--
-- Currently this is separated into two responsibilities:
--   * aggregation of demands
--   * distribution of a signal to many observers
--
-- The main challenges for implementing a DemandMonitor seem to be:
--
--   * robust handling of cyclic update relationships 
--   * final garbage collection of demands
--
-- Any shared demand aggregator that feeds back into the RDP system
-- admits potential cyclic feedback. Demand monitors are an example.
-- Unfortunately, there is no robust mechanism to deal with cycles
-- within a given step (runStepper) - the ln_touch mechanism assumes
-- acyclic graphs, and the goals of snapshot consistency and bounded
-- computation conflict with techniques to detect or break cycles.
--
-- Consequently, Sirea uses multiple steps for demand aggregators. 
-- The demand updates in one round will not be observed until some
-- later step - usually the next step (via onNextStep). This is not
-- ideal. Efficiency may suffer for observing intermediate updates.
-- A chain of demand monitors may require many steps to propagate.
-- Each step has some costs associated with it. 
--
-- There is an advantage: updates that come in a later step can be
-- choked a little if there is no significant update or if it does
-- not become significant until later in the future. This is similar
-- to choking of updates between partitions. Choking updates helps 
-- regulate and prioritize performance with cyclic feedback. Since
-- DemandAggr is used directly or indirectly for many resources, 
-- most feedback behavior can be addressed.
--
-- GC of a demand monitor is generally continuous, happening due to
-- stability updates of the relevant demand sources. However, after
-- the last updates, vestigial data may be held by aggregators and
-- by downstream observers. To clear local data, extra cleanup may
-- be scheduled on heartbeats (once it seems there are no demands).
-- To flush the downstream observers, stability is reset to Nothing.
--
-- Downstream observers must be aware that `Nothing` is not forever
-- for a demand aggregator signals, and actually corresponds to a
-- stability of ((`subtractTime` dtDaggrHist) `fmap` getStepTime).
-- This isn't trivial, but can be handled with some discipline.
--
module Sirea.Internal.DemandMonitorData
    ( DemandAggr, newDemandAggr, newDemandLnk
    , MonitorDist, newMonitorDist, mainMonitorLnk, newMonitorLnk
    ) where

import Data.IORef
import Data.Maybe (fromMaybe, isNothing)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Control.Applicative
import Control.Monad (unless, when, void, sequence_, mapM_)
import Control.Exception (assert)
import Sirea.Signal
import Sirea.Time
import Sirea.Link
import Sirea.Partition
import Sirea.PCX
import qualified Sirea.Internal.Table as Table
import Sirea.Internal.BCross (isUrgentUpdate) -- to avoid duplication of code
import Sirea.Internal.Tuning (dtDaggrHist, dtMdistHist)

type Key = Int

-- convenient partition data (to avoid replicate lookups)
data PartD = PartD 
    { pd_time       :: !(IO T)
    , pd_phaseDelay :: !(IO () -> IO ())
    , pd_endOfStep  :: !(IO () -> IO ())
    , pd_stepDelay  :: !(IO () -> IO ())
    , pd_eventually :: !(IO () -> IO ())
    }
mkPartD :: (Partition p) => PCX p -> PartD
mkPartD cp = PartD  
    { pd_time       = getStepTime cp
    , pd_endOfStep  = atEndOfStep cp
    , pd_phaseDelay = phaseDelay cp
    , pd_stepDelay  = onNextStep cp
    , pd_eventually = onHeartbeat cp
    }     

-- | DemandAggr: aggregates and processes many concurrent demands.
data DemandAggr e z = DemandAggr 
    { da_data       :: !(IORef DemandData)  -- mutable state 
    , da_nzip       :: !([Sig e] -> Sig z)  -- compute the result signal
    , da_link       :: !(LnkUp z)           -- process the updated signal
    , da_partd      :: !PartD               -- partition data to support GC
    }

data DemandData e = DemandData
    { dd_active     :: {-# UNPACK #-} !Bool      -- is update scheduled?
    , dd_flush      :: {-# UNPACK #-} !Bool      -- is flush requested?
    , dd_tmup       :: {-# UNPACK #-} !(Maybe T) -- time of earliest demand update
    , dd_stable     :: {-# UNPACK #-} !(Maybe T) -- time of reported stability
    , dd_nextKey    :: {-# UNPACK #-} !Key       -- next key for the table
    , dd_table      :: !(M.Map Key (SigSt e))    -- track active demands
    }

-- | Create a demand aggregator, given the output target.
newDemandAggr
    :: (Partition p)
    => PCX p 
    -> (LnkUp z)
    -> ([Sig e] -> Sig z)
    -> IO (DemandAggr e z)
newDemandAggr cp zlu zpf = DemandAggr
    <$> newIORef ddZero
    <*> pure zpf
    <*> pure zlu
    <*> pure (mkPartD cp)

ddZero :: DemandData e
ddZero = DemandData
    { dd_active  = False
    , dd_flush   = False 
    , dd_tmup    = Nothing
    , dd_stable  = Nothing
    , dd_nextKey = 80000
    , dd_table   = M.empty
    }

-- | Create a new link to an existing demand aggregator.
newDemandLnk :: DemandAggr e z -> IO (LnkUp e)
newDemandLnk da =
    newIORef Nothing >>= \ rfK -> -- acquire Key later, in Demand Monitor's partition
    let getKey = loadKey da rfK in
    let touch = return () in -- touch is applied in update step
    let update su = getKey >>= \ k -> updateDaggr da k su in
    let lu = LnkUp { ln_touch = touch, ln_update = update } in
    return lu

loadKey :: DemandAggr e z -> IORef (Maybe Key) -> IO Key
loadKey da rfK =
    readIORef rfK >>= \ mk ->
    case mk of
        Just k -> return k
        Nothing ->
            readIORef (da_data da) >>= \ dd ->
            let k = succ (dd_nextKey dd) in
            let dd' = dd { dd_nextKey = k } in
            k `seq` dd' `seq` 
            writeIORef (da_data da) dd' >>
            writeIORef rfK (Just k) >>
            return k

-- Update the DemandAggr and activate it (if not active)
updateDaggr :: DemandAggr e z -> Key -> SigUp e -> IO ()
updateDaggr da k su = 
    readIORef (da_data da) >>= \ dd ->
    let bWasActive = dd_active dd in
    let tmup' = leastTime (dd_tmup dd) (snd `fmap` su_state su) in
    let tbl = dd_table dd in
    let st  = fromMaybe st_zero $ M.lookup k tbl in
    let st' = st_sigup su st in
    let tbl' = M.insert k st' tbl in
    let dd' = dd { dd_active = True, dd_tmup = tmup', dd_table = tbl' } in
    dd' `seq` writeIORef (da_data da) dd' >>
    unless bWasActive (activateDaggr da)
    
-- An `active` daggr will be considered at the end of the current 
-- step, and may result in an update for the next step. Activity is
-- cleared at the end of the step. 
activateDaggr :: DemandAggr e z -> IO () 
activateDaggr da = pd_endOfStep (da_partd da) (weighDaggr da)

-- Schedule the DemandAggr to flush updates on the next heartbeat.
-- This will push pending updates even if they're for the distant
-- future. This improves anticipation, albeit sometimes at greater
-- cost for rework. It also keeps choked data flowing.
flushDaggr :: DemandAggr e z -> IO ()
flushDaggr da = pd_eventually (da_partd da) $
    readIORef (da_data da) >>= \ dd ->
    let bWasActive = dd_active dd in
    let dd' = dd { dd_active = True, dd_flush = True } in
    writeIORef (da_data da) dd' >>
    unless bWasActive (activateDaggr da)

-- when we've activated a DemandAggr, we'll decide what to do with
-- it at the end of the round. If any updates are present, they will
-- be delivered in some later round (either the next round or on a
-- heartbeat, depending on urgency of updates). Or if there is 
-- nothing, we might reset the daggr to its initial state.
weighDaggr :: DemandAggr e z -> IO ()
weighDaggr da =
    readIORef (da_data da) >>= \ dd ->
    if (M.null (dd_table dd)) then sheatheDaggr da
                              else deliverDaggr da

-- sheatheDaggr will reset stability of DemandAggr to Nothing. This
-- should occur only via sheduleDaggr for cleanup purposes. This is
-- needed to help downstream observers flush data when there are no
-- demand sources to actively maintain stability. 
sheatheDaggr :: DemandAggr e z -> IO ()
sheatheDaggr da =
    readIORef (da_data da) >>= \ dd ->
    assert (dd_active dd && isNothing (dd_tmup dd) && M.null (dd_table dd)) $ 
    let bWasSheathed = isNothing (dd_stable dd) in
    let dd' = DemandData 
                { dd_active = False, dd_flush = False
                , dd_tmup = Nothing, dd_stable = Nothing 
                , dd_nextKey = (dd_nextKey dd) -- preserved
                , dd_table = M.empty
                } 
    in
    dd' `seq` writeIORef (da_data da) dd' >>= \ _ ->
    unless bWasSheathed $
        let nextRoundAction = 
            ln_touch (da_link da) >>
            let su = SigUp { su_state = Nothing, su_stable = Nothing } in
            let updateAction = ln_update (da_link da) su in 
            pd_phaseDelay (da_partd da) updateAction
        in
        pd_stepDelay (da_partd da) nextRoundAction

-- deliverDaggr is activated at the very end of the round. Updates,
-- if any, will be delivered in a later step. A negligible update to
-- stability may be dropped entirely, while an update to state may
-- be delayed and reconsidered if it is far ahead of stability and
-- real-time.
--
-- Note: I could make better decisions about updates if I applied 
-- equality filters at this point, but complexity would explode. 
-- The global benefits would be marginal: one fewer steps per cycle.
deliverDaggr :: DemandAggr e z -> [(Key,SigSt e)] -> IO ()
deliverDaggr da =
    pd_time (da_partd da) >>= \ tNow ->
    readIORef (da_data da) >>= \ dd ->
    let tbl  = dd_table dd in
    let lst  = M.toAscLst tbl in
    -- compute time of stability
    let tMax = Just (tNow `subtractTime` dtDaggrHist) in -- buffer for late demands
    let tDem = foldl' leastTime Nothing $ (st_stable . snd) `fmap` lst in -- aggregate reported stability
    let tTgt = leastTime tDem tMax in -- goal stability (aggregate with upper bound at tMax)
    let tRec = dd_stable dd <|> tMax in -- effective recorded/reported stability
    let tStable = max <$> tTgt <*> tRec in -- enforce non-decreasing stability reports
    -- compute the GC'd table from prior table and new stability.
    let lst' = mapMaybe (gcDaggrTable tStable) lst in
    let tbl' = M.fromAscLst lst' in
    -- compute the proposed update.
    let tUpd = max <$> (dd_tmup dd) <*> tRec in -- truncate updates earlier than recorded stability!
    let su   = case tUpd of
                    Nothing -> SigUp { su_state = Nothing, su_stable = tmStable }
                    Just tu -> let eSigs = ((`s_trim` tu) . st_signal . snd) `fmap` lst in
                               let zSig  = da_nzip da eSigs in
                               SigUp { su_state = Just(zSig, tu), su_stable = tmStable }
    in
    -- A non-urgent update may be delayed to a later heartbeat.
    -- Some updates may be dropped entirely, if insignificant.
    let bFlushed    = (dd_flush dd) && (not . isNothing) (dd_tmup dd) in
    let bUrgent     = bFlushed || isUrgentUpdate (dd_stable dd) su in
    let bClockBound = isNothing tDem in -- can't assume more stability updates coming!
    if bUrgent -- modulo cycles, most updates are urgent... cleanup and send!
       then let dd' = dd { dd_active = False, dd_flush = False
                         , dd_tmup = Nothing, dd_table = tbl' } 
            in
            dd' `seq` writeIORef (da_data da) dd' >>
            -- if no active demand sources, repeat flush until sheatheDaggr
            when bClockBound (flushDaggr da) >> 
            let nextRoundAction = -- touch, delay, update (like bcross)
                    ln_touch (da_link da) >>
                    let updateAction = ln_update (da_link da) su in
                    pd_phaseDelay (da_partd da) updateAction 
            in
            pd_stepDelay (da_partd da) nextRoundAction
       else let dd' = dd { dd_active = False, dd_flush = False } in
            dd' `seq` writeIORef (da_data da) dd' >>
            let bUpdatesPending = (not . isNothing) (dd_tmup dd') in
            let bShouldRevisit = bClockBound || bUpdatesPending in
            when bShouldRevisit (flushDaggr da)

-- Garbage Collection of DemandAggr elements. An demand source can
-- be eliminated if it is fully stable and all its active values are
-- in the past. (We'll hold onto unstable, inactive demand sources
-- to regulate stability of the demand monitor.)
gcDaggrTable :: Maybe T -> (Key, SigSt e) -> Maybe (Key, SigSt e)
gcDaggrTable Nothing  _ = Nothing
gcDaggrTable (Just tm) (k,st) =
    let (x,sf) = s_sample (st_signal st) tm in
    let bDone = isNothing (st_stable st) && 
                isNothing x && s_is_final sf tm 
    in
    if bDone then Nothing
             else Just (k, st { st_signal = sf })

leastTime :: Maybe T -> Maybe T -> Maybe T
leastTime l r = (min <$> l <*> r) <|> l <|> r
    
monotonicStability :: Maybe T -> Maybe T -> Bool
monotonicStability (Just t0) (Just tf) = (tf >= t0)
monotonicStability _ _ = True


-- | MonitorDist supports output to multiple observers (monitors) of
-- a resource or signal. A MonitorDist has three responsibilities:
-- 
--   * distribute a signal to many observers
--   * mask input signal with observer's query
--   * keep some history for late-arriving observers
--
-- This is mostly quite simple. The main difficulty is the GC aspect
-- when the main `z` signal goes inactive. The inactive signal may
-- still have some vestigial values that must be cleared over time
-- based on the heartbeat (and adjusted for dtMdistHist).
--
-- Assuming the md_signal is kept intact, the individual links can
-- use it more directly rather than keeping their own copies. 
--
data MonitorDist z = MonitorDist 
    { md_data       :: !(IORef (MDD z))   -- mutable data of MonitorDist
    , md_partd      :: !(PartD)           -- scheduler and time
    , md_default    :: !(Sig z)           -- default signal
    }
-- mutable data for the full MonitorDist
data MDD z = MDD
    { mdd_signal    :: {-# UNPACK #-} !(SigSt z) -- track current signal
    , mdd_cleanup   :: {-# UNPACK #-} !Bool      -- is cleanup scheduled?
    , mdd_nextKey   :: {-# UNPACK #-} !Key       -- next key for table
    , mdd_table     :: !(M.Map Key (LnkUp z))    -- active observers
    }

-- | track a new set of observers
newMonitorDist :: (Partition p) => PCX p -> Sig z -> IO (MonitorDist z)
newMonitorDist cp z0 = MonitorDist 
    <$> newIORef (mddZero z0)
    <*> pure (mkPartD cp)
    <*> pure z0

mddZero :: Sig z -> MDD z
mddZero z0 = MDD
    { mdd_signal  = st_zero { st_signal = z0 } 
    , mdd_cleanup = False
    , mdd_nextKey = 60000 -- arbitrary 
    , mdd_table   = M.empty
    }

-- There are a couple concerns when cleaning up a MonitorDist. 
--   * we need to cleanup on heartbeats, which must be idempotent
--   * recent update must be stored locally, available to monitors
-- The latter point means we don't need to store the z signal at
-- each monitor link; we can just use shared MonitorDist storage.
--
-- So we'll clean-up a MonitorDist only at the end of the round, and
-- we'll mark this for idempotence in case of heartbeat cleanups.
--
scheduleCleanupMD :: MonitorDist z -> IO ()
scheduleCleanupMD md =
    readIORef (md_data md) >>= \ mdd ->
    unless (mdd_cleanup mdd) $
        let mdd' = mdd { mdd_cleanup = True } in
        writeIORef (md_data md) mdd' >>
        pd_endOfStep (md_partd md) (cleanupMD md)

eventuallyCleanupMD :: MonitorDist z -> IO ()
eventuallyCleanupMD md = pd_eventually (md_partd md) $ scheduleCleanup md
    
-- end of round cleanup or GC
cleanupMD :: MonitorDist z -> IO ()
cleanupMD md = 
    readIORef (md_data md) >>= \ mdd ->
    when (mdd_cleanup mdd) $
        let tbl = mdd_table mdd in
        let lst = 
        let md' = mdd { 
    { mdd_signal    :: {-# UNPACK #-} !(SigSt z) -- track current signal
    , mdd_cleanup   :: {-# UNPACK #-} !Bool      -- is cleanup scheduled?
    , mdd_nextKey   :: {-# UNPACK #-} !Key       -- next key for table
    , mdd_table     :: !(M.Map Key (LnkUp z))    -- active observers
    }    

f

-- | Each MonitorDist has one main LnkUp where it receives a primary
-- signal, which is later distributed to observers. The primary must
-- be updated from only one source in the same partition. Updates 
-- are immediately delivered to all observing links. In addition, the
-- signal value is stored for late-arriving observers or observers 
-- that otherwise cannot immediately process the update. 
primaryMonitorLnk :: MonitorDist z -> LnkUp z
primaryMonitorLnk md = LnkUp { ln_touch = touch, ln_update = update }
    where touch = -- touch all active observers!
            readIORef (md_data md) >>= \ mdd ->
            let st = mdd_signal mdd in
            unless (st_expect st) $
                let lOut = M.elems (mdd_table mdd) in
                let st' = st_poke st in
                let mdd' = mdd { mdd_signal = st' } in
                writeIORef (md_data md) mdd' >>
                mapM_ ln_touch lOut
          update su =
            readIORef (md_data md) >>= \ mdd ->
            let st   = mdd_signal mdd in
            let st'  = st_sigup su st in
            let mdd' =  
            pd_time (md_partd md) >>= \ tNow ->
            let tMax = Just $ tNow `subtractTime` dtDaggrHist in
            let tEff = leastTime tMax (su_stable su) in
            let tGC  = (`subtractTime` dtMdistHist) `fmap` tEff in
            let bClockBound = isNothing (su_stable su) in
            

          stInit = st_zero { st_signal = s_always (md_default md) }
          update su = -- record signal, then update observers.
            readIORef (md_signal md) >>= \ st ->
            -- TODO: Figure out how to integrate time with the cleanup...
            md_time md >>= \ tNow ->
            let tEff = 
            let bExpected = st_expect st in
            let st' = st_sigup su st in
            let stc = (`st_clear` st') . (`subtractTime` dtMdistHist) in
            let stGC = maybe stInit stc (su_stable su) in
            stGC `seq` -- GC older values in signal
            writeIORef (md_signal md) stGC >>
            -- second, deliver signal update to all existing observers
            unless bExpected (mapM_ (ln_touch . snd) lst) >> -- touch, if needed
            mapM_ ((`ln_update` su) . snd) lst -- update
    

-- MonLn is data associated with one newMonitorLnk
data MonLn z = MonLn
    { mln_data   :: !(IORef MLD)     -- mutable data; tracking of signals
    , mln_lzout  :: !(LnkUp z)       -- destination of observed signal
    , mln_mdist  :: !(MonitorDist z) -- source of observed signal
    }

-- mutable data for a monitor link
data MLD = MLD 
    { mld_key    :: {-# UNPACK #-} !(Maybe Key)
    , mld_signal :: {-# UNPACK #-} !(SigSt ())
    , mld_tmup   :: {-# UNPACK #-} !(Maybe T)
    }

-- | Each observer will build a link to the MonitorDist. The task of
-- this link is to mask the MonitorDist signal with the observer's
-- query. For this to work well, the MonitorDist signal should be
-- continuous, or at least "continuous while there are observers".
--
-- The observer's signal represents duration and timing of active
-- observation. It has no direct effect on the signal, but can be
-- leveraged in wrappers that use demand monitors internally.
--
-- If there are no active demands, the MonitorDist will treat the
-- demand stability as a simple function of the partition's time.
--
newMonitorLnk :: MonitorDist z -> LnkUp z -> IO (LnkUp ())
newMonitorLnk md lzo = lnMonitor <$> newMonLn md lzo

mldZero :: MLD
mldZero = MLD 
    { mld_key = Nothing
    , mld_sig = st_zero
    , mld_tmup = Nothing
    }

newMonLn :: MonitorDist z -> LnkUp z -> IO (MonLn z)
newMonLn md lzo = MonLn 
    <$> newIORef sm_zero
    <*> pure lzo 
    <*> pure md
    <*> newIORef Nothing

-- lnMonitor is similar to ln_withSigM except that it must manage
-- installation of the LnkUp resource. 
lnMonitor :: MonLn z -> LnkUp ()
lnMonitor mln = loi
    where loi = LnkUp { ln_touch = pokeO, ln_update = updateO }
          lzi = LnkUp { ln_touch = pokeZ, ln_update = updateZ }
          lzo = mln_lzout mln
          rfSigM = mln_rfSigM mln
          onPoke fn =   
            readIORef rfSigM >>= \ sm ->
            writeIORef rfSigM (fn st_poke sm) >>
            unless (sm_waiting sm) (ln_touch lzo)
          pokeO = onPoke sm_update_l
          pokeZ = onPoke sm_update_r
          updateO su = modifyIORef rfSigM (sm_sigup_l su) >> maybeInstallMon mln >> emit
          updateZ su = modifyIORef rfSigM (sm_sigup_r su) >> emit
          emit =
            readIORef rfSigM >>= \ sm ->
            unless (sm_waiting sm) $
                let sm' = sm_cleanup (sm_stable sm) sm in
                sm' `seq` writeIORef rfSigM sm' >>
                maybeUninstallMon mln >>
                let su = sm_emit (flip s_mask) sm in 
                ln_update lzo su



--
-- I keep a heuristic estimate of when an mln should be installed.
-- (Where `installed` means observing the `b` signal.) 
shouldBeInstalled :: SigM () b -> Bool
shouldBeInstalled sm = 
    let tMin = leastTime (sm_stable sm) (sm_tmup sm) in
    case tMin of
        Nothing -> False
        Just tm ->  
            let sig = (st_signal . sm_lsig) sm in
            let done = s_term sig tm in
            not done

          maybeInstall =
            readIORef rfSigM >>= \ sm ->
            when (shouldBeInstalled sm) $ 
                installMonLn mln
          maybeUninstall =
            readIORef rfSigM >>= \ sm ->
            unless (shouldBeInstalled sm) $
                uninstallMonLn mln


uninstallMonLn, installMonLn :: MonLn z -> IO ()
installMonLn mln =
    let rfIdx = mln_rfIdx mln in
    let rfSigM = mln_rfSigM mln in
    let md = mln_mdist mln in
    let lzi = mln_lzout mln in
    readIORef rfIdx >>= \ mbk ->
    case mbk of
        Just _ -> return () -- already installed
        Nothing ->
            -- prepare for future z inputs 
            readIORef (md_nextid md) >>= \ k0 ->
            let k = succ k0 in
            k `seq` 
            writeIORef (md_nextid md) k >>
            writeIORef rfIdx (Just k) >>
            Table.put (md_table md) k (Just lzi) >>
            -- obtain initial state for z signal (& touch state)
            readIORef (md_signal md) >>= \ sz ->
            readIORef rfSigM >>= \ sm ->
            let sm' = sm { sm_rsig = sz } in
            writeIORef rfSigM sm' 
          

uninstallMonLn mln =
    let rfIdx = mln_rfIdx mln in
    let rfSigM = mln_rfSigM mln in
    let md = mln_mdist mln in
    readIORef rfIdx >>= \ mbk ->
    case mbk of
        Nothing -> return () -- not installed
        Just k  -> -- clear associated states!
            Table.put (md_table md) k Nothing >>
            writeIORef rfSigM sm_zero >>
            writeIORef rfIdx Nothing



