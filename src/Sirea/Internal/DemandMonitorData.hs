
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
-- ideal. Efficiency can suffer for observing intermediate updates.
-- Partitions with responsibilities (e.g. rendering OpenGL frames)
-- will need to execute more runStepper actions between frames.
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
--
module Sirea.Internal.DemandMonitorData
    ( DemandAggr, newDemandAggr, newDemandLnk
    , MonitorDist, newMonitorDist, mainMonitorLnk, newMonitorLnk
    ) where

import Data.IORef
import Data.Maybe (fromMaybe, isNothing)
import Data.List (foldl')
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


type Table = Table.Table
type Key = Table.Key

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
    { da_data       :: !(IORef DemandData)  -- mutable state (other than table)
    , da_demands    :: !(Table (SigSt e))   -- track active demand data
    , da_nzip       :: !([Sig e] -> Sig z)  -- compute the result signal
    , da_link       :: !(LnkUp z)           -- process the updated signal
    , da_partd      :: !PartD               -- partition data to support GC
    }

data DemandData = DemandData 
    { dd_active     :: {-# UNPACK #-} !Bool      -- is update scheduled?
    , dd_flush      :: {-# UNPACK #-} !Bool      -- is flush requested?
    , dd_nextKey    :: {-# UNPACK #-} !Key       -- next key for the table
    , dd_tmup       :: {-# UNPACK #-} !(Maybe T) -- time of earliest demand update
    , dd_stable     :: {-# UNPACK #-} !(Maybe T) -- time of reported stability
    }

-- | Create a demand aggregator, given the output target.
newDemandAggr
    :: (Partition p)
    => PCX p 
    -> (LnkUp z)
    -> ([Sig e] -> Sig z)
    -> DT
    -> IO (DemandAggr e z)
newDemandAggr cp zlu zpf zeq dtHist = 
    newIORef ddZero >>= \ rfDD ->
    Table.new >>= \ tbl ->
    let da = DemandAggr 
                { da_data    = rfDD
                , da_demands = tbl
                , da_nzip    = zpf
                , da_link    = zlu
                , da_partd   = mkPartD cp
                }
    in
    (return $! da)

ddZero :: DemandData
ddZero = DemandData
    { dd_active  = False
    , dd_flush   = False 
    , dd_nextKey = 80000
    , dd_tmup    = Nothing
    , dd_stable  = Nothing
    }

-- | Create a new link to an existing demand aggregator.
newDemandLnk :: DemandAggr e z -> IO (LnkUp e)
newDemandLnk da =
    newIORef Nothing >>= \ rfK -> -- acquire Key later, in Demand Monitor's partition
    let getKey = loadKey da rfK in
    let touch = return () in -- ignore touches (updates wait to next round anyway)
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
    let dd' = dd { dd_active = True, dd_tmup = tmup' } in
    dd' `seq` writeIORef (da_data da) dd' >>
    Table.get (da_demands da) k >>= \ mbSt ->
    let st = fromMaybe st_zero mbSt in
    let st' = st_sigup su st in
    Table.put (da_demands da) k (Just st') >>
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
-- heartbeat, depending on urgency of updates). 
weighDaggr :: DemandAggr e z -> IO ()
weighDaggr da =
    Table.toList (da_demands da) >>= \ lst ->
    if (null list) then sheatheDaggr da 
                   else deliverDaggr da lst

-- sheatheDaggr will reset stability of DemandAggr to Nothing. This
-- should occur only via sheduleDaggr for cleanup purposes. This is
-- needed to help downstream observers flush data when there are no
-- demand sources to actively maintain stability. 
sheatheDaggr :: DemandAggr e z -> IO ()
sheatheDaggr da =
    readIORef (da_data da) >>= \ dd ->
    assert (dd_active dd && isNothing (dd_tmup dd)) $ 
    let bWasSheathed = isNothing (dd_stable dd) in
    let dd' = DemandData 
                { dd_active = False, dd_flush = False
                , dd_tmup = Nothing, dd_stable = Nothing 
                , dd_nextKey = (dd_nextKey dd) -- preserved
                } 
    in
    dd' `seq` writeIORef (da_data da) dd' >>= \ _ ->
    unless bWasSheathed $
        ln_touch (da_link da) >>
        let su = SigUp { su_state = Nothing, su_stable = Nothing } in
        let updateAction = ln_update (da_link da) su in 
        pd_phaseDelay (da_partd da) updateAction

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
deliverDaggr da lst =
    readIORef (da_data da) >>= \ dd ->
    pd_time (da_partd da) >>= \ tNow ->
    -- compute time of stability
    let tMax = Just (tNow `subtractTime` dtDaggrHist) in -- buffer for late demands
    let tDem = foldl' leastTime Nothing $ (st_stable . snd) `fmap` lst in -- aggregate reported stability
    let tTgt = leastTime tDem tMax in -- goal stability (aggregate with upper bound at tMax)
    let tRec = dd_stable dd <|> tMax in -- effective recorded/reported stability
    let tStable = max <$> tTgt <*> tRec in -- enforce non-decreasing stability reports
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
    let bClockBound = isNothing tDem in
    if bUrgent -- most updates are urgent... cleanup and send!
       then let dd' = dd { dd_active = False, dd_flush = False, dd_tmup = Nothing } in
            writeIORef (da_data da) dd' >>
            mapM_ (gcDaggrTable (da_demands da) tStable) lst >> -- GC recorded demands
            -- if no active demand sources, repeat flush until sheatheDaggr
            when bClockBound (scheduleDaggr da) >> 
            let nextRoundAction = -- touch, delay, update (like bcross)
                    ln_touch (da_link da) >>
                    pd_phaseDelay (da_partd da) (ln_update (da_link da) su)
            in
            pd_stepDelay (da_partd da) nextRoundAction
       else let dd' = dd { dd_active = False, dd_flush = False } in
            writeIORef (da_data da) dd' >>
            let bUpdatesPending = (not . isNothing) (dd_tmup dd') in
            let bShouldRevisit = bClockBound || bUpdatesPending in
            when bShouldRevisit (scheduleDaggr da)

-- Garbage Collection of DemandAggr elements. An demand source can
-- be eliminated if it is fully stable and all its active values are
-- in the past. (We'll hold onto unstable, inactive demand sources
-- to regulate stability of the demand monitor.)
gcDaggrTable :: Table (SigSt e) -> Maybe T -> (Key, SigSt e) -> IO ()
gcDaggrTable tbl Nothing (k,_) = Table.put tbl k Nothing
gcDaggrTable tbl (Just tm) (k,st) =
    let (x,sf) = s_sample (st_signal st) tm in
    let bDone = isNothing (st_stable st) && 
                isNothing x &&  
                s_is_final sf tm 
    in
    if bDone then Table.put tbl k Nothing
             else let st' = st { st_signal = sf } in
                  st' `seq` Table.put tbl k (Just st')

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
-- This is pretty simple. The masking is the most difficult aspect,
-- and is straightforward enough (achieved on the per-monitor link).
--
data MonitorDist z = MonitorDist 
    { md_signal     :: !(IORef (SigSt z)) -- primary signal
    , md_nextKey    :: !(IORef Key)       -- next key for observers
    , md_observers  :: !(Table (LnkUp z)) -- collection of observers
    , md_time       :: !(IO T)            -- est. wall clock time
    , md_default    :: !(Sig z)           -- default signal
    }

-- | track a new set of observers
newMonitorDist :: (Partition p) => PCX p -> Sig z -> IO (MonitorDist z)
newMonitorDist cp z0 = MonitorDist
    <$> newIORef stInit 
    <*> newIORef 60000 -- arbitrary
    <*> Table.new
    <*> pure (getStepTime cp)
    <*> pure z0
    where stInit = st_zero { st_signal = z0 }

-- | Each MonitorDist has one main LnkUp where it receives a primary
-- signal, which is later distributed to observers. The primary must
-- be updated from only one source in the same partition. 
primaryMonitorLnk :: MonitorDist z -> LnkUp z
primaryMonitorLnk md = LnkUp { ln_touch = touch, ln_update = update }
    where touch = -- touch all active observers!
            readIORef (md_signal md) >>= \ st ->
            unless (st_expect st) $
                let st' = st_poke st in
                writeIORef (md_signal md) st' >>
                Table.toList (md_table md) >>=
                mapM_ (ln_touch . snd) 
          stInit = st_zero { st_signal = s_always (md_default md) }
          update su = -- record signal, then update observers.
            readIORef (md_signal md) >>= \ st ->
            md_time md >>= \ tNow ->
            let tEff = 
            let bExpected = st_expect st in
            let st' = st_sigup su st in
            let stc = (`st_clear` st') . (`subtractTime` dtMdistHist) in
            let stGC = maybe stInit stc (su_stable su) in
            stGC `seq` -- GC older values in signal
            writeIORef (md_signal md) stGC >>
            -- second, deliver signal update to all existing observers
            Table.toList (md_table md) >>= \ lst ->
            unless bExpected (mapM_ (ln_touch . snd) lst) >> -- touch, if needed
            mapM_ ((`ln_update` su) . snd) lst -- update
    
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

-- MonLn is data associated with one newMonitorLnk
data MonLn z = MonLn
    { mln_rfSigM :: !(IORef (SigM () z)) -- local record of signals
    , mln_lzout  :: !(LnkUp z)           -- final data target (after mask)
    , mln_mdist  :: !(MonitorDist z)     -- source of data
    , mln_rfKey  :: !(IORef (Maybe Key)) -- identifier in MonitorDist
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
                let sigZ  = (st_signal . sm_rsig) sm in
                let sigO  = (st_signal . sm_lsig) sm in
                let sig   = s_mask sigZ sigO in
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



