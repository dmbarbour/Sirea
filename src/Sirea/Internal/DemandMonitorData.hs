
-- Utilities for tracking demands and monitors of demand.
--
-- Currently this is separated into two responsibilities:
--
--   * aggregation of demands
--   * distribution of a signal to many observers
--
-- Demand aggregation in RDP occurs at external resources and allows
-- potential cycles to occur between components. Cycles can model
-- interactive systems and coordination patterns. Sirea will handle 
-- cycles robustly and efficiently, assuming they are dampened to be
-- computed over time.
--
-- Demand aggregators are stabilized against the clock rather than
-- demand sources. This makes it easy to reason about stability and
-- performance, but can result in data loss or inconsistency in case
-- of straggling updates. Despite this, developers can still depend
-- on snapshot consistency and eventual consistency. Correct use of
-- `bdelay` can prevent stability issues, especially in loops.
--
-- A DemandAggr may report `Nothing` for stability without really 
-- meaning that it's stable forever; it just means there is no 
-- active demand source. 
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

-- convenient access to time and schedulers 
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
    { da_data       :: !(IORef (DemandData e z)) -- mutable state 
    , da_nzip       :: !([Sig e] -> Sig z)       -- compute the result signal
    , da_link       :: !(LnkUp z)                -- process the updated signal
    , da_partd      :: !PartD                    -- partition scheduler
    }

data DemandData e z = DemandData
    { dd_touchCt    :: {-# UNPACK #-} !Int       -- waiting on updates?
    , dd_flush      :: {-# UNPACK #-} !Bool      -- GC flush requested?
    , dd_flushSched :: {-# UNPACK #-} !Bool      -- is GC flush scheduled?
    , dd_tmup       :: {-# UNPACK #-} !(Maybe T) -- time of earliest demand update
    , dd_stable     :: {-# UNPACK #-} !(Maybe T) -- time of reported stability
    , dd_nextKey    :: {-# UNPACK #-} !Key       -- next key for the table
    , dd_table      :: !(M.Map Key (SigSt e))    -- track active demands
    , dd_pending    :: {-# UNPACK #-} !(Maybe (SigUp z)) -- any undelivered update.
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
    { dd_touchCt    = 0
    , dd_flush      = False
    , dd_flushSched = False
    , dd_tmup       = Nothing
    , dd_stable     = Nothing
    , dd_nextKey    = 80000 -- arbitrary
    , dd_table      = M.empty 
    , dd_pending    = Nothing
    }

-- | Create a new link to an existing demand aggregator.
newDemandLnk :: DemandAggr e z -> IO (LnkUp e)
newDemandLnk da =
    newIORef Nothing >>= \ rfK -> -- acquire Key later, in Demand Monitor's partition
    let getKey = loadKey da rfK in
    let touch = getKey >>= touchDaggr da in
    let update su = getKey >>= \ k -> updateDaggr da k su in
    let lu = LnkUp { ln_touch = touch, ln_update = update } in
    return lu

-- keys are computed with explicitly lazy IO, via an IORef
-- (this helps assure me of which thread it runs in)
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

-- touch the DemandAggr and activate it (if not active)
touchDaggr :: DemandAggr e z -> Key -> IO ()
touchDaggr da k = 
    readIORef (da_data da) >>= \ dd ->
    let tbl = dd_table dd in
    let st = fromMaybe st_zero $ M.lookup k tbl in
    unless (st_expect st) $ -- block redundant touch
        let st' = st_poke st in
        let tbl' = M.insert k st' tbl in
        let tc'  = succ (dd_touchCt dd) in
        let dd'  = dd { dd_touchCt = tc', dd_table = tbl', dd_flush = False } in
        dd' `seq` writeIORef (da_data da) dd' >>
        when (1 == tc') (activateDaggr da)

-- Propagate touch. Potentially detect cycles. Cycles are reliably
-- detected on the first `ln_touch` that touches the cycle, and need
-- be broken only in one location, so break them where detected.
--
-- Assumptions: this is called only on the first touch in a step.
activateDaggr :: DemandAggr e z -> IO () 
activateDaggr da = 
    ln_touch (da_link da) >>
    readIORef (da_data da) >>= \ dd -> 
    let tc = dd_touchCt dd in
    let bCycleDetected = (tc > 1) in
    when bCycleDetected (onNextPhase (breakDaggrCycle da))
    where onNextPhase = pd_phaseDelay (da_partd da)

-- extract an update from DemandData (pending or otherwise)
getPendingUpdateDD :: DemandData e z -> SigUp z
getPendingUpdateDD dd = 
    case dd_pending dd of
        Nothing -> SigUp { su_stable = dd_stable dd, su_state = Nothing } 
        Just su -> su

-- After we perform a touch, we must perform an update. Cycles don't
-- allow us to wait for touches in step. Consistency requires update
-- be atomic with respect to a step. In this case, we'll either send
-- a NOP update or an update for last step (if pending).
breakDaggrCycle :: DemandAggr e z -> IO ()
breakDaggrCycle da =
    readIORef (da_data da) >>= \ dd ->
    let tc = dd_touchCt dd in
    assert (tc > 0) $
    let tc' = succ tc in -- extra touch to block normal updates
    let su = getPendingUpdateDD dd in
    let dd' = dd { dd_touchCt = tc', dd_pending = Nothing, dd_stable = (su_stable su) } in
    dd' `seq` writeIORef (da_data da) dd' >> -- 
    pd_endOfStep (da_partd da) (repairDaggrCycle da) >> -- later handle this step's updates
    ln_update (da_link da) su -- force an update to break cycles

-- update the DemandAggr and process it (if last update)
updateDaggr :: DemandAggr e z -> Key -> SigUp e -> IO ()
updateDaggr da k su = 
    readIORef (da_data da) >>= \ dd ->
    let dd' = updateDD k su dd in
    let bFinal = (0 == dd_touchCt dd') in
    dd' `seq` writeIORef (da_data da) dd' >>
    when bFinal (deliverDaggr da)
    
updateDD :: Key -> SigUp e -> DemandData e z -> DemandData e z
updateDD k su dd = dd'
    where tbl  = dd_table dd
          st   = fromMaybe st_zero $ M.lookup k tbl
          st'  = assert (st_expect st) $ st_sigup su st 
          tbl' = M.insert k st' tbl
          tc   = dd_touchCt dd  
          tc'  = assert (tc > 0) $ pred tc
          tu   = snd `fmap` su_state su
          tmup'= leastTime (dd_tmup dd) tu
          dd'  = dd { dd_touchCt = tc', dd_tmup = tmup', dd_table = tbl' }

-- deliverDaggr will process the normal DemandAggr update. This will
-- prepare and deliver the update. 
deliverDaggr :: DemandAggr e z -> IO ()
deliverDaggr da = 
    processDaggr da >>
    readIORef (da_data da) >>= \ dd ->
    let su = getPendingUpdateDD dd in
    let dd' = dd { dd_pending = Nothing, dd_stable = (su_stable su) } in
    dd' `seq` writeIORef (da_data da) dd' >>
    when (dd_flush dd) (scheduleDaggrFlush da) >>
    ln_update (da_link da) su

-- After we break a demand aggregation cycle, we must recover it in
-- a later step. Here, we'll decide whether to schedule for the next
-- step or next heartbeat. Or maybe drop a NOP update.
repairDaggrCycle :: DemandAggr e z -> IO ()
repairDaggrCycle da = 
    clearCycleTouch >> -- clear the touch from breakDaggrCycle
    processDaggr da >> -- compute next update
    readIORef (da_data da) >>= \ dd ->
    let su = getPendingUpdate dd in
    let bUrgent = isUrgentUpdate (dd_stable dd) su in
    let bMustUp = not (isNothing (su_state su)) in
    if bUrgent 
        then pd_stepDelay (da_partd da) (pseudoUpdate da) 
        else when bMustUp (scheduleFlushDaggr da)
    where clearCycleTouch =
            readIORef (da_data da) >>= \ dd ->
            assert (1 == dd_touchCt dd) $ -- all incoming updates received!
            let dd' = dd { dd_touchCt = 0 } in
            writeIORef (da_data da) dd'

-- processDaggr will compute a new pending update and GC the demand
-- data. Stability for the update is set based on the partition 
-- clock, independent of demand sources. The reported stability is
-- not change, since processDaggr only computes the next update
-- without reporting it.
processDaggr :: DemandAggr e z -> IO ()
processDaggr da = 
    readIORef (da_data da) >>= \ dd ->
    pd_time (da_partd da) >>= \ tClock ->
    let tTgt = tClock `subtractTime` dtDaggrHist in -- target stability
    let tRec = dd_stable dd <|> Just tTgt in -- effective recorded stability
    let tbl  = dd_table dd in
    let lst  = M.toAscList tbl in
    let lst' = mapMaybe (daggrTableGC tTgt) lst in -- GC of demand sources
    let tbl' = M.fromAscList lst' in
    let done = null lst' in
    let bInactive = all isNothing (fmap (st_stable . snd) lst') in
    let bNeedFlush = bInactive && not done in
    let tStable = if done then Nothing else Just tTgt in
    let tUp = max <$> tRec <*> dd_tmup dd in -- chops straggling updates.
    let suNew = case tUp of
            Nothing -> SigUp { su_state = Nothing, su_stable = tStable } 
            Just tu -> let eSigs = ((`s_trim` tu) . st_signal . snd) `fmap` lst in
                       let zSig  = dd_nzip dd eSigs in
                       SigUp { su_state = Just (zSig,tu), su_stable = tStable }
    in
    let su = maybe suNew (`su_piggyback` suNew) (dd_pending dd) in
    let dd' = dd { dd_pending = Just su
                 , dd_table = tbl'
                 , dd_tmup = Nothing
                 }
    in 
    su `seq` dd' `seq`
    writeIORef (da_data da) dd' >>
    when bNeedFlush (scheduleFlushDaggr da)


-- Garbage Collection of DemandAggr elements. An demand source can
-- be eliminated if it is fully stable and all its active values are
-- in the past. (We'll hold onto unstable, inactive demand sources
-- to regulate stability of the demand monitor.)
daggrTableGC :: T -> (Key, SigSt e) -> Maybe (Key, SigSt e)
daggrTableGC tm (k,st) =
    let (x,sf) = s_sample (st_signal st) tm in
    let bDone = isNothing (st_stable st) && 
                isNothing x && s_is_final sf tm 
    in
    if bDone then Nothing
             else Just (k, SigSt { st_signal = sf })

-- Schedule a flushDaggr operation for the next heartbeat. Ensures
-- that no more than one flush is scheduled (for bounded space).
-- Flush may be canceled and renewed by intermediate udpates.
scheduleFlushDaggr :: DemandAggr e z -> IO ()
scheduleFlushDaggr da = 
    readIORef (da_data da) >>= \ dd ->
    let bSchedFlush = not (dd_flushSched dd) in
    let dd' = dd { dd_flush = True, dd_flushSched = True } in
    dd' `seq` writeIORef (da_data da) dd' >>
    when bSchedFlush (pd_eventually (da_partd da) (flushDaggr da))

-- a `flushDaggr` operation is a pseudo-update (doesn't modify the
-- table) intended to support cleanup requirements. Idempotent, and
-- may be canceled by intermediate updates to the DemandAggr. 
flushDaggr :: DemandAggr e z -> IO ()
flushDaggr da =
    readIORef (da_data da) >>= \ dd ->
    let bDoFlush = dd_flush dd in
    let dd' = dd { dd_flush = False, dd_flushSched = False } in
    dd' `seq` writeIORef (da_data da) dd' >>
    when bDoFlush (pseudoUpdate da)

-- a pseudo-update acts much like a DemandLnk update, except it does
-- not modify the table of demand sources. This is used for flush or
-- step-delay of cycles.
pseudoUpdate :: DemandAggr e z -> IO ()
pseudoUpdate da = updateLater >> touchNow 
    where touchNow =
            readIORef (da_partd da) >>= \ dd ->
            let tc' = succ (dd_touchCt dd) in
            let dd' = dd { dd_touchCt = tc', dd_flush = False } in
            dd' `seq` writeIORef (da_partd da) dd' >>
            when (1 == tc') (activateDaggr da)
          updateLater = pd_phaseDelay (da_partd da) $
            readIORef (da_data da) >>= \ dd ->
            let tc' = pred (dd_touchCt dd) in
            assert (tc' >= 0) $
            let dd' = dd { dd_touchCt = tc' } in
            dd' `seq` writeIORef (da_data da) dd' >>
            when (0 == tc') (deliverDaggr da)


leastTime :: Maybe T -> Maybe T -> Maybe T
leastTime l r = (min <$> l <*> r) <|> l <|> r

-- | MonitorDist supports output to multiple observers (monitors) of
-- a resource or signal. A MonitorDist has three responsibilities:
-- 
--   * distribute a signal to many observers
--   * mask input signal with observer's query
--   * keep some history for late-arriving observers
--
-- This is mostly quite simple. The main difficulty is the GC aspect
-- when the main `z` signal becomes inactive. The inactive signal
-- may still have some vestigial values that must be cleared over on
-- the heartbeat (and adjusted for dtMdistHist).
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
    , mdd_cleanup   :: {-# UNPACK #-} !Bool      -- cleanup on heartbeat?
    , mdd_nextKey   :: {-# UNPACK #-} !Key       -- next key for table!   
    , mdd_table     :: !(M.Map Key (LnkUp z))    -- to alert active observers
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


-- | Each MonitorDist has one main LnkUp where it receives a primary
-- signal, which is later distributed to observers. The primary must
-- be updated from only one source in the same partition. Updates 
-- are immediately delivered to all observing links. In addition, the
-- signal value is stored for late-arriving observers or observers 
-- that otherwise cannot immediately process the update. 
primaryMonitorLnk :: MonitorDist z -> LnkUp z
primaryMonitorLnk md = LnkUp { ln_touch = touch, ln_update = update }
    where touch = 
            readIORef (md_data md) >>= \ mdd ->
            let st = mdd_signal mdd in
            unless (st_expect st) $
                let lOut = M.elems (mdd_table mdd) in
                let st' = st_poke st in
                let mdd' = mdd { mdd_signal = st' } in
                writeIORef (md_data md) mdd' >> -- record touch
                mapM_ ln_touch lOut -- touch all active observers
          update su =
            readIORef (md_data md) >>= \ mdd ->
            let lOut = M.elems (mdd_table mdd) in
            let st   = mdd_signal mdd in
            let st'  = assert (st_expect st) $ st_sigup su st in
            let mdd' = mdd { mdd_signal = st',  } in
            writeIORef (md_data md) mdd' >>
            -- cleanup at end of step to simplify observers
            pd_endOfStep (md_partd md) (cleanupMD md) >>
            -- distribute update to all observers!
            mapM_ (`ln_update` su) lOut
    
-- end of round cleanup or GC
cleanupMD :: MonitorDist z -> IO ()
cleanupMD md = 
    

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



