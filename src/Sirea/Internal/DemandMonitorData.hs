
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
    , MonitorDist, newMonitorDist, primaryMonitorLnk, newMonitorLnk
    ) where

import Data.IORef
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import qualified Data.Map.Strict as M
import Control.Applicative
import Control.Monad (unless, when)
import Control.Exception (assert)
import Sirea.Signal
import Sirea.Time
import Sirea.Link
import Sirea.Partition
import Sirea.PCX
import Sirea.Internal.BCross (isUrgentUpdate) -- to avoid duplication of code
import Sirea.Internal.Tuning (dtDaggrHist, dtMdistHist)
import Sirea.Internal.LTypes -- for convenient SigSt, et. al.

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
    , dd_flush      :: !Bool                     -- GC flush requested?
    , dd_flushSched :: !Bool                     -- is GC flush scheduled?
    , dd_tmup       :: !(Maybe T)                -- time of earliest demand update
    , dd_stable     :: !(Maybe T)                -- time of reported stability
    , dd_nextKey    :: {-# UNPACK #-} !Key       -- next key for the table
    , dd_table      :: !(M.Map Key (SigSt e))    -- track active demands
    , dd_pending    :: !(Maybe (SigUp z))        -- any undelivered update.
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

ddZero :: DemandData e z
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
    let getKey = loadDaggrKey da rfK in
    let touch = getKey >>= touchDaggr da in
    let update su = getKey >>= \ k -> updateDaggr da k su in
    let lu = LnkUp { ln_touch = touch, ln_update = update } in
    return lu

-- keys are computed with explicitly lazy IO, via an IORef
-- (this helps assure me of which thread it runs in)
loadDaggrKey :: DemandAggr e z -> IORef (Maybe Key) -> IO Key
loadDaggrKey da rfK =
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
        dd' `seq` 
        writeIORef (da_data da) dd' >>
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
    dd' `seq` 
    writeIORef (da_data da) dd' >> -- 
    pd_endOfStep (da_partd da) (repairDaggrCycle da) >> -- later handle this step's updates
    ln_update (da_link da) su -- force an update to break cycles

-- update the DemandAggr and process it (if last update)
updateDaggr :: DemandAggr e z -> Key -> SigUp e -> IO ()
updateDaggr da k su = 
    readIORef (da_data da) >>= \ dd ->
    let dd' = updateDD k su dd in
    let bFinal = (0 == dd_touchCt dd') in
    dd' `seq`
    writeIORef (da_data da) dd' >>
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
    dd' `seq` 
    writeIORef (da_data da) dd' >>
    ln_update (da_link da) su

-- After we break a demand aggregation cycle, we must recover it in
-- a later step. Here, we'll decide whether to schedule for the next
-- step or next heartbeat. Or maybe drop a NOP update.
repairDaggrCycle :: DemandAggr e z -> IO ()
repairDaggrCycle da = 
    clearCycleTouch >> -- clear the touch from breakDaggrCycle
    processDaggr da >> -- compute next update
    readIORef (da_data da) >>= \ dd ->
    let su = getPendingUpdateDD dd in
    let bUrgent = isUrgentUpdate (dd_stable dd) su in
    let bMustUp = not (isNothing (su_state su)) in
    if bUrgent 
        then pd_stepDelay (da_partd da) (pseudoUpdate da) 
        else when bMustUp (scheduleFlushDaggr da)
    where clearCycleTouch =
            readIORef (da_data da) >>= \ dd ->
            assert (1 == dd_touchCt dd) $ -- should be finished with updates
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
    let tUp = max <$> tRec <*> dd_tmup dd in -- chop any straggling updates.
    let suNew = case tUp of
            Nothing -> SigUp { su_state = Nothing, su_stable = tStable } 
            Just tu -> let eSigs = ((`s_trim` tu) . st_signal . snd) `fmap` lst in
                       let zSig  = da_nzip da eSigs in
                       zSig `seq`
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
    let st' = st { st_signal = sf } in
    if bDone then Nothing else Just (k, st')

-- Schedule a flushDaggr operation for the next heartbeat. Ensures
-- that no more than one flush is scheduled (for bounded space).
-- Flush may be canceled and renewed by intermediate udpates.
scheduleFlushDaggr :: DemandAggr e z -> IO ()
scheduleFlushDaggr da = 
    readIORef (da_data da) >>= \ dd ->
    let bSchedFlush = not (dd_flushSched dd) in
    let dd' = dd { dd_flush = True, dd_flushSched = True } in
    writeIORef (da_data da) dd' >>
    when bSchedFlush (pd_eventually (da_partd da) (flushDaggr da))

-- a `flushDaggr` operation is a pseudo-update (doesn't modify the
-- table) intended to support cleanup requirements. Idempotent, and
-- may be canceled by intermediate updates to the DemandAggr. 
flushDaggr :: DemandAggr e z -> IO ()
flushDaggr da =
    readIORef (da_data da) >>= \ dd ->
    let bDoFlush = dd_flush dd in
    let dd' = dd { dd_flush = False, dd_flushSched = False } in
    writeIORef (da_data da) dd' >>
    when bDoFlush (pseudoUpdate da)

-- a pseudo-update acts much like a DemandLnk update, except it does
-- not modify the table of demand sources. This is used for flush or
-- step-delay of cycles.
pseudoUpdate :: DemandAggr e z -> IO ()
pseudoUpdate da = updateLater >> touchNow 
    where touchNow =
            readIORef (da_data da) >>= \ dd ->
            let tc' = succ (dd_touchCt dd) in
            let dd' = dd { dd_touchCt = tc', dd_flush = False } in
            writeIORef (da_data da) dd' >>
            when (1 == tc') (activateDaggr da)
          updateLater = onNextPhase $
            readIORef (da_data da) >>= \ dd ->
            let tc' = pred (dd_touchCt dd) in
            assert (tc' >= 0) $
            let dd' = dd { dd_touchCt = tc' } in
            writeIORef (da_data da) dd' >>
            when (0 == tc') (deliverDaggr da)
          onNextPhase = pd_phaseDelay (da_partd da)


leastTime :: Maybe T -> Maybe T -> Maybe T
leastTime l r = (min <$> l <*> r) <|> l <|> r

-- | MonitorDist supports output to multiple observers (monitors) of
-- a resource or signal. For simplicity, the main signal is stored 
-- in only one place to support observers. For simple GC, observers
-- are kept in a simple table (modeled with a map). 
data MonitorDist z = MonitorDist 
    { md_data       :: !(IORef (MDD z))   -- mutable data of MonitorDist
    , md_partd      :: !(PartD)           -- scheduler and time
    }

-- mutable data for the full MonitorDist
data MDD z = MDD
    { mdd_signal    :: !(SigSt z)           -- track current signal
    , mdd_tmup      :: !(Maybe T)           -- time of recent update
    , mdd_flush     :: !Bool                -- special update on hearbeat?
    , mdd_nextKey   :: {-# UNPACK #-} !Key  -- next key for table!   
    , mdd_table     :: !(M.Map Key (MLN z)) -- set of active observers
    }

data MLN z = MLN 
    { mln_lzout     :: !(LnkUp z)           -- final output (after mask!)
    , mln_signal    :: !(SigSt ())          -- observer query (mask)
    , mln_tmup      :: !(Maybe T)           -- tracks observed update time
    }

-- | track a new set of observers
newMonitorDist :: (Partition p) => PCX p -> Sig z -> IO (MonitorDist z)
newMonitorDist cp z0 = MonitorDist 
    <$> newIORef (mddZero z0)
    <*> pure (mkPartD cp)

mddZero :: Sig z -> MDD z
mddZero z0 = MDD
    { mdd_signal  = st_zero { st_signal = z0 } 
    , mdd_tmup    = Nothing
    , mdd_flush   = False
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
                let st' = st_poke st in
                let mdd' = mdd { mdd_signal = st' } in
                let lst = M.elems (mdd_table mdd) in
                writeIORef (md_data md) mdd' >>
                mapM_ touchOne lst
          touchOne mln =
            unless (st_expect (mln_signal mln)) $
                ln_touch (mln_lzout mln)
          update su =
            readIORef (md_data md) >>= \ mdd ->
            let st   = mdd_signal mdd in
            assert (st_expect st) $
            let st'  = st_sigup su st in
            let tu   = snd `fmap` su_state su in
            let lst  = M.elems (mdd_table mdd) in
            let mdd' = mdd { mdd_signal = st', mdd_tmup = tu } in
            mdd' `seq` writeIORef (md_data md) mdd' >>  -- track state
            pd_endOfStep (md_partd md) (cleanupMD md) >> -- schedule GC
            pd_time (md_partd md) >>= \ tNow -> -- may need for effective stability
            mapM_ (updateMD tNow mdd') lst
 
-- update MLN.
--
-- The main difficulty seems to be recognition of a `final` update,
-- and GC of historical data.  Does not perform any GC, only delivers update if
-- possible at the given moment. The main difficulty here seems
-- to be deciding when to deliver the "final" update for a given
-- MLN observer. That can occur if the effective stability is
-- greater than the 
updateMD :: T -> MDD z -> MLN z -> IO ()
updateMD tNow mdd mln =
    let bWait = st_expect (mdd_signal mdd) || st_expect (mln_signal mln) in
    unless bWait $
        error "TODO!"
    {-
        let tmup  = leastTime (mdd_tmup mdd) (mln_tmup mln) in
        let tStable = 
        let su    = case tmup of
                Nothing -> SigUp { su_
    -}

cleanupMD :: MonitorDist z -> IO ()
cleanupMD = error "TODO!"
 

{-

    
-- End of round cleanup for MonitorDist. Simply cuts recorded signal
-- based on current time, and possibly schedules further cleanup on
-- a later heartbeat (if necessary). 
cleanupMD :: MonitorDist z -> IO ()
cleanupMD md = 
    readIORef (md_data md) >>= \ mdd ->
    pd_time (md_partd md) >>= \ tClock ->
    let tCut = tClock `subtractTime` dtMdistHist in
    let st = mdd_signal mdd in
    let st' = st_clear tCut st in
    let bNeedsMoreCleanup = -- heartbeat-based cleanup
            isNothing (st_stable st) &&
            not (s_is_final tCut (st_signal st'))
    in
    let mdd' = mdd { mdd_signal = st' } in
    mdd' `seq` 
    writeIORef (md_data md) mdd' >>
    when bNeedsMoreCleanup (schedCleanupMD md)

-- Schedule further cleanup on a later heartbeat. This is idempotent
-- to ensure that only one cleanup is scheduled on next heartbeat.
-- (MonitorDist might cleanup twice in a round if it is then updated
-- in the heartbeat round. But this is okay; cleanup is idempotent.)
schedCleanupMD :: MonitorDist z -> IO ()
schedCleanupMD md =
    readIORef (md_data md) >>= \ mdd ->
    unless (mdd_cleanup mdd) $
        let mdd' = mdd { mdd_cleanup = True } in
        writeIORef (md_data md) mdd' >>
        eventually runCleanup
    where eventually = pd_eventually (md_partd md)
          atEndOfStep = pd_endOfStep (md_partd md)
          runCleanup = 
            readIORef (md_data md) >>= \ mdd ->
            let mdd' = mdd { mdd_cleanup = False } in
            writeIORef (md_data md) mdd' >>
            atEndOfStep (cleanupMD md)

-- install, returning the key     
installMD :: MonitorDist z -> LnkUp z -> IO Key
installMD md lz =
    readIORef (md_data md) >>= \ mdd ->
    let k = succ (mdd_nextKey mdd) in
    let tbl' = M.insert k lz (mdd_table mdd) in
    let bExpecting = st_expect (mdd_signal mdd) in
    let mdd' = mdd { mdd_nextKey = k, mdd_table = tbl' } in
    k `seq` mdd' `seq` 
    writeIORef (md_data md) mdd' >>
    return k

-- uninstall, using the key. The key should not be used again.
uninstallMD :: MonitorDist z -> Key -> IO ()
uninstallMD md k =
    readIORef (md_data md) >>= \ mdd ->
    assert (M.member k (mdd_table mdd)) $
    let tbl' = M.delete k (mdd_table mdd) in
    let mdd' = mdd { mdd_table = tbl' } in
    mdd' `seq` 
    writeIORef (md_data md) mdd'

newMonLn :: MonitorDist z -> LnkUp z -> IO (MonLn z)
newMonLn md lzo = MonLn 
    <$> newIORef mldZero
    <*> pure lzo 
    <*> pure md

mldZero :: MLD
mldZero = MLD 
    { mld_key = Nothing
    , mld_signal = st_zero
    , mld_tmup = Nothing
    , mld_cleanup = False
    }

-}

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
newMonitorLnk md lzo = 
    newIORef Nothing >>= \ rfK ->
    let getKey = loadMDKey md rfK in
    let touch = getKey >>= touchMLN md lzo in
    let update su = getKey >>= \ k -> updateMLN md k su in
    let lu = LnkUp { ln_touch = touch, ln_update = update } in
    return lu

loadMDKey :: MonitorDist z -> IORef (Maybe Key) -> IO Key
loadMDKey md rfK =
    readIORef rfK >>= \ mbK ->
    case mbK of
        Just k -> return k
        Nothing ->
            readIORef (md_data md) >>= \ mdd ->
            let k = succ (mdd_nextKey mdd) in
            let mdd' = mdd { mdd_nextKey = k } in
            k `seq` mdd' `seq`
            writeIORef (md_data md) mdd' >>
            writeIORef rfK (Just k) >>
            return k

touchMLN :: MonitorDist z -> LnkUp z -> Key -> IO ()
touchMLN = error "TODO!"

updateMLN :: MonitorDist z -> Key -> SigUp () -> IO ()
updateMLN = error "TODO!"

{-

-- install MonLn to observe the associated MonitorDist
installMonLn :: MonLn z -> IO ()
installMonLn mln =
    readIORef (mln_data mln) >>= \ mld ->
    when (isNothing (mld_key mld)) $
        -- install and track key
        let lzi = monLnRecvZ mln in
        installMD (mln_mdist mln) lzi >>= \ k ->
        let mld' = mld { mld_key = Just k } in
        writeIORef (mln_data mln) mld' >>
        -- potentially deliver touch (if waiting on z)
        readIORef (md_data (mln_mdist mln)) >>= \ mdd ->
        let bTouchZ = st_expect (mdd_signal mdd) in
        when bTouchZ (ln_touch lzi)

uninstallMonLn :: MonLn z -> IO ()
uninstallMonLn mln =
    readIORef (mln_data mln) >>= \ mld ->
    case (mld_key mld) of
        Nothing -> return ()
        Just k ->
            let mld' = mld { mld_key = Nothing } in
            mld' `seq` 
            writeIORef (mln_data mln) mld' >>
            uninstallMD (mln_mdist mln) k

-- the main input link for a monitor link. The main `z` signal is 
-- stored at an associated MonitorDist, so this mostly forwards the
-- touch and tracks the update times.
monLnRecvZ :: MonLn z -> LnkUp z
monLnRecvZ mln = LnkUp { ln_touch = touch, ln_update = update} where
    touch = 
        readIORef (mln_data mln) >>= \ mld ->
        let bAlreadyTouchedO = st_expect (mld_signal mld) in
        unless bAlreadyTouchedO (ln_touch (mln_lzout mln))
    update su =
        readIORef (mln_data mln) >>= \ mld ->
        let bWaitingOnO = st_expect (mld_signal mld) in
        let tmup' = leastTime (mld_tmup mld) (snd `fmap` su_state su) in
        let mld' = mld { mld_tmup = tmup' } in
        writeIORef (mln_data mln) mld' >>
        unless bWaitingOnO (monLnEmit mln)

-- observer input for a monitor link. The observed `o` signal is
-- stored locally, and may need to be GC'd on heartbeats in some
-- special cases. 
monLnRecvO :: MonLn z -> LnkUp o
monLnRecvO mln = LnkUp { ln_touch = touch, ln_update = update} where
    md = mln_mdist mln
    testZTouched = (st_expect . mdd_signal) <$> readIORef (md_data md)
    touch = 
        installMonLn mln >> -- install whenever active
        readIORef (mln_data mln) >>= \ mld ->
        let st = mld_signal mld in
        unless (st_expect st) $
            let st' = st_poke st in
            let mld' = mld { mld_signal = st' } in
            writeIORef (mln_data mln) mld' >>
            testZTouched >>= \ bZTouched ->
            unless bZTouched (ln_touch (mln_lzout mln))
    update su = 
        readIORef (mln_data mln) >>= \ mld ->
        let st = mld_signal mld in
        assert (st_expect st) $
        let st' = st_sigup su st in
        let tmup' = leastTime (mld_tmup mld) (snd `fmap` su_state su) in
        let mld' = mld { mld_signal = st', mld_tmup = tmup' } in
        writeIORef (mln_data mln) mld' >>
        testZTouched >>= \ bWaitingOnZ ->
        unless bWaitingOnZ (monLnEmit mln)

-- emit the primary update for a monitor link after both signals are
-- stable within the step, also initiates cleanup (possibly with
-- extra cleanup scheduled in future steps). 
monLnEmit :: MonLn z -> IO ()
monLnEmit mln =
    let md = mln_mdist mln in
    readIORef (mln_data mln) >>= \ mld ->
    readIORef (md_data md) >>= \ mdd ->
    pd_time (md_partd md) >>= \ tNow ->
    let stO = mld_signal mld in
    let stZ = mdd_signal mdd in
    -- at this point, we should be ready to emit, not waiting.
    assert ((not . st_expect) stO && (not . st_expect) stZ) $

    -- TODO: Some bad logic here. The `tStable` value must be set
    --   to Nothing in the special case where stO is fully stable
    --   and final beyond the point to which stZ is stable.
    undefined

-}

{-


    let tStableZ = st_stable stZ <|> Just (tNow `subtractTime` dtDaggrHist) in
    let tStableO = st_stable stO in
    let tStable = leastTime tStableZ tStableO in
    let sig = s_mask (st_signal stZ) (st_signal stO) in
    let su  = case (mdd_tmup mdd) of
            Nothing -> SigUp { su_state = Nothing, su_stable = tStable } 
            Just tu -> SigUp { su_state = Just (sig, tu), su_stable = tStable }
    in
    let (st',bUninstall,bSchedCleanup) = mldSigCleanup stO tStable in
    let mld' = mld { mld_tmup = Nothing, mld_signal = st' } in
    mld' `seq` writeIORef (mln_data mln) mld' >>
    when bUninstall (uninstallMonLn mln) >>
    when bSchedCleanup (schedMonLnCleanup mln) >>
    ln_update (mln_lzout mln) su

-- cleanup the signal given a stability value. Returns a triple:
--    (st', bUninstall, bSchedCleanup)
-- st' - the post-clean signal
-- bUninstall - don't need to listen for more `z` updates
-- bSchedCleanup - schedule a delayed cleanup
mldSigCleanup :: SigSt () -> Maybe T -> (SigSt (),  Bool, Bool)
mldSigCleanup _ Nothing = (st_zero, True, False)
mldSigCleanup st (Just tm) = (st',bUninstall,bSchedCleanup) where 
    st' = 

-- the cleanup operation; unfortunately duplicates some computation
-- from the update operation, but that's a tradeoff for a simpler
-- external interface and to support 
monLnCleanup :: MonLn z -> IO ()    
monLnCleanup mln =
    let md = mln_mdist mln in
    readIORef (mln_data mln) >>= \ mld ->
    readIORef (md_data md) >>= \ mdd ->
    pd_time (md_partd md) >>= \ tNow ->
    let stO' = maybe st_zero (`st_clear` stO) tStable in
    let bFinal = maybe True (s_term (st_signal stO')) tStable in
    let bNeedsMoreCleanup = not bFinal &&
                            isNothing (st_stable stZ) && 
                            isNothing (st_stable stO)
    in
    
    let bNeedsExtra
    
    -}


        

    

                
            




