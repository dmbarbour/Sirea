
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
--
-- TODO: Consider switching to hashtables for performance. Could be
-- a considerable improvement for performance.
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
    assert (tc > 0) $ -- should have pending update (due to cycle)
    let tc' = succ tc in -- extra touch to block 'normal' updates
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
-- clock, independent of demand sources. The reported stability will
-- not change, since processDaggr only computes the next update
-- without reporting it.
processDaggr :: DemandAggr e z -> IO ()
processDaggr da = 
    readIORef (da_data da) >>= \ dd ->
    assert (0 == dd_touchCt dd) $
    pd_time (da_partd da) >>= \ tClock ->
    let tTgt = tClock `subtractTime` dtDaggrHist in -- target stability
    let tRec = dd_stable dd <|> Just tTgt in -- effective recorded stability
    let tbl  = dd_table dd in
    let lst  = M.toAscList tbl in
    let lst' = mapMaybe (daggrTableGC tTgt) lst in -- GC of demand sources
    let tbl' = M.fromAscList lst' in
    let done = null lst' in
    let bInactive = all (isNothing . st_stable . snd) lst' in
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
scheduleFlushDaggr da = schedFlush where
    schedFlush =
        readIORef (da_data da) >>= \ dd ->
        let bSchedFlush = not (dd_flushSched dd) in
        let dd' = dd { dd_flush = True, dd_flushSched = True } in
        dd' `seq` writeIORef (da_data da) dd' >>
        when bSchedFlush (pd_eventually (da_partd da) flush)
    flush = 
        readIORef (da_data da) >>= \ dd ->
        let bDoFlush = dd_flush dd in
        let dd' = dd { dd_flush = False, dd_flushSched = False } in
        dd' `seq` writeIORef (da_data da) dd' >>
        when bDoFlush (pseudoUpdate da)

-- a pseudo-update acts much like a DemandLnk update, except it does
-- not modify the table of demand sources. This is used for flush or
-- step-delay updates to resolve cycles. It will not interfere with
-- normal updates in the same step.
pseudoUpdate :: DemandAggr e z -> IO ()
pseudoUpdate da = updateLater >> touchNow 
    where touchNow =
            readIORef (da_data da) >>= \ dd ->
            let tc' = succ (dd_touchCt dd) in
            let dd' = dd { dd_touchCt = tc', dd_flush = False } in
            writeIORef (da_data da) dd' >>
            when (1 == tc') (activateDaggr da)
          updateLater = pd_phaseDelay (da_partd da) update
          update = 
            readIORef (da_data da) >>= \ dd ->
            let tc' = pred (dd_touchCt dd) in
            assert (tc' >= 0) $
            let dd' = dd { dd_touchCt = tc' } in
            writeIORef (da_data da) dd' >>
            when (0 == tc') (deliverDaggr da)


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
    , mdd_flush     :: !Bool                -- scheduled heartbeat GC flush?
    , mdd_cleanup   :: !Bool                -- scheduled cleanup this step?
    , mdd_tmup      :: !(Maybe T)           -- time of recent update
    , mdd_nextKey   :: {-# UNPACK #-} !Key  -- next key for table!   
    , mdd_table     :: !(M.Map Key (MLN z)) -- set of active observers
    }

data MLN z = MLN 
    { mln_lzout     :: !(LnkUp z)           -- final output (after mask!)
    , mln_signal    :: !(SigSt ())          -- observer query (the mask)
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
--
-- Each monitor link might be touched twice per round - once for the
-- primary Z input, once for the mask. This should be okay; touch is
-- assumed to be idempotent.
--
primaryMonitorLnk :: MonitorDist z -> LnkUp z
primaryMonitorLnk md = LnkUp { ln_touch = touch, ln_update = update }
    where touch = 
            readIORef (md_data md) >>= \ mdd ->
            let st = mdd_signal mdd in
            unless (st_expect st) $
                let st' = st_poke st in
                let mdd' = mdd { mdd_signal = st' } in
                let lst = M.elems (mdd_table mdd) in
                writeIORef (md_data md) mdd' >> -- record touch
                mapM_ (ln_touch . mln_lzout) lst -- touch each observer
          update su =
            -- record update
            readIORef (md_data md) >>= \ mdd ->
            let st = mdd_signal mdd in
            assert (st_expect st) $
            let st' = st_sigup su st in -- updated signal, not yet GC'd
            let tu = fmap snd (su_state su) in -- time of update
            let bSchedCleanup = not (mdd_cleanup mdd) in
            let mdd' = mdd { mdd_signal = st', mdd_tmup = tu
                           , mdd_cleanup = True } 
            in
            mdd' `seq` writeIORef (md_data md) mdd' >>
            -- must perform cleanup at end of step (i.e. to clear mdd_tmup)
            when bSchedCleanup (schedCleanupMD md) >>
            -- deliver updates to ready observers.
            let lst = filter (not . st_expect . mln_signal) 
                             (M.elems (mdd_table mdd')) 
            in
            pd_time (md_partd md) >>= \ tNow ->
            mapM_ (deliverUpdateMD tNow mdd') lst

-- deliverUpdateMD will deliver the actual update to the observer,
-- providing a masked signal update if necessary or just a stability
-- update. It is necessary that both inputs have received updates 
-- (if any in the current step) before this is executed. 
--
-- The stability for the update may be `Nothing` if this value will
-- be fully cleared. 
deliverUpdateMD :: T -> MDD z -> MLN z -> IO ()
deliverUpdateMD tNow mdd mln = assert bReady $ deliver where
    deliver = su `seq` ln_update (mln_lzout mln) su
    su = SigUp { su_state = suState, su_stable = suStable }
    bReady = bReadyMDD && bReadyMLN
    bReadyMDD = (not . st_expect . mdd_signal) mdd
    bReadyMLN = (not . st_expect . mln_signal) mln
    suState = case tu of
        Nothing -> Nothing
        Just tm -> let sigZ = s_trim ((st_signal . mdd_signal) mdd) tm in
                   let sigM = s_trim ((st_signal . mln_signal) mln) tm in
                   Just (s_mask sigZ sigM, tm)
    tu = leastTime (mdd_tmup mdd) (mln_tmup mln)
    suStable = if bDone then Nothing else Just tStableAbs
    bDone = isNothing ((st_stable . mln_signal) mln) &&
            s_term ((st_signal . mln_signal) mln) tStableZ
    tStableAbs = maybe tStableZ (min tStableZ) (st_stable (mln_signal mln))
    tStableZ = fromMaybe tDaggrHist (st_stable (mdd_signal mdd)) 
    tDaggrHist = tNow `subtractTime` dtDaggrHist

-- schedCleanupMD will handle GC of a MonitorDist. It schedules an
-- action at the end of the current step (to ensure all updates 
-- have been processed and delivered) to clear unnecessary data and
-- dead links. (A link is 'dead' if inactive in the present and 
-- foreseeable future.) In some cases, this may also schedule a
-- flush - basically a cleanup on a later heartbeat to handle any
-- time-bounded cleanup operations.
--
-- This assumes `mdd_cleanup` has already been set to true
schedCleanupMD :: MonitorDist z -> IO ()
schedCleanupMD md = schedCleanup where
    schedCleanup = pd_endOfStep (md_partd md) cleanup
    cleanup =
        pd_time (md_partd md) >>= \ tNow ->
        readIORef (md_data md) >>= \ mdd ->
        assert (mdd_cleanup mdd) $
        let mdd' = cleanMDD tNow mdd in
        let bSchedFlush = (mdd_flush mdd') && not (mdd_flush mdd) in
        mdd' `seq` writeIORef (md_data md) mdd' >>
        when bSchedFlush schedFlush
    schedFlush = pd_eventually (md_partd md) flush
    flush =
        readIORef (md_data md) >>= \ mdd ->
        assert (mdd_flush mdd) $
        let bSchedCleanup = not (mdd_cleanup mdd) in
        let mdd' = mdd { mdd_flush = False, mdd_cleanup = True } in
        writeIORef (md_data md) mdd' >>
        when bSchedCleanup schedCleanup
    cleanMDD tNow mdd = 
        assert ((not . st_expect . mdd_signal) mdd) $
        -- GC the main signal
        let st = mdd_signal mdd in
        let tDaggrHist = tNow `subtractTime` dtDaggrHist in
        let tStableZ = fromMaybe tDaggrHist (st_stable st) in
        let tMdistHist = tNow `subtractTime` dtMdistHist in
        let tGC = maybe tMdistHist (min tMdistHist) (st_stable st) in
        let st' = st_clear tGC st in -- GC'd signal
        -- GC observer link table
        let tbl = mdd_table mdd in
        let lst = M.toAscList tbl in
        let lst' = mapMaybe (cleanMLN tStableZ) lst in
        let tbl' = M.fromAscList lst' in
        -- compute need for flush 
        let bInactiveMDD = (isNothing . st_stable) st' in
        let bInactiveMLN = all (isNothing . st_stable . mln_signal . snd) lst' in
        let bInactive = bInactiveMDD && bInactiveMLN in
        let bCleanMDD = s_is_final (st_signal st') tGC in
        let bCleanMLN = null lst' in
        let bClean = bCleanMDD && bCleanMLN in
        let bNeedFlush = bInactive && not bClean in
        -- fresh MDD value
        MDD { mdd_signal = st'
            , mdd_tmup = Nothing
            , mdd_flush = bNeedFlush 
            , mdd_cleanup = False
            , mdd_nextKey = (mdd_nextKey mdd)
            , mdd_table = tbl'
            }
    cleanMLN tStableZ (k,mln) =
        assert ((not . st_expect . mln_signal) mln) $        
        let st = mln_signal mln in
        let tGC = maybe tStableZ (min tStableZ) (st_stable st) in
        let st' = st_clear tGC st in
        let bDone = s_term (st_signal st') tStableZ in
        let mln' = MLN { mln_signal = st'
                       , mln_tmup = Nothing
                       , mln_lzout = (mln_lzout mln) }
        in
        mln' `seq`
        if bDone then Nothing else Just (k,mln')

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

-- track expected updates to avoid redundant computations
touchMLN :: MonitorDist z -> LnkUp z -> Key -> IO ()
touchMLN md lu k = 
    readIORef (md_data md) >>= \ mdd ->
    let mln = fromMaybe (mlnZero lu) (M.lookup k (mdd_table mdd)) in
    let st = mln_signal mln in
    unless (st_expect st) $
        let st' = st_poke st in
        let mln' = mln { mln_signal = st' } in
        let tbl' = M.insert k mln' (mdd_table mdd) in
        let mdd' = mdd { mdd_table = tbl' } in
        mln' `seq` mdd' `seq` 
        writeIORef (md_data md) mdd' >>
        ln_touch (mln_lzout mln')

mlnZero :: LnkUp z -> MLN z 
mlnZero lzOut = 
    MLN { mln_lzout = lzOut
        , mln_signal = st_zero
        , mln_tmup = Nothing }

-- Process an update to the masking signal for an observer. The mask
-- describes durations of observation and ensures duration coupling.
-- An update may be necessary
--
-- Link updates will cause a full GC of the MonitorDist (and all 
-- observer links), at the end of the step.
updateMLN :: MonitorDist z -> Key -> SigUp () -> IO ()
updateMLN md k su = 
    readIORef (md_data md) >>= \ mdd ->
    let tbl = mdd_table mdd in
    let mln = case (M.lookup k tbl) of
                Nothing -> error "illegal state in monitor: link update without touch"
                Just x -> x
    in
    assert (st_expect (mln_signal mln)) $
    let st' = st_sigup su (mln_signal mln) in
    let tu  = fmap snd (su_state su) in
    let mln' = mln { mln_signal = st', mln_tmup = tu } in
    let tbl' = M.insert k mln' tbl in
    let bSchedCleanup = not (mdd_cleanup mdd) in
    let mdd' = mdd { mdd_table = tbl', mdd_cleanup = True } in
    let bReady = not (st_expect (mdd_signal mdd')) in
    let deliver = pd_time (md_partd md) >>= \ tNow ->
                  deliverUpdateMD tNow mdd' mln'
    in
    mln' `seq` mdd' `seq`
    writeIORef (md_data md) mdd' >>
    when bSchedCleanup (schedCleanupMD md) >>
    when bReady deliver



