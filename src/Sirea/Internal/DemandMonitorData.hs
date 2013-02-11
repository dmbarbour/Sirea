
-- Utilities for tracking demands and monitors of demand.
--
-- Currently this is separated into three responsibilities:
--
--   * aggregation of demands
--   * distribution of a signal to many observers
--
-- Demand aggregation in RDP occurs at external resources and allows
-- potential cycles to occur between components. Cycles can model
-- interactive systems and coordination patterns. Sirea will handle 
-- cycles robustly, but (at least for now) not efficiently. 
--
-- Ideally, I could detect cycles and clip them at a minimal subset
-- of locations, thus exposing less intermediate state and doing
-- minimal rework. Cycles have form: monitor >>> process >>> demand.
-- However, they are difficult to detect due to dynamic behavior and
-- intermediate processing. 
--
-- Until I have a robust, cheap way to detect cycles, I must assume
-- they exist. For now, demands are always delivered the step after
-- they become available. 
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
    , pollDemandAggr, pollMonitorDist
    ) where

import Data.IORef
import Data.Unique
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
import Sirea.Internal.Tuning (dtDaggrHist, dtMdistHist, dtFutureChoke)
import Sirea.Internal.LTypes -- for convenient SigSt, et. al.

--import Debug.Trace (traceIO)

-- TODO: Fix the future-choke mechanisms for DemandAggr so that I
-- don't really need choking elsewhere to control cycles.
--  (basically, allow waiting multiple heartbeats for flush.)

-- | DemandAggr: aggregates and processes many concurrent demands.
--
-- For now, DemandAggr will use a conservative mechanism for cycles:
-- updates in one step are always reported in a later step. This has
-- some unfortunate consequences for performance, resulting rework.
--
data DemandAggr e z = DemandAggr 
    { da_data       :: !(IORef (DemandData e z)) -- mutable state 
    , da_nzip       :: !([Sig e] -> Sig z)       -- compute the result signal
    , da_link       :: !(LnkUp z)                -- processes updated signal
    , da_psched     :: !PSched                   -- partition scheduler
    , da_ident      :: !Unique                   -- unique identifier for cycles
    }

data DemandData e z = DemandData
    { dd_flush      :: !Bool                     -- is hearbeat flush scheduled?
    --, dd_cut        :: !Bool                     -- break cycles at this DemandAggr?
    --, dd_stable     :: !StableT                  -- last reported stability
    , dd_touchCt    :: {-# UNPACK #-} !Int       -- active touches on demand aggr
    , dd_tmup       :: !(Maybe T)                -- time of earliest demand update
    , dd_table      :: !(M.Map Unique (SigSt e)) -- track active demand signals
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
    <*> getPSched cp
    <*> newUnique

ddZero :: DemandData e z
ddZero = DemandData
    { dd_flush      = False
    , dd_touchCt    = 0
    --, dd_cut        = False
    , dd_tmup       = Nothing
    , dd_table      = M.empty 
    }


-- | Create a new link to an existing demand aggregator.
newDemandLnk :: DemandAggr e z -> IO (LnkUp e)
newDemandLnk da = mkDemandLnk da <$> newUnique 

mkDemandLnk :: DemandAggr e z -> Unique -> LnkUp e
mkDemandLnk da k = LnkUp touch update idle where
    touch = touchDaggr da k 
    idle = idleDaggr da k
    update = updateDaggr da k

-- touch a particular demand signal (at key). I'll need to track
-- touch if we're ever to handle cycles properly; for now, it 
-- also identifies when the last update arrives.
touchDaggr :: DemandAggr e z -> Unique -> IO ()
touchDaggr da k =
    readIORef (da_data da) >>= \ dd ->
    let st = fromMaybe st_zero (M.lookup k (dd_table dd)) in
    unless (st_expect st) $
        let st' = st_poke st in
        let tbl' = M.insert k st' (dd_table dd) in
        let tc' = succ (dd_touchCt dd) in
        let dd' = dd { dd_touchCt = tc', dd_table = tbl' } in
        writeIORef (da_data da) dd' >>
        when (1 == tc') (firstTouchDaggr da)

-- called on first touch on a DemandAggr. Currently a NOP.
firstTouchDaggr :: DemandAggr e z -> IO ()
firstTouchDaggr _ = return ()

-- called on idle for a demand source
idleDaggr :: DemandAggr e z -> Unique -> StableT -> IO ()
idleDaggr da k tS = 
    readIORef (da_data da) >>= \ dd ->
    let st = fromMaybe st_zero (M.lookup k (dd_table dd)) in
    assert (st_expect st) $
    let st' = st_idle tS st in
    let tc' = pred (dd_touchCt dd) in
    let tbl' = M.insert k st' (dd_table dd) in
    let dd' = dd { dd_table = tbl', dd_touchCt = tc' } in
    dd' `seq` writeIORef (da_data da) dd' >>
    when (0 == tc') (finalUpdateDaggr da)

-- called on update for a demand source
updateDaggr :: DemandAggr e z -> Unique -> StableT -> T -> Sig e -> IO ()
updateDaggr da k tS tU su = 
    readIORef (da_data da) >>= \ dd ->
    let st = fromMaybe st_zero (M.lookup k (dd_table dd)) in
    assert (st_expect st) $
    let st' = st_update tS tU su st in
    let tc' = pred (dd_touchCt dd) in
    let tbl' = M.insert k st' (dd_table dd) in
    let tmup' = Just $! maybe tU (min tU) (dd_tmup dd) in
    let dd' = dd { dd_touchCt = tc', dd_table = tbl', dd_tmup = tmup' } in
    dd' `seq` writeIORef (da_data da) dd' >>
    when (0 == tc') (finalUpdateDaggr da)

-- called on last demand-source update in a round. This will make a
-- decision on whether the update occurs next round or on some
-- future heartbeat (flush).
finalUpdateDaggr :: DemandAggr e z -> IO ()
finalUpdateDaggr da = 
    let pd = da_psched da in
    p_stepTime pd >>= \ tNow ->
    readIORef (da_data da) >>= \ dd ->
    assert (0 == dd_touchCt dd) $
    let bUrgent = case dd_tmup dd of
            Nothing -> False
            Just tU -> (tU < addTime tNow dtFutureChoke)
    in
    if bUrgent then p_onNextStep pd (deliverDaggr da) else
    unless (dd_flush dd) $
        let dd' = dd { dd_flush = True } in
        writeIORef (da_data da) dd' >>
        p_eventually pd (flushDaggr da)

-- at the moment, flushDaggr is simply a GC operation unless there
-- is a pending update, in which case it will deliver the update.
flushDaggr :: DemandAggr e z -> IO ()
flushDaggr da = initFlush where
    initFlush = 
        readIORef (da_data da) >>= \ dd ->
        assert (dd_flush dd) $
        let bDeliver = (not . isNothing . dd_tmup) dd in
        if bDeliver then deliver dd else flush dd
    deliver dd =
        let dd' = dd { dd_flush = False } in
        writeIORef (da_data da) dd' >>
        deliverDaggr da
    flush dd =
        p_stepTime (da_psched da) >>= \ tNow ->
        let tGC = tNow `subtractTime` dtDaggrHist in
        let lst = M.toAscList (dd_table dd) in
        let lst' = mapMaybe (daggrTableGC tGC) lst in
        let tbl' = M.fromAscList lst' in
        let bFlushAgain = (not . null) lst' 
                       && all (isDoneT . st_stable . snd) lst' 
        in
        let dd' = dd { dd_table = tbl', dd_flush = bFlushAgain } in
        dd' `seq` writeIORef (da_data da) dd' >>
        when bFlushAgain (p_eventually (da_psched da) (flushDaggr da))

-- called at beginning of new round. Possibly called twice, once via
-- flush and once via normal update mechanisms. So it is idempotent.
deliverDaggr :: DemandAggr e z -> IO ()
deliverDaggr da = maybeDeliver where
    maybeDeliver =
        readIORef (da_data da) >>= \ dd ->
        case dd_tmup dd of
            Nothing -> return ()
            Just tU -> doDeliver dd tU 
    doDeliver dd tU0 =
        p_stepTime (da_psched da) >>= \ tNow ->
        let tTgt = tNow `subtractTime` dtDaggrHist in
        let tU = max tU0 tTgt in -- cut old updates
        let lst = M.toAscList (dd_table dd) in
        let lst' = mapMaybe (daggrTableGC tTgt) lst in
        let tbl' = M.fromAscList lst' in
        let eSigs = fmap ((`s_trim` tU) . st_signal . snd) lst in
        let su = da_nzip da eSigs in
        let bNeedFlush = not (null lst') 
                      && all (isDoneT . st_stable . snd) lst'
        in
        let bInitFlush = not (dd_flush dd) && bNeedFlush in
        let bFlushSched = dd_flush dd || bNeedFlush in
        let dd' = dd { dd_flush = bFlushSched 
                     , dd_table = tbl'
                     , dd_tmup = Nothing
                     }
        in
        tU `seq` su `seq` dd' `seq`
        writeIORef (da_data da) dd' >>
        when bInitFlush (p_eventually (da_psched da) (flushDaggr da)) >>
        p_onUpdPhase (da_psched da) (ln_update (da_link da) DoneT tU su) >>
        ln_touch (da_link da)

-- Garbage Collection of DemandAggr elements. A demand source can
-- be eliminated if it isn't active (even if unstable, since Daggr
-- doesn't utilize input stability).
daggrTableGC :: T -> (k, SigSt e) -> Maybe (k, SigSt e)
daggrTableGC tGC (k,st) =
    assert (not (st_expect st)) $
    let s' = s_trim (st_signal st) tGC in
    let bDone = isDoneT (st_stable st) && s_term s' tGC in
    let st' = st { st_signal = s' } in
    if bDone then Nothing else Just (k, st')


-- | pollDemandAggr enables access to the current value held by a
-- DemandAggr. There are no promises about how much may have been
-- GC'd; polling can miss updates and lose information.
pollDemandAggr :: DemandAggr e z -> IO (Sig z)
pollDemandAggr da =
    readIORef (da_data da) >>= \ dd ->
    let lst = (st_signal . snd) <$> M.toAscList (dd_table dd) in
    let sig = da_nzip da lst in
    return sig

-- | MonitorDist supports output to multiple observers (monitors) of
-- a resource or signal. For simplicity, the main signal is stored 
-- in only one place to support observers. For simple GC, observers
-- are kept in a simple table (modeled with a map). 
--
-- The md_track function will wrap touch actions on observer links.
-- This is intended to help detect partition-local cycles by feeding
-- some extra knowledge back to the signal source. 
data MonitorDist z = MonitorDist 
    { md_data       :: !(IORef (MDD z))   -- mutable data of MonitorDist
    , md_psched     :: !PSched            -- for scheduling events
    }

-- mutable data for the full MonitorDist
data MDD z = MDD
    { mdd_signal    :: !(SigSt z)           -- track primary signal
    , mdd_flush     :: !Bool                -- scheduled heartbeat GC flush?
    , mdd_cleanup   :: !Bool                -- scheduled cleanup this step?
    , mdd_tmup      :: !(Maybe T)           -- time of recent update (*)
    , mdd_table     :: !(M.Map Unique (MLN z)) -- set of active observers
    }
-- (*) mdd_tmup is needed by observers that are waiting for their 
--       observer-signal updates. It is cleared on next touch.

data MLN z = MLN 
    { mln_link      :: !(LnkUp z)           -- observer update callbacks
    , mln_signal    :: !(SigSt ())          -- observer query (the mask)
    , mln_tmup      :: !(Maybe T)           -- tracks observed update time
    }

-- | Each MonitorDist will handle a set of observers for one signal.
newMonitorDist :: (Partition p) => PCX p -> Sig z -> IO (MonitorDist z)
newMonitorDist cp z0 = MonitorDist 
    <$> newIORef (mddZero z0)
    <*> getPSched cp

mddZero :: Sig z -> MDD z
mddZero z0 = MDD
    { mdd_signal  = st_zero { st_signal = z0 }
    , mdd_tmup    = Nothing
    , mdd_flush   = False
    , mdd_cleanup = False
    , mdd_table   = M.empty
    }

-- | Obtain the current signal held by a MonitorDist.
pollMonitorDist :: MonitorDist z -> IO (Sig z)
pollMonitorDist md = 
    readIORef (md_data md) >>=
    return . st_signal . mdd_signal

-- | Each MonitorDist has one main LnkUp where it receives a primary
-- signal, which is later distributed to observers. The primary must
-- be updated from only one source in the same partition. Updates 
-- are immediately delivered to all observing links. In addition, the
-- signal value is stored for late-arriving observers or observers 
-- that otherwise cannot immediately process the update. 
--
-- A cleanup of the MonitorDist is performed every step it activates
-- or possibly also on a flush.
--
primaryMonitorLnk :: MonitorDist z -> LnkUp z
primaryMonitorLnk md = LnkUp touch update idle where
    touch = 
        readIORef (md_data md) >>= \ mdd ->
        let st = mdd_signal mdd in
        unless (st_expect st) $
            -- traceIO "touchPrimaryMD" >>
            let st' = st_poke st in
            let bSchedCleanup = not (mdd_cleanup mdd) in
            let mdd' = mdd { mdd_signal = st', mdd_cleanup = True } in
            writeIORef (md_data md) mdd' >> -- record touch
            when bSchedCleanup (schedCleanupMD md) >> -- schedule cleanup
            -- touch each observer link (where needed).
            let lst = filter (not . st_expect . mln_signal) (M.elems (mdd_table mdd')) in
            mapM_ (ln_touch . mln_link) lst
    idle tS = processUpdates $ \ mdd ->
        let st' = st_idle tS (mdd_signal mdd) in
        mdd { mdd_signal = st' } 
    update tS tU su = processUpdates $ \ mdd ->
        let st' = st_update tS tU su (mdd_signal mdd) in
        let tmup' = Just $! maybe tU (min tU) (mdd_tmup mdd) in
        mdd { mdd_signal = st', mdd_tmup = tmup' } 
    processUpdates fnUpMDD =
        readIORef (md_data md) >>= \ mdd ->
        assert ((st_expect . mdd_signal) mdd) $
        let mdd' = fnUpMDD mdd in
        assert (mdd_cleanup mdd') $
        mdd' `seq` writeIORef (md_data md) mdd' >>
        let lObservers = filter (not . st_expect . mln_signal)
                                (M.elems (mdd_table mdd'))
        in
        p_stepTime (md_psched md) >>= \ tNow ->
        mapM_ (deliverUpdateMD tNow mdd') lObservers

-- deliverUpdateMD will deliver the actual update to the observer,
-- providing a masked signal update if necessary or just a stability
-- update. It is necessary that both inputs have received updates 
-- (if any in the current step) before this is executed. 
--
-- The stability for the update may be `Nothing` if this value will
-- be fully cleared. 
deliverUpdateMD :: T -> MDD z -> MLN z -> IO ()
deliverUpdateMD tNow mdd mln = assert bReady $ deliver where
    bReady = bReadyMDD && bReadyMLN
    bReadyMDD = (not . st_expect . mdd_signal) mdd
    bReadyMLN = (not . st_expect . mln_signal) mln
    tmup = leastTime (mdd_tmup mdd) (mln_tmup mln)
    maskSig tU =
        let sigZ = s_trim ((st_signal . mdd_signal) mdd) tU in
        let sigM = s_trim ((st_signal . mln_signal) mln) tU in
        s_mask sigZ sigM
    deliver =
        case tmup of
            Nothing -> ln_idle (mln_link mln) tS
            Just tU -> ln_update (mln_link mln) tS tU (maskSig tU)
    tS = if bDone then DoneT else StableT tStableAbs
    bDone = isDoneT ((st_stable . mln_signal) mln) &&
            s_term ((st_signal . mln_signal) mln) tStableZ
    tDaggrHist = tNow `subtractTime` dtDaggrHist
    tStableZ = case (st_stable (mln_signal mln)) of
            DoneT -> tDaggrHist
            StableT tm -> tm
    tStableAbs = case (st_stable (mdd_signal mdd)) of
            DoneT -> tStableZ
            StableT tm -> min tm tStableZ

leastTime :: Maybe T -> Maybe T -> Maybe T
leastTime l r = (min <$> l <*> r) <|> l <|> r



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
    schedCleanup = p_onStepEnd (md_psched md) cleanup
    cleanup =        
        readIORef (md_data md) >>= \ mdd ->
        assert (mdd_cleanup mdd) $
        p_stepTime (md_psched md) >>= \ tNow ->
        -- traceIO ("cleanupMD @ " ++ show tNow) >>
        let mdd' = cleanMDD tNow mdd in
        let bSchedFlush = (mdd_flush mdd') && not (mdd_flush mdd) in
        mdd' `seq` writeIORef (md_data md) mdd' >>
        when bSchedFlush schedFlush
    schedFlush = p_eventually (md_psched md) flush
    flush =
        -- traceIO "flushMD" >>
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
        let tStableZ = fromStableT (tNow `subtractTime` dtDaggrHist) (st_stable st) in
        let tHist = tNow `subtractTime` dtMdistHist in
        let tGC = maybeStableT tHist (min tHist) (st_stable st) in
        let st' = st_clear (StableT tGC) st in -- GC'd signal
        -- GC observer link table
        let tbl = mdd_table mdd in
        let lst = M.toAscList tbl in
        let lst' = mapMaybe (cleanMLN tStableZ) lst in
        let tbl' = M.fromAscList lst' in
        -- compute need for flush 
        let bInactiveMDD = (isDoneT . st_stable) st' in
        let bInactiveMLN = all (isDoneT . st_stable . mln_signal . snd) lst' in
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
            , mdd_table = tbl'
            }
    cleanMLN tStableZ (k,mln) =
        assert ((not . st_expect . mln_signal) mln) $        
        let st = mln_signal mln in
        let tGC = maybeStableT tStableZ (min tStableZ) (st_stable st) in
        let st' = st_clear (StableT tGC) st in
        let bDone = s_term (st_signal st') tStableZ in
        let mln' = mln { mln_signal = st', mln_tmup = Nothing } in
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
newMonitorLnk md lzo = newMonitorLnk' md lzo <$> newUnique

newMonitorLnk' :: MonitorDist z -> LnkUp z -> Unique -> LnkUp ()
newMonitorLnk' md lu k = LnkUp touch update idle where
    touch = touchMLN md lu k
    update = updateMLN md k
    idle = idleMLN md k

-- track expected updates to avoid redundant computations
touchMLN :: MonitorDist z -> LnkUp z -> Unique -> IO ()
touchMLN md lu k = 
    readIORef (md_data md) >>= \ mdd ->
    let mln = fromMaybe (mlnZero lu) (M.lookup k (mdd_table mdd)) in
    let st = mln_signal mln in
    unless (st_expect st) $
        -- traceIO ("touchMLN " ++ show k) >>
        let bNeedTouch = (not . st_expect . mdd_signal) mdd in
        let st' = st_poke st in
        let mln' = mln { mln_signal = st' } in
        let tbl' = M.insert k mln' (mdd_table mdd) in
        let mdd' = mdd { mdd_table = tbl' } in
        mln' `seq` mdd' `seq` writeIORef (md_data md) mdd' >>
        when bNeedTouch (ln_touch (mln_link mln'))

mlnZero :: LnkUp z -> MLN z 
mlnZero lzOut = 
    MLN { mln_link = lzOut
        , mln_signal = st_zero
        , mln_tmup = Nothing }

-- Process an update to the masking signal for an observer. The mask
-- describes durations of observation and ensures duration coupling.
-- An update may be necessary
--
-- Link updates will cause a full GC of the MonitorDist (and all 
-- observer links), at the end of the step.
updateMLN :: MonitorDist z -> Unique -> StableT -> T -> Sig () -> IO ()
updateMLN md k tS tU su = updateMLN' md k up where
    up mln = 
        let st' = st_update tS tU su (mln_signal mln) in
        let tmup' = Just tU in
        mln { mln_signal = st', mln_tmup = tmup' }

idleMLN :: MonitorDist z -> Unique -> StableT -> IO ()
idleMLN md k tS = updateMLN' md k up where
    up mln =
        let st' = st_idle tS (mln_signal mln) in
        mln { mln_signal = st' }

updateMLN' :: MonitorDist z -> Unique -> (MLN z -> MLN z) -> IO ()
updateMLN' md k fnup =
    -- traceIO ("updateMLN " ++ show k) >>
    readIORef (md_data md) >>= \ mdd ->
    let tbl = mdd_table mdd in
    let mln = case (M.lookup k tbl) of
                Nothing -> error "illegal state in monitor: link update without touch"
                Just x -> x
    in
    assert (st_expect (mln_signal mln)) $
    assert (isNothing (mln_tmup mln)) $ 
    let mln' = fnup mln in
    let tbl' = M.insert k mln' tbl in
    let bSchedCleanup = not (mdd_cleanup mdd) in
    let mdd' = mdd { mdd_table = tbl', mdd_cleanup = True } in
    let bReady = (not . st_expect . mdd_signal) mdd' in
    let deliver = p_stepTime (md_psched md) >>= \ tNow ->
                  deliverUpdateMD tNow mdd' mln'
    in
    mln' `seq` mdd' `seq`
    writeIORef (md_data md) mdd' >>
    when bSchedCleanup (schedCleanupMD md) >>
    when bReady deliver


