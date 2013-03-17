
-- Utilities for tracking demands and monitors of demand.
--
-- Responsibilities:
--
--   * aggregation of demands (DemandAggr)
--   * distribution of a signal to many observers (MonitorDist)
--
-- Robust cycle handling is also essential, but is addressed by 
-- choke from another module. Every demand aggregator is choked. 
--
-- Monitors help with late-arriving observers by keeping a little 
-- extra history and some information about recent cycle tests.
--
-- Each observer is stable up to earliest activation. This is 
-- important for restarts, to prevent capture of old stability
-- values.
-- 
module Sirea.Internal.DemandMonitorData
    ( DemandAggr, newDemandAggr, newDemandLnk
    , MonitorDist, newMonitorDist, primaryMonitorLnk, newMonitorLnk
    ) where

import Data.IORef
import Data.Unique
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Applicative
import Control.Monad (unless, when)
import Control.Exception (assert)
import Sirea.Signal
import Sirea.Time
import Sirea.UnsafeLink
import Sirea.Partition
import Sirea.Internal.Tuning (dtDaggrHist, dtMdistHist)
import Sirea.Internal.LTypes -- for convenient SigSt, et. al.
import Sirea.Internal.Choke

-- import Debug.Trace (traceIO)
-- showK = ("U#" ++) . show . hashUnique

-- | DemandAggr: aggregates and processes many concurrent demands.
-- Will also flush data on heartbeats whenever stability is clock
-- bounded.
data DemandAggr e z = DemandAggr 
    { da_data       :: !(IORef (DemandData e z)) -- mutable state 
    , da_nzip       :: !([Sig e] -> Sig z)       -- compute the result signal
    , da_link       :: !(LnkUp z)             -- processes updated signal
    , da_psched     :: !PSched                   -- partition scheduler
    }

data DemandData e z = DemandData
    { dd_stable     :: !(Maybe StableT)          -- active stabiliy (if active)
    , dd_touchCt    :: {-# UNPACK #-} !Int       -- active touches on demand aggr
    , dd_flush      :: !Bool                     -- heartbeat flush requested?
    , dd_flushSched :: !Bool                     -- heartbeat flush scheduled?
    , dd_tmup       :: !(Maybe T)                -- time of earliest demand update
    , dd_table      :: !(M.Map Unique (SigSt e)) -- track active demand signals
    }

-- | Create a demand aggregator, given the output target.
-- NOTE: Client must choke the given link if there is any risk of
-- cyclic dependency.
newDemandAggr :: PSched -> (LnkUp z) -> ([Sig e] -> Sig z) -> IO (DemandAggr e z)
newDemandAggr pd lu zp = 
    newIORef ddZero >>= \ rf ->
    newChoke pd lu >>= \ luChoked ->
    return (DemandAggr rf zp luChoked pd)

ddZero :: DemandData e z
ddZero = DemandData
    { dd_touchCt    = 0
    , dd_stable     = Nothing
    , dd_flush      = False
    , dd_flushSched = False
    , dd_tmup       = Nothing
    , dd_table      = M.empty 
    }

-- | Create a new link to an existing demand aggregator.
newDemandLnk :: DemandAggr e z -> IO (LnkUp e)
newDemandLnk da = mkDemandLnk da <$> newUnique 

mkDemandLnk :: DemandAggr e z -> Unique -> LnkUp e
mkDemandLnk da k = LnkUp touch update idle cyc where
    touch = touchDaggr da k 
    idle = idleDaggr da k
    update = updateDaggr da k
    cyc = ln_cycle (da_link da) -- handled by choke

-- touch a particular demand signal (at key). I'll need to track
-- touch if we're ever to handle cycles properly; for now, it 
-- also identifies when the last update arrives.
touchDaggr :: DemandAggr e z -> Unique -> IO ()
touchDaggr da k =
    readIORef (da_data da) >>= \ dd ->
    let tbl = dd_table dd in
    let st = fromMaybe st_zero (M.lookup k tbl) in
    unless (st_expect st) $
        let st' = st_poke st in
        let tbl' = M.insert k st' tbl in
        let tc' = succ (dd_touchCt dd) in
        let dd' = dd { dd_touchCt = tc', dd_table = tbl' } in
        dd' `seq` writeIORef (da_data da) dd' >>
        -- traceIO ("touchDaggr " ++ showK k ++ ". tc = " ++ show tc') >>
        when (1 == tc') (ln_touch (da_link da))
        
       
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
    -- traceIO ("idleDaggr " ++ showK k ++ ". tc = " ++ show tc') >>
    when (0 == tc') (deliverDaggr da)

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
    -- traceIO ("updateDaggr " ++ showK k ++ ". tc = " ++ show tc') >>
    when (0 == tc') (deliverDaggr da)

-- deliverDaggr is called in the update phase by idle, update, or 
-- flush. It will compute and deliver an update.
--
-- At the moment, stability here is just a fixed function of the 
-- clock. I'm contemplating tweaking this so demand aggregators
-- reduce stability when the known inputs are less stable. But for
-- now, this should be okay.
deliverDaggr :: DemandAggr e z -> IO ()
deliverDaggr da = 
    stepTime (da_psched da) >>= \ tNow ->
    -- traceIO ("deliverDaggr @ " ++ show tNow) >>
    readIORef (da_data da) >>= \ dd ->
    assert (0 == dd_touchCt dd) $
    let lst = M.toAscList (dd_table dd) in
    let tSMax = StableT $! tNow `subtractTime` dtDaggrHist in
    let tSMin = fromMaybe tSMax (dd_stable dd) in
    let mbtSL = leastActiveStability (fmap snd lst) in
    let tS = maybe tSMax (min tSMax . max tSMin) mbtSL in
    let lst' = mapMaybe (daggrTableGC (inStableT tS)) lst in
    let tbl' = M.fromAscList lst' in
    let stable' = if (null lst') then Nothing else Just $! tS in
    let bNeedFlush = not (null lst') && isNothing mbtSL in
    let bInitFlush = not (dd_flushSched dd) && bNeedFlush in
    let bFlushSched = dd_flushSched dd || bNeedFlush in
    let dd' = DemandData { dd_stable = stable'
                         , dd_touchCt = 0
                         , dd_flush = bNeedFlush
                         , dd_flushSched = bFlushSched 
                         , dd_tmup = Nothing
                         , dd_table = tbl'
                         }
    in
    dd' `seq` writeIORef (da_data da) dd' >>
    when bInitFlush (eventually (da_psched da) (flushDaggr da)) >>
    case dd_tmup dd of
        Nothing -> 
            ln_idle (da_link da) tS 
        Just tU0 ->
            let tU = max tU0 (inStableT tSMin) in
            let lSigs = fmap ((`s_trim` tU) . st_signal . snd) lst in
            let su = da_nzip da lSigs in
            ln_update (da_link da) tS tU su

-- Garbage Collection of DemandAggr elements. A demand source can
-- be eliminated if it isn't active (even if unstable, since Daggr
-- doesn't utilize input stability).
daggrTableGC :: T -> (k, SigSt e) -> Maybe (k, SigSt e)
daggrTableGC tGC (k,st) =
    let s' = s_trim (st_signal st) tGC in
    let bDone = s_term s' tGC in
    let st' = st { st_signal = s' } in
    if bDone then Nothing else st' `seq` Just (k, st')

-- A flush acts as a false update. Flush is called from deliverDaggr
-- when the DemandAggr's stability is clock-bounded, to run on the
-- next heartbeat (assuming no flush is active). Flush can be
-- canceled by subsequent updates, though it will still be processed
-- on the heartbeat.
flushDaggr :: DemandAggr e z -> IO ()
flushDaggr da = initFlush where
    initFlush = 
        readIORef (da_data da) >>= \ dd ->
        assert (dd_flushSched dd) $
        if dd_flush dd then continue dd else cancel dd
    cancel dd = 
        let dd' = dd { dd_flushSched = False } in
        dd' `seq` writeIORef (da_data da) dd'
    continue dd = 
        let tc' = succ (dd_touchCt dd) in
        let dd' = dd { dd_flushSched = False, dd_touchCt = tc' } in
        writeIORef (da_data da) dd' >>
        onUpdPhase (da_psched da) finiFlush >>
        when (1 == tc') (ln_touch (da_link da))
    finiFlush =
        readIORef (da_data da) >>= \ dd ->
        assert (dd_flush dd) $
        let tc' = pred (dd_touchCt dd) in
        let dd' = dd { dd_flush = False, dd_touchCt = tc' } in
        writeIORef (da_data da) dd' >>
        when (0 == tc') (deliverDaggr da)


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
-- Note: I'm not using SigSt here because I cannot expect the 
--  demand source to respect monotonic stability guarantees
--  (which are asserted for SigSt).
data MDD z = MDD
    { mdd_signal    :: !(Sig z)             -- track primary signal
    , mdd_stable    :: !(Maybe StableT)     -- stability while active
    , mdd_expect    :: !Bool                -- expecting an update?
    , mdd_cycle     :: !CycleSet            -- cycleSet memory (for new links)
    , mdd_flush     :: !Bool                -- scheduled heartbeat GC flush?
    , mdd_cleanup   :: !Bool                -- scheduled cleanup this step?
    , mdd_tmup      :: !(Maybe T)           -- time of recent update (*)
    , mdd_table     :: !(M.Map Unique (MLN z)) -- set of active observers
    }
-- (*) mdd_tmup is needed by observers that are waiting for their 
--       observer-signal updates. It is cleaned up at end of step.

data MLN z = MLN 
    { mln_link      :: !(LnkUp z)        -- observer update callbacks
    , mln_signal    :: !(SigSt ())       -- observer query (the mask)
    , mln_init      :: !(Maybe StableT)  -- earliest activation time
    , mln_tmup      :: !(Maybe T)        -- tracks observed update time
    }

-- | Each MonitorDist will handle a set of observers for one signal.
newMonitorDist :: PSched -> Sig z -> IO (MonitorDist z)
newMonitorDist pd z0 = 
    newIORef (mddZero z0) >>= \ rf ->
    return (MonitorDist rf pd)

mddZero :: Sig z -> MDD z
mddZero z0 = MDD
    { mdd_signal  = z0
    , mdd_stable  = Nothing
    , mdd_expect  = False
    , mdd_cycle   = S.empty
    , mdd_tmup    = Nothing
    , mdd_flush   = False
    , mdd_cleanup = False
    , mdd_table   = M.empty
    }


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
primaryMonitorLnk md = LnkUp touch update idle cyc where
    touch = 
        readIORef (md_data md) >>= \ mdd ->
        unless (mdd_expect mdd) $
            -- traceIO "touchPrimaryMD" >>
            let bSchedCleanup = not (mdd_cleanup mdd) in
            let mdd' = mdd { mdd_expect = True, mdd_cleanup = True } in
            writeIORef (md_data md) mdd' >> -- record touch
            when bSchedCleanup (schedCleanupMD md) >> -- schedule cleanup
            -- touch each observer link (where needed).
            let lst = filter (not . st_expect . mln_signal) (M.elems (mdd_table mdd')) in
            mapM_ (ln_touch . mln_link) lst
    cyc n = 
        readIORef (md_data md) >>= \ mdd ->
        let n0 = mdd_cycle mdd in
        let nf = S.union n0 n in
        unless (S.size n0 == S.size nf) $ 
            -- track set for new observers
            let bSchedCleanup = not (mdd_cleanup mdd) in
            let mdd' = mdd { mdd_cycle = nf, mdd_cleanup = True } in
            writeIORef (md_data md) mdd' >>
            when bSchedCleanup (schedCleanupMD md) >>
            -- deliver potential cycles to existing observers
            let lst = M.elems (mdd_table mdd') in
            mapM_ (flip ln_cycle nf . mln_link) lst
    idle tS = processUpdates tS $ \ mdd ->
        mdd { mdd_stable = Just tS, mdd_expect = False } 
    update tS tU su = processUpdates tS $ \ mdd ->
        let s' = s_switch (mdd_signal mdd) tU su in
        let tmup' = Just $! maybe tU (min tU) (mdd_tmup mdd) in
        mdd { mdd_signal = s', mdd_stable = Just tS
            , mdd_expect = False, mdd_tmup = tmup' } 
    processUpdates tS fnUpMDD =
        readIORef (md_data md) >>= \ mdd ->
        assert (mdd_expect mdd) $
        let mdd' = fnUpMDD mdd in
        assert ((not . mdd_expect) mdd') $
        assert (mdd_cleanup mdd') $
        mdd' `seq` writeIORef (md_data md) mdd' >>
        deliverUpdates tS mdd'
    deliverUpdates tS mdd =
        let lObs = M.elems (mdd_table mdd) in
        let lReadyObs = filter (not . st_expect . mln_signal) lObs in
        mapM_ (deliverUpdateMD tS mdd) lReadyObs


-- deliverUpdateMD will deliver the actual update to the observer,
-- providing a masked signal update if necessary or just a stability
-- update. It is necessary that both inputs have received updates 
-- (if any in the current step) before this is executed. 
--
-- The stability value is adjusted for:
--   * observer's stability
--   * source's stability
--   * observer activation time
-- The last point helps with restarts, to keep stability values
-- from being captured in a loop after hibernation etc.
deliverUpdateMD :: StableT -> MDD z -> MLN z -> IO ()
deliverUpdateMD tMDD mdd mln = deliver where
    tMddObs = maybe tMDD (max tMDD) (mln_init mln) 
    tS = min tMddObs $ st_stable (mln_signal mln)
    tmup = leastTime (mdd_tmup mdd) (mln_tmup mln)
    maskSig tU =
        let sigZ = s_trim (mdd_signal mdd) tU in
        let sigM = s_trim ((st_signal . mln_signal) mln) tU in
        s_mask sigZ sigM
    deliver =
        -- traceIO ("deliverUpdateMD @ " ++ show tNow) >>
        case tmup of
            Nothing -> ln_idle (mln_link mln) tS
            Just tU -> ln_update (mln_link mln) tS tU (maskSig tU)

leastTime :: Maybe T -> Maybe T -> Maybe T
leastTime (Just a) (Just b) = Just $! min a b
leastTime Nothing b = b
leastTime a Nothing = a


-- schedCleanupMD will handle GC of a MonitorDist. This waits until
-- the end of a step in order to ensure all observers in a step have
-- equal opportunity to observe the signal. If the demand source is
-- inactive, cleanup will flush periodically until the demand signal
-- is in its final state.
--
-- The caller should set mdd_cleanup. schedCleanup will unset it.
--
schedCleanupMD :: MonitorDist z -> IO ()
schedCleanupMD md = schedCleanup where
    schedCleanup = onStepEnd (md_psched md) runCleanup
    runCleanup =
        readIORef (md_data md) >>= \ mdd ->
        assert (mdd_cleanup mdd) $
        stepTime (md_psched md) >>= \ tNow ->
        let mdd' = cleanMDD tNow mdd in
        let bSchedFlush = (mdd_flush mdd') && not (mdd_flush mdd) in
        mdd' `seq` writeIORef (md_data md) mdd' >>
        when bSchedFlush schedFlush
    schedFlush = eventually (md_psched md) flush
    flush = -- flush just requests another cleanup
        readIORef (md_data md) >>= \ mdd ->
        assert (mdd_flush mdd) $
        let bSchedCleanup = not (mdd_cleanup mdd) in
        let mdd' = mdd { mdd_flush = False, mdd_cleanup = True } in
        writeIORef (md_data md) mdd' >>
        when bSchedCleanup schedCleanup
    cleanMDD tNow mdd = 
        assert ((not . mdd_expect) mdd) $
        -- GC the main signal. Keep history for late-arriving observers.
        let tMDHist = StableT $ tNow `subtractTime` dtMdistHist in
        let tCutMDD = inStableT $ maybe tMDHist (min tMDHist) (mdd_stable mdd) in
        let s' = s_trim (mdd_signal mdd) tCutMDD in -- GC old data
        -- GC the observer signals. Recognize 'DoneT' for MDD might not be.
        let tDAHist = tNow `subtractTime` dtDaggrHist in
        let tCutMLN = maybe tDAHist inStableT (mdd_stable mdd) in
        let lst = M.toAscList (mdd_table mdd) in
        let lst' = mapMaybe (cleanMLN tCutMLN) lst in
        let tbl' = M.fromAscList lst' in
        -- Compute need to flush.
        let bInactiveMDD = maybe True (s_is_final s' . inStableT) (mdd_stable mdd) in
        let bInactiveMLN = all (st_term . mln_signal . snd) lst' in
        let bCleanMDD = s_is_final s' tCutMDD in
        let bCleanMLN = null lst' in
        let bInactive = bInactiveMDD && bInactiveMLN in
        let bClean = bCleanMDD && bCleanMLN in
        let bNeedFlush = not bClean && bInactive in
        let bFlushSched = mdd_flush mdd || bNeedFlush in
        -- if inactive main signal, set stability to Nothing
        let tSA = if bInactiveMDD then Nothing else mdd_stable mdd in
        -- cleaned result
        MDD { mdd_signal = s'
            , mdd_stable = tSA
            , mdd_expect = False
            , mdd_cycle = S.empty
            , mdd_flush = bFlushSched
            , mdd_tmup = Nothing
            , mdd_cleanup = False
            , mdd_table = tbl'
            }
    cleanMLN tCutMD (k,mln) =
        assert ((not . st_expect . mln_signal) mln) $
        let st = mln_signal mln in
        let tGC = min tCutMD (inStableT (st_stable st)) in
        let s' = s_trim (st_signal st) tGC in
        let bDone = s_term s' tGC in
        if bDone then Nothing else
        let st' = st { st_signal = s' } in
        let mln' = mln { mln_signal = st', mln_tmup = Nothing } in
        mln' `seq` Just (k,mln')

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
newMonitorLnk' md lu k = LnkUp touch update idle cyc where
    touch = touchMLN md lu k
    update = updateMLN md k
    idle = idleMLN md k
    cyc = ln_cycle lu 

-- track expected updates to avoid redundant computations; if this
-- is the first we've heard of this link, we must also send a record
-- of possible cycles.
touchMLN :: MonitorDist z -> LnkUp z -> Unique -> IO ()
touchMLN md lu0 k = 
    readIORef (md_data md) >>= \ mdd ->
    let mbMLN = M.lookup k (mdd_table mdd) in
    let bInit = isNothing mbMLN in
    let mln = fromMaybe (mlnZero lu0) mbMLN in
    let st = mln_signal mln in
    unless (st_expect st) $
        -- traceIO ("touchMLN " ++ showK k) >>
        let lu = mln_link mln in
        let bNeedTouch = (not . mdd_expect) mdd in
        let bNeedCyc = bInit && (not . S.null . mdd_cycle) mdd in
        let bSchedCleanup = not (mdd_cleanup mdd) in
        let st' = st_poke st in
        let mln' = mln { mln_signal = st' } in
        let tbl' = M.insert k mln' (mdd_table mdd) in
        let mdd' = mdd { mdd_table = tbl', mdd_cleanup = True } in
        mln' `seq` mdd' `seq` writeIORef (md_data md) mdd' >>
        when bNeedCyc (ln_cycle lu (mdd_cycle mdd')) >>
        when bNeedTouch (ln_touch lu) >>
        when bSchedCleanup (schedCleanupMD md)

mlnZero :: LnkUp z -> MLN z 
mlnZero lzOut = 
    MLN { mln_link = lzOut
        , mln_signal = st_zero
        , mln_tmup = Nothing 
        , mln_init = Nothing
        }

-- Process an update to the masking signal for an observer. The mask
-- describes durations of observation and ensures duration coupling.
-- An update may be necessary
--
-- Link updates will cause a full GC of the MonitorDist (and all 
-- observer links), at the end of the step.
--
-- The first update on an MLN will activate it, and determines a
-- minimum stability for further observations (i.e. the observer's
-- signal will never earlier than when we begin observing). This 
-- helps control against bad behavior for stability in cycles after
-- waking from suspend or other pauses in computation.
updateMLN :: MonitorDist z -> Unique -> StableT -> T -> Sig () -> IO ()
updateMLN md k tS tU su = updateMLN' md k $ \ mln ->
    let st' = st_update tS tU su (mln_signal mln) in
    let tmup' = Just $! maybe tU (min tU) (mln_tmup mln) in
    let init' = mln_init mln <|> (Just $! min (StableT tU) tS) in
    mln { mln_signal = st', mln_tmup = tmup', mln_init = init' }

idleMLN :: MonitorDist z -> Unique -> StableT -> IO ()
idleMLN md k tS = updateMLN' md k $ \ mln ->
    let st' = st_idle tS (mln_signal mln) in
    mln { mln_signal = st' }

updateMLN' :: MonitorDist z -> Unique -> (MLN z -> MLN z) -> IO ()
updateMLN' md k fnup =
    readIORef (md_data md) >>= \ mdd ->
    let tbl = mdd_table mdd in
    let mln = case (M.lookup k tbl) of
                Nothing -> error "illegal state in monitor"
                Just x -> x
    in
    assert ((st_expect . mln_signal) mln) $
    let mln' = fnup mln in
    assert ((not . st_expect . mln_signal) mln') $
    let tbl' = M.insert k mln' tbl in
    let mdd' = mdd { mdd_table = tbl' } in
    let bReady = (not . mdd_expect) mdd' in
    let deliver = getTSMD (md_psched md) mdd' >>= \ tS ->
                  deliverUpdateMD tS mdd' mln'
    in
    mln' `seq` mdd' `seq`
    writeIORef (md_data md) mdd' >>
    when bReady deliver

-- get stability time for MDD, using clock if Z inactive
getTSMD :: PSched -> MDD z -> IO (StableT) 
getTSMD pd = maybe comp return . mdd_stable where
    comp = (StableT . (`subtractTime` dtDaggrHist)) <$> stepTime pd




