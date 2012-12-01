
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
-- A Demand Monitor admits potential cyclic dependencies, and Sirea
-- offers no effective means to detect them. Even if it did, the
-- `ln_touch` mechanism does not handle cycles robustly - to wait on
-- on a cyclic update would result in logical deadlock. 
-- 
-- The current design is to update demand monitors at the start of
-- each round, as though the updates come from another partition. 
-- This protects snapshot consistency up to loops, but does have a
-- cost: unnecessary intermediate updates are observed; efficiency
-- of the full system can suffer. 
--
-- 
-- The `ln_touch` mechanism does not handle cycles robustly. It can
-- lead to cyclic waits and a form of deadlock. To ensure snapshot
-- consistency, all updates from a round must be processed together.
-- Without `ln_touch`, this requires updates of a round be processed
-- in a later round. onNextStep is used; updates in each round apply
-- to the next. 
--
-- With updates to the collective demand delayed across a step, the
-- demand monitor acts much like a resource in another partition.
-- But the resulting behavior is robust to cyclic relationships,
-- consistent, and easy enough to understand or explain.
--
-- This leaves GC. When demands are active, it's easy enough to rely
-- on demand updates to gradually GC the older demand signals. But a
-- small history of demands is kept in case of late-arriving demands
-- sources. This history will still be in memory at the end, so must
-- be collected by other means. To this end, I'll schedule a `flush`
-- if it seems there are no more updates coming. 
--
-- Special Notes: A DemandAggr's output link may return to `Nothing` 
-- for stability many times. When this happens, stability can be
-- accessed as a simple function of the current time (based on the
-- provided history value). 
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
import Sirea.Internal.Tuning (dtInsigStabilityUp, dtFutureChoke, dtDaggrHist, dtMdistHist)


type Table = Table.Table
type Key = Table.Key


-- convenient partition data (to avoid replicate lookups)
data PartD = PartD 
    { pd_time       :: !(IO T)
    , pd_phaseDelay :: !(IO () -> IO ())
    , pd_stepDelay  :: !(IO () -> IO ())
    , pd_eventually :: !(IO () -> IO ())
    }
mkPartD :: (Partition p) => PCX p -> PartD
mkPartD cp = PartD  
    { pd_time       = getStepTime cp
    , pd_phaseDelay = phaseDelay cp
    , pd_stepDelay  = onNextStep cp
    , pd_eventually = eventually cp
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
    let r = DemandAggr 
                { da_data = rfDD
                , da_demands = tbl
                , da_nzip = zpf
                , da_link = zlu
                , da_partd = mkPartD cp
                , da_hist = dtHist
                }
    in
    (return $! r)

ddZero :: DemandData
ddZero = DemandData
    { dd_active = False 
    , dd_nextKey = 80000
    , dd_tmup = Nothing
    , dd_stable = Nothing
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
    let bActive = dd_active dd in
    let tmup' = leastTime (dd_tmup dd) (snd `fmap` su_state su) in
    let dd' = dd { dd_active = True, dd_tmup = tmup' } in
    dd' `seq` writeIORef (da_data da) dd' >>
    Table.get (da_demands da) k >>= \ mbSt ->
    let st = fromMaybe st_zero mbSt in
    let st' = st_sigup su st in
    Table.put (da_demands da) k (Just st') >>
    unless bActive (activateDaggr da)
    
-- Schedule the DemandAggr to run in the next step.
-- (assumes dd_active already set)
activateDaggr :: DemandAggr e z -> IO () 
activateDaggr da = (pd_stepDelay . da_partd) da (deliverDaggr da)

-- Schedule a GC flush for DemandAggr. This acts the same as an
-- empty update to the DemandAggr, delivered on a heartbeat.
-- (This means the flush update runs on the step AFTER the 
-- heartbeat, as a normal activation.)
scheduleFlushDaggr :: DemandAggr e z -> IO ()
scheduleFlushDaggr da = (pd_eventually . da_partd) da $
    readIORef (da_data da) >>= \ dd ->
    unless (dd_active dd) $
        let dd' = dd { dd_active = True } in
        writeIORef (da_data da) dd' >>
        activateDaggr da

-- deliverDaggr is activated ONLY at the beginning of a new step.
-- It will evaluate whether an update should occur. If so, it
-- may send a touch and prepare its state. The state is computed
-- at this point in order to ignore updates from the new round.
deliverDaggr :: DemandAggr e z -> IO ()
deliverDaggr da =
    -- Need a bunch of resources for this action!
    Table.toList (da_demands da) >>= \ lst ->
    if null lst then resetDaggr da 
                else normalDaggr da lst

-- resetDaggr will reset the stability to Nothing. This should be
-- the eventual result when demands are released on a DemandAggr.
-- This basically signals that the demand monitor is waiting on new 
-- demand sources. This happen due to `scheduleFlushDaggr` (since an
-- update always results in a non-empty list).
resetDaggr :: DemandAggr e z -> IO ()
resetDaggr da =
    readIORef (da_data da) >>= \ dd ->
    assert (dd_active dd && isNothing (dd_tmup dd)) $ 
    let dd' = dd { dd_active = False, dd_tmup = Nothing, dd_stable = Nothing } in
    dd' `seq` writeIORef (da_data da) dd' >>
    ln_touch (da_link da) >>
    let su = SigUp { su_state = Nothing, su_stable = Nothing } in
    let updateAction = ln_update (da_link da) su in 
    (pd_phaseDelay . da_partd) da updateAction

-- most DemandAggr activations have some set of active demands, kept
-- in a table and provided here as a list. This becomes the source of
-- the current signal. Note that stability never isNothing after the
-- normalDaggr updates. If it isNothing before the first update, it 
-- is treated as a function of current time.
normalDaggr :: DemandAggr e z -> [(Key,SigSt e)] -> IO ()
normalDaggr da lst =
    (pd_time . da_partd) da >>= \ tmNow ->
    readIORef (da_data da) >>= \ dd ->
    assert (dd_active dd) $
    -- a lot of logic goes into computing the new stability value!
    let tmDem = foldl' leastTime Nothing $ fmap (st_stable . snd) lst in
    let tmMax = Just $ tmNow `subtractTime` dtDaggrHist in -- buffers for late-arriving demands
    let tmRec = dd_stable dd <|> tmMax in -- recorded stability (or now, if no record)
    let tmTgt = leastTime tmDem tmMax in -- upper bound at tmMax
    let tmStable = max <$> tmRec <*> tmTgt in -- lower bound at tmRec
    -- may need to truncate late-arriving updates based on recorded stability
    let tmUpdate = max <$> tmRec <*> dd_tmup dd in
    let bNeedFlush = isNothing tmDem in -- can't expect more stability updates?
    let dd' = dd { dd_active = False, dd_tmup = Nothing, dd_stable = tmStable } in
    -- Note: I could presumably choose to delay this update, e.g. if the udpate
    -- is several seconds beyond stability. It may be worth doing so.
    writeIORef (da_data da) dd' >> -- commit to activation
    mapM_ (gcDaggrTable (da_table da) tmStable) lst >> -- GC data in table (hold onto lst)
    when bNeedFlush (scheduleFlushDaggr da) >>
    ln_touch (da_link da) >> -- touch observers of demand source
    -- The update happens next phase, to match bcross
    let su = case tmUpdate of
                Nothing  -> SigUp { su_state = Nothing, su_stable = tmStable }
                Just tm  -> let lsigs = fmap (st_signal . snd) lst in
                            let sigz  = da_nzip da lsigs in
                            let sigzTrim = sigz `s_trim` tm in
                            SigUp { su_state = (sigzTrim, tm), su_stable = tmStable }
    in
    let updateAction = ln_update (da_link da) su in
    (pd_phaseDelay . da_partd) da updateAction

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
-- a resource. MonitorDist is very simple; most of the logic is at
-- each observer link. All MonitorDist does is ensure fresh updates
-- will reach the observer links.
--
-- A significant concern is how to deal with MonitorDist when there
-- are no primary signal updates, when we have observers without
-- something active to observe.
--
data MonitorDist z = MonitorDist 
    { md_signal     :: !(IORef (SigSt z)) -- primary signal
    , md_observers  :: !(RefSpace (LnkUp z)) -- collection of observers
    , md_time       :: !(IO T)            -- est. wall clock time
    }

-- | track a new set of observers
newMonitorDist :: z -> IO T -> IO (MonitorDist z)
newMonitorDist z0 now = MonitorDist
    <$> newIORef stInit 
    <*> newIORef 60000 -- a single observer may use many keys, one on each install
    <*> Table.new      -- table of observers, initially empty
    <*> pure now       -- est. of current time; used when Demands are inactive
    where stInit = st_zero { st_signal = s_always z0 }

-- | Support update of the observed signal. Signal should always be
-- active; it is masked by the signal of each observer.
mainMonitorLnk :: MonitorDist z -> LnkUp z
mainMonitorLnk md = LnkUp { ln_touch = touch, ln_update = update }
    where touch = 
            readIORef (md_signal md) >>= \ st ->
            unless (st_expect st) $
                let st' = st_poke st in
                writeIORef (md_signal md) st' >>
                Table.toList (md_table md) >>=
                mapM_ (ln_touch . snd) 
          update su =
            -- first, record the signal for newcoming observers
            readIORef (md_signal md) >>= \ st ->
            let bExpected = st_expect st in
            let st' = st_sigup su st in
            let stc = (`st_clear` st') . (`subtractTime` dt_mdist_hist) in
            let stGC = maybe st_zero stc (su_stable su) in
            stGC `seq` -- GC older values in signal
            writeIORef (md_signal md) stGC >>
            -- second, deliver signal update to all existing observers
            Table.toList (md_table md) >>= \ lst ->
            unless bExpected (mapM_ (ln_touch . snd) lst) >> -- touch, if needed
            mapM_ ((`ln_update` su) . snd) lst -- update
    
-- | create a new observer link. The signal to MonitorDist is masked
-- by the unit signal representing observer activity. 
--
-- The link will be installed and uninstalled from the MonitorDist
-- automatically, whenever there is significant change in state.
-- Installation is evaluated on update to the () signal. Uninstall
-- happens after emit.
--
-- KNOWN BUGS: if there are no Demands, then md_signal will lack a
-- valid stability value. We cannot allow a later reduction in
-- stability. How to best handle this?! Could record a reported
-- stability, and perhaps reduce stability a little to accommodate
-- a late-arriving demand source?
newMonitorLnk :: MonitorDist z -> LnkUp z -> IO (LnkUp ())
newMonitorLnk md lzo = lnMonitor <$> newMonLn md lzo

-- MonLn is parameters & cache associated with one newMonitorLnk
data MonLn z = MonLn
    { mln_rfSigM :: !(IORef (SigM () z)) -- cached signals
    , mln_tStable:: !(IORef (Maybe T))   -- guards reported stability
    , mln_lzout  :: !(LnkUp z)           -- observed data destination
    , mln_mdist  :: !(MonitorDist z)     -- source of data
    , mln_rfIdx  :: !(IORef (Maybe Key)) -- identifier in MonitorDist
    }

newMonLn :: MonitorDist z -> LnkUp z -> IO (MonLn z)
newMonLn md lzo = MonLn 
    <$> newIORef sm_zero
    <*> newIORef Nothing
    <*> pure lzo 
    <*> pure md
    <*> newIORef Nothing

-- test: should this resource be installed?
--   this is a conservative test; it may answer True in some cases
--   where the resource doesn't need to be installed, so long as it
--   answers False eventually when observer signal becomes inactive.
shouldBeInstalled :: SigM a b -> Bool
shouldBeInstalled sm = 
    let tMin = leastTime (sm_stable sm) (sm_tmup sm) in
    case tMin of
        Nothing -> False
        Just tm ->  
            let sig = (st_signal . sm_lsig) sm in
            let done = s_term sig tm in
            not done

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

-- this is very similar to ln_withSigM except the second element is
-- automatically installed to and uninstalled from the associated
-- MonitorDist.
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
          updateO su = modifyIORef rfSigM (sm_sigup_l su) >> maybeInstall >> emit
          updateZ su = modifyIORef rfSigM (sm_sigup_r su) >> emit
          maybeInstall =
            readIORef rfSigM >>= \ sm ->
            when (shouldBeInstalled sm) $ 
                installMonLn mln
          maybeUninstall =
            readIORef rfSigM >>= \ sm ->
            unless (shouldBeInstalled sm) $
                uninstallMonLn mln
          emit =
            readIORef rfSigM >>= \ sm ->
            unless (sm_waiting sm) $
                let sm' = sm_cleanup (sm_stable sm) sm in
                sm' `seq` writeIORef rfSigM sm' >>
                maybeUninstall >>
                let sigZ  = (st_signal . sm_rsig) sm in
                let sigO  = (st_signal . sm_lsig) sm in
                let sig   = s_mask sigZ sigO in
                let su = sm_emit (flip s_mask) sm in 
                ln_update lzo su


