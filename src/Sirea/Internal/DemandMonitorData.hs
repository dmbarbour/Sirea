
-- Utilities for tracking demands and monitors of demand.
--
-- Currently this is separated into two responsibilities:
--   * aggregation of demands
--   * distribution of a signal to many observers
--
-- Demand Monitors are stabilized based on known demand sources and
-- the 'real-time' obtained from an external time source. A late 
-- arriving demand source can be retroactively accommodated for
-- a few milliseconds. 
--
-- Old demands must be cleaned up over time. Mostly, the system can
-- depend on the stability updates to help GC the demands. But due
-- to keeping a few extra milliseconds of data (for new demands),
-- finalization cleanup must occur by the pulse (heartbeat) in order
-- to flush the last bits from memory. 
--
module Sirea.Internal.DemandMonitorData
    ( DemandAggr, newDemandAggr, newDemandLnk
    , MonitorDist, newMonitorDist, mainMonitorLnk, newMonitorLnk
    ) where

import qualified Sirea.Internal.Table as Table
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
--import Sirea.Internal.LTypes

type Table = Table.Table
type Key = Table.Key

-- 
-- DemandAggr will automatically keep a certain amount of "extra" 
-- history to admit late-arriving demands sources. How much? Well,
-- this does have a cost to stability, so it shouldn't be much.
-- This will be kept relative to wall-clock time  
-- Also, the use of anticipation helps mitigate the requirement,
-- so it doesn't need to be very much. 
--
-- MonitorDist also keeps history to accommodate late-arriving 
-- observers. However, this history has no significant costs other 
-- than storage. It can be a fair bit longer, and so it is. 
--
-- If there are no demands, the monitor will also use dt_mdist_hist
-- to estimate stability of the demands relative to itself. (This
-- is not ideal, but is the best I can do without using dedicated
-- partition for demand monitors.)
--
dt_daggr_hist :: DT
dt_daggr_hist = 0.024

dt_mdist_hist :: DT
dt_mdist_hist = 0.126 

-- convenient partition data (to avoid replicate lookups)
data PartD = PartD 
    { pd_time       :: (IO T)
    , pd_eventually :: (IO () -> IO ())
    , pd_phaseDelay :: (IO () -> IO ())
    }
mkPartD :: (Partition p) => PCX p -> PartD
mkPartD cp = PartD  
    { pd_time       = getStepTime cp
    , pd_eventually = eventually cp
    , pd_phaseDelay = phaseDelay cp
    }     


-- | DemandAggr: aggregates and processes many concurrent demands.
--
-- NOTE: DemandAggr will block propagation of cyclic updates from
-- within the current step. Such updates will eventually deliver, 
-- due to the periodic global stability updates. This will dampen
-- local cycles, often excessively.
--
-- Stability of DemandAggr is simply stability of the incoming
-- demands or an estimated current time as adjusted to support late
-- arriving demand sources). 
--
-- Periodic cleanup is modeled by a stability update of an inactive 
-- signal. This ensures it can mix freely with regular updates.
data DemandAggr e z = DemandAggr 
    { da_active     :: !(IORef Bool)        -- to detect cyclic touch or update
    , da_touchCt    :: !(IORef Int)         -- count of non-cyclic touches
    , da_tmup       :: !(IORef (Maybe T))   -- lowest update time (at the moment)
    , da_stable     :: !(IORef (Maybe T))   -- track reported stability
    , da_table      :: !(Table (SigSt e))   -- tracking demand data
    , da_nzip       :: !([Sig e] -> Sig z)  -- compute the result signal
    , da_next       :: !(LnkUp z)           -- process the updated signal
    , da_partd      :: !PartD
    }

-- | Create a demand aggregator, given the output target.
newDemandAggr
    :: (Partition p)
    => PCX p 
    -> (LnkUp z)
    -> ([Sig e] -> Sig z)
    -> (z -> z -> Bool)
    -> IO (DemandAggr e z)
newDemandAggr cp zlu zpf zeq = DemandAggr 
    <$> newIORef False -- inactive
    <*> newIORef 0     -- untouched
    <*> newIORef Nothing -- not updated
    <*> newIORef Nothing -- no stability
    <*> newIORef 80000 -- initial key
    <*> Table.new      -- no demands
    <*> pure zpf       -- provided zip function
    <*> pure zlu       -- provided 
    <*> pure (mkPartD cp)
    <*> pure now
    <*> pure later

--    wrapEqFilter dt_eqf eqfn (mainMonitorLnk md) >>= \ monEqf -> 



getIndex :: IORef Key -> IORef (Maybe Key) -> IO Key
getIndex rfNxt rfK =
    readIORef rfK >>= \ mk ->
    case mk of
        Just k -> return k
        Nothing ->
            readIORef rfNxt >>= \ k0 ->
            let k = succ k0 in
            k `seq`
            writeIORef rfNxt k >>
            writeIORef rfK (Just k) >>
            return k

-- increment touch and return whether it is the first touch.
incTouch :: IORef Int -> IO Bool
incTouch rf =
    readIORef rf >>= \ n ->
    let n' = succ n in
    n' `seq` writeIORef rf n' >>
    (return $! 1 == n')

-- decrement touch and return whether it is the last touch.
decTouch :: IORef Int -> IO Bool
decTouch rf =
    readIORef rf >>= \ n ->
    let n' = pred n in
    n' `seq` writeIORef rf n' >>
    (return $! 0 == n')

-- | Create a new link to an existing demand aggregator.
newDemandLnk :: DemandAggr e z -> IO (LnkUp e)
newDemandLnk da =
    newIORef Nothing >>= \ rfIdx -> -- to acquire index later.
    let getIdx = getIndex (da_nextid da) rfIdx in
    let touch = getIdx >>= touchDaggr da in
    let update su = getIdx >>= \ ix -> updateDaggr da ix su in
    let lu = LnkUp { ln_touch = touch, ln_update = update } in
    return lu

touchDaggr :: DemandAggr e z -> Key -> IO ()
touchDaggr da k = 
    readIORef (da_active da) >>= \ bCycle ->
    unless bCycle $ 
        Table.get (da_table da) k >>= \ mbSt ->
        let st = fromMaybe st_zero mbSt in
        unless (st_expect st) $
            let st' = st_poke st in
            Table.put (da_table da) k (Just st') >>
            incTouch (da_touchCt da) >>= \ bFirst ->
            when bFirst $
                writeIORef (da_active da) True >>
                ln_touch (da_next da) >>
                writeIORef (da_active da) False

updateDaggr :: DemandAggr e z -> Key -> SigUp e -> IO ()
updateDaggr da k su =
    -- track earliest time-of-update. 
    readIORef (da_tmup da) >>= \ tmup ->
    let tmsu = fmap snd $ su_state su in
    let tmup' = leastTime tmup tmsu in
    writeIORef (da_tmup da) tmup' >>
    -- update the element and the touch-count
    Table.get (da_table da) k >>= \ mbSt ->
    let st  = fromMaybe st_zero mbSt in
    let st' = st_sigup su st in
    Table.put (da_table da) k (Just st') >>
    when (st_expect st) (void $ decTouch (da_touchCt da)) >>
    -- deliver the updates (if possible)
    maybeDeliverDaggr da

-- send a zero-update to clear the DemandAggr
clearDaggr :: DemandAggr e z -> IO ()
cle


-- deliver aggregated demands if nothing is holding us back
maybeDeliverDaggr :: DemandAggr e z -> IO ()
maybeDeliverDaggr da =
    readIORef (da_active da) >>= \ bActive ->
    readIORef (da_touchCt da) >>= \ nTouchCt ->
    let bWait = bActive || (0 /= nTouchCt) in
    unless bWait $ 
        writeIORef (da_active da) True >> -- block cycles
        deliverDaggr da >>
        writeIORef (da_active da) False

-- deliver aggregated demands, perform GC, etc.
deliverDaggr :: DemandAggr e z -> IO ()
deliverDaggr da =
    -- obtain the collection of demand signals
    let tbl = da_table da in
    Table.toList tbl >>= \ lst -> 
    -- compute stability based on all demands, current time, and committed stability
    da_time da >>= \ tmClock -> -- wall-clock time
    let tmNow = Just $ tmClock `subtractTime` dt_daggr_hist in
    readIORef (da_stable da) >>= \ tmRec ->  -- recorded stability (non-decreasing!)
    let tmMin = tmRec <|> tmNow in -- using tmNow if tmRec isNothing
    let tmDem = foldl' leastTime Nothing $ fmap (st_stable . snd) lst in
    let tmTgt = leastTime tmDem tmNow in -- using tmNow if less than earliest demanded time
    let tmStable = max <$> tmMin <*> tmTgt in
    writeIORef (da_stable da) tmStable >>
    -- perform garbage collection of elements in the table
    mapM_ (tblGC tbl tmStable) lst >>
    -- compute the update time, constrained by committed stability.
    readIORef (da_tmup da) >>= \ tmUpd ->
    writeIORef (da_tmup da) Nothing >>
    let tmSigUp = (max <$> tmRec <*> tmUpd) <|> tmUpd in
    case tmSigUp of
        Nothing ->
            let su = SigUp { su_state = Nothing, su_stable = tmStable } in
            ln_update (da_next da) su
        Just tm ->
            let lsigs = fmap (st_signal . snd) lst in
            let sigz = da_nzip da lsigs in
            let sigzTrim = s_trim sigz tm in
            let su = SigUp { su_state = Just (sigzTrim,tm), su_stable = tmStable } in
            ln_update (da_next da) su



-- need to GC the hashtable based on lowest stability forall elements.
tblGC :: Table (SigSt a) -> Maybe T -> (Key,SigSt a) -> IO ()
tblGC tbl Nothing (idx,_) = Table.put tbl idx Nothing
tblGC tbl (Just tm) (idx,st) = 
    let (x,sf) = s_sample (st_signal st) tm in
    let bDone = isNothing x && s_is_final sf tm in 
    if bDone -- no need for demand before next update?
        then Table.put tbl idx Nothing
        else let st' = st { st_signal = sf } in
             st' `seq` Table.put tbl idx (Just st')


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
    , md_nextid     :: !(IORef Key)       -- hashtable key
    , md_table      :: !(Table (LnkUp z)) -- collection of observers
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


