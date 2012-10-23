
-- Utilities for tracking demands and monitors of demand.
--
-- Currently this is separated into two responsibilities:
--   * aggregation of demands
--   * distribution of a signals to many observers
--
-- Intermediate filtration of equal values or choking of
-- full updates to control cycles is suggested.
--
-- The composition is performed by the DemandMonitor module.
-- 
module Sirea.Internal.DemandMonitorData
    ( DemandAggr, newDemandAggr, newDemandLnk
    , MonitorDist, newMonitorDist, extractEmitLnk, newMonitorLnk
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
import Sirea.Internal.LTypes

type Table = Table.Table
type Key = Table.Key

-- TUNING:
-- 
-- DemandAggr will automatically keep a certain amount of "extra" 
-- history to admit late-arriving demands sources. How much? Well,
-- this does have a cost to stability, so it shouldn't be much. 
-- Also, the use of anticipation helps mitigate the requirement,
-- so it doesn't need to be very much. 
--
-- MonitorDist also keeps history to accommodate late-arriving 
-- observers. However, this history has no significant costs other 
-- than storage. It can be a fair bit longer. 
--
dt_daggr_hist :: DT
dt_daggr_hist = 0.03

dt_mdist_hist :: DT
dt_mdist_hist = 0.21   

-- | DemandAggr: aggregates and processes many concurrent demands.
--
-- NOTE: DemandAggr will block propagation of cyclic updates from
-- within the current step. Such updates will eventually deliver, 
-- due to the periodic global stability updates. This will dampen
-- local cycles; if developers need cycles to operate reliably,
-- they must cross partitions.
-- 
data DemandAggr e z = DemandAggr 
    { da_active     :: !(IORef Bool)        -- to detect cyclic touch or update
    , da_touchCt    :: !(IORef Int)         -- count of non-cyclic touches
    , da_nextid     :: !(IORef Key)         -- next hashtable ID
    , da_tmup       :: !(IORef (Maybe T))   -- lowest update time (at the moment)
    , da_stable     :: !(IORef (Maybe T))   -- track last reported stability
    , da_table      :: !(Table (SigSt e))   -- tracking demand data
    , da_nzip       :: !([Sig e] -> Sig z)  -- compute the result signal
    , da_next       :: !(LnkUp z)           -- process the updated signal
    }

-- | Create a demand aggregator, given the output target.
newDemandAggr 
    :: ([Sig e] -> Sig z)
    -> (LnkUp z)
    -> IO (DemandAggr e z)
newDemandAggr zpf zlu = DemandAggr 
    <$> newIORef False -- inactive
    <*> newIORef 0     -- untouched
    <*> newIORef 80000 -- initial key
    <*> newIORef Nothing -- not updated
    <*> newIORef Nothing -- no reported stability
    <*> Table.new      -- no demands
    <*> pure zpf       -- provided zip function
    <*> pure zlu       -- provided 

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
    let getIdx = (getIndex . da_nextid) da rfIdx in
    let touch = getIdx >>= touchDaggr da in
    let update su = getIdx >>= \ ix -> updateDaggr da ix su in
    let lu = LnkUp { ln_touch = touch, ln_update = update } in
    return lu

touchDaggr :: DemandAggr e z -> Key -> IO ()
touchDaggr da k = 
    readIORef (da_active da) >>= \ bCycle ->
    unless bCycle $ Table.get (da_table da) k >>= \ mbSt ->
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
    -- compute and validate stability (could change validation fail
    -- to warning and use recorded stability, but fail means demand
    -- source may have lost information)
    readIORef (da_stable da) >>= \ tmRecordedStability -> 
    let tmDemandStability = foldl' leastTime Nothing $ fmap (st_stable . snd) lst in
    let tmTargetStability = fmap (`subtractTime` dt_daggr_hist) tmDemandStability in
    let tmStable = (max <$> tmRecordedStability <*> tmTargetStability) <|> tmTargetStability in
    assert (monotonicStability tmRecordedStability tmDemandStability) $
    writeIORef (da_stable da) tmStable >> -- note that recorded stability < stability of inputs
    -- perform garbage collection of elements in the table
    mapM_ (tblGC tbl tmStable) lst >>
    -- compute the update.
    readIORef (da_tmup da) >>= \ tmUpd ->
    writeIORef (da_tmup da) Nothing >>
    case tmUpd of
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
data MonitorDist z = MonitorDist 
    { md_signal     :: !(IORef (SigSt z)) -- primary signal
    , md_nextid     :: !(IORef Key)       -- hashtable key
    , md_table      :: !(Table (LnkUp z)) -- collection of observers
    }

-- | track a new set of observers
newMonitorDist :: IO (MonitorDist z)
newMonitorDist = MonitorDist
    <$> newIORef (st_poke st_zero) -- poke ensures initial observers wait for initial update
    <*> newIORef 60000 -- a single observer may use many keys, one on each install
    <*> Table.new      -- table of observers, initially empty

-- | Support update of the observed signal. Signal should always be
-- active; it is masked by the signal of each observer.
extractEmitLnk :: MonitorDist z -> LnkUp z
extractEmitLnk md = LnkUp { ln_touch = touch, ln_update = update }
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
-- happens after LnkUp z is emitted.
newMonitorLnk :: MonitorDist z -> LnkUp z -> IO (LnkUp ())
newMonitorLnk md lzo =
    -- state of each observer link consists of a pair,
    -- tracking installation, index, and input state.
    newIORef Nothing >>= \ rfIdx -> -- installed? if so, index?
    newIORef sm_zero >>= \ rfSigM -> -- state of signals?
    return (lnMonitor md lzo rfIdx rfSigM)

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

uninstallMD, installMD :: MonitorDist z -> LnkUp z -> IORef (Maybe Key) -> IORef (SigM () z) -> IO ()
installMD md lzi rfIdx rfSigM =
    readIORef rfIdx >>= \ mbk ->
    case mbk of
        Just _ -> return ()
        Nothing ->
            -- prepare for future z inputs 
            readIORef (md_nextid md) >>= \ k0 ->
            let k = succ k0 in
            k `seq` 
            writeIORef (md_nextid md) k >>
            writeIORef rfIdx (Just k) >>
            Table.put (md_table md) k (Just lzi) >>
            -- obtain initial state for z signal
            readIORef (md_signal md) >>= \ sz ->
            readIORef rfSigM >>= \ sm ->
            let sm' = sm { sm_rsig = sz } in
            writeIORef rfSigM sm' 

uninstallMD md _ rfIdx rfSigM =
    readIORef rfIdx >>= \ mbk ->
    case mbk of
        Nothing -> return ()
        Just k  -> -- clear associated states!
            Table.put (md_table md) k Nothing >>
            writeIORef rfSigM sm_zero >>
            writeIORef rfIdx Nothing 

-- this is very similar to ln_withSigM except the second element is
-- automatically installed to and uninstalled from the associated
-- MonitorDist.
lnMonitor :: MonitorDist z -> LnkUp z -> IORef (Maybe Key) -> IORef (SigM () z) -> LnkUp ()
lnMonitor md lzo rfIdx rfSigM = loi
    where loi = LnkUp { ln_touch = pokeO, ln_update = updateO }
          lzi = LnkUp { ln_touch = pokeZ, ln_update = updateZ }
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
                installMD md lzi rfIdx rfSigM
          maybeUninstall =
            readIORef rfSigM >>= \ sm ->
            unless (shouldBeInstalled sm) $
                uninstallMD md lzi rfIdx rfSigM
          emit =
            readIORef rfSigM >>= \ sm ->
            unless (sm_waiting sm) $
                let sm' = sm_cleanup (sm_stable sm) sm in
                sm' `seq` writeIORef rfSigM sm' >>
                maybeUninstall >>
                let su = sm_emit (flip s_mask) sm in 
                ln_update lzo su
                        

