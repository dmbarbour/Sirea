
-- Utilities for tracking demands and monitors of demand.
--
-- Currently this is separated into two responsibilities:
--   * aggregation of demands
--   * distribution of signals to monitors
--
-- Intermediate filtration of equal values or choking of
-- full updates to control cycles is suggested.
--
-- The composition is performed by the DemandMonitor module.
-- 
module Sirea.Internal.DemandMonitorData
    ( DemandAggr, newDemandAggr, newDemandLnk
    --, MonitorDist, newMonitorDist, newMonitorLnk
    ) where

import qualified Sirea.Internal.Table as Table
import Data.IORef
import Data.Maybe (fromMaybe, isNothing)
import Data.List (foldl')
import Control.Applicative
import Control.Monad (unless, when, void)
import Control.Exception (assert)
import Sirea.Signal
import Sirea.Time
import Sirea.Link

-- Using some utilities from other internals rather than reproducing them.
import Sirea.Internal.LTypes (SigSt(..), st_zero, st_poke, st_sigup)

type Table = Table.Table
type Key = Table.Key

-- TUNING:
-- 
-- hist:
-- DemandAggr will automatically keep a certain amount of "extra" 
-- history to admit late-arriving demands sources. How much? Well,
-- this does have a cost to stability, so it shouldn't be much! 
-- Also, the use of anticipation helps mitigate the requirement,
-- so it doesn't need to be very much. 
--
dt_daggr_hist :: DT
dt_daggr_hist = 0.03  -- how much history to keep? (observed stability - dt_hist)

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

    


                
    

{-

newDemandAggr :: ([Sig e] -> Sig z) 
              -> 
IO (DemandAggr e z)



-- DemandMonitorData
--   currently a couple tables and some stateful summary data
--   (the stateful data is used to keep algorithmic costs down)
--
-- zipfn: a function to combine signal sources; must be commutative
-- peqfn: a partial equality function; return true iff two values 
--   are known to be equal; may return false if unknown
-- history: buffer to accommodate straggling (newcoming) monitors or
--   demand sources. (Might later want to split so that new monitors
--   are supported longer than new sources? think about it.)
--
-- The DemandMonitorData currently assumes (possibly incorrectly) 
-- that the current demand stability is a decent estimate of new
-- demand or monitor times. 
-- 
data DemandMonitorData e z = DemandMonitor
    { dmd_zipfn     :: !([Sig e] -> Sig z)     -- zip the demand signals
    , dmd_peqfn     :: !(z -> z -> Bool)       -- clean up redundant updates
    , dmd_history   :: !(DT)                   -- for eventual consistency
    , dmd_signal    :: !(IORef (Sig z))        -- last reported signal
    , dmd_stable    :: !(IORef (Maybe T))      -- last reported stability
    , dmd_update    :: !(IORef (Maybe T))      -- earliest demand update
    , dmd_dtouch    :: !(IORef Int)            -- # expected updates on demand facets
    , dmd_mtouch    :: !(IORef Int)            -- # expected updates on monitor facets
    , dmd_nxtid     :: !(IORef Int32)          -- create local unique ID for table
    , dmd_dTable    :: !(HT.HashTable (DemandData e))
    , dmd_mTable    :: !(HT.HashTable (MonitorData z))
    }
data DemandData e = DemandData
    { dd_sig :: !(SigSt e) -- current demand status
    }
data MonitorData z = MonitorData
    { md_sig :: !(SigSt ()) -- monitoring status 
    , md_lnk :: !(LnkUp z)  -- to receive monitor updates
    , md_upd :: !(Maybe T)  -- recent update time (if any)
    }

dd_zero :: DemandData e
dd_zero = DemandData st_zero

md_zero :: LnkUp z -> MonitorData z
md_zero lu = MonitorData st_zero lu Nothing

touchMD :: MonitorData z -> MonitorData z 
touchMD md = 
    let st = md_sig md in
    let st' = st_poke st in
    md { md_sig = st' }

newDemandMonitorData 
    :: ([Sig e] -> Sig z) -- nzip
    -> (z -> z -> Bool)   -- peq
    -> DT                 -- how much to accommodate stragglers?
    -> IO (DemandMonitorData e z)
newDemandMonitorData nzip peq dtHist =
    assert (dtHist >= 0) $ 
    DemandMonitorData
        <$> pure nzip 
        <*> pure peq
        <*> pure dtHist
        <*> newIORef (nzip []) -- current signal
        <*> newIORef (Nothing) -- current signal stability
        <*> newIORef (Nothing) -- earliest demand update
        <*> newIORef 0      -- initial touch count
        <*> newIORef 400000 -- arbitrary starting index
        <*> HT.new (==) HT.hashInt
        <*> HT.new (==) HT.hashInt

-- obtain the index for a new demand or monitor facet 
-- (must be performed in demand monitor's thread)
getIndex :: DemandMonitorData e z -> IORef (Maybe Int) -> IO Int
getIndex dmd rf = readIORef rf >>= maybe newIndex return
    where rfNxt = dmd_nxtid dmd 
          newIndex = readIORef rfNxt >>= \ n ->
                     let n' = succ n in
                     n' `seq` 
                     writeIORef rfNxt n' >>
                     writeIORef rf (Just n') >>
                     return n'

-- touch monitor data (if it is not already recorded as touched)
-- this will touch a specific monitor, and add it if necessary.
touchMonitorData :: DemandMonitorData e z -> Int -> LnkUp z -> IO ()
touchMonitorData dmd ix lu =
    HT.lookup (dmd_mTable dmd) ix >>= \ mbmd ->
    let md = maybe (md_zero lu) id mbmd in
    unless ((st_expect . md_sig) md) $
        let md' = touchMD md in
        let rfct = dmd_mtouch dmd in
        HT.update (dmd_mTable dmd) ix md' >>
        readIORef rfct >>= \ ct ->
        (writeIORef rfct $! succ ct) >>
        ln_touch (md_lnk md') 

-- touch demand data (if it is not already touched)
-- first touch will cause touch of all present monitors
touchDemandData :: DemandMonitorData e z -> Int -> IO ()
touchDemandData dmd ix =
    HT.lookup (dmd_dTable dmd) ix >>= \ mbdd ->
    let dd = maybe (dd_zero) id mbdd in
    unless ((st_expect . dd_sig) dd) $
        let dd' = touchDD dd in
        HT.update (dmd_dTable dmd) ix dd' >>
        readIORef (dmd_dtouch dmd) >>= \ ct ->
        let ct' = succ ct in
        ct' `seq` writeIORef (dmd_dtouch dmd) ct' >>
        when (1 == ct') (touchAllMonitors dmd)

-- whenever we first touch a demand-data, we'll deliver a touch on
-- all monitors. This is not recorded anywhere, so may be redundant
-- with touchMonitorData. (That is okay; touch is idempotent; it 
-- matters more that the touch happens.)
touchAllMonitors            
        
        

    case mbmd of
        Nothing -> addTouched (md_zero lu)
        Just md -> 
            unless ((st_expect . md_sig) md) $
                addTouch
                let md' = touchMD md in
                
            let md = m
HT.insert (dmd_mTable 

-- strict modifyIORef (why isn't this standard?)
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' rf fn =

getDemandData :: DemandMonitorData e z -> Int -> IO (DemandData e)
getDemandData dmd ix = maybe dd_zero id `fmap` HT.lookup (dmd_dTable dmd) ix

expectingDemandUpdates :: DemandMonitorData e z -> IO Bool
expectingDemandUpdates dmd = (> 0) <$> readIORef (dmd_dtouch dmd)

expectingMonitorUpdates :: DemandMonitorData e z -> IO Bool
expectingMonitorUpdates dmd = (> 0) <$> readIORef (dmd_mtouch dmd)

expectingUpdates :: DemandMonitorData e z -> IO Bool
expectingUpdates dmd = 
    expectingDemandUpdates dmd >>= \ bD ->
    if bD then return True else
    expectingMonitorUpdates dmd

-- getMonitorData may need to `touch` a new monitor if the touch
-- count for demands is > 0. The LnkUp is only used when a fresh
-- MonitorData is needed.
getMonitorData :: DemandMonitorData e z -> Int -> LnkUp z -> IO (MonitorData z)
getMonitorData dmd ix lu = HT.lookup (dmd_mTable dmd) ix >>= maybe newMon return 
    where newMon = 
            expectingDemandUpdates dmd >>= \ bAutoTouch ->
            if (not bAutoTouch) then return md else
            HT.insert (dmd_mTable dmd) ix md >>
            touchMonitorData dmd ix >>
            

            
            
 let md = md_zero lu in
                    HT.insert (dmd_mTable dmd) ix md >>
                    readIORef (dmd_dtouch dmd) >>= \ nTouchCtD ->
                    when (nTouchCtD > 0) (touchMonitorData dmd ix lu) >>
                    readIORef (dmd_dtouch dmd) >>= \ nTouchCtD ->
                    when (nTouchCtD > 0) $
                        
                        touchMonitorData dmd ix lu
                    let bTouch = (nTouch > 0) in
                    if (not btouch) then 

-- get the demand data associated with a particular demand source.
dmd_getDemandData :: DemandMonitorData e z -> Int -> IO (DemandData e)
dmd_getDemandData dmd ix = maybe dd_zero id `fmap` HT.lookup (dmd_demand dmd) ix

-- get monitor data (subscription times, 
dmd_getMonitorData :: DemandMonitorData e z -> Int -> LnkUp z -> IO (MonitorData z)
dmd_getMonitorData dmd ix lu = 
    HT.lookup (dmd_monitor dmd) ix >>= \ mmd ->
    case mmd of
        Just md -> return md
        Nothing -> 
            let md = md_zero lu in

            



dmd_touchDemandFacet :: DemandMonitorData e z -> Int -> IO ()
dmd_touchDemandFacet dmd ix = 
    dmd_getDemandData dmd ix >>= \ dd ->
    let st = dd_sig dd in
    let bTouched = st_expect st in
    unless bTouched $
        let st' = st_poke st in
        let dd' = dd { dd_sig = st' } in
        HT.update (dmd_demand dmd) ix dd' >>
        readIORef (dmd_expect dmd) >>= \ nTC ->
            
        let dd' = dd { dd_sig = st
    if bTouched
        then 
error "TODO!"
 {-            loadIndex >>= \ ix ->
            dmd_getDemandData dmd ix >>= \ dd ->
            let st = dd_sig dd in
            unless (st_expect st) $  -}

dmd_updateDemandFacet :: DemandMonitorData e z -> Int -> SigUp e -> IO ()
dmd_updateDemandFacet dmd n su = error "TODO!"



dmd_touchMonitorFacet :: DemandMonitorData e z -> Int -> IO ()
dmd_touchMonitorFacet = error "TODO!"

dmd_updateMonitorFacet :: DemandMonitorData e z -> Int -> SigUp () -> IO ()
dmd_updateMonitorFacet = error "TODO!"


-- a demand facet will receive demand updates and touches. It does 
-- not propagate the signal! This is a sink. Client should apply a 
-- process for proper duration coupling. 
newDemandFacet :: DemandMonitorData e z -> IO (LnkUp e)
newDemandFacet dmd =
    newIORef Nothing >>= \ rfIndex -> -- GUID would be better for distributed system
    let loadIndex = getIndex (dmd_dTable dmd) rfIndex in
    let touch = loadIndex >>= dmd_touchDemandFacet dmd in
    let update su = loadIndex >>= \ ix -> dmd_updateDemandFacet dmd ix su in
    return $ LnkUp { ln_touch = touch, ln_update = update }

-- a newMonitorFacet will create a monitor facet. This tracks the
-- demand signal for masking against the active signal. 
newMonitorFacet :: DemandMonitorData e z -> LnkUp z -> IO (LnkUp ())
newMonitorFacet dmd lu =
    newIORef Nothing >>= \ rfIndex -> -- GUID would be better for distributed system
    let loadIndex = getIndex (dmd_mTable dmd) rfIndex in
    let touch = loadIndex >>= \ ix -> dmd_touchMonitorFacet dmd ix in
    let update su = loadIndex >>= \ ix -> dmd_updateMonitorFacet dmd ix su in
    return $ LnkUp { ln_touch = touch, ln_update = update }

-}



