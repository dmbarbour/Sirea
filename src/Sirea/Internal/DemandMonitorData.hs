
-- Utilities for tracking demands and monitors of demand.
module Sirea.Internal.DemandMonitorData
    ( DemandMonitorData(..)
    , DemandData(..), dd_zero
    , MonitorData(..), md_zero
    , newDemandMonitorData, newDemandFacet, newMonitorFacet, dmd_getNextIndex
    , dmd_getDemandData, dmd_touchDemandFacet, dmd_updateDemandFacet
    , dmd_getMonitorData, dmd_touchMonitorFacet, dmd_updateMonitorFacet
    ) where

import qualified Data.HashTable as HT
import Data.IORef
import Control.Applicative
import Sirea.Internal.LTypes
import Sirea.Signal
import Sirea.Time

data DemandMonitorData e z = DemandMonitorData
    { dmd_zipfn     :: !([Sig e] -> Sig z)-- zip concurrent demands (should be commutative)
    , dmd_peqfn     :: !(z -> z -> Bool)  -- partial equality on z (may return false for equal values)
    , dmd_stable    :: !(IORef (Maybe T)) -- current stability
    , dmd_signal    :: !(IORef (Sig z))   -- current signal 
    , dmd_lastid    :: !(IORef Int)       -- next index (monitor or demand; could use GUID)
    , dmd_update    :: !(IORef (Maybe T)) -- time of earliest demand update (if any)
    , dmd_expect    :: !(IORef Int)       -- touch count
    , dmd_demand    :: !(HT.HashTable Int (DemandData e))
    , dmd_monitor   :: !(HT.HashTable Int (MonitorData z))
    }
data DemandData e = DemandData
    { dd_sig        :: !(SigSt e) -- current demand status
    }

dd_zero :: DemandData e
dd_zero = DemandData st_zero

data MonitorData z = MonitorData
    { md_sig :: !(SigSt ()) -- monitoring status (removed when final)
    , md_lnk :: !(LnkUp z)  -- to receive monitor updates
    , md_upd :: !(Maybe T)  -- recent update time (if any)
    }
md_zero :: MonitorData z
md_zero = MonitorData st_zero ln_zero Nothing

arbitraryStartingIdx :: Int
arbitraryStartingIdx = 1000

newDemandMonitorData 
    :: ([Sig e] -> Sig z) 
    -> (z -> z -> Bool)
    -> IO (DemandMonitorData e z)
newDemandMonitorData zip peq = DemandMonitorData
    <$> pure zip 
    <*> pure peq
    <*> newIORef Nothing  -- initial stability
    <*> newIORef (zip []) -- initial signal 
    <*> newIORef arbitraryStartingIdx 
    <*> newIORef Nothing  -- update time
    <*> newIORef 0        -- touch count
    <*> HT.new (==) HT.hashInt -- demand index
    <*> HT.new (==) HT.hashInt -- monitor index

dmd_getNextIndex :: DemandMonitorData e z -> IO Int
dmd_getNextIndex dmd =
    readIORef (dmd_lastid dmd) >>= \ nLast ->
    let nCurr = succ nLast in
    nCurr `seq` writeIORef (dmd_lastid dmd) nCurr >>
    return nCurr

dmd_getDemandData :: DemandMonitorData e z -> Int -> IO (DemandData e)
dmd_getDemandData dmd ix = maybe dd_zero id `fmap` HT.lookup (dmd_demand dmd) ix

dmd_touchDemandFacet :: DemandMonitorData e z -> Int -> IO ()
dmd_touchDemandFacet dmd ix = error "TODO!"
 {-            loadIndex >>= \ ix ->
            dmd_getDemandData dmd ix >>= \ dd ->
            let st = dd_sig dd in
            unless (st_expect st) $  -}

dmd_updateDemandFacet :: DemandMonitorData e z -> Int -> SigUp e -> IO ()
dmd_updateDemandFacet dmd n su = error "TODO!"


dmd_getMonitorData :: DemandMonitorData e z -> Int -> IO (MonitorData z)
dmd_getMonitorData dmd ix = maybe md_zero id `fmap` HT.lookup (dmd_monitor dmd) ix

dmd_touchMonitorFacet :: DemandMonitorData e z -> Int -> LnkUp z -> IO ()
dmd_touchMonitorFacet = error "TODO!"

dmd_updateMonitorFacet :: DemandMonitorData e z -> Int -> LnkUp z -> SigUp () -> IO ()
dmd_updateMonitorFacet = error "TODO!"

-- obtain an index from the demand monitor (if we don't already have one)
getIndex :: DemandMonitorData e z -> IORef (Maybe Int) -> IO Int
getIndex dmd rf = readIORef rf >>= maybe newIndex return
    where newIndex = dmd_getNextIndex dmd >>= \ n ->
                     writeIORef rf (Just n) >>
                     return n

newDemandFacet :: DemandMonitorData e z -> IO (LnkUp e)
newDemandFacet dmd =
    newIORef Nothing >>= \ rfIndex -> -- GUID would be better for distributed system
    let loadIndex = getIndex dmd rfIndex in
    let touch = loadIndex >>= dmd_touchDemandFacet dmd in
    let update su = loadIndex >>= \ ix -> dmd_updateDemandFacet dmd ix su in
    return $ LnkUp { ln_touch = touch, ln_update = update }

newMonitorFacet :: DemandMonitorData e z -> LnkUp z -> IO (LnkUp ())
newMonitorFacet dmd lu =
    newIORef Nothing >>= \ rfIndex -> -- GUID would be better for distributed system
    let loadIndex = getIndex dmd rfIndex in
    let touch = loadIndex >>= \ ix -> dmd_touchMonitorFacet dmd ix lu in
    let update su = loadIndex >>= \ ix -> dmd_updateMonitorFacet dmd ix lu su in
    return $ LnkUp { ln_touch = touch, ln_update = update }



