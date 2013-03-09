{-# LANGUAGE GADTs, DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances #-}

-- | Sirea.DemandMonitor provides a useful resource for many types
-- of data, but unfortunately requires an Ord constraint, hindering
-- use of opaque data types (e.g. functions, or dynamic behaviors).
--
-- This is especially unfortunate since dynamic behaviors cannot be
-- shared via stateful means (only volatile mechanisms).
--
-- The DemandListMonitor module does not process the values before
-- they are monitored. Unfortunately, this also means the values
-- may have duplicates and non-deterministic ordering. Duplicates
-- can violate RDP's idempotence invariants. Discipline is needed
-- to safely (for RDP) use these demand-list monitors, i.e. treating
-- lists as sets.
--
module Sirea.DemandListMonitor 
    ( demandListMonitor
    ) where

-- TODO: partition IO access into the partition's demand monitors (SRef-like?)

import Control.Applicative
import Data.Function (fix)
import Data.Typeable
import Sirea.Signal
import Sirea.Behavior
import Sirea.B
import Sirea.UnsafeLink
import Sirea.Partition
import Sirea.PCX
import Sirea.DemandMonitor (DemandMonitor)
import Sirea.Internal.DemandMonitorData

-- | Obtain ambient instances of demand-monitors by type and name.
--
-- SAFETY CONCERNS: The output list has non-deterministic order and
-- may contain duplicates. However, to support RDP's invariants it
-- is essential to not observe those duplicates, i.e. to treat this
-- as a set. 
demandListMonitor :: (Partition p, Typeable e) => String -> DemandMonitor B p e [e]
demandListMonitor nm = fix $ \ dm ->
    let cwToDMD cw = getPCX dm cw >>= fmap getDMD . findByNameInPCX nm in
    let d = demandFacetB $ fmap fst . cwToDMD in
    let m = monitorFacetB $ fmap snd . cwToDMD in
    (d,m)



-- load PCX for correct partition (type system tricks)
getPCX :: (Partition p) => DemandMonitor b p e z -> PCX W -> IO (PCX p)
getPCX _ = findInPCX

newtype UDMD e = UDMD { getDMD :: (DemandAggr e [e], MonitorDist [e]) } deriving (Typeable)

instance (Partition p, Typeable e) => Resource p (UDMD e) where
    locateResource _ cp = UDMD <$> newUDMD cp
instance (Partition p, Typeable e) => NamedResource p (UDMD e)
 
-- newDMD will return a coupled DemandAggr and MonitorDist pair.
newUDMD :: (Partition p) => PCX p -> IO (DemandAggr e [e], MonitorDist [e])
newUDMD cp =     
    getPSched cp >>= \ pd ->
    newMonitorDist pd (s_always []) >>= \ md ->
    let lnMon = primaryMonitorLnk md in
    newDemandAggr pd lnMon sigZipLists >>= \ d ->
    return (d,md)


demandFacetB :: (PCX W -> IO (DemandAggr e z)) -> B (S p e) (S p ())
demandFacetB getDA = bvoid (unsafeLinkB_ lnDem) >>> bconst () where
    lnDem cw = getDA cw >>= newDemandLnk

monitorFacetB :: (PCX W -> IO (MonitorDist z)) -> B (S p ()) (S p z)
monitorFacetB getMD = unsafeLinkBL lnMon where
    lnMon cw lu = getMD cw >>= flip newMonitorLnk lu

sigZipLists :: [Sig e] -> Sig [e]
sigZipLists = foldr (s_full_zip jf) (s_always [])
    where jf (Just x) (Just xs) = Just (x:xs)
          jf _ sx = sx


