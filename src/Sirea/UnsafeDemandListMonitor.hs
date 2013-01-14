{-# LANGUAGE GADTs, DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances #-}

-- | Sirea.DemandMonitor provides a useful resource for many types
-- of data, but unfortunately requires an Ord constraint, hindering
-- use of opaque data types (e.g. functions, or dynamic behaviors).
--
-- This is especially unfortunate since dynamic behaviors cannot be
-- shared via stateful means (only volatile mechanisms).
--
-- This UnsafeDemandListMonitor does not process the values before
-- they are monitored. Unfortunately, this also means the values
-- may have duplicates and non-deterministic ordering. Duplicates
-- can violate RDP's idempotence invariants. Discipline is needed
-- to safely (for RDP) use these demand-list monitors, i.e. treating
-- lists as sets.
--
module Sirea.UnsafeDemandListMonitor 
    ( HasUnsafeDemandListMonitor(..)
    ) where

-- TODO: partition IO access into the partition's demand monitors (SRef-like?)

import Control.Applicative
import Control.Exception (assert)
import Data.Function (fix)
import Data.Typeable
import Sirea.Signal
import Sirea.Behavior
import Sirea.B
import Sirea.BCX
import Sirea.Link
import Sirea.Partition
import Sirea.PCX
import Sirea.DemandMonitor (DemandMonitor)
import Sirea.Internal.DemandMonitorData

-- | Obtain ambient instances of demand-monitors by type and name.
-- The list is unsorted and may have duplicates. Developers should
-- nonetheless attempt to treat it as a set.
class HasUnsafeDemandListMonitor b p where 
    unsafeDemandListMonitor :: (Typeable e) => String -> DemandMonitor b p e [e]

instance (Partition p) => HasUnsafeDemandListMonitor (BCX w) p where
    unsafeDemandListMonitor = demandListMonitorBCX

demandListMonitorBCX :: (Partition p, Typeable e) => String -> DemandMonitor (BCX w) p e [e]
demandListMonitorBCX nm = fix $ \ dm ->
    let cwToDMD = getDMD . findByNameInPCX nm . getP dm in
    let d = wrapBCX $ demandFacetB . fst . cwToDMD in
    let m = wrapBCX $ monitorFacetB  . snd . cwToDMD in
    (d,m)

-- load PCX for correct partition (type system tricks)
getP :: (Partition p) => DemandMonitor b p e z -> PCX w -> PCX p
getP _ = findInPCX

newtype UDMD e = UDMD { getDMD :: (DemandAggr e [e], MonitorDist [e]) } deriving (Typeable)

instance (Partition p, Typeable e) => Resource p (UDMD e) where
    locateResource _ cp = UDMD <$> newUDMD cp
 
-- newDMD will return a coupled DemandAggr and MonitorDist pair.
newUDMD :: (Partition p) => PCX p -> IO (DemandAggr e [e], MonitorDist [e])
newUDMD cp =     
    newMonitorDist cp (s_always []) >>= \ md ->
    newDemandAggr cp (primaryMonitorLnk md) sigZipLists >>= \ d ->
    return (d,md)

demandFacetB :: DemandAggr e z -> B (S p e) (S p ())
demandFacetB da = bvoid (unsafeLinkB lnDem) >>> bconst ()
    where lnDem ln = assert (ln_dead ln) $ LnkSig <$> newDemandLnk da

monitorFacetB :: MonitorDist z -> B (S p ()) (S p z)
monitorFacetB md = unsafeLinkB lnMon
    where lnMon LnkDead = return LnkDead -- dead code elim.
          lnMon (LnkSig lu) = LnkSig <$> newMonitorLnk md lu

sigZipLists :: [Sig e] -> Sig [e]
sigZipLists = foldr (s_full_zip jf) (s_always [])
    where jf (Just x) (Just xs) = Just (x:xs)
          jf _ sx = sx


