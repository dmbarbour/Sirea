{-# LANGUAGE GADTs, DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances,
             GeneralizedNewtypeDeriving
  #-}

-- | RDP behaviors are effectful, albeit in a constrained manner. A
-- resource's state may be influenced by the set of demands on it. A
-- behavior observing a resource may react to present or anticipated 
-- resource state.
--
-- Demand monitors are a simple, useful resource.
--
-- The state of a demand monitor resource is simply equal to the set
-- of demand signals sent to it. Demand monitors are modeled with a 
-- pair of behaviors: one to impose demand, one to monitor state -
-- respectively called 'demand facet' and 'monitor facet'.
--
-- Demand monitors support one-to-many or many-to-one communication,
-- and simple collaboration patterns. They are volatile - continuous
-- signals are necessary to keep a value in the monitor, and some
-- values might never be observed.
--
-- Demand monitors are an 'abundant' resource: an application may
-- access however many it needs. Unfortunately, an unused demand
-- monitor will not be fully GC'd, so it is necessary to avoid
-- gratuitously initializing monitors.
--
-- The weakness of demand monitors is stability. A demand set will
-- aggregate instability from every contributing demand, which can
-- result in problems scaling if fan-in is large. 
--
-- This module provides concrete demand monitors for BCX, and a 
-- generic interface via the HasDemandMonitor typeclass.
--
module Sirea.DemandMonitor 
    ( DemandMonitor
    , demandMonitor
    , bdemand, bmonitor
    , activityMonitor 
    , bactivate, bactive  
    , demandListMonitor
    , bdemandl, bmonitorl
    ) where

-- TODO: Consider making DemandMonitor more compositional, by having
-- a set for input as well as for output. I.e. `Set a ~> Set a` with
-- the input set containing a few elements and the output containing
-- a union of all elements. This would make it easier to work with
-- collections in general.

import Control.Applicative
import Data.Maybe (isJust)
import Data.Function (fix)
import Data.Typeable
import Data.Set (Set)
import qualified Data.Set as S
import Sirea.Signal
import Sirea.Behavior
import Sirea.B
import Sirea.UnsafeLink
import Sirea.Partition
import Sirea.PCX
import Sirea.Internal.DemandMonitorData
--import Sirea.Internal.B0Impl (wrapLnEqShift)

-- | DemandMonitor is a synonym for the (demand,monitor) facet pair 
type DemandMonitor b p e z = (b (S p e) (S p ()), b (S p ()) (S p z))

-- | Obtain a demand monitor resource, as a (demand,monitor) pair.
--
-- This demand monitor will return the set of active demands.
demandMonitor :: (Ord e, Typeable e, Partition p) => String -> DemandMonitor B p e (Set e)
demandMonitor nm = fix $ \ dm ->
    let cwToDMD cw = getPCX dm cw >>= fmap getDMD . findByNameInPCX nm in
    let d = demandFacetB $ fmap fst . cwToDMD in
    let m = monitorFacetB $ fmap snd . cwToDMD in
    (d,m)

-- | contribute demands to a set of demands, which can be observed
-- by any 'bmonitor' with the same identifying string.
--
-- Note that if you use a demand monitor in this fashion, it is wise
-- to use a common string variable to guard against spelling errors
-- or inconsistency in refactoring. Each string discovers a distinct
-- demand monitor resource.
bdemand :: (Ord e, Typeable e, Partition p) => String -> B (S p e) (S p ())
bdemand = fst . demandMonitor

-- | observe set of active demands.
bmonitor :: (Ord e, Typeable e, Partition p) => String -> B (S p ()) (S p (Set e))
bmonitor = snd . demandMonitor

-- | activityMonitor is a specialized demand monitor with unit type,
-- which means it only monitors whether at least one input signal is
-- active. This observed value is 'True' for durations where there
-- is at least one active demand.
activityMonitor :: (Partition p) => String -> DemandMonitor B p () Bool 
activityMonitor nm = fix $ \ dm ->
    let cwToAMon cw = getPCX dm cw >>= fmap getAMon . findByNameInPCX nm in
    let d = demandFacetB $ fmap fst . cwToAMon in
    let m = monitorFacetB $ fmap snd . cwToAMon in
    (d,m)

-- | activate an activityMonitor resource
bactivate :: (Partition p) => String -> B (S p ()) (S p ())
bactivate = fst . activityMonitor

-- | test whether an activityMonitor resource is active.
bactive :: (Partition p) => String -> B (S p ()) (S p Bool)
bactive = snd . activityMonitor

-- load PCX for correct partition (type system tricks)
getPCX :: (Partition p) => DemandMonitor b p e z -> PCX W -> IO (PCX p)
getPCX _ = findInPCX

newtype DMD e = DMD { getDMD :: (DemandAggr e (Set e), MonitorDist (Set e)) } 
    deriving (Typeable)

instance (Partition p, Typeable e, Ord e) => Resource p (DMD e) where
    locateResource _ cp = DMD <$> newDMD cp
instance (Partition p, Typeable e, Ord e) => NamedResource p (DMD e)
 
-- newDMD will return a coupled DemandAggr and MonitorDist pair.
newDMD :: (Partition p, Ord e) => PCX p -> IO (DemandAggr e (Set e), MonitorDist (Set e))
newDMD cp =     
    getPSched cp >>= \ pd ->
    newMonitorDist pd (s_always S.empty) >>= \ md ->
    let lu = primaryMonitorLnk md in
    --wrapLnEqShift (==) lu >>= \ luEq ->
    newDemandAggr pd lu (s_adjeqf (==) . setZip) >>= \ d ->
    return (d,md)

setZip :: (Ord e) => [Sig e] -> Sig (Set e)
setZip [] = s_always S.empty
setZip (s:[]) = s_full_map (Just . maybe S.empty S.singleton) s
setZip ss = s_zip S.union s1 s2 where
    (h1,h2) = splitAt (length ss `div` 2) ss
    s1 = setZip h1
    s2 = setZip h2

-- TODO: seek more efficient zip operations. 

newtype AMon = AMon { getAMon :: (DemandAggr () Bool, MonitorDist Bool) } 
    deriving (Typeable)

instance (Partition p) => Resource p AMon where
    locateResource _ cp = AMon <$> newAMon cp
instance (Partition p) => NamedResource p AMon
 
-- newAM will return a coupled DemandAggr and MonitorDist pair.
newAMon :: (Partition p) => PCX p -> IO (DemandAggr () Bool, MonitorDist Bool)
newAMon cp =     
    getPSched cp >>= \ pd ->
    newMonitorDist pd (s_always False) >>= \ md ->
    let lu = primaryMonitorLnk md in
    --wrapLnEqShift (==) lu >>= \ luEq ->
    newDemandAggr pd lu amonZip >>= \ d ->
    return (d,md)

amonZip :: [Sig ()] -> Sig Bool
amonZip = 
    s_full_map (Just . isJust) .
    s_const () .
    foldr s_merge s_never 

demandFacetB :: (PCX W -> IO (DemandAggr e z)) -> B (S p e) (S p ())
demandFacetB getDA = bvoid (unsafeLinkB_ lnDem) >>> bconst () where
    lnDem cw = getDA cw >>= newDemandLnk

monitorFacetB :: (PCX W -> IO (MonitorDist z)) -> B (S p ()) (S p z)
monitorFacetB getMD = unsafeLinkBL lnMon where
    lnMon cw lu = getMD cw >>= flip newMonitorLnk lu


-- | demandListMonitor is necessary for types that cannot meet the
-- Ord constraint. It behaves similar to demandMonitor, but there
-- are some extra safety concerns: the resulting list has a non
-- deterministic ordering, and may contain duplicates, neither of
-- which should affect observable behavior. Developers must be
-- careful to only use the monitored results in a context or manner
-- where ordering or duplication is irrelevant.
demandListMonitor :: (Partition p, Typeable e) => String -> DemandMonitor B p e [e]
demandListMonitor nm = fix $ \ dm ->
    let cwToDMD cw = getPCX dm cw >>= fmap getLDMD . findByNameInPCX nm in
    let d = demandFacetB $ fmap fst . cwToDMD in
    let m = monitorFacetB $ fmap snd . cwToDMD in
    (d,m)

-- | Contribute demand to a list; useful if type lacks Ord property.
-- Demand lists are entirely distinct from demand sets of bdemand.
bdemandl :: (Partition p, Typeable e) => String -> B (S p e) (S p ())
bdemandl = fst . demandListMonitor

-- | Monitor a list of demands. Note that the list should be treated
-- as a set - i.e. ordering and duplication must not affect observed
-- behavior (otherwise this introduces non-determinism).
bmonitorl :: (Partition p, Typeable e) => String -> B (S p ()) (S p [e])
bmonitorl = snd . demandListMonitor

newtype LDMD e = LDMD { getLDMD :: (DemandAggr e [e], MonitorDist [e]) } deriving (Typeable)
instance (Partition p, Typeable e) => Resource p (LDMD e) where
    locateResource _ cp = LDMD <$> newLDMD cp
instance (Partition p, Typeable e) => NamedResource p (LDMD e)
 
-- newDMD will return a coupled DemandAggr and MonitorDist pair.
newLDMD :: (Partition p) => PCX p -> IO (DemandAggr e [e], MonitorDist [e])
newLDMD cp =     
    getPSched cp >>= \ pd ->
    newMonitorDist pd (s_always []) >>= \ md ->
    let lu = primaryMonitorLnk md in
    newDemandAggr pd lu sigZipLists >>= \ d ->
    return (d,md)

sigZipLists :: [Sig e] -> Sig [e]
sigZipLists = foldr (s_full_zip jf) (s_always [])
    where jf (Just x) (Just xs) = Just (x:xs)
          jf _ xs = xs






