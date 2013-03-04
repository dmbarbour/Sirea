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
    , activityMonitor   
    ) where

-- TODO: Consider making DemandMonitor more compositional, by having
-- a set for input as well as for output. I.e. `Set a ~> Set a` with
-- the input set containing a few elements and the output containing
-- a union of all elements. This would make it easier to work with
-- collections in general.

import Control.Applicative
import Control.Exception (assert)
import Data.Maybe (maybeToList)
import Data.Function (fix)
import Data.Typeable
import Sirea.Signal
import Sirea.Behavior
import Sirea.B
import Sirea.UnsafeLink
import Sirea.Partition
import Sirea.PCX
import Sirea.Internal.DemandMonitorData
import Sirea.Internal.B0Impl (wrapEqFilter)
import Sirea.Internal.Tuning (dtEqf)

-- | DemandMonitor is a synonym for the (demand,monitor) facet pair 
type DemandMonitor b p e z = (b (S p e) (S p ()), b (S p ()) (S p z))

-- | Obtain a demand monitor resource, as a (demand,monitor) pair.
demandMonitor :: (Ord e, Typeable e, Partition p) => String -> DemandMonitor B p e [e]
demandMonitor nm = fix $ \ dm ->
    let cwToDMD cw = getPCX dm cw >>= fmap getDMD . findByNameInPCX nm in
    let d = demandFacetB $ fmap fst . cwToDMD in
    let m = monitorFacetB $ fmap snd . cwToDMD in
    (d,m)

-- | An activityMonitor is a simple wrapper for a demand monitor on
-- a unit signal, to change the result to a boolean. (For unit, the
-- only monitored values would be [] or [()].)
activityMonitor :: String -> DemandMonitor B p () Bool 
activityMonitor nm = (d,m') where
    (d,m) = demandMonitor nm
    m' = m >>> bfmap (not . null)

-- load PCX for correct partition (type system tricks)
getPCX :: (Partition p) => DemandMonitor b p e z -> PCX W -> IO (PCX p)
getPCX _ = findInPCX

newtype DMD e = DMD { getDMD :: (DemandAggr e [e], MonitorDist [e]) } deriving (Typeable)

instance (Partition p, Typeable e, Ord e) => Resource p (DMD e) where
    locateResource _ cp = DMD <$> newDMD cp
instance (Partition p, Typeable e, Ord e) => NamedResource p (DMD e)
 
-- newDMD will return a coupled DemandAggr and MonitorDist pair.
--   
newDMD  :: (Partition p, Ord e) => PCX p -> IO (DemandAggr e [e], MonitorDist [e])
newDMD cp =     
    newMonitorDist cp (s_always []) >>= \ md ->
    wrapEqFilter dtEqf (==) (primaryMonitorLnk md) >>= \ lnMon ->
    newDemandAggr cp lnMon dmdZip >>= \ d ->
    return (d,md)

-- Zip function for demand monitors, will provide sorted lists in
-- each instant then filter equivalent instants over time. This is
-- further combined with a state-based choke and equality filter.
dmdZip :: (Ord e) => [Sig e] -> Sig [e]
dmdZip = s_adjeqf (==) . sigMergeSortSet compare

demandFacetB :: (PCX W -> IO (DemandAggr e z)) -> B (S p e) (S p ())
demandFacetB getDA = bvoid (unsafeLinkB lnDem) >>> bconst ()
    where lnDem cw ln = assert (ln_dead ln) $ 
                getDA cw >>= \ da ->
                newDemandLnk da >>= \ lu' ->
                return (LnkSig lu')

monitorFacetB :: (PCX W -> IO (MonitorDist z)) -> B (S p ()) (S p z)
monitorFacetB getMD = unsafeLinkB lnMon
    where lnMon _ LnkDead = return LnkDead -- dead code elim.
          lnMon cw (LnkSig lu) = 
            getMD cw >>= \ md ->
            newMonitorLnk md lu >>= \ lu' ->
            return (LnkSig lu')

-- merge-sort a list of signals, eliminating duplicates as we go.
-- My (unproven) intuition is that a merge-sort in this manner will
-- be able to reuse some of the computation when there are only a
-- few updates.
--
sigMergeSortSet :: (e -> e -> Ordering) -> [Sig e] -> Sig [e]
sigMergeSortSet _ [] = s_always []
sigMergeSortSet _ (s:[]) = s_full_map (Just . maybeToList) s
sigMergeSortSet cmp ss = 
    let n = length ss in
    let half = n `div` 2 in
    let (ssHd,ssTl) = splitAt half ss in
    let sigHd = sigMergeSortSet cmp ssHd in
    let sigTl = sigMergeSortSet cmp ssTl in
    s_zip (mergeListSet cmp) sigHd sigTl

-- merge two list-sets into a new list-set. A list-set is
-- already ordered and without duplicates, which simplifies
-- comparison. 
mergeListSet :: (e -> e -> Ordering) -> [e] -> [e] -> [e]
mergeListSet _ [] ys = ys
mergeListSet _ xs [] = xs
mergeListSet cmp xs@(x:xs') ys@(y:ys') =
    case cmp x y of
        LT -> x:(mergeListSet cmp xs' ys )
        EQ -> x:(mergeListSet cmp xs' ys')
        GT -> y:(mergeListSet cmp xs  ys')

