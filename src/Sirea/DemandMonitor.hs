{-# LANGUAGE GADTs, DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances #-}

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
-- aggregate instability from every contributing demand. To mitigate
-- this, a few variations on demand monitors provide a specialized
-- view of demands.
--
-- This module provides concrete demand monitors for BCX, and a 
-- generic interface via the HasDemandMonitor typeclass.
--
module Sirea.DemandMonitor 
    ( DemandMonitor
    , HasDemandMonitor(..)
    ) where

import Control.Applicative
import Control.Exception (assert)
import Data.Maybe (maybeToList)
import Data.Function (fix)
import Data.Typeable
import Sirea.Signal
import Sirea.Behavior
import Sirea.B
import Sirea.BCX
import Sirea.Link
import Sirea.Partition
import Sirea.PCX
import Sirea.Internal.DemandMonitorData
import Sirea.Internal.BImpl (wrapEqFilter)
import Sirea.Internal.Tuning (dtEqf)

-- | DemandMonitor is a synonym for the (demand,monitor) facet pair 
type DemandMonitor b p e z = (b (S p e) (S p ()), b (S p ()) (S p z))

-- | HasDemandMonitor is the generic interface to obtain a demand
-- monitor. Demand monitors are specific to the function and all 
-- arguments used to identify them.
class HasDemandMonitor b p where
    -- | The idealized demand monitor observes the set of demands,
    -- represented here as a sorted list (low to high) without any
    -- duplicates. Unfortunately, this requires an Ord constraint,
    -- so is unsuitable for opaque types (e.g. functions or dynamic
    -- behaviors).
    demandMonitor :: (Ord e, Typeable e) => String -> DemandMonitor b p e [e]

    -- | The demandListMonitor does not process the set of demands,
    -- so may include duplication and disorder but can represent the
    -- opaque types. Developers should treat it as a set regardless.
    demandListMonitor :: (Typeable e) => String -> DemandMonitor b p e [e]

    -- | An activityMonitor is highly specialized to report a simple
    -- boolean value indicating whether a resource has any demand.
    -- This can be useful to dynamically load resources (though the
    -- AgentResource may prove more efficient for that purpose).
    activityMonitor :: String -> DemandMonitor b p () Bool

{-
    -- | A minimax monitor is similar to a demand monitor, but will
    -- only return the lowest and highest values in the set. This
    -- improves stability relative to a larger set of demands, and
    -- is useful in cases where you can't deal with many concurrent
    -- demands anyway.
    minimaxMonitor :: (Ord e, Typeable e) => String -> DemandMonitor b p e [e]
-}

instance (Partition p) => HasDemandMonitor (BCX w) p where
    demandMonitor = demandMonitorBCX
    demandListMonitor = demandListMonitorBCX
    activityMonitor = activityMonitorBCX

demandMonitorBCX :: (Partition p, Typeable e, Ord e) => String -> DemandMonitor (BCX w) p e [e]
demandMonitorBCX nm = fix $ \ dm ->
    let deMon = getDeMon . findByNameInPCX nm . getP dm in
    let de = wrapBCX $ fst . deMon in
    let mon = wrapBCX $ snd . deMon in
    (de,mon)

demandListMonitorBCX :: (Partition p, Typeable e) => String -> DemandMonitor (BCX w) p e [e]
demandListMonitorBCX nm = fix $ \ dm ->
    let deMonL = getDeMonL . findByNameInPCX nm . getP dm in
    let de = wrapBCX $ fst . deMonL in
    let mon = wrapBCX $ snd . deMonL in
    (de,mon)

activityMonitorBCX :: (Partition p) => String -> DemandMonitor (BCX w) p () Bool
activityMonitorBCX nm = fix $ \ dm ->
    let aMon = getAMon . findByNameInPCX nm . getP dm in
    let de = wrapBCX $ fst . aMon in
    let mon = wrapBCX $ snd . aMon in
    (de,mon)

-- load PCX for correct partition (type system tricks)
getP :: (Partition p) => DemandMonitor b p e z -> PCX w -> PCX p
getP _ = findInPCX

newtype DeMon p e = DeMon { getDeMon :: (DemandMonitor B p e [e]) } deriving (Typeable)
newtype DeMonL p e = DeMonL { getDeMonL :: (DemandMonitor B p e [e]) } deriving (Typeable)
newtype AMon p = AMon { getAMon :: (DemandMonitor B p () Bool) } deriving (Typeable)

instance (Partition p, Typeable e, Ord e) => Resource p (DeMon p e) where
    locateResource _ pcx = DeMon <$> newDemandMonitor mergeSort (==) pcx
        where mergeSort = sigMergeSortSet compare
instance (Partition p, Typeable e) => Resource p (DeMonL p e) where
    locateResource _ pcx = DeMonL <$> newDemandMonitor sigListZip neq pcx
        where neq [] [] = True
              neq _ _ = False
instance (Partition p) => Resource p (AMon p) where
    locateResource _ pcx = AMon <$> newDemandMonitor sigAny (==) pcx
 
-- newDemandMonitor is the basis for other demand monitors.
-- developers to control the zip function and an adjacency filter.
newDemandMonitor :: (Partition p) 
                     => ([Sig e] -> Sig z) -- n-zip function
                     -> (z -> z -> Bool)   -- partial equality (false if unknown)
                     -> PCX p -- partition resources (for time
                     -> IO (B (S p e) (S p ()), B (S p ()) (S p z)) 
newDemandMonitor zfn eqfn cp = 
    newMonitorDist cp (zfn []) >>= \ md ->
    wrapEqFilter dtEqf eqfn (primaryMonitorLnk md) >>= \ lnMon ->
    newDemandAggr cp lnMon zfn >>= \ da ->
    return (demandFacetB da, monitorFacetB md)

demandFacetB :: DemandAggr e z -> B (S p e) (S p ())
demandFacetB da = bvoid (unsafeLinkB lnDem) >>> bconst ()
    where lnDem ln = assert (ln_dead ln) $ LnkSig <$> newDemandLnk da

monitorFacetB :: MonitorDist z -> B (S p ()) (S p z)
monitorFacetB md = unsafeLinkB lnMon
    where lnMon LnkDead = return LnkDead -- dead code elim.
          lnMon (LnkSig lu) = LnkSig <$> newMonitorLnk md lu


-- sigListZip will essentially zip a collection of signals into a
-- signal of collections. The resulting signal is always active, but 
-- has value [] when the argument contains no active signals. In RDP
-- this is used for demand monitors, where the signal is masked by 
-- the observer's demand (to ensure duration coupling).
--
-- The output at any given instant will be ordered the same as the
-- collection of signals. The input list must be finite.
sigListZip :: [Sig a] -> Sig [a]
sigListZip = foldr (s_full_zip jf) (s_always [])
  where jf (Just x) (Just xs) = Just (x:xs)
        jf _ xs = xs



-- merge-sort a list of signals, eliminating duplicates as we go.
-- My (unproven) intuition is that a merge-sort in this manner will
-- be able to reuse some of the computation when there are only a
-- few updates.
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


sigAny :: [Sig a] -> Sig Bool
sigAny sigs = s_full_map isActive sigsMerged
    where sigsMerged = foldr (<|>) empty sigs
          isActive Nothing  = Just False
          isActive (Just _) = Just True

{-
-- take the k minimum or maximum elements. (Min and max is much
-- more stable than taking the middle elements, which could 
-- vary based on changes at either end.)
sigKMin :: (Ord e) => Int -> [Sig e] -> Sig [e]
sigKMin k = fmap (take k) . (sigMergeSortSet compare)

sigKMax :: (Ord e) => Int -> [Sig e] -> Sig [e]
sigKMax k = fmap (take k) . (sigMergeSortSet (flip compare))
-}    


