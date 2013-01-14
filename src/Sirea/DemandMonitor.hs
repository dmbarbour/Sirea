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
    , HasDemandMonitor(..)
    , activityMonitor   
    ) where

-- TODO: partition IO access into the partition's demand monitors (SRef-like?)
-- TODO: decide how to handle opaque non-Ord demand types
--   option 1: via dedicated class 
--   option 2: via dedicated BCX instance
--   option 3: using semantic hack like StableName (..I'd rather not..)
--   option 4: developing a PartialOrd class?

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

-- | Obtain ambient instances of demand-monitors by type and name.
-- The output list will be sorted (ascending), without duplicates.
class HasDemandMonitor b p where 
    demandMonitor :: (Ord e, Typeable e) => String -> DemandMonitor b p e [e]

-- | An activityMonitor is simply a demand monitor that reports only
-- whether any activity is present. Note that if you're always going
-- to perform the same action when active, an AgentResource would be
-- more appropriate.
activityMonitor :: (HasDemandMonitor b p, BFmap b) => String -> DemandMonitor b p () Bool
activityMonitor nm = (d',m') where
    (d,m) = demandMonitor nm
    d' = bfmap AM >>> d
    m' = m >>> bfmap (not . null)

newtype AM = AM () deriving (Typeable, Eq, Ord) -- to separate activityMonitor instances

instance (Partition p) => HasDemandMonitor (BCX w) p where
    demandMonitor = demandMonitorBCX

demandMonitorBCX :: (Partition p, Typeable e, Ord e) => String -> DemandMonitor (BCX w) p e [e]
demandMonitorBCX nm = fix $ \ dm ->
    let cwToDMD = getDMD . findByNameInPCX nm . getP dm in
    let d = wrapBCX $ demandFacetB . fst . cwToDMD in
    let m = wrapBCX $ monitorFacetB  . snd . cwToDMD in
    (d,m)

-- load PCX for correct partition (type system tricks)
getP :: (Partition p) => DemandMonitor b p e z -> PCX w -> PCX p
getP _ = findInPCX

newtype DMD e = DMD { getDMD :: (DemandAggr e [e], MonitorDist [e]) } deriving (Typeable)

instance (Partition p, Typeable e, Ord e) => Resource p (DMD e) where
    locateResource _ cp = DMD <$> newDMD mergeSort (==) cp
        where mergeSort = sigMergeSortSet compare
 
-- newDMD will return a coupled DemandAggr and MonitorDist pair.
newDMD  :: (Partition p)
        => ([Sig e] -> Sig z)
        -> (z -> z -> Bool)
        -> PCX p 
        -> IO (DemandAggr e z, MonitorDist z)
newDMD zfn eqfn cp =     
    newMonitorDist cp (zfn []) >>= \ md ->
    wrapEqFilter dtEqf eqfn (primaryMonitorLnk md) >>= \ lnMon ->
    newDemandAggr cp lnMon zfn >>= \ d ->
    return (d,md)

demandFacetB :: DemandAggr e z -> B (S p e) (S p ())
demandFacetB da = bvoid (unsafeLinkB lnDem) >>> bconst ()
    where lnDem ln = assert (ln_dead ln) $ LnkSig <$> newDemandLnk da

monitorFacetB :: MonitorDist z -> B (S p ()) (S p z)
monitorFacetB md = unsafeLinkB lnMon
    where lnMon LnkDead = return LnkDead -- dead code elim.
          lnMon (LnkSig lu) = LnkSig <$> newMonitorLnk md lu

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

