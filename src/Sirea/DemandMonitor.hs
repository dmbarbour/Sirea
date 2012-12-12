{-# LANGUAGE GADTs #-}

-- | RDP behaviors are effectful, albeit in a constrained manner. A
-- resource's state can be influenced by the set of demands on it. A
-- behavior that observes the resource may respond based on present
-- and anticipated resource state.  
--
-- A simple kind of resource is Demand Monitors.
--
-- The state of a demand monitor resource is simply equal to the set
-- of demands on it at a given instant. Demands are imposed through
-- a behavior called the demand facet. The state of the resource is
-- observed through behaviors called monitor facets. 
--
-- Demand monitors have a number of useful applications: as volatile
-- registries or blackboards for modeling agent environments, and as
-- building blocks for AgentResource based resource adapters. Demand
-- monitor is basis for one-to-many and many-to-many communications.
-- Demand monitors are an abundant resource, so an application can
-- access as many as it needs.
--
-- The weakness of demand monitors is stability. A demand set will
-- aggregate instability from every contributing demand. To mitigate
-- this, some demand monitors provide a simplified view of demand.
--
module Sirea.DemandMonitor 
    (
    ) where

import Control.Applicative
import Control.Exception (assert)
import Data.Maybe (maybeToList)
import Sirea.Signal
import Sirea.Behavior
import Sirea.B
import Sirea.Link
import Sirea.Time
import Sirea.Partition
import Sirea.PCX
import Sirea.Internal.DemandMonitorData
import Sirea.Internal.BImpl (wrapEqFilter)
import Sirea.Internal.Tuning (dtEqf)

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

-- take the k minimum or maximum elements. (Min and max is much
-- more stable than taking the middle elements, which could 
-- vary based on changes at either end.)
sigKMin :: (Ord e) => Int -> [Sig e] -> Sig [e]
sigKMin k = fmap (take k) . (sigMergeSortSet compare)

sigKMax :: (Ord e) => Int -> [Sig e] -> Sig [e]
sigKMax k = fmap (take k) . (sigMergeSortSet (flip compare))


{-

-- | Create a list-based demand monitor in the IO monad. The list
-- should be treated as a set (behavior independent of duplication
-- and ordering). If `e` is Ord, suggest use of newDemandSetMonitor
-- so it is an actual set.
--
-- This is generally not for direct use by Sirea clients; instead,
-- it serves as a primitive for a ResourceSpace or BCX, which can
-- introduce a demand monitor where it is needed.
newDemandMonitor :: PCX p -> IO (B (S p e) (S p ()) , B (S p ()) (S p [e]))
newDemandMonitor = newDemandMonitorBase sigListZip bothNil
    where bothNil [] [] = True -- nils are equal
          bothNil _ _ = False -- everything else is unknown



-- newDemandMonitorZeq is similar to newDemandMonitorZ, but adds
-- an adjacency equality filter to the result. The equality filter
-- will eliminate redundant updates over time, and eliminate some
-- redundancies in the short term (by delaying updates). These
-- eliminations can reduce rework downstream.
--
-- See also: the notes for newDemandMonitorZ
newDemandMonitorZeq :: (Eq z) 
                    => ([Sig e] -> Sig z)
                    -> PCX p 
                    -> IO (B (S p e) (S p ()) , B (S p ()) (S p z))
newDemandMonitorZeq = flip newDemandMonitorBase (==)

-- | newDemandSetMonitor will provide the output as an ordered set 
-- of the inputs, with duplicates eliminated. It will also perform
-- filtering of duplicate sets, to avoid redundant updates. This 
-- should be preferred to the simple list-based demand monitor, as
-- it is far more deterministic and stable, but not all types have
-- ordinal properties.
--
-- The resulting set is ordered from lowest to highest.
newDemandSetMonitor :: (Ord e) => PCX p -> IO (B (S p e) (S p ()), B (S p ()) (S p [e]))
newDemandSetMonitor = newDemandMonitorZeq (sigMergeSortSet compare)





-- | newActivityMonitor is a demand-monitor that returns whether or
-- not there is any active demand on the demand facet. This is very
-- stable to changes, and useful for many systems. 
newActivityMonitor :: PCX p -> IO (B (S p ()) (S p ()), B (S p ()) (S p Bool))
newActivityMonitor = newDemandMonitorZeq sigAny


-- | newKMaximumMonitor will monitor a list of the top K demands (if
-- that many exist), rated and sorted based on Ordinal functions. 
-- This limits instability to the top K demands. Usually, K should
-- be small. Note: K is counted after duplicates are eliminated.
--
-- Motivation: top K demands is probably more stable than N demands, 
-- assuming N > K. Computation costs are also under better control.
-- Improves stability and performance relative to DemandSetMonitor. 
newKMaximumMonitor :: (Ord e) => Int -> PCX p -> IO (B (S p e) (S p ()), B (S p ()) (S p [e]))
newKMaximumMonitor k =
    if (k <  1) then return (bconst (), bconst []) 
                else newDemandMonitorZeq (sigKMax k) 


-- | newKMinimumMonitor is simply the KMaximiumMonitor inverted.
newKMinimumMonitor :: (Ord e) => Int -> PCX p -> IO (B (S p e) (S p ()), B (S p ()) (S p [e]))
newKMinimumMonitor k = 
    if (k <  1) then return (bconst (), bconst []) 
                else newDemandMonitorZeq (sigKMin k)

-}

    
{-
    
    --( DemandAggr, newDemandAggr, newDemandLnk
    --, MonitorDist, newMonitorDist, mainMonitorLnk, newMonitorLnk    new
    

  {-  newDemandMonitorData zfn eqfn dmd_default_history >>= \ dmd ->
    return (demandFacetB dmd, monitorFacetB dmd)

demandFacetB :: DemandMonitorData e z -> B w (S p e) (S p ())
demandFacetB dmd = (unsafeLinkB lnk &&& bconst ()) >>> bsnd
    where lnk = MkLnk { ln_tsen = True, ln_peek = 0, ln_build = build }
          build k = assert (ln_dead k) $ LnkSig <$> newDemandFacet dmd

monitorFacetB :: DemandMonitorData e z -> B w (S p ()) (S p z)
monitorFacetB dmd = unsafeLinkB lnk
    where lnk = MkLnk { ln_tsen = True, ln_peek = 0, ln_build = build } 
          build LnkDead = return LnkDead
          build (LnkSig lu) = LnkSig <$> newMonitorFacet dmd lu
   -}




-- kmax is a variation of lzip that only returns the maximum k
-- signals according to the Ordinal type. These signals are sorted
-- from max downwards, with no duplicates. k should be small. The
-- resulting signal is always active, with value [] if no input is
-- active. 
--
-- Note: the current implementation will report many redundant 
-- updates based on changes in signals not contributing to the
s_kmax :: (Ord a) => Int -> [Sig a] -> Sig [a]
s_kmax = sigKBest (>)

-- kmin is similar to kmax, but returns the lowest signals and
-- sorts from min upwards, no duplicates. k should be small.
s_kmin :: (Ord a) => Int -> [Sig a] -> Sig [a]
s_kmin = sigKBest (<)

-- return the k `best` signals according to a comparison function
-- (in order from best to worst). This assumes k is relatively
-- small; very large k could become expensive quickly. 
--
-- Note: here I assume two values are equal if neither is better
-- than the other. This assumption is valid because sigKBest is
-- only instantiated from Ord functions (<) and (>).
--
-- TODO: find some way to optimize the `zip`, so that we have 
-- more stability; i.e. reduce to just the contributing signals
-- at any given instant and reduce need for adjeqf filtering. 
sigKBest :: (e -> e -> Bool) -> Int -> [Sig e] -> Sig [e]
sigKBest bt k ls =
    if (k < 1) then s_always [] else
    if (k == 1) then sigBest bt ls else
    fmap (take k) $ foldr (s_full_zip jf) (s_always []) ls
    where jf (Just x) (Just xs) = Just (sortedInsert bt x xs)
          jf _ xs = xs

sortedInsert :: (x -> x -> Bool) -> x -> [x] -> [x]
sortedInsert _ x [] = (x:[])
sortedInsert bt x (y:ys) = if (x `bt` y) then (x:y:ys) else y:(si y ys)
    where -- decide if x is after k or dropped (if equal to k)
            si k [] = if (k `bt` x) then (x:[]) else []
            si k (z:zs) = 
                if (x `bt` z)
                    then if (k `bt` x) then (x:z:zs) -- insert x after k
                                       else (z:zs)   -- equal to last; drop x
                    else z:(si z zs) -- x inserts somewhere after z

-- return the best single signal according to the comparison 
-- function. Used by sigKBest for the simple case where only
-- one value is returned (can optimize for this a bit).
--
-- TODO: something like `weave` here, to avoid unnecessary 
-- updates? Or maybe I could run some filters and merges?
sigBest :: (e -> e -> Bool) -> [Sig e] -> Sig [e]
sigBest bt = foldr (s_full_zip jf) (s_always [])
    where jf (Just x) (Just []) = Just (x:[])
          jf (Just x) xs@(Just (y:_)) =
            if (x `bt` y) 
                then Just (x:[]) 
                else xs
          jf _ xs = xs


-}


