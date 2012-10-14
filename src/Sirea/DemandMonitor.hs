{-# LANGUAGE GADTs #-}

-- | RDP behaviors are effectful, albeit in a constrained manner. 
-- RDP will influence resources via a set of concurrently active 
-- signals at any given instant.
--
-- Among the simplest useful resources for RDP is a demand monitor.
-- A demand monitor allows developers to observe the set of active
-- demands. Demand monitors are memoryless; after demand halts, it
-- is no longer observed. Demand monitors enable a reflective style
-- for programming, i.e. developers observe presence of behaviors
-- by their reflection in a demand monitor.
--
-- This allows indirect communication - one-to-many, many-to-many.
-- It is analogous to a bulletin board or registry, albeit without 
-- persistent state. Demand monitors are also a suitable primitive
-- for explaining many-to-one observation by a resource of demands.
-- Demand monitors are useful for open or extensible systems, where
-- new agents can join and contribute behaviors.
--
-- Every demand monitor has two associated behaviors: one to publish
-- demand, one to monitor it. These are called the demand facet and
-- the monitor facet, respectively. Using dynamic behaviors, it is
-- possible to separate these facets to enforce secure communication
-- patterns (cf. object capability model). 
--
-- A difficulty with demand monitors is that they are very unstable.
-- When demands change at independent frequency, the set of demands
-- fluctuates more rapidly than any individual demand. Consequently,
-- demand monitors are most suitable for highly stable demands, or
-- for relatively few publishers (few-to-many). 
--
-- To improve stability, some specializations exist - monitors that
-- return only the highest or lowest demands, or only report whether
-- active demand exists. 
--
-- The volatile, stateless semantics of demand monitors are valuable
-- for reasoning about correctness over time. Demand monitor is not
-- the only model with this property; for example, it is possible to
-- achieve similar characteristics for constraint systems. Stability
-- of constraint systems could even be superior: maintain a solution
-- with minimal changes over time to adapt changing constraints.
--
-- This module provides concrete implementations of demand monitors
-- and a few variations (activity monitors, minimum and maximums).
-- While several operations create `new` monitors, those are for
-- developers; clients should obtain demand monitors as resources
-- via discovery. 
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
import Sirea.Internal.DemandMonitorData

-- | Create a list-based demand monitor in the IO monad. The list
-- should be treated as a set (behavior independent of duplication
-- and ordering). If `e` is Ord, suggest use of newDemandSetMonitor
-- so it is an actual set.
--
-- This is generally not for direct use by Sirea clients; instead,
-- it serves as a primitive for a ResourceSpace or BCX, which can
-- introduce a demand monitor where it is needed.
newDemandMonitor :: IO (B (S p e) (S p ()) , B (S p ()) (S p [e]))
newDemandMonitor = newDemandMonitorBase sigListZip bothNil
    where bothNil [] [] = True -- nils are equal
          bothNil _ _ = False -- everything else is unknown


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

-- newDemandMonitorZeq is similar to newDemandMonitorZ, but adds
-- an adjacency equality filter to the result. The equality filter
-- will eliminate redundant updates over time, and eliminate some
-- redundancies in the short term (by delaying updates). These
-- eliminations can reduce rework downstream.
--
-- See also: the notes for newDemandMonitorZ
newDemandMonitorZeq :: (Eq z) => ([Sig e] -> Sig z) 
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
newDemandSetMonitor :: (Ord e) => IO (B (S p e) (S p ()), B (S p ()) (S p [e]))
newDemandSetMonitor = newDemandMonitorZeq (sigMergeSortSet compare)

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

-- merge two list-sets into a new list-set
mergeListSet :: (e -> e -> Ordering) -> [e] -> [e] -> [e]
mergeListSet _ [] ys = ys
mergeListSet _ xs [] = xs
mergeListSet cmp xs@(x:xs') ys@(y:ys') =
    case cmp x y of
        LT -> x:(mergeListSet cmp xs' ys )
        EQ -> x:(mergeListSet cmp xs' ys')
        GT -> y:(mergeListSet cmp xs  ys')


-- | newActivityMonitor is a demand-monitor that returns whether or
-- not there is any active demand on the demand facet. This is very
-- stable to changes, and useful for many systems. 
newActivityMonitor :: IO (B (S p ()) (S p ()), B (S p ()) (S p Bool))
newActivityMonitor = newDemandMonitorZeq sigAny

sigAny :: [Sig a] -> Sig Bool
sigAny sigs = s_full_map isActive sigsMerged
    where sigsMerged = foldr (<|>) empty sigs
          isActive Nothing  = Just False
          isActive (Just _) = Just True

-- | newKMaximumMonitor will monitor a list of the top K demands (if
-- that many exist), rated and sorted based on Ordinal functions. 
-- This limits instability to the top K demands. Usually, K should
-- be small. Note: K is counted after duplicates are eliminated.
--
-- Motivation: top K demands is probably more stable than N demands, 
-- assuming N > K. Computation costs are also under better control.
-- Improves stability and performance relative to DemandSetMonitor. 
newKMaximumMonitor :: (Ord e) => Int -> IO (B (S p e) (S p ()), B (S p ()) (S p [e]))
newKMaximumMonitor k =
    if (k <  1) then return (bconst (), bconst []) 
                else newDemandMonitorZeq (sigKMax k) 

-- take the k max; could be much more optimal if I sorted in the
-- opposite direction (to eliminate the reverse)
sigKMax :: (Ord e) => Int -> [Sig e] -> Sig [e]
sigKMax k = fmap (take k) . (sigMergeSortSet (flip compare))

-- | newKMinimumMonitor is simply the KMaximiumMonitor inverted.
newKMinimumMonitor :: (Ord e) => Int -> IO (B (S p e) (S p ()), B (S p ()) (S p [e]))
newKMinimumMonitor k = 
    if (k <  1) then return (bconst (), bconst []) 
                else newDemandMonitorZeq (sigKMin k)

sigKMin :: (Ord e) => Int -> [Sig e] -> Sig [e]
sigKMin k = fmap (take k) . (sigMergeSortSet compare)

-- NOTE: As tempting as a `median monitor` might be, it is not a 
-- viable construct in RDP. Due to spatial idempotence, RDP is not
-- allowed to count equivalent demands. I could return the "middle"
-- demand (not including duplicates), but it doesn't correspond to 
-- any useful statistic, and would not be especially stable.

-- how much history to keep, to accommodate straggling monitors or  
-- demand resources. This can often be kept small; anticipation of
-- dynamic behaviors will cover most concerns. A larger value will
-- offer a greater range for eventual consistency, but also increase
-- buffering costs and delays operations that wait for stability. 
--
-- NOTE: I could probably split the demand monitor history to
-- separately support straggling demand sources (which reduces the
-- stability) and straggling monitors (which merely increases the
-- local history buffer). I haven't found a strong motivation to do
-- so, yet.
dmd_default_history :: DT
dmd_default_history = 0.025

-- newDemandMonitorBase is the basis for other demand monitors.
-- It allows developers to control the zip function and an
-- adjacency filter.
--
-- The equality function may be partial; it should return False if
-- the equality is not known or computation of equality would be
-- divergent.
--
newDemandMonitorBase :: ([Sig e] -> Sig z) -- n-zip function
                     -> (z -> z -> Bool)   -- partial equality (false if unknown)
                     -> IO (B (S p e) (S p ()), B (S p ()) (S p z)) 
newDemandMonitorBase zfn eqfn = error "TODO: newDemandMonitorBase!"
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





