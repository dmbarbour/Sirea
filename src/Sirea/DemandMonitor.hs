
-- | RDP behaviors are effectful, albeit in a constrained manner to
-- support declarative reasoning about and composition of effects. 
-- RDP can influence a resource via the set of active signals to the
-- resource at any given instant. (Signals in this influential role 
-- are called demands, thus the name Reactive Demand Programming.)
--
-- Among the simplest useful resources for RDP is a demand monitor.
-- A demand monitor allows developers to observe the set of active
-- demands. Demand monitors are memoryless; after demand halts, it
-- is no longer observed. Demand monitors enable a reflective style
-- for programming, i.e. developers observe presence of behaviors
-- by their reflection in a demand monitor.
--
-- This allows indirect communication, one-to-many or many-to-many.
-- It is analogous to a bulletin board or registry, albeit without 
-- persistent state. Demand monitors are also a suitable primitive
-- for explaining many-to-one observation of demands by a resource.
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
-- for relatively few publishers (few-to-many). When RDP developers 
-- need stability, alternatives exist: state models can be stable, 
-- and stateless logics can leverage non-determinism for stability.
--
-- Note: this module provides very concrete implementations for the
-- demand monitors.
--
module Sirea.DemandMonitor 
    ( newDemandMonitor
    , newDemandMonitorZ
    , newDemandMonitorZeq
    , newActimon, newMaximon, newMinimon
    ) where

import Sirea.Signal
import Sirea.Behavior
import Sirea.B

-- | Create a list-based demand monitor in the IO monad. The list
-- should be treated as a set (behavior independent of duplication
-- and ordering). Returns (demand facet, monitor facet) pair.
--
-- This is generally not for direct use by Sirea clients; instead,
-- it serves as a primitive for a ResourceSpace or BCX, which can
-- introduce a demand monitor where it is needed.
newDemandMonitor :: IO (B w (S p e) (S p ()) , B w (S p ()) (S p [e]))
newDemandMonitor = newDemandMonitorZ s_select

-- | newDemandMonitorZ allows a flexible n-zip function across all
-- available signals. The idea is to reduce computation by making it
-- once per demand monitor rather than once per observer.
--
-- Note: the given zip function must always return an active z value
-- even when the input list is empty or all signals are inactive. 
-- The monitor facet masks this signal with its own activity profile
-- to ensure duration coupling.
newDemandMonitorZ :: ([Sig e] -> Sig z) -- n-zip function
                  -> IO (B w (S p e) (S p ()) , B w (S p ()) (S p z))
newDemandMonitorZ = flip newDemandMonitorBase ((const . const) False)

-- | newDemandMonitorZEq is similar to newDemandMonitorZ, but adds
-- an adjacency equality filter to the result. This can improve
-- stability by delaying updates that don't seem to change any
-- observed values in the short term.
--
-- See also: the notes for newDemandMonitorZ
newDemandMonitorZeq :: (Eq z) => ([Sig e] -> Sig z) 
                    -> IO (B w (S p e) (S p ()) , B w (S p ()) (S p z))
newDemandMonitorZeq = flip newDemandMonitorBase (==)


-- newDemandMonitorBase is the basis for other demand monitors.
-- It allows developers to control the zip function and an
-- adjacency filter.
newDemandMonitorBase :: ([Sig e] -> Sig z) -- n-zip function
                     -> (z -> z -> Bool)   -- partial equality function (may be false if unknown)
                     -> IO (B w (S p e) (S p ()), B w (S p ()) (S p z)) 
newDemandMonitorBase = error "TODO!"
-- 


-- | newActimon (Activity Monitor) is a demand-monitor that simply 
-- returns whether or not there is any active demand on the demand
-- facet. This is a highly stable variation of demand monitor, and 
-- a useful one in many cases. 
newActimon :: IO (B w (S p ()) (S p ()), B w (S p ()) (S p Bool))
newActimon = newDemandMonitorZeq sigAny

sigAny :: [Sig a] -> Sig Bool
sigAny = error "TODO!"
{- 
s_select :: [Sig a] -> Sig [a]
s_select = foldr (s_full_zip jf) (s_always [])
  where jf (Just x) (Just xs) = Just (x:xs)
        jf _ xs = xs
-}

-- | newMaximon (K Maximum Monitor) will monitor a list of the top 
-- K demands (if that many exist), rated and sorted based on Ordinal 
-- functions. This can help control stability relative to a full
-- demand monitor, limiting instability to the top K demands.
newMaximon :: (Ord e) => Int -> IO (B w (S p e) (S p ()), B w (S p ()) (S p [e]))
newMaximon k =
    if (k <  1) then return (bconst (), bconst []) else
    if (1 == k) then newDemandMonitorZeq (sigBest (>))
                else newDemandMonitorZeq (sigKBest (>) k)

-- | newMinimon (K Minimum Monitor) is simply the newMaximon inverted.
newMinimon :: (Ord e) => Int -> IO (B w (S p e) (S p ()), B w (S p ()) (S p [e]))
newMinimon k = 
    if (k <  1) then return (bconst (), bconst []) else
    if (1 == k) then newDemandMonitorZeq (sigBest (<))
                else newDemandMonitorZeq (sigKBest (<) k)


-- return the `best` signal according to a comparison function
sigBest :: (e -> e -> Bool) -> [Sig e] -> Sig [e]
sigBest = error "TODO!"

-- return the k `best` signals according to a comparison function
-- (in order from best to worst). This assumes k is relatively
-- small; very large k could become expensive quickly. 
sigKBest :: (e -> e -> Bool) -> Int -> [Sig e] -> Sig [e]
sigKBest = error "TODO!"



-- NOTE: I'm not entirely sure what I want for a generic demand
-- monitor interface, so I'll start with a concrete model and see
-- where I end up. 


-- DESIGN CONSIDERATION: Should demand monitors also be named by
-- unique string? (e.g. URL?) or by Typeable Ordinal?
--   Advantages:
--     1. easy to have local demand monitors
--     2. easy to have `evals` use unique demand monitors.
--   Disadvantages:
--     1. easier to misname a demand monitor (but could use `()` for main per type)





