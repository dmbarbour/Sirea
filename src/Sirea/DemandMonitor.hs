
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
    ( newDemandMonitor, newDemandMonitorZ, newDemandMonitorZeq
    , newActivityMonitor, newKMaximumMonitor, newKMinimumMonitor
    -- looking for a good excuse for `newPokemon`
    ) where

import qualified Data.HashTable as HT
import Data.IORef
import Control.Applicative
import Sirea.Signal
import Sirea.Behavior
import Sirea.Time
import Sirea.B

import Sirea.Internal.LTypes

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
-- an adjacency equality filter to the result. The equality filter
-- will eliminate redundant updates over time, and eliminate some
-- redundancies in the short term (by delaying updates). These
-- eliminations can reduce rework downstream.
--
-- See also: the notes for newDemandMonitorZ
newDemandMonitorZeq :: (Eq z) => ([Sig e] -> Sig z) 
                    -> IO (B w (S p e) (S p ()) , B w (S p ()) (S p z))
newDemandMonitorZeq = flip newDemandMonitorBase (==)

-- | newActivityMonitor is a demand-monitor that returns whether or
-- not there is any active demand on the demand facet. This is very
-- stable to changes, and useful for many systems. 
newActivityMonitor :: IO (B w (S p ()) (S p ()), B w (S p ()) (S p Bool))
newActivityMonitor = newDemandMonitorZeq sigAny

sigAny :: [Sig a] -> Sig Bool
sigAny sigs = s_full_map isActive sigsMerged
    where sigsMerged = foldr (<|>) empty sigs
          isActive Nothing  = Just False
          isActive (Just _) = Just True

-- | newKMaximumMonitor will monitor a list of the top K demands (if
-- that many exist), rated and sorted based on Ordinal functions. 
-- This limits instability to the top K demands. Usually, K should
-- be small, often 1 or 2.
newKMaximumMonitor :: (Ord e) => Int -> IO (B w (S p e) (S p ()), B w (S p ()) (S p [e]))
newKMaximumMonitor k =
    if (k <  1) then return (bconst (), bconst []) else
    if (1 == k) then newDemandMonitorZeq (sigBest (>))
                else newDemandMonitorZeq (sigKBest (>) k)

-- | newKMinimumMonitor is simply the KMaximiumMonitor inverted.
newKMinimumMonitor :: (Ord e) => Int -> IO (B w (S p e) (S p ()), B w (S p ()) (S p [e]))
newKMinimumMonitor k = 
    if (k <  1) then return (bconst (), bconst []) else
    if (1 == k) then newDemandMonitorZeq (sigBest (<))
                else newDemandMonitorZeq (sigKBest (<) k)

-- return the `best` signal according to a comparison function
sigBest :: (e -> e -> Bool) -> [Sig e] -> Sig [e]
sigBest betterThan = foldr (s_full_zip jf) (s_always [])
    where jf (Just x) (Just []) = Just (x:[])
          jf (Just x) xs@(Just (y:_)) =
            if (x `betterThan` y) 
                then Just (x:[]) 
                else xs
          jf _ xs = xs

-- return the k `best` signals according to a comparison function
-- (in order from best to worst). This assumes k is relatively
-- small; very large k could become expensive quickly. 
--
-- Note: here I assume two values are equal if neither is better
-- than the other. This assumption is valid because sigKBest is
-- only instantiated from Ord functions (<) and (>).
sigKBest :: (e -> e -> Bool) -> Int -> [Sig e] -> Sig [e]
sigKBest betterThan k = fmap (take k) . foldr (s_full_zip jf) (s_always [])
    where jf (Just x) (Just xs) = Just (sortedInsert betterThan x xs)
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



data DemandMonitorData e z = DemandMonitorData
    { dmd_zipfn     :: !([Sig e] -> Sig z)
    , dmd_eqfn      :: !(z -> z -> Bool)
    , dmd_mkid      :: !(IORef Int)       -- next index
    , dmd_stable    :: !(IORef (Maybe T)) -- current stability
    , dmd_update    :: !(IORef (Maybe T)) -- earliest update
    , dmd_expect    :: !(IORef Int)       -- touch count
    , dmd_demand    :: !(HT.HashTable Int (DemandData e))
    , dmd_monitor   :: !(HT.HashTable Int (MonitorData z))
    }
data DemandData e = DemandData
    { dd_idx :: !Int        -- index for this DemandData
    , dd_sig :: !(SigSt e)  -- demand status
    }
data MonitorData z = MonitorData
    { md_idx :: !Int        -- index for this MonitorData
    , md_sig :: !(SigSt ()) -- monitoring status (removed when final)
    , md_lnk :: !(LnkUp z)  -- to receive monitor updates
    }

-- newDemandMonitorBase is the basis for other demand monitors.
-- It allows developers to control the zip function and an
-- adjacency filter.
newDemandMonitorBase :: ([Sig e] -> Sig z) -- n-zip function
                     -> (z -> z -> Bool)   -- partial equality function (may be false if unknown)
                     -> IO (B w (S p e) (S p ()), B w (S p ()) (S p z)) 
newDemandMonitorBase zfn eqfn = error "TODO!"
-- 


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





