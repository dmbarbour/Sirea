
-- | RDP behaviors are effectful, albeit in a constrained manner to
-- support declarative reasoning about and composition of effects. 
-- RDP can influence a resource via the set of active signals to the
-- resource at any given instant. (Signals in this influential role 
-- are called demands, thus the name Reactive Demand Programming.)
--
-- Among the simplest useful resources for RDP is a demand monitor.
-- A demand monitor allows developers to observe the set of active
-- demands. Demand monitors are volatile; when a demand halts, it
-- cannot be monitored.
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
module Sirea.DemandMonitor 
    (
    ) where

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





