
-- | RDP behaviors are effectful, and one of the simplest effects is
-- to observe concurrent demands. The simplest approach, though not
-- the most efficient, is to simply return a set of active demands.
--
-- Ability to observe concurrent demands is valuable for blackboard
-- systems, publish-subscribe models, registries, and other shared
-- environments in multi-agent systems. 
--
-- Demand monitors serve this role. In addition to demand monitors
-- that return the set of demands (as a list), specialized demand 
-- monitors can improve performance for common patterns, such as:
--
--    * obtaining the minimum value (maximum, via type transform) 
--    * determining, simply, whether or not there is any demand
--
-- These specializations result in greater stability for observers,
-- and greater efficiency (fewer changes to compute).
--
-- Demand monitors are modeled as a coupled pair of behaviors:
--
--    * one behavior to publish the "demand" signals
--    * one behavior to subscribe and "monitor" collective demand
--
-- Developers place demand on one behavior and observe demand on the
-- other. In a proper RDP system, the separation would support any
-- object capability security patterns. In Sirea, demand monitors
-- are generally accessed by type and partition.
-- 
-- Demand monitors do introduce a volatile state concept, which can
-- be achieved by clever use of `bdelay` to feed a demand monitor's
-- past into its future. This technique is unstable, inefficient,
-- and not recommended for most problems. Other packages provide 
-- access to explicit and effective state models for RDP.
--
-- Demand monitors are deterministic based on active demand, which
-- leads in part to their instability (i.e. any change in demand by
-- any contributor can result in computation by all observers). To
-- achieve greater stability requires state or indeterminism.
--
module Sirea.DemandMonitor 
    (
    ) where

-- DESIGN CONSIDERATION: Should demand monitors also be named by
-- unique string? (e.g. URL?) or by Typeable Ordinal?
--   Advantages:
--     1. easy to have local demand monitors
--     2. easy to have `evals` use unique demand monitors.
--   Disadvantages:
--     1. easier to misname a demand monitor (but could use `()` for main per type)





