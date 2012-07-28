
-- | RDP behaviors are effectful, and one of the simplest effects is
-- to observe concurrent demands. The simplest approach, though not
-- the most efficient, is to simply return a set of active demands.
-- Ability to observe concurrent demands is valuable for blackboard
-- systems, publish-subscribe models, registries, and other shared
-- environments for multi-agent systems. 
--
-- Demand monitors serve this role. Demand monitors are modeled as a
-- tightly coupled pair of behaviors:
--
--    * one behavior to publish the "demand" signals
--    * one behavior to subscribe and "monitor" collective demand
--
-- The basic monitors respond with the set of demands. Specialized
-- demand monitors exist, responding with minimum demand or whether 
-- demand is present. The specializations can improve efficiency and
-- stability, albeit at significant cost to expressiveness. 
--
-- Demand monitors do introduce a volatile state concept, which can
-- be achieved by clever use of `bdelay` to feed a demand monitor's
-- past into its future. This technique is unstable, inefficient,
-- and risks feedback cycles. Other packages provide more effective 
-- state models for RDP.
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





