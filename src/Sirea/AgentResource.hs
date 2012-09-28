{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}

-- | AgentResource provides a convenient way to treat RDP behaviors
-- as shared resources. Developers could achieve equivalent effect
-- at least two ways purely within RDP:
--
--   * replicate behavior wherever used (leverage RDP's idempotence)
--   * behavior near toplevel that observes activity monitor
--
-- However, Sirea does not optimize replicate behaviors, and placing
-- a behavior near the toplevel can be annoying when the reason for
-- it is buried deep in some implementation-dependent resource. This
-- module thus provides advantages for convenience and performance.
-- 
-- AgentResource treats the agent as defined at the toplevel and
-- observing the activity monitor. When any demand is placed on the
-- monitor, the agent will see it, activate, and begin performing
-- its primary function. When there is no more demand for the agent,
-- the agent returns to waiting passively on the monitor.
--
-- The primary reason to use an AgentResource is to support resource
-- adapters. It is often convenient to define a resource's behavior,
-- or at least segments of it, in terms of RDP behaviors. Also, the
-- uniqueness of `invokeAgent` simplifies assertions of idempotence.
-- Safe use of UnsafeOnUpdate is easier to validate if performed by
-- an invoked agent.
--
-- To communicate more than demand for the agent's services requires
-- indirect methods - shared state, demand monitor, or blackboard
-- metaphor.
--
module Sirea.AgentResource
    ( invokeAgent
    , AgentBehavior(..)
    ) where

import Data.Typeable
import Sirea.Behavior
import Sirea.BCX

-- | Agent behaviors for AgentResource are defined in a typeclass.  
--
-- Use `invokeAgent` to compile and install the agent behavior when
-- there is need for it. Each agent behavior is associated with a
-- partition and a duty. 
--
-- Suggestion: Encapsulate a duty newtype and associated invokeAgent
-- behavior within a module, such that clients cannot accidentally
-- duplicate the agentBehaviorSpec within a SireaApp. Protect the
-- uniqueness guarantee.
class (Typeable p, Typeable duty) => AgentBehavior p duty where
    -- | This should be instantiated as: agentBehaviorSpec _ = ...
    -- The `duty` parameter is undefined, used for its type.
    agentBehaviorSpec :: duty -> BCX w (S p ()) (S p ())


-- | To access an agent behavior as a resource, use `invokeAgent`.
-- This will ensure only one copy of the agent behavior is active,
-- even if invoked from multiple locations in the Sirea application.
-- The agent will remain active so long as there is at least one
-- demand for it. If there is no anticipated demand for the agent,
-- it will be uninstalled and GC'd until next invocation.
--
-- Using `agentBehavior` directly would instead cause one copy of
-- the behavior to be installed for each use. Due to idempotence,
-- the resulting system would have the same effects. However, it
-- will be more expensive if there is more than one instance of the
-- agent behavior installed and active.
--
-- The uniqueness of  
invokeAgent :: (AgentBehavior p duty) => duty -> BCX w (S p ()) (S p ())
invokeAgent = error "TODO! invokeAgent"


-- TODO: Will need to consider this whole AgentResource concept again for plugins.
--  (It might also need first-class treatment there, to avoid instantiating any
--  agents that are not, at some point, necessary to the application.)

-- 
--   1. AgentResource essentially needs an activity monitor.
--   2. Agents must exist in a partition. (Multi-param typeclass?)
--   3. Agents cannot be parameterized, since


