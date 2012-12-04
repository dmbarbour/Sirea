{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}

-- | AgentResource enables developers of FFI or resource adapters to
-- efficiently and easily mix RDP code with their implementation.
--  
-- Conceptually, we want some resources to be controlled by services
-- external to the Sirea application. Instead of direct influence on
-- resources, we communicate with the services, which will control
-- the resources on our behalf. This design can reduce replication 
-- of effort within or between applications.
--
-- AgentResource can represent passive services, those active only
-- during a request. This is sufficient for demand-driven resources. 
-- If an active service is needed (e.g. with stateful control), the
-- developer will need something more. 
-- 
-- The word "agent" refers to the view of concurrent RDP behaviors
-- and toplevel applications as agents in multi-agent system. Each
-- AgentResource operates near the toplevel of the application, but
-- only while actively invoked from the main application. Even when
-- an agent is invoked many times concurrently, only one instance is
-- created.
--
-- The resulting exclusive control can also simplify implementation,
-- e.g. making it easy to achieve RDP's idempotence properties with
-- UnsafeOnUpdate.
--
-- Communication with agents is typically indirect via shared state,
-- demand monitor, or other blackboard metaphor. 
--
module Sirea.AgentResource
    ( invokeAgent
    , AgentBehavior(..)
    ) where

import Data.Typeable
import Sirea.Behavior
import Sirea.BCX

-- | The RDP behaviors of AgentResources are defined in a typeclass.
-- The behaviors are indexed by partition and a `duty` type. Use the
-- `invokeAgent` behavior to compile and install a unique instance
-- of the AgentResource (even if invoked many times concurrently).
--
-- Recommendation is to keep the `duty` types hidden, and export the
-- behaviors that use invokeAgent directly. This ensures uniqueness.
class (Partition p, Typeable duty) => AgentBehavior p duty where
    -- | This should be instantiated as: agentBehaviorSpec _ = ...
    -- The `duty` parameter is undefined, used only for type. 
    agentBehaviorSpec :: duty -> BCX w (S p ()) (S p ())


-- | `invokeAgent` will install a unique instance of agent behavior
-- (one for each unique partition-duty pair). This behavior is built
-- and installed on demand, then uninstalled and GC'd when there is
-- no active demand, potentially many times in a Haskell process. 
-- Concurrent invocations do not result in extra instances. 
--
-- Logically, use of `invokeAgent` should have the same results as
-- many concurrent instances due to RDP's idempotence. However, the
-- unique installation may be much more efficient and will simplify
-- safe use of non-idempotent implementations (e.g. UnsafeOnUpdate). 
--
invokeAgent :: (AgentBehavior p duty) => duty -> BCX w (S p ()) (S p ())
invokeAgent = error "TODO! invokeAgent"


-- TODO: Will need to consider this whole AgentResource concept again for plugins.
--  (It might also need first-class treatment there, to avoid instantiating any
--  agents that are not, at some point, necessary to the application.)

-- 
--   1. AgentResource essentially needs an activity monitor.
--   2. Agents must exist in a partition. (Multi-param typeclass?)
--   3. Agents cannot be parameterized, since


