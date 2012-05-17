
-- | A resource discovery idiom for Haskell, without globals.
module Sirea.PCX
    ( PCX -- abstract
    , Resource(..)
    ) where



-- | PCX p - A Partition Context.
--
-- RDP has a conservative notion of resources: services, resources,
-- shared state, etc. exist outside behaviors; nothing is created,
-- nothing is destroyed. Developers will use discovery idioms, paths
-- and names. RDP is effectful, so there is no semantic problem with
-- initializing resources as they are discovered. Infinite spaces of
-- resources can be abstracted then lazily initialized. 
--
-- Each RDP application tends to use its own little corner of an 
-- infinite space of resources. Access to resources abstracted this
-- way is very declarative. 
--
-- Sirea is embedded in Haskell, which takes a m
--
--
--
-- This approaches to resource acquisition is very declarative, but
-- som
-- spaces of resources may be 
--
-- The effectful nature of RDP allows influencing state
-- state of discovered resources - e.g. automatically initializing 
-- each resource as it is discovered.
--
-- 
-- RDP application describes and maintains relationships between 
-- resources, continuous observation and influence.

 - nothing is new,
-- and nothing is destroyed. 
----
-- In RDP, resources are `external` to behaviors. The concept of new
-- resources - newIORef, new Object(), etc. - is rejected. State is
-- always bound to a resource that exists outside the RDP behavior.
-- Instead of `new`, developers use discovery idioms. Resources are
-- located by search or by path. Similarly, nothing is destroyed, 
-- but resources might be reset when necessary.
--
-- Abstract spaces of resources may be unbounded. RDP applications
-- can start at a unique location in that space (e.g. identified by
-- actual host and Haskell process). Clever design and discipline 
-- can get close to `new` for developers intent on those patterns, 
-- e.g. use state to claim surrogate keys.
--
-- This c
 
--

--
-- 
-- c
--
-- For declarative programming, Sirea uses a type-driven resource
-- acquisition strategy. 

Conceptually, the resources already exist
-- and are just waiting to be discovered, combined, and utilized.
-- (This is also the concept used by RDP. RDP does not support the
-- concept of `new` objects, all state and resources are external to
-- the computation.)
--
-- 

-- 
--
-- PCX provides an alternative to global state when supporting type
-- based resour
--
-- Sirea supports type-driven construction of 

