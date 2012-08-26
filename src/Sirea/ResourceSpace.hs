
-- | RDP has a conservative notion of resources: nothing is created,
-- nothing is destroyed. That is, there is no equivalent to `new` or
-- `delete`, nor `malloc` and `free`. Those could be modeled using
-- external state and a globally unique identifier, but RDP favors a
-- resource discovery idiom. Resources are located and accessed by 
-- external stable identifiers (e.g. URI or file path). If a virtual
-- resource does not exist, it may be implicitly created when used.
--
-- Module `PCX` is for static, global resources, identified by type.
--
-- This module supports dynamic resources, discovered at runtime via
-- configuration files and such. It assumes a tree-structured model
-- with relative paths, without direct path from child to parent. 
-- I.e. there is no equivalent to `..` path of filesystems. Security
-- can be achieved by controlling distribution of resource spaces.
--
-- Resource Spaces should generally support a `reset` capability, to 
-- set them to an initial state. This is useful to explain initial 
-- states, and to model volatile resources used by applications or
-- dynamic behaviors. Similarly, resource spaces may be unavailable
-- for various reasons, including active reset. 
--
module Sirea.ResourceSpace
    (
    ) where



