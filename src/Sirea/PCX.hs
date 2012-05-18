{-# LANGUAGE EmptyDataDecls, Rank2Types #-}

-- | Declarative resource linking mechanism for Haskell.
--
-- RDP has a conservative notion of resources: services, resources,
-- shared state, etc. are external to behaviors; nothing is created,
-- nothing is destroyed. Developers will use discovery idioms, paths
-- and names. RDP is effectful, so there is no semantic concern with
-- initializing resources after they are discovered. 
--
-- A useful idiom: abstract infinite spaces of resources, and lazily
-- initialize resources (if necessary) as they are discovered.
--
-- It is easy to partition infinite space into more infinite spaces.
-- Every RDP application can thus have its own, infinite corner of 
-- the universe. This is compatible with the perspective that it is 
-- RDP "all the way down" - an RDP application is a dynamic behavior
-- that manipulates resources in a local partition.
--
-- Sirea also uses this conservative notion of resources to achieve
-- a more declarative programming experience. This is expressed in
-- PCX, and is *type driven* - developers may find any resource of
-- class Resource.
--
-- The idea with PCX is to present resources as though they already
-- exist, as though PCX is an infinite namespace, and resources are
-- accessible if only we can name them. The naming in PCX is based
-- on Data.Typeable, though developers are free to extend this by
-- naming resources that represent spaces of resources with another
-- naming conventions.
--
-- PCX is most useful for volatile resources, which will not survive
-- destruction of the Haskell process. Persistent resources benefit
-- by use of volatile proxies, e.g. to cache a value. PCX is used in
-- Sirea core for threads and hooking up communication between them.
-- It will be more heavily used for FFI adapters, e.g. to represent
-- control over a GLUT window.
--
-- NOTE: Threading PCX through an application would grow irritating.
-- However, a simple behavior transformer can make it a lot nicer.
-- Another module in Sirea will provide the BCX type to carry an
-- initial PCX to every element in a behavior that might want it.
-- 
module Sirea.PCX
    ( PCX    -- abstract
    , newPCX -- a new toplevel
    , findIn -- the lookup function
    , Resource(..)
    ) where

import Data.IORef
import Data.Typeable
import Data.Dynamic

-- | PCX p - Partition Resource Context. Abstract.
--
-- A Partition context is an infinite space of resources, but holds
-- only one resource of each type. For more resources of a given 
-- type, consider using:
--   * a child PCX; look for the same resource there
--   * a newtype with a phantom type (per instance)
--   * a resource representing a mutable collection
--
data PCX p = PCX 
    { pcx_ident :: [TypeRep]
    , pcx_store :: IORef [Dynamic]
    }

-- | Resource - found inside a PCX. 
--
-- Resources are constructed in IO, but developers should protect an
-- illusion that resources existed prior the locator operation, i.e.
-- we are locating resources, not creating them. This requires there
-- be no observable side-effects in the locator, and that resources 
-- are passive, at least an IO operation is invoked on them.
--
-- The locator has recursive access to other resources, and to an
-- argument representing the unique ID of that resource (up to the
-- newPCX, anyway).
--
-- Resources are named by a path of [TypeReps], which includes the
class (Typeable r) => Resource r where
    locateResource :: [TypeRep] -> PCX p -> IO r

-- | findIn pcx - Each PCX contains one of each Resource.
findIn :: (Resource r) => PCX p -> IO r
findIn = undefined

-- | newPCX - a `new` PCX space, unique and fresh. An initial name
-- may be provided based on the root type. While developers could 
-- create more than one PCX, one is sufficient, since PCX is itself
-- a resource.
newPCX :: IO (PCX ())
newPCX = undefined






