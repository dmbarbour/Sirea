{-# LANGUAGE FlexibleInstances #-}

-- | A declarative resource linking mechanism for Sirea and Haskell.
--
-- RDP has a conservative notion of resources: nothing is created,
-- nothing is destroyed. That is, there is no equivalent notion to
-- `new` or `delete`, nor even `newIORef`. Instead, resources are
-- external and eternal. Developers use discovery and reset idioms.
--
-- Many resources are "abundant" and can be discovered in quantity
-- as needed by specifying unique names or paths. For example, files
-- and directories in a filesystem are abundant resources. By clever
-- naming and partitioning, developers can achieve a flexible set of
-- resources to support a dynamic set of forms, accounts, contracts,
-- clients, and relationships. 
--
-- Resources represent services, state, sensors, actuators, or FFI.
-- An application may use fine-grained resources for specific forms,
-- widgets, registries, and similar. In general, stateful resources
-- should be persistent unless a natural reason exists for the state 
-- to be volatile. Stateful resources cannot be destroyed, but might
-- be `reset` to a default state (thus recovering storage costs).
--
-- Sirea supports this conservative notion of resources with PCX, a
-- "Partitioned resource ConteXt". PCX is a generic context object,
-- capable of representing ad-hoc resources in abundance. Resources
-- in a PCX are identified by type and path strings. PCX enables a
-- very declarative programming experience. Developers do not need
-- to explicitly create or hook resources together. 
--
-- PCX directly supports volatile resources. However, non-volatile
-- resources are represented by volatile proxy. A proxy is useful
-- for caches, network connections, FFI adapters, and so on. 
--
-- To support resets and similar, PCX comes with a standard events
-- model. Developers can register resources for ad-hoc events. This
-- should be used with caution, lest it violate the illusion that a
-- resource was always present.
--
-- PCX does not support configuration, override, or injection. Not
-- directly, anyway. For Sirea, configuration should be achieved at
-- the RDP layer anyway (modeling reactive configurations). 
--
module Sirea.PCX
    ( PCX       -- abstract
    , newPCX    -- a new toplevel
    , findInPCX -- the lookup function
    , pcxPath   -- identifier for the PCX.
    , Resource(..)
    ) where

import Data.Typeable
import Data.Dynamic
import Data.IORef
import Data.Monoid
import Control.Applicative
import Control.Monad.Fix (mfix)
import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)

-- TODO: a generic eventing system for resources.

-- | PCX p - Partition Resource Context. Abstract.
--
-- A partition context is an infinite, uniform space of resources.
-- It holds one resource of each type. Conceptually, it is already
-- holding those resources, and we just need to look for them. So
-- access to any particular resource is idempotent and offers a
-- pretense of purity. The actual implementation uses IO to create
-- resources lazily (and unsafely) when we need them.
--
-- Multiple instances of one type are easily achieved by modeling
-- another resource space as a resource. E.g. if you want a space
-- with integers mapped to state, you can do that - just write the
-- type and add a Resource instance for it. Child PCX contexts are
-- accessible (as resources).
--
-- NOTE: `PCX w` has connotations that `w` is the full world, i.e.
-- the root partition created by `newPCX`. It is also used in type
-- matching to provide a little extra protection against accidental
-- connections between SireaApp applications. `PCX p` refers to a
-- child PCX for a specific thread or partition. Partitions do not
-- directly share resources, but behaviors orchestrate communication
-- between partitions to allow indirect sharing.
--
data PCX p = PCX 
    { pcx_ident :: [TypeRep]
    , pcx_store :: IORef [Dynamic]
    }

instance Typeable1 PCX where
    typeOf1 _ = mkTyConApp tycPCX []
        where tycPCX = mkTyCon3 "sirea-core" "Sirea.PCX" "PCX"

-- | pcxPath identifies a PCX relative to its initial construction,
-- across child PCX resources. This offers resources unique, stable,
-- path-based identity within an application, which can be leveraged
-- for orthogonal persistence (e.g. keys in a database). 
pcxPath :: PCX p -> [TypeRep]
pcxPath = pcx_ident


-- | Resource - found inside a PCX. 
--
-- Resources are constructed in IO, but developers should protect an
-- illusion that resources existed prior the locator operation, i.e.
-- we are locating resources, not creating them. This requires:
--
--   * no observable side-effects in the locator
--   * no observable effects for mere existence of resource
--   * not sensitive to thread in which construction occurs
--
-- That is, we shouldn't see anything unless we agitate resources by
-- further IO operations. If we create a resource but don't ever use
-- it, there should be no significant effects.
class (Typeable r) => Resource r where
    locateResource :: PCX p -> IO r

instance (Typeable p) => Resource (PCX p) where
    locateResource pcx =
        mfix $ \ pcx' ->
        newIORef [] >>= \ store' ->
        let p      = typeOfPCX pcx' in
        let ident' = (typeOf p):(pcx_ident pcx) in
        return (PCX { pcx_ident = ident', pcx_store = store' })
        where typeOfPCX :: PCX p -> p
              typeOfPCX _ = undefined

-- Some utility instances.
instance (Typeable a, Monoid a) => Resource (IORef a) where
    locateResource _ = newIORef mempty
instance (Resource x, Resource y) => Resource (x,y) where
    locateResource pcx = return (findInPCX pcx, findInPCX pcx)
instance (Resource x, Resource y, Resource z) => Resource (x,y,z) where
    locateResource pcx = return (findInPCX pcx, findInPCX pcx, findInPCX pcx)
instance (Resource w, Resource x, Resource y, Resource z) 
    => Resource (w,x,y,z) where
    locateResource pcx = return (findInPCX pcx, findInPCX pcx
                                ,findInPCX pcx, findInPCX pcx)
instance (Resource v, Resource w, Resource x, Resource y, Resource z) 
    => Resource (v,w,x,y,z) where
    locateResource pcx = return (findInPCX pcx, findInPCX pcx, findInPCX pcx
                                ,findInPCX pcx, findInPCX pcx)
instance (Resource u, Resource v, Resource w, Resource x
         ,Resource y, Resource z) => Resource (u,v,w,x,y,z) where
    locateResource pcx = return (findInPCX pcx, findInPCX pcx, findInPCX pcx
                                ,findInPCX pcx, findInPCX pcx, findInPCX pcx)
instance (Resource t, Resource u, Resource v, Resource w, Resource x
         ,Resource y, Resource z) => Resource (t,u,v,w,x,y,z) where
    locateResource pcx = return (findInPCX pcx, findInPCX pcx, findInPCX pcx
                , findInPCX pcx, findInPCX pcx, findInPCX pcx, findInPCX pcx)
   

-- | Find a resource in the partition context based on its type.
--
-- This provides a pure interface to represent that the resource
-- already exists (according to the abstraction) and we're just
-- searching for it. Resources are initialized lazily. Since 
-- lookups are idempotent, there are no issues of unsafe IO being
-- duplicated.
--
-- Assume finding resources in PCX is moderately expensive. Rather
-- than looking for the resources you need each time you need them,
-- try to apply PCX and obtain resources up front for partial 
-- evaluation.
--
findInPCX :: (Resource r) => PCX p -> r
findInPCX = unsafePerformIO . findInPCX_IO

findInPCX_IO :: (Resource r) => PCX p -> IO r 
findInPCX_IO pcx =  
    unsafeInterleaveIO (locateResource pcx) >>= \ newR ->
    atomicModifyIORef (pcx_store pcx) (loadOrAdd newR)

loadOrAdd :: (Typeable r) => r -> [Dynamic] -> ([Dynamic],r)
loadOrAdd newR dynL =
    case fromDynList dynL of
        Just oldR -> (dynL, oldR)
        Nothing ->
            let dynR = toDyn newR in
            dynTypeRep dynR `seq` -- for consistency
            (dynR:dynL, newR)

fromDynList :: (Typeable r) => [Dynamic] -> Maybe r
fromDynList [] = Nothing
fromDynList (x:xs) = fromDynamic x <|> fromDynList xs

-- | newPCX - a `new` PCX space, unique and fresh.
--
-- You can find child PCX spaces if more than one resource of a
-- given type is necessary. 
newPCX :: IO (PCX w)
newPCX = 
    newIORef [] >>= \ rf ->
    return $ PCX { pcx_ident = [], pcx_store = rf  }

-- NOTE: it would be trivial to extend PCX with resources accessed by Ordinal.
-- (Even via a Resource, this could be done.)
-- Might be worth doing to support rich resources with dynamic behaviors.

-- TODO: maybe add support for ordinal-indexed spaces for state and demand monitors.


-- DECISION: combine responsibility into PCX?
--  a) simple string-based access to PCX.
--  b) add some extra mechanisms for onReset and onStop

-- How to model this?
--  As a typeclass or set of typeclasses: 
--    very tempting
--    could support "adding" features such as
--       resets (for types that can represent disruption)
--       splicing (hard or soft links)
--       history or versioning
--       cloning
--       merging?
--  Perhaps as a concrete structure: ResourceSpace a
--    Could not add new spatial-structures (splicing, etc.)
--    But could add `a`-dependent features.
--
-- I'd like to have ResourceSpace be a lot closer to RDP, i.e. so 
-- there isn't much work done by partitions.  
--
--  If I do typeclass, might want ability to hide the type from
--  client behind a set of interfaces? But might not be big deal.
--
--  Default state of a resource must be a function of the path to
--  that resource. It may actually be a fairly complex function of
--  said path. 
--
--  How shall I define a path to a resource?
--
--  I could use a simple sequence of strings. Or I could use a
--  sequence of flexible values. As a sequence of strings is very
--  tempting (fits well with URLs). ["Hello","World"]. But simple
--  support for integers could be nice, i.e. for cleaner ordering,
--  and it might be useful to also support set-based paths (similar
--  to Reiser FS ideas?)
-- 
--  Bah! Keep It Simple, Stupid!
--
--  For now, maybe limit to sequence of simple, case-insensitive, 
--  alphanumeric strings. It should be easier to add to this than
--  to subtract from it.
--  


