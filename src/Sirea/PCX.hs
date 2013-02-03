{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}

-- | A declarative resource linking mechanism for Sirea and Haskell.
--
-- RDP has a conservative notion of resources: nothing is created,
-- nothing is destroyed. That is, there is no equivalent notion to
-- `new` or `delete`, nor even `newIORef`. Instead, resources are
-- external; developers use discovery idioms. In applications that
-- need dynamic resources, discovery is directed by domain values,
-- e.g. using client identifiers to specify unique files or tables
-- in a database. 
--
-- Resources represent services, state, sensors, actuators, or FFI.
--
-- State resources tend to be "abundant" such that developers can
-- discover however many they need (e.g. one for each client, or for
-- each form or widget). The conservative resource philosophy works
-- very well with orthogonal persistence of state, and in Sirea (or
-- any RDP implementation) state resources ought to be persistent 
-- unless they have a good, natural excuse to be otherwise (such as
-- windowed history, or tuple spaces with short expirations). When 
-- clean state is necessary, using explicit resets independent from
-- application restarts is a good idea for live programming.
--
-- State resources can still be modular and secure via partitioning
-- schemes, providing different partitions to different subprograms.
--
-- The PCX type supports this conservative notion of resources in
-- Sirea. By nature, PCX carries only volatile resources, but that
-- includes volatile proxies to persistent resources (maintained in
-- a database or filesystem). PCX is mostly used behind the scenes,
-- when adapting new resources to Sirea's BCX behavior model. 
--
module Sirea.PCX
    ( PCX       -- abstract
    , PCXPath
    , newPCX    -- a new toplevel
    , findInPCX, findByNameInPCX -- the lookup functions
    , Resource(..)
    ) where

import Data.Typeable
import Data.Dynamic
--import Control.Concurrent.MVar
import Data.IORef
import qualified Data.Map as M

-- TODO: consider using Data.Map for higher performance lookups.
import Control.Monad.Fix (mfix)
import System.IO.Unsafe (unsafeDupablePerformIO, unsafeInterleaveIO)

-- | PCX p - Partition Resource Context. Abstract.
--
-- A partition context is a vast space of resources. Conceptually, 
-- it already holds the resources, but we locate them as needed. In
-- practice, the resource is created on the first lookup and further
-- lookups will return the same resource.
--
-- Resources may be uniquely identified by type and string. However,
-- there currently is no mechanism in PCX to GC resources without 
-- collecting the whole PCX. Developers should be careful to use the
-- resources they need, or perhaps especially model the cases where 
-- some extra GC is appropriate. 
--
-- At the moment, PCX serializes all lookup operations, and lookups
-- will be performed in the IO monad. (The original design modeled a
-- pure lookup, but in practice the IO monad is always available
-- when searching a PCX so I've decided to avoid the unsafe IO.) 
--
-- NOTE: `PCX w` has connotations that `w` is the full world, i.e.
-- the root partition created by `newPCX`. It is also used in type
-- matching to provide a little extra protection against accidental
-- connections between SireaApp applications. `PCX p` refers to a
-- child PCX for a specific thread or partition. Partition resources
-- should be manipulated only by that partition thread. The PCX type
-- is an instance of resource.
--
data PCX p = PCX 
    { pcx_ident :: !(PCXPath)
    , pcx_store :: !(IORef PCXStore)
    } deriving(Typeable)

type PCXStore = M.Map (TypeRep,String) Dynamic

-- | The PCX Path is a path of types and strings, ordered from leaf
-- to root. Every resource has a unique path (from newPCX) that is
-- accessible via locateResource.
type PCXPath = [(TypeRep,String)]

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
--
-- Every resource has a unique path in an instance of a SireaApp.
-- This path is provided to the constructor because it may be useful
-- for persistence or generation of a pseudo-random default state.
--
class (Typeable r) => Resource p r where
    locateResource :: PCXPath -> PCX p -> IO r

instance (Typeable p) => Resource p0 (PCX p) where
    locateResource p _ =
        newIORef M.empty >>= \ store' ->
        return (PCX { pcx_ident = p, pcx_store = store' })


-- | Find a resource in the partition context based on its type.
--
--     findInPCX = findByNameInPCX ""
--
findInPCX :: (Resource p r) => PCX p -> r
findInPCX = findByNameInPCX ""

-- | Find a resource in a partition based on both name and type.
--
-- Notionally, the resource already exists, we aren't creating it.
-- In practice, the resource is lazily initialized, which may prove
-- unsafe if the resource doesn't obey the rules (e.g. no observable
-- effects at initialization). 
--
-- This provides a pure interface to represent that the resource
-- already exists (according to the abstraction) and we're just
-- searching for it. Resources are initialized lazily. Since 
-- lookups are idempotent, most issues with unsafe IO are gone.
--
-- Assume finding resources in PCX is moderately expensive. Rather
-- than looking for the resources you need each time you need them,
-- try to apply PCX and obtain resources just once, up front. PCX
-- is designed expecting a low load for lookups.
--
-- Use of names can support dynamic behaviors and metaprogramming,
-- but should be used with caution. There is no way to GC old names.
--
findByNameInPCX :: (Resource p r) => String -> PCX p -> r
findByNameInPCX nm pcx = unsafeDupablePerformIO (findByNameInPCX_IO nm pcx)

findByNameInPCX_IO :: (Resource p r) => String -> PCX p -> IO r 
findByNameInPCX_IO nm pcx = mfix $ \ rForType ->
    let pElt = (typeOf rForType, nm) in
    let path = pElt : pcx_ident pcx in
    unsafeInterleaveIO (locateResource path pcx) >>= \ newR ->
    atomicModifyIORef (pcx_store pcx) (loadOrAdd nm newR)

loadOrAdd :: (Typeable r) => String -> r -> PCXStore -> (PCXStore,r)
loadOrAdd nm r tbl =
    let k = (typeOf r, nm) in
    let mbr = fromDynamic =<< M.lookup k tbl in
    case mbr of 
        Just r0 -> (tbl,r0) -- no changes
        Nothing -> 
            let tbl' = M.insert k (toDyn r) tbl in
            (tbl',r)


-- | newPCX - a `new` PCX space, unique and fresh.
--
-- You can find child PCX spaces if more than one resource of a
-- given type is necessary. 
newPCX :: String -> IO (PCX w)
newPCX nm = 
    newIORef M.empty >>= \ rf ->
    let path = [(typeOf (), nm)] in
    let pcx = PCX { pcx_ident = path, pcx_store = rf } in
    return pcx



