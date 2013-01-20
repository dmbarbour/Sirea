{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}

-- | A declarative resource linking mechanism for Sirea and Haskell.
--
-- RDP has a conservative notion of resources: nothing is created,
-- nothing is destroyed. That is, there is no equivalent notion to
-- `new` or `delete`, nor even `newIORef`. Instead, resources are
-- external, and developers use discovery and reset idioms.
--
-- Many resources are "abundant" and may be discovered in quantities
-- as needed by providing unique names or paths. For example, files
-- and directories in a filesystem are abundant resources. By clever
-- partitioning and generation of names, a dynamic set of abundant 
-- resources can be represented. Secure, modular partitioning can be
-- achieved by eliminating ambient authority and `..` reverse paths.
--
-- Resources represent services, state, sensors, actuators, or FFI.
-- Abundant resources, such as state, should be deterministically
-- named - e.g. using a client name for dynamic resources. Stateful
-- resources are generally persistent for RDP unless they have some
-- natural explanation for their volatility. External resources are
-- eternal resources, thus need no initialization or finalization.
-- But some state resources might be `reset` to a pristine state to
-- recover storage costs.  
-- 
-- Sirea supports RDP's conservative notion of resources with PCX, a
-- generic context object capable of representing ad-hoc resources.
-- PCX supports volatile resources. Persistent resources are modeled
-- with a volatile proxy, which may provide cache or adapters. PCX
-- resources are uniquely identified by paths of types and strings.
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
import Data.IORef
import qualified Data.Map as M

-- TODO: consider using Data.Map for higher performance lookups.
import Control.Monad.Fix (mfix)
import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)

-- | PCX p - Partition Resource Context. Abstract.
--
-- A partition context is a vast space of resources. Conceptually, 
-- it already holds the resources, and we locate them on demand. The
-- implementation is technically lazy IO to initialize resources as
-- needed, but this is hidden from the users if the Resources meet
-- their contract.
--
-- NOTE: `PCX w` has connotations that `w` is the full world, i.e.
-- the root partition created by `newPCX`. It is also used in type
-- matching to provide a little extra protection against accidental
-- connections between SireaApp applications. `PCX p` refers to a
-- child PCX for a specific thread or partition. Partition resources
-- should be manipulated only by that partition thread.
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
-- lookups are idempotent, there are no issues of unsafe IO being
-- duplicated.
--
-- Assume finding resources in PCX is moderately expensive. Rather
-- than looking for the resources you need each time you need them,
-- try to apply PCX and obtain resources up front for partial so the
-- lookup costs are paid only once.
--
-- Use of names can support dynamic behaviors and metaprogramming,
-- but should be used with caution. There is no way to GC old names.
--
findByNameInPCX :: (Resource p r) => String -> PCX p -> r
findByNameInPCX nm pcx = unsafePerformIO (findByNameInPCX_IO nm pcx)

findByNameInPCX_IO :: (Resource p r) => String -> PCX p -> IO r 
findByNameInPCX_IO nm pcx = mfix $ \ r -> 
    let pElt = (typeOf r, nm) in
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



