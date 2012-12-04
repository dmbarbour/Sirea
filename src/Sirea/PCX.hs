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
import Control.Applicative
import Control.Monad.Fix (mfix)
import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)

-- TODO: a generic eventing system for resources.

-- | PCX p - Partition Resource Context. Abstract.
--
-- A partition context is an infinite, uniform space of resources.
-- Each partition holds any number of resources of each type, each
-- with a unique string identifier. Conceptually, it already holds
-- these resources, and we just need to locate them on demand. The
-- implementation is essentially lazy IO to initialize resources
-- when they are needed. (For safety, resources must not have any
-- observable effects caused by mere initialization.)
--
-- NOTE: `PCX w` has connotations that `w` is the full world, i.e.
-- the root partition created by `newPCX`. It is also used in type
-- matching to provide a little extra protection against accidental
-- connections between SireaApp applications. `PCX p` refers to a
-- child PCX for a specific thread or partition. Partition resources
-- should be manipulated only by that partition thread.
--
data PCX p = PCX 
    { pcx_ident :: PCXPath
    , pcx_store :: IORef [(Dynamic, String)]
    }

-- | The PCX Path is a path of types and strings, ordered from leaf
-- to root. Every resource has a unique path (from newPCX) that is
-- accessible via locateResource.
type PCXPath = [(TypeRep,String)]

instance Typeable1 PCX where
    typeOf1 _ = mkTyConApp tycPCX []
        where tycPCX = mkTyCon3 "sirea-core" "Sirea.PCX" "PCX"

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
class (Typeable r) => Resource r where
    locateResource :: PCXPath -> PCX p -> IO r

instance (Typeable p) => Resource (PCX p) where
    locateResource p _ =
        newIORef [] >>= \ store' ->
        return (PCX { pcx_ident = p, pcx_store = store' })


-- | Find a resource in the partition context based on its type.
--
--     findInPCX = findByNameInPCX ""
--
findInPCX :: (Resource r) => PCX p -> r
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
findByNameInPCX :: (Resource r) => String -> PCX p -> r
findByNameInPCX nm pcx = unsafePerformIO (findByNameInPCX_IO nm pcx)

findByNameInPCX_IO :: (Resource r) => String -> PCX p -> IO r 
findByNameInPCX_IO nm pcx = mfix $ \ r -> 
    let pElt = (typeOf r, nm) in
    let path = pElt : pcx_ident pcx in
    unsafeInterleaveIO (locateResource path pcx) >>= \ newR ->
    atomicModifyIORef (pcx_store pcx) (loadOrAdd nm newR)


loadOrAdd :: (Typeable r) => String -> r -> [(Dynamic,String)] -> ([(Dynamic,String)],r)
loadOrAdd nm newR dynL =
    case fromDynList nm dynL of
        Just oldR -> (dynL, oldR)
        Nothing ->
            let dynR = toDyn newR in
            dynTypeRep dynR `seq` -- for consistency
            nm `seq`
            ((dynR,nm):dynL, newR)

fromDynList :: (Typeable r) => String -> [(Dynamic,String)] -> Maybe r
fromDynList _ [] = Nothing
fromDynList nm ((x,s):xs) = 
    if nm == s then fromDynamic x <|> fromDynList nm xs
               else fromDynList nm xs

-- | newPCX - a `new` PCX space, unique and fresh.
--
-- You can find child PCX spaces if more than one resource of a
-- given type is necessary. 
newPCX :: String -> IO (PCX w)
newPCX nm = 
    let pElt = (typeOf pcxRoot, nm) in
    let path = [pElt] in
    newIORef [] >>= \ rf ->
    let pcx = PCX { pcx_ident = path, pcx_store = rf } in
    return pcx
    where pcxRoot :: PCX ()
          pcxRoot = undefined

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


