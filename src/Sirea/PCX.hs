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
    , RPath     -- path type
    , newPCX    -- a new toplevel
    , findInPCX, findByNameInPCX -- the lookup functions
    , Resource(..)
    ) where


import Data.Typeable
import Data.Dynamic
import Control.Concurrent.MVar
import qualified Data.Map as M
import Control.Monad.Fix (mfix)

-- TODO: consider changing toplevel `PCX w` to `PCX W`

-- | PCX p - Partition Resource Context. Abstract.
--
-- A partition context is a vast space of resources. Conceptually, 
-- it already holds the resources, but we locate them as needed. In
-- practice, the resource is created on the first lookup and further
-- lookups will return the same resource. Resources are uniquely 
-- identified (located) based on type (via Typeable) and string. 
--
-- A weakness of PCX is that there is no mechanism to GC resources
-- without collecting the whole space. Consequently, developers must
-- use a relatively stable set of resources to avoid growing the
-- memory overheads. If more dynamism is required, it is necessary
-- to create resource types that handle the dynamism internally.
--
-- A PCX is MT-safe and will serialize all lookup operations. Lookup
-- should be considered moderately expensive, so the result should
-- be remembered if the resource will be needed often. Since PCX is
-- idempotent and lookups are commutative, it is safe to use PCX via
-- unsafePerformIO (or even unsafeDupablePerformIO). But this will
-- not be the default.
--
data PCX p = PCX 
    { pcx_path :: !(RPath)
    , pcx_store :: !(MVar Store)
    } deriving(Typeable)

type Store = M.Map (TypeRep,String) Dynamic

-- | The PCX Path is a path of types and strings, ordered from leaf
-- to root. Every resource has a unique path (from newPCX) that is
-- accessible via locateResource.
type RPath = [(TypeRep,String)]

-- | Resource - can be found inside a PCX. 
--
-- Resources are constructed in IO, but developers should protect an
-- illusion that resources existed prior the locator operation, i.e.
-- we are locating resources, not creating them. This requires:
--
--   * no observable side-effects in the locator
--   * no observable effects for mere existence of resource
--   * not sensitive to thread in which construction occurs
--
-- We shouldn't see anything unless we interact with resources with
-- further IO operations. If we create a resource but don't ever use
-- it, there should be no significant effects. The most common IO
-- action for creating resources will probably be newIORef.
--
-- For safety, the idiom is hide the resource type inside a module
-- and wrap the find operation. This can provide some constraints on
-- the lookup operations.
--
-- Every resource has a unique path relative to a root PCX. The path
-- supports persistence or generation of default states. This path
-- will be stable across program executions.
--
class (Typeable r) => Resource p r where
    locateResource :: RPath -> PCX p -> IO r

instance (Typeable p) => Resource p0 (PCX p) where
    locateResource p _ =
        newMVar M.empty >>= \ s ->
        return (PCX { pcx_path = p, pcx_store = s })

-- | Find a resource in the partition context based on its type.
--
--     findInPCX = findByNameInPCX ""
--
findInPCX :: (Resource p r) => PCX p -> IO r
findInPCX = findByNameInPCX ""

-- | Find a resource in a partition based on both name and type.
--
-- Notionally, the resource already exists, we aren't creating it.
-- In practice, the resource is created on the first lookup, and all
-- subsequent lookups (with the same string and type) will return
-- the same resource. To protect notional existence, resources are
-- not to have observable side-effects until we interact with them.
--
findByNameInPCX :: (Resource p r) => String -> PCX p -> IO r
findByNameInPCX nm pcx = mfix $ \ rForType ->
    let k = (typeOf rForType, nm) in
    modifyMVar (pcx_store pcx) $ \ tbl ->
        let elt = fromDynamic =<< M.lookup k tbl in
        case elt of
            Just r -> return (tbl,r)
            Nothing ->
                locateResource (k:pcx_path pcx) pcx >>= \ r ->
                let tbl' = M.insert k (toDyn r) tbl in
                r `seq` tbl' `seq`
                return (tbl',r)

-- | newPCX - a `new` toplevel PCX
newPCX :: String -> IO (PCX w)
newPCX nm = 
    newMVar M.empty >>= \ s ->
    let p = (typeOf (), nm):[] in
    let pcx = PCX { pcx_path = p, pcx_store = s } in
    return pcx



