{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}

-- | PCX is a context object providing access to abstract, volatile,
-- external resources: FFI, services, state, sensors, actuators, UI.
-- PCX supports the abstraction that these resources already exist,
-- that the client is just locating or discovering them, linking not
-- creating. RDP requires this conservative notion of resources. RDP
-- relies on external resources to provide state, and the ultimate
-- role of an RDP application is to bind and orchestrate resources
-- through use of signals.
--
-- When resources are external to an application, modularity has a
-- different flavor. Instead of encapsulation, we use partitioning.
-- The idea is basically that a parent application can grant access
-- to different subcontexts for different subprograms. Untrusted 
-- subprograms can thus be prevented from interfering with trusted
-- or sensitive subprograms. PCX is conceptually a partition of a
-- larger resource context. 
--
-- PCX resources are necessarily volatile, i.e. because they exist
-- within a Haskell process. However, they may be volatile proxies
-- for persistent state. An advantage of external state resources 
-- is natural support for orthogonal persistence. State resources
-- for Sirea shall be persistent unless they have a good, natural
-- explanation to be volatile (e.g short windowed history).
--
module Sirea.PCX
    ( Resource(..)
    , NamedResource
    , PCX, newPCX
    , findInPCX -- lookup by type
    , findByNameInPCX -- lookup by type and string
    , RPath -- stable resource path descriptor
    ) where

import Data.Typeable
import Data.Dynamic
import Control.Concurrent.MVar
import qualified Data.Map.Strict as M
import Control.Monad.Fix (mfix)

-- | PCX p - Partitioned Resource Context.
--
-- A partition context is a vast space of resources. Conceptually, 
-- it already holds the resources, and we locate them as needed. In
-- practice, the resource is created on the first lookup and further
-- lookups will find the same resource. Resources are uniquely 
-- identified (and located) based on type (via Typeable) and string.
--
-- A weakness of PCX is that there is no mechanism to GC resources
-- without collecting the whole space. Consequently, developers must
-- use a relatively stable set of resources to avoid growing the
-- memory overheads. If more dynamism is required, it is necessary
-- to create resource types that handle the dynamism internally.
--
-- PCX is implemented to be MT-safe and reentrant. However, cyclic
-- dependencies are not supported. (I.e. reentrancy only allows the
-- lookup of resources so long as they don't form a cycle.)
--
data PCX p = PCX 
    { pcx_path    :: !(RPath)
    , pcx_store   :: !(MVar Store)
    } deriving(Typeable)

type Store = M.Map (TypeRep,String) (MVar Dynamic)

-- | The PCX RPath is a path of types and strings, ordered from leaf
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
--   * not sensitive to time of construction
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
-- NOTE: While resources may depend on other resources, dependencies
-- must currently be acyclic (due to the implementation of PCX).
--
class (Typeable r) => Resource p r where
    locateResource :: RPath -> PCX p -> IO r

-- | NamedResource is simply a declaration that allows access to a
-- resource by string. This prevents accidental use of findByName
-- when a resource should be identified only by type and partition.
--
-- For named resources, findInPCX = findByNameInPCX ""
class (Resource p r) => NamedResource p r

-- | Find a resource by type. 
findInPCX :: (Resource p r) => PCX p -> IO r
findInPCX = findByNameInPCX' ""


-- | Find a resource in a partition based on both name and type.
--
-- Notionally, the resource already exists, we aren't creating it.
-- In practice, the resource is created on the first lookup, and all
-- subsequent lookups (with the same string and type) will return
-- the same resource. To protect notional existence, resources are
-- not to have observable side-effects until we interact with them.
--
findByNameInPCX :: (NamedResource p r) => String -> PCX p -> IO r
findByNameInPCX = findByNameInPCX'

findByNameInPCX' :: (Resource p r) => String -> PCX p -> IO r
findByNameInPCX' nm cp = mfix $ \ rForTypeOnly ->
    let k = (typeOf rForTypeOnly, nm) in
    pcxGetMVK cp k >>= \ (mvk,bFirst) ->
    if bFirst 
       then locateResource (k:pcx_path cp) cp >>= \ r ->
            putMVar mvk (toDyn r) >>
            return r
       else readMVar mvk >>= \ dynR ->
            let Just r = fromDynamic dynR in
            return r
    where badR = error "illegal PCX state"

-- returns (MVar for element, First lookup (i.e. empty mvar))
-- The MVar is only written to on the first lookup.
pcxGetMVK :: PCX p -> (TypeRep,String) -> IO (MVar Dynamic, Bool)
pcxGetMVK cp k = 
    modifyMVar (pcx_store cp) $ \ tbl ->
        case M.lookup k tbl of
            Just mvk -> return (tbl,(mvk,False))
            Nothing ->
                 newEmptyMVar >>= \ mvk ->
                 let tbl' = M.insert k mvk tbl in
                 return (tbl', tbl' `seq` (mvk,True))

-- | newPCX - a `new` resource context.
newPCX :: RPath -> IO (PCX w)
newPCX p = 
    newMVar M.empty >>= \ s ->
    let pcx = PCX { pcx_path = p, pcx_store = s } in
    return pcx


