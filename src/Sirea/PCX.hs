{-# LANGUAGE EmptyDataDecls #-}

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
    , pcxid  -- the current path in the PCX. 
    , newPCX -- a new toplevel
    , findIn -- the lookup function
    , Resource(..)
    ) where

import Data.IORef
import Control.Concurrent.MVar
import Data.Typeable
import Data.Dynamic

-- | PCX p - Partition Resource Context. Abstract.
--
-- A partition context is an infinite, uniform space of resources.
-- It holds one resource of each type. Though, it is easy to model
-- multiple instances:
--   * a child PCX; look for the same resource there
--   * a newtype with a phantom type (per instance)
--   * a resource representing a mutable collection
--
-- PCX uses dynamic types, decoupling its interface from particulars
-- of the behaviors being implemented. 
-- PCX is potentially infinite to deal with unknown cases, but is 
-- designed for the common case where only a small, static set of
-- resources is looked up *once* (at build time) then used for a
-- long time.
data PCX p = PCX 
    { pcx_ident :: [TypeRep]
    , pcx_store :: IORef [Dynamic]
    , pcx_mutex :: MVar ()
    }

pcxid :: PCX p -> [TypeRep]
pcxid = pcx_ident

-- | Resource - found inside a PCX. 
--
-- Resources are constructed in IO, but developers should protect an
-- illusion that resources existed prior the locator operation, i.e.
-- we are locating resources, not creating them. This requires:
--
--   * no observable side-effects in the locator
--   * resources are passive until operations invoked on them
--
-- The locator has recursive access to other resources, and to an
-- argument representing the unique ID of that resource up to the 
-- newPCX.
class (Typeable r) => Resource r where
    locateResource :: PCX p -> IO r

-- | findIn pcx - Each PCX contains one of each Resource.
--
-- This operation is idempotent. The same resource will be found
-- every time.
findIn :: (Resource r) => PCX p -> IO r
findIn pcx = 
    takeMVar (pcx_mutex pcx) >>
    readIORef (pcx_store pcx) >>= \ lDyns ->
    case fromDynList lDyns of
        Just r -> 
            putMVar (pcx_mutex pcx) () >> 
            return r
        Nothing ->
            locateResource  pcx >>= \ r ->
            writeIORef (pcx_store pcx) ((toDyn r):lDyns) >> 
            putMVar (pcx_mutex pcx) () >>
            return r

fromDynList :: (Typeable r) => [Dynamic] -> Maybe r
fromDynList [] = Nothing
fromDynList (x:xs) = maybe (fromDynList xs) Just (fromDynamic x) 

-- | newPCX - a `new` PCX space, unique and fresh. An initial name
-- may be provided based on the root type. While developers could 
-- create more than one PCX, one is sufficient, since PCX is itself
-- a resource.
newPCX :: IO (PCX ())
newPCX = 
    newIORef [] >>= \ rf ->
    newMVar () >>= \ mx ->
    let pcx = PCX { pcx_ident = []
                  , pcx_store = rf 
                  , pcx_mutex = mx } in
    return pcx






