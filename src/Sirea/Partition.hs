{-# LANGUAGE EmptyDataDecls #-}

-- | Reactive Demand Programming (RDP) design is for open, scalable,
-- distributed systems. Sirea is much more humble: just one Haskell
-- process. But it is still useful to model concurrent behaviors in
-- Sirea, i.e. different partitions for different adapter tasks, and
-- as a proof of concept.
--
-- This module provides behaviors for signals to cross Partitions.
-- Each partition has one Haskell thread. Trivial partitions merely
-- process RDP updates, but many represent resources and continuous
-- or periodic tasks (persisting state, maintaining a GLUT window,
-- watching the filesystem, etc.). This is achieved via typeclass.
--
-- Sirea makes partitions very convenient - just name them by type,
-- or infer them at `bcross`. This is very declarative. Partition 
-- threads are only created if the partition is used.
--
-- Partitions don't communicate directly. RDP behaviors orchestrate 
-- communication between them, via `bcross`. Updates are delivered
-- in batches, which are processed in a group. This ensures snapshot
-- consistency: between `runStepper` calls, signals from each remote
-- vat seem frozen. (Snapshot consistency does allow "glitches" in a
-- sense: Alice's view of Charlie might be inconsistent with Alice's
-- view of Bob's view of Charlie. But it resists most malign cases.)
--
-- Use of `bcross` should usually be coupled with `bdelay`, which
-- models communication latency and resists straggling updates. RDP
-- does support eventual consistency, internally, but little can be
-- done for the spoken word, spent arrow, or neglected opportunity.
-- (Hopefully you anticipated a close approximation of the updated
-- signal!) Each resource can have its own policy for retroactive
-- correction, possible "undo", etc. But it is often best just to  
-- achieve a more consistent system by using delay.
--
-- NOTE: Partition threads must use non-blocking IO if they interact
-- with legacy libraries or the operating system. Sirea waits when a
-- thread falls behind (for predictable time and space properties). 
-- Blocking IO can cause the app to freeze. (Fork another thread if
-- necessary; just don't block the `runStepper` operation.)
--
module Sirea.Partition 
    ( Partition(..)
    , BCross(..)
    , BScope(..)
    , Scope
    , Pt, P0
    , Stepper(..)
    , Stopper(..)
    ) where

import Sirea.Behavior
import Sirea.PCX
import Sirea.Internal.PTypes
import Sirea.Internal.Thread

import Data.Typeable
import Data.IORef
import Control.Concurrent (forkIO)

-- | Cross between partitions. Note that this behavior requires the
-- `b` class itself to encapsulate knowledge of how the partitions
-- are accessed. In the normal use case, partitions are created when
-- you cross into them by type, i.e. bcross into a GLUT partition in
-- order to create and control a GLUT window. The illusion is that
-- the partitions have always existed, they're just passive until
-- agitated - i.e. discovery and manipulation rather than creation.
--
-- Cross from a partition to itself may optimize to identity.
class BCross b where
    bcross :: (Partition p, Partition p') => b (S p x) (S p' x)

-- | Scopes are lightweight partitions, all within a single thread.
-- Scopes may have sub-scopes, simply push or pop like a stack. The
-- data plumbing between scopes should be free after compile.
--
-- Scopes may be useful to modularize resources, i.e. act as virtual
-- objects within a partition, with distinct type-based identity.
class BScope b where
    bpushScope :: b (S p x) (S (Scope s p) x)
    bpopScope  :: b (S (Scope s p) x) (S p x)

-- | Partition p - indicates a toplevel partition type, and also 
-- can override the default partition thread constructor.
--
-- Note: While partitions may have responsibilities beyond Sirea RDP
-- updates, they should not cause any significant effects until so
-- directed by RDP behaviors. 
-- 
-- Partition should later support dependency injection via Make.
-- This would be criticial for most non-trivial partitions, since
-- otherwise there is no clear way to hook up to behaviors. 
--
class (Typeable p) => Partition p where
    -- | create a new partition thread. The default implementation
    -- creates a thread with forkIO that will simply run RDP events
    -- until stopped. 
    -- 
    -- PCX supports communication between threads and RDP behaviors. 
    newPartitionThread :: PCX p -> Stepper -> IO Stopper

-- | Pt is a type for trivial partitions. These partitions have no
-- responsibilities, other than to process available RDP updates as
-- fast as possible. They might gain responsibilities via behaviors
-- that don't need any special treatment from their thread.
--
-- While partitioning can be a basis for parallelism, it weakens the
-- consistency properties of Sirea applications. (Within a partition
-- you have determinism up to input. Across partitions, you only get
-- snapshot consistency and eventual consistency. Straggling updates
-- are possible if a thread falls behind.) Consider whether `bspark` 
-- or `bstrat` is sufficient for parallelism.
--
-- Partitions are better justified when they represent resources and
-- various IO responsibilities.
--   
data Pt x

instance Typeable1 Pt where
    typeOf1 _ = mkTyConApp tyConPt []
        where tyConPt = mkTyCon3 "Sirea" "Partition" "Pt"
instance (Typeable x) => Partition (Pt x) where
    newPartitionThread _ stepper = 
        newIORef emptyStopData >>= \ rfStop ->
        forkIO (simplePartitionLoop rfStop stepper) >>
        return (makeStopper rfStop)


-- | P0 is the initial or main partition for a Sirea application. It
-- has a thread, but one controlled by the Sirea client rather than
-- created by Sirea. See Sirea.Build for more information.
data P0
instance Typeable P0 where
    typeOf _ = mkTyConApp tyConP0 []
        where tyConP0 = mkTyCon3 "Sirea" "Partition" "P0"
instance Partition P0 where
    newPartitionThread _ = error "cannot create main thread"

-- | Scopes are a thread-local alternative to full partitions. Scope
-- may be useful to provide extra type names for resources.
data Scope s p
instance Typeable2 Scope where
    typeOf2 _ = mkTyConApp tycScope []
        where tycScope = mkTyCon3 "Sirea" "Partition" "Scope"



