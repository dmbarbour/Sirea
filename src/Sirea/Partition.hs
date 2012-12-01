{-# LANGUAGE EmptyDataDecls #-}

-- | Reactive Demand Programming (RDP) design is for open, scalable,
-- distributed systems. Sirea is much more humble: just one Haskell
-- process. But it is still useful to model concurrent behaviors in
-- Sirea - for task concurrency, and proof of concept for modeling
-- spatial orchestration.
-- 
-- This module provides behaviors for signals to cross Partitions.
-- Each partition has one Haskell thread. Trivial partitions merely
-- process RDP updates, but many represent resources and continuous
-- or periodic tasks (persisting state, maintaining a GLUT window,
-- watching the filesystem, etc.). A typeclass allows clients to 
-- create partitions for specific tasks.
--
-- Sirea makes partitions very convenient - just name them by type,
-- or infer them at `bcross`. This is very declarative. Partition 
-- threads are only created if the partition is used. Partitions 
-- can be abstracted by typeclass, possibly by existentials. They
-- communicate via inboxes processed on runStepper operations. For
-- weakly periodic tasks (GC, persistence, polling) a pulse message
-- is regularly broadcast across all partitions that need it. 
--
-- Use bdelay with bcross to model the communication overheads, and
-- computation costs within each partition. There is no delay by
-- default.
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
{-  , BScope(..)
    , Scope      -}
    , Pt, P0
    , Stepper(..)
    , Stopper(..)
    , onNextStep, eventually, phaseDelay
    , getStepTime 
    ) where

import Sirea.Behavior
import Sirea.PCX
import Sirea.Internal.PTypes
import Sirea.Internal.Thread
import Sirea.Internal.PulseSensor (addPulseAction)
import Sirea.Time

import Data.Typeable
import Data.IORef
import Control.Concurrent (forkIO)
import GHC.Conc (labelThread)

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

{-
-- | Scopes are lightweight partitions, all within a single thread.
-- Scopes may have sub-scopes, simply push or pop like a stack. The
-- data plumbing between scopes should be free after compile.
--
-- Scopes may be useful to modularize resources, i.e. act as virtual
-- objects within a partition, with distinct type-based identity.
class BScope b where
    bpushScope :: b (S p x) (S (Scope s p) x)
    bpopScope  :: b (S (Scope s p) x) (S p x)

-}

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
    -- | create a new partition thread, with access to partition
    -- resources via PCX. (Recommend use of GHC.Conc.labelThread.)
    newPartitionThread :: PCX p -> Stepper -> IO Stopper


-- | onNextStep will delay any IO action until the next runStepper
-- operation by an associated partition. onNextStep is mt-safe and
-- will trigger the stepper event (indicating work is available).
-- This makes it a useful way for worker threads to publish info
-- back to their controlling partition. onNextStep tasks will run
-- in the same order they are added.
--
-- Use of `onNextStep` is not guaranteed very near shutdown. 
onNextStep :: (Partition p) => PCX p -> IO () -> IO ()
onNextStep = addTCRecv . findInPCX

-- | `eventually` will delay an IO task based on an external event.
-- Currently, this event coincides with the main signal's heartbeat,
-- which runs at 10-20Hz. Use of `eventually` will add work to local
-- queue then (if the queue was empty) register for the event. The
-- queue will eventually run as one lump sum batch via onNextStep.
--
-- Use of `eventually` is unsuitable if you need precise periodic
-- tasks, but is sufficient for cleanup tasks or mid-rate polling.
-- `eventually` is mt-safe, and eventually tasks will run in the
-- same order they are added. 
-- 
eventually :: (Partition p) => PCX p -> IO () -> IO ()
eventually = addPulseAction

-- | `phaseDelay` causes the provided action to be delayed within 
-- the current step. This is used primarily so that touch actions
-- can complete prior to updates. phaseDelay is used at bcross so
-- touches are propagated and updates combined to minimize rework.
--
-- A partition can execute any number of phases within a runStepper
-- action, though there are usually only two (touch then update).
-- 
-- phaseDelay is not mt-safe; it must be used from target partition,
-- and should only be used within a step.  
phaseDelay :: (Partition p) => PCX p -> IO () -> IO ()
phaseDelay = addTCWork . findInPCX

-- | getStepTime will obtain an effective wall-clock time associated
-- with the most recent step. This value is computed at the start of
-- each step (i.e. right at `runStepper`), and guaranteed monotonic.
--
-- Note: getStepTime is not mt-safe. It must only be used from the
-- associated partition thread that runs stepper events.
--
getStepTime :: (Partition p) => PCX p -> IO T
getStepTime = getTCTime . findInPCX

-- | Pt is a type for trivial partitions. These partitions have few
-- responsibilities, other than to process available RDP updates as
-- fast as possible and perform specified step or pulse actions. 
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
        where tyConPt = mkTyCon3 "sirea-core" "Sirea.Partition" "Pt"
instance (Typeable x) => Partition (Pt x) where
    newPartitionThread cp stepper = 
        newIORef emptyStopData >>= \ rfStop ->
        forkIO (simplePartitionLoop rfStop stepper) >>= \ tid ->
        labelThread tid (getLabel cp) >>
        return (makeStopper rfStop)

getLabel :: (Typeable x) => PCX x -> String
getLabel = show . typeOf . getPTX 
    where getPTX :: PCX x -> Pt x
          getPTX _ = undefined

-- | P0 is the initial or main partition for a Sirea application. It
-- has a thread, but one controlled by the Sirea client rather than
-- created by Sirea. See Sirea.Build for more information.
data P0
instance Typeable P0 where
    typeOf _ = mkTyConApp tyConP0 []
        where tyConP0 = mkTyCon3 "sirea-core" "Sirea.Partition" "P0"
instance Partition P0 where
    newPartitionThread = error "special case: main thread is not constructed"

{-
-- | Scopes are a thread-local alternative to full partitions. Scope
-- may be useful to provide extra type names for resources.
data Scope s p
instance Typeable2 Scope where
    typeOf2 _ = mkTyConApp tycScope []
        where tycScope = mkTyCon3 "sirea-core" "Sirea.Partition" "Scope"
-}


