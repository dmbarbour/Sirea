{-# LANGUAGE EmptyDataDecls, DeriveDataTypeable, 
             FlexibleInstances, MultiParamTypeClasses #-}

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
    , Pt, P0, W
    , Stepper(..)
    , Stopper(..)
    , PSched, Sched(..) -- re-exported
    , getPSched
    ) where

import Sirea.Behavior
import Sirea.PCX
import Sirea.Internal.CC
import Sirea.Internal.PTypes
import Sirea.Internal.Thread
import Sirea.Internal.PulseSensor (getPulseScheduler)

import Data.Typeable
import Data.IORef
import Control.Concurrent (forkIO)
import GHC.Conc (labelThread)

-- | Cross between partitions. Note that this behavior requires the
-- `b` class itself to encapsulate knowledge of how the partitions
-- are accessed. In the normal use case, partitions are created when
-- you cross into them by type, i.e. bcross into a GLUT partition in
-- order to create and control a GLUT window. The illusion is that
-- the partitions have always existed, they're just passive unless
-- you control them - i.e. discovery, not creation.
--
-- Cross from a partition to itself may optimize to identity.
class BCross b where
    bcross :: (Partition p, Partition p') => b (S p x) (S p' x)

-- | Partition p - indicates a toplevel partition type, and also 
-- can override the default partition thread constructor. The
-- partition must return its own stopper operation, which will be
-- run from within the same partition when it is time to halt the
-- application.
--
-- Note: Partitions are Resources (see PCX) and should not have any
-- significant side-effects until some effects are demanded.
--
class (Typeable p) => Partition p where
    -- | create a new partition thread, with access to partition
    -- resources via PCX. (Recommend use of GHC.Conc.labelThread.)
    newPartitionThread :: PCX p -> Stepper -> IO Stopper

-- We need a child PCX for each partition.
instance (Partition p) => Resource W (PCX p) where 
    locateResource rp _ = newPCX rp

-- | The W type represents the toplevel PCX. Each thread partition 
-- operates directly under the world or process level partition, W.
data W

-- | PSched is a partition scheduler, operating on partition threads
-- in the IO monad.
type PSched = Sched IO

-- | Given the PCX for a partition, we can obtain the scheduler,
-- though doing so is optional. See Sirea.PSched for more info.
getPSched :: (Partition p) => PCX p -> IO PSched
getPSched cp = 
    findInPCX cp >>= \ tc ->
    getPulseScheduler cp >>= \ onPulse ->
    return $! Sched 
        { stepTime   = getTCTime tc
        , onNextStep = addTCRecv tc
        , onUpdPhase = addTCWork tc
        , onStepEnd  = addTCSend tc
        , eventually = onPulse
        }

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
data Pt x deriving(Typeable)

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
data P0 deriving(Typeable)

instance Partition P0 where
    newPartitionThread = error "special case: main thread is not constructed"


