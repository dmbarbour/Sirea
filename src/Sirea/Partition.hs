{-# LANGUAGE EmptyDataDecls #-}

-- | Reactive Demand Programming (RDP) is designed for:
--
--   * open system: diverse authority, administrations, resources
--   * distributed systems: latency, concurrency, partitioning
--   * scalability: parallelism, mirroring, code distribution
--
-- Sirea has a much easier target: one Haskell application. 
--
-- But Sirea also aims to provide a proof of concept for RDP, that
-- it will scale to open, distributed systems. Consequently, it uses
-- a partition and communication model that would be effective for a
-- distributed system.
--
-- SIGNALS ACROSS PARTITIONS
-- =========================
--
-- Signals in Sirea behaviors are typed (S p a). This corresponds to
-- a concrete signal value of type (Sig a). The `p` type represents
-- partition, where the signal's value is hosted. Signals must be in
-- the same partition to be combined, e.g. by bzip or bdisjoin.
--
-- To move signals between partitions, developers may use `bcross`.
--
--     bcross :: (Partition p, Partition p') => b (S p x) (S p' x)
-- 
-- bcross moves data between two Sirea partitions, each of which has
-- a lightweight Haskell thread. Conveniently, Sirea will create the
-- threads needed for all the bcross behaviors - one per partition
-- (leveraging Data.Typeable). 
--
-- Lightweight partitions are also accessible as scopes - partitions
-- within a single thread. Scopes are popped or pushed like a stack.
--
-- PARTITIONS AS RESOURCE CONTROLLERS
-- ==================================
--
-- Sirea threads can perform useful work, more than processing RDP
-- updates. For example, depending on type a particular partition
-- might create itself as a bound thread to control a GLUT window,
-- and may have associated behaviors to observe or influence such
-- properties as framerate, mouse position, and what is displayed.
--
-- To support this, the threads are created by a typeclass, and a
-- Stepper object is provided to process incoming RDP updates. 
-- Sirea does not own the main loop of any thread, just requires 
-- that the Stepper object be called regularly or when updates are
-- available (set an event!). 
--
-- The difficulty is hooking threads into the RDP behaviors without
-- a lot of awkward data plumbing or use of global state. I think to
-- develop a dependency injection model for Haskell, Control.Make.
-- Both behaviors and partitions could then share dependencies in a
-- formal manner. 
--
-- COMMUNICATION AND CONSISTENCY
-- =============================
--
-- Sirea threads communicate via RDP updates. They should otherwise
-- be independent - no channels, no shared references, no STM. 
--
-- RDP updates are communicated in batches. Rather than immediately
-- delivering a signal update, `bcross` will append a batch for the
-- target thread. All outgoing batches are delivered at end of step.
-- At start of step, all available batches are processed.
--
-- Batch processing grants each thread a "snapshot" consistent view
-- of every other thread. This is convenient for reasoning: multiple
-- signals from one thread will at least be internally consistent. 
-- This handles most malign glitches. (Batch processing is also an
-- advantage for efficiency.)
--
-- Larger inconsistency is still possible. Alice's view of Bob may
-- differ from Alice's view of Charlie's view of Bob due to latency
-- in propagation of updates. Appropriate use of delay would mask
-- the problem, shifting it to the future where it can be resolved
-- before observed.
--
-- In addition to snapshot consistency and delay, RDP can provide 
-- eventual consistency (EC). But there are limits on what EC can do
-- for retroactive correction of state. "Four things come not back: 
-- the spoken word, the sped arrow, the past life, and the neglected
-- opportunity." Better to use appropriate delays. In a distributed
-- system, one might use disruption instead of admitting straggling
-- updates. 
-- 
-- Sirea does not model disruption. Instead it prevents threads from
-- falling too far behind (or getting too far ahead), by waiting on 
-- the slow threads.
-- 
-- MUST USE NON-BLOCKING IDIOMS
-- ============================
--
-- To control performance and ensure progress, Sirea ensures that no
-- thread gets too far "ahead" of other threads. Currently this is
-- implemented in terms of number of batches in flight, but another
-- valid approach would be to measure it in milliseconds. Bounds on
-- communication buffers help control memory overheads.
--
-- So long as every thread individually makes progress, the Sirea 
-- application as a whole will make progress. And if every thread 
-- keeps up with its workload, communication becomes wait free. The
-- buffers may be large so threads rarely encounter their limits.
--
-- But this blocking mechanism means the whole Sirea application can
-- halt if any particular thread halts. Threads should not perform 
-- blocking IO. (If necessary, a Sirea thread could fork a non-Sirea
-- worker thread to perform the blocking IO.) Sirea threads should
-- use only the RDP mechanisms to communicate.
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

import Data.Typeable
import Data.IORef
import Control.Exception (assert)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar


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
    -- The PCX parameter provides an alternative to global state for
    -- connecting resources to the partitions that control them. The
    -- PCX 
    --   * create resources when the partition is created.
    --   * access resources created by other behaviors.
    --
    -- In general, a thread should only access a small, static set
    -- of PCX resources, corresponding to a set of object methods.
    -- Use collections to track multiple demands.
    --
    newPartitionThread :: PCX p -> Stepper -> IO Stopper

-- | Pt is simply a default class of partitions, named by typeables.
-- A Pt partition runs in a forever loop, processing RDP updates.  
--
-- This can be useful for pipeline parallelism, but the concurrency
-- properties mean you weaken consistency. (Sirea provides snapshot
-- consistency by batch-updates between partitions.) The concurrency
-- would be better justified if the thread had some IO role, e.g. to
-- watch the filesystem or manage a GLUT window.
--
-- (If you just want parallelism, use bspark, or bstrat and btouch.)
--
-- Mostly, Pt serves as an example partition type, and takes care of
-- the obvious, trivial case so that other people don't need to. 
--   
data Pt x

instance Typeable1 Pt where
    typeOf1 _ = mkTyConApp tyConPt []
        where tyConPt = mkTyCon3 "Sirea" "Partition" "Pt"

instance (Typeable x) => Partition (Pt x) where
    newPartitionThread _ = newPartitionThreadPt

-- | P0 = Pt () - a special partition representing the
-- main RDP behavior. (The crossB behavior has special
-- code for the main parititon.)
type P0 = Pt ()

-- | Cross between partitions (without syntactic hassle). 
class BCross b where
    bcross :: ( Partition p, Partition p') => b (S p x) (S p' x)

-- | Scopes are lightweight partitions, all within a single thread.
-- Scopes may have sub-scopes, simply push or pop like a stack. The
-- data plumbing between scopes should be free (after compile).
--
-- Scopes may be useful to represent distinct resource instances or
-- to constrain against accidental communication between parts of a
-- behavior.
--
class BScope b where
    bpushScope :: b (S p x) (S (Scope s p) x)
    bpopScope  :: b (S (Scope s p) x) (S p x)

data Scope s p
instance Typeable2 Scope where
    typeOf2 _ = mkTyConApp tycScope []
        where tycScope = mkTyCon3 "Sirea" "Partition" "Scope"


-- | Stepper - incremental processing of RDP updates in Sirea.
--
-- In Sirea, each partition has one Haskell thread, and each Sirea
-- thread has one Stepper object. The stepper is responsible for 
-- receiving available signal updates, performing RDP processing,
-- and sending batched updates to other Sirea threads. Between steps
-- the thread may have other responsibilities.
--
-- A step will run quickly if there is nothing to do. There is no 
-- wait for input. However, running a step may cause a wait on 
-- output if the target thread is falling behind. Progress for the
-- application requires every Sirea thread to keep up with available
-- updates.
--
-- When you build the main Sirea behavior for an application, a 
-- Stepper is returned. The Sirea behavior is not actually started
-- until the first runSireaStep is executed. 
--
-- runStepper: process available updates, deliver outputs. Returns 
--   very quickly if there is nothing to do, so should wait on event
--   or a fast-pased periodic task (e.g. render frame) after a step.
--
-- addStepperEvent: add a callback to occur when work is available.
--   The callback must be wait free, and must not call runStepper
--   itself - a typical behavior would be `tryPutMVar`. Events must
--   be reset per step, and are called only once. If work available
--   when the event is set, it is called immediately.
--
data Stepper = Stepper 
    { runStepper      :: IO () -- ^ incremental step
    , addStepperEvent :: IO () -> IO () -- ^ notify of work to do
    }

-- | Stopper should provide a way to gracefully halt Sirea threads. 
-- RDP behaviors should be shut down when the control signals become
-- inactive. However, these operations tell a thread to release any
-- resources, to perform any final commits for persistence, etc. and
-- to stop whether or not the signal is still active.
--
-- Note that you can set stopper events before running the stopper.
-- If activity halts for any reason, the stopper events will fire.
data Stopper = Stopper
    { runStopper      :: IO () -- ^ asynchronous stop
    , addStopperEvent :: IO () -> IO () -- ^ notify when stopped
    }

-------------------------------------------------
-- Implementing the default partition behavior --
-------------------------------------------------

-- ((doStop,isStopped),stopEvents)
type StopData = ((Bool,Bool),IO()) 

makeStopper :: IORef StopData -> Stopper
makeStopper rf = Stopper 
    { runStopper = modifyIORef rf doStop
    , addStopperEvent = addStopDataEvent rf
    }

addStopDataEvent :: IORef StopData -> IO () -> IO ()
addStopDataEvent rf ev = atomicModifyIORef rf addEv >>= id
    where addEv sd@((_,True),_) = (sd,ev)
          addEv (bbstop,e0) = ((bbstop,ev>>e0),return ())

doStop :: StopData -> StopData
doStop ((_,b),e) = ((True,b),e)

emptyStopData :: StopData
emptyStopData = ((False,False),return ())

finiStopData :: IORef StopData -> IO ()
finiStopData rf = atomicModifyIORef rf fini >>= id
    where fini ((shouldStop,isStopped),runEvents) =
                assert shouldStop $
                assert (not isStopped) $
                (((True,True),return()),runEvents)

newPartitionThreadPt :: Stepper -> IO Stopper
newPartitionThreadPt stepper =
    newEmptyMVar >>= \ rfWait ->
    newIORef emptyStopData >>= \ rfStop -> 
    let event = tryPutMVar rfWait () >> return () in
    let stop  = finiStopData rfStop in
    let loop  = addStepperEvent stepper event >>
                takeMVar rfWait >>
                runStepper stepper >>
                readIORef rfStop >>= \ ((shouldStop,_),_) ->
                if shouldStop then stop else loop
    in 
    forkIO loop >>
    return (makeStopper rfStop)   


