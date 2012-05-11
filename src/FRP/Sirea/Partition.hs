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
-- the same partition to be combined, e.g. by bzip.
--
-- To move signals between partitions, developers may use `bcross`.
--
--     bcross :: (Partition p, Partition p') => b (S p x) (S p' x)
-- 
-- bcross moves between primary Sirea partitions, each of which has
-- a lightweight Haskell thread. Conveniently, Sirea will create the
-- threads needed for all the bcross behaviors - one per partition
-- (leveraging Data.Typeable). 
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
-- Simplistic, variable-based adapters are provided for interface 
-- between declarative RDP behavior and the imperative IO threads.
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
-- for retroactive correction of state. 
--
--    "Four things come not back: the spoken word, the sped arrow,
--     the past life, and the neglected opportunity." - proverb
--
-- RDP might use a little EC to mask variation in latency, but for
-- long term issues it is better to model the problem as disruption.
-- In a distributed system, a remote system that falls far behind 
-- would be treated as network disruption.
-- 
-- Sirea does not model disruption. For EC, Sirea provides precise
-- stability tracking but leaves the issue of how much EC up to each
-- resource (e.g. a hundred milliseconds).
-- 
-- MUST USE NON-BLOCKING IDIOMS
-- ============================
--
-- To control performance and ensure progress, Sirea ensures that no
-- thread gets too far "ahead" of other threads. Currently this is
-- implemented in terms of number of batches in flight, but another
-- valid approach would be to measure it in time (milliseconds). 
--
-- So long as every thread individually makes progress, the Sirea 
-- application as a whole will make progress. And if every thread 
-- keeps up with its workload, communication becomes wait free. The
-- buffers can be large enough that threads rarely encounter the 
-- limits.
--
-- But this blocking mechanism means the whole Sirea application can
-- halt if any particular thread halts. Threads should not perform 
-- blocking IO. (If necessary, a Sirea thread could fork a non-Sirea
-- worker thread to perform the blocking IO.) Sirea threads should
-- use only the RDP mechanisms to communicate, and signals shouldn't
-- contain MVars, IVars, Chans, IORefs, TVars, etc. 
--
module FRP.Sirea.Partition 
    ( Stepper(..)
    , Stopper(..)
    , Partition(..)
    ) where

import FRP.Sirea.Behavior

import Data.Typeable
import Data.IORef
import Control.Exception (assert)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

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
-- runStepper: process available updates and outputs. Will return 
--   very quickly if there is nothing to do; should not be polled.
--   If a thread has its own fast-paced periodic action (e.g. draw
--   frames at 30Hz) then a call at start and stop of each frame
--   should be quite sufficient.
--
-- addStepperEvent: add a callback event to notify that stepper has
--   something useful to do. This event is called once - immediately
--   or when a batch of RDP updates become available. It should be
--   safe to call from another thread, and wait-free. Events must be 
--   reset per step.
--
data Stepper = Stepper 
    { runStepper      :: IO ()
    , addStepperEvent :: IO () -> IO () 
    }

-- | Stopper should provide a way to gracefully halt Sirea threads. 
-- RDP behaviors should be shut down when the control signals become
-- inactive. However, these operations tell a thread to release any
-- resources, to perform any final commits for persistence, etc. and
-- to stop whether or not the signal is still active. 
--
-- A potential reason to stop even with active signals is to restart
-- the partition with a new initialization behavior, i.e. for live
-- programming. 
data Stopper = Stopper
    { runStopper      :: IO () -- ^ asynchronous stop
    , addStopperEvent :: IO () -> IO () -- ^ notify when stopped
    }


-- | PFactory p - an object that will create a partition.
-- (Allows Partition class constructor to be overridden in Make.)
data PFactory p = PFactory 
    { newPartitionThread :: Stepper -> IO Stopper
    }

tcPFactory :: TyCon
tcPFactory = mkTyCon3 "Sirea" "Partition" "PFactory"

instance Typeable1 PFactory where
    typeOf1 _ = mkTyConApp tcPFactory []

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
    -- | partition factory - creates a new thread for the partition. 
    partitionFactory :: {- Make -} IO (PFactory p)
    partitionFactory = return defaultPFactory

-- | partition crossing behavior.
-- 
-- If the partition threads do not already exist, they will be
-- created when the bcross behavior is first used. 
class BPartition b where
    bcross :: (Partition p, Partition p')
           => b (S p x) (S p' x)

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
          addEv ((b0,False),e0) = (((b0,False),ev>>e0),return ())

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

defaultNewPartitionThread :: Stepper -> IO Stopper
defaultNewPartitionThread stepper =
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

defaultPFactory :: PFactory p
defaultPFactory = PFactory
    { newPartitionThread = defaultNewPartitionThread 
    }


{-

TWO TYPES OF PARTITION:
  bcross - between true partitions
  bscope - between thread-local partitions

limited number of comm batch-update between threads
- combine batches and updates if one thread is faster than another (efficiency)
- ensure progress of all threads is relatively close (but not lockstep)
- limits memory consumption (max # in-flight batches and updates)
- limits amount of computation in step (max # incoming batches)

Threads can have other responsibilities
- e.g. maintain GLUT window. 

NOT SUPPORTED BY SIREA (BUT DOCUMENTED ANYWAY)
  bremote - remote partitions, like bcross but extra constraints:
     serializable message
     extra response type to model disruption

Support for easy integration with imperative ops within threads.
  partition-specific variables, observable in RDP.
  may set future values, observe current values. 
  (named by type)
  update only in partition's thread
  observe only via RDP; updates not observable until next RDP step

  linear variables, may be assigned by exactly one source?
    (build-time error if used more than once).
  
  

bscope is free. It is useful if:
* you need more partition types (e.g. to model distinct resources, objects)
* you want to more precisely organize your application

CONFIGURATION:
  a typeclass that operates in Control.Make to build the threads


Sirea also provides a logical, thread-local variation of partitioning called `bscope`. Scopes can provide some extra structure to an application, and provide a decent way to organize resources (e.g. model multiple scopes). Scopes are useful because resources are often associated with specific partitions by type, and scopes allow you to model more partition types without adding threads. 


Note that you can specify `bfmap` and similar in other partitions. This corresponds to code distribution. Between Haskell threads, it is trivial. But RDP is designed for code distribution between processes.


In general, developers should seek to minimize use of bcross. Even between Haskell threads, considerable latency may be introduced due to scheduling. To help model this, each use of `bcross` may hide an implicit `bdelay` based on configuration. 

-}

