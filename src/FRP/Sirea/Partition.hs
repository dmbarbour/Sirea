{-# LANGUAGE TypeFamilies #-}

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
-- LIGHTWEIGHT PARTITIONS
-- ======================
--
-- Sirea can support lightweight partitions called "scopes" within a
-- thread. Scopes may be useful to represent namespaces or objects
-- when resources are identified based on partition types. Movement
-- between scopes is by push and pop operations. 
--     
--     bpushScope :: b (S p x) (S (Scope s p) x)
--     bpopScope  :: b (S (Scope s p) x) (S p x)
--
-- Note: scopes are not partitions. You cannot cross to a particular
-- scope in another partition. 
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
-- Sirea provides simple adapters for communication between threads
-- and the RDP behaviors that observe or influence them. If these
-- are insufficient, more can be created using unsafeLnkB.
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
    (
    ) where



import Data.Typeable



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

