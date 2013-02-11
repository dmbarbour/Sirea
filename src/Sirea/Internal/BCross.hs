{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances,
             DeriveDataTypeable
  #-}

-- | BCross is the implementation of cross for B and BCX.
--
-- It also contains types associated with partitions and crossB, 
-- i.e. used in BCX.
--
-- Sirea makes a promise called "snapshot consistency", which means
-- that partitions operate on a frozen picture of other partitions.
-- This isn't global consistency, since it is only pairwise between
-- partitions. But it does eliminate most malign glitches common to
-- naive implementations of concurrent reactive models. 
--
-- To achieve this consistency, a batching mechanism is used between
-- partitions. Updates for a remote partition from a given step are
-- delivered only when the step is complete. This is modeled via an
-- outbox between any two partitions, managed by `bcross` behaviors.
--
-- BCross is a potential point of choke for updates. In Sirea, every 
-- update has an arrive-by date. If that date is far in the future by
-- many seconds, then it could be delayed a little. For snapshot 
-- consistency, a whole batch could be delayed, or nothing. However,
-- I have decided to NOT pursue this because, as a solution, it is
-- not very robust under extension (adding code to observe another 
-- resource in a partition resource could break existing code). The
-- cross behavior will drop insignificant stability updates.
--
-- BCross uses a bounded buffer - a limited number of batches may be
-- "in flight" between any two partitions. This limit helps ensure 
-- Sirea's soft-realtime and soft-bounded memory properties. The
-- receiving partition will drain the full buffer at runStepper, so
-- there is no risk of cyclic waits.
--
-- On receipt, it is possible for multiple batches to piggyback by
-- eliminating defunct signal updates and combining touches. This
-- helps regulate performance: a partition that falls behind becomes
-- more efficient, enabling it to catch back up.
-- 
module Sirea.Internal.BCross 
    ( crossB
    , phaseDelayB
    , GobStopper(..)
    , runGobStopper
    , OutBox
    ) where

import Data.Typeable
import Data.Function (fix)
import Data.IORef
import Control.Applicative
import Control.Exception (assert)
import Control.Concurrent.MVar 
import Control.Monad (join, unless, when)

import Sirea.Internal.BTypes
import Sirea.Internal.STypes
import Sirea.Internal.LTypes
import Sirea.Internal.PTypes
import Sirea.Internal.BImpl (phaseUpdateB)
import Sirea.Internal.PulseSensor (initPulseListener)
import Sirea.Internal.Tuning ( batchesInFlight, tAncient, dtInsigStabilityUp)
import Sirea.Partition
import Sirea.PCX
import Sirea.Behavior
import Sirea.Time

type Event = IO ()
type Work = IO ()

-- crossB will:
--    add the update as a task to perform in another partition.
--    allow updates on input to buffer and batch (bounded buffer)
-- crossB is eliminated if typeOf p1 == typeOf p2
--
crossB :: (Partition p1, Partition p2) => PCX w -> B (S p1 x) (S p2 x)
crossB cw = fix $ \ b ->
    let (p1,p2) = getPartitions b in
    if (typeOf p1 == typeOf p2) 
      then B_mkLnk tr_fwd (return . fnStay)
      else sendB cw p1 p2 >>> phaseDelayB cw

-- this is a total hack in haskell to access typesystem data
getPartitions :: B (S p1 x) (S p2 x) -> (p1,p2)
getPartitions _ = (undefined,undefined)

fnStay :: Lnk (S p x) -> Lnk (S p' x)
fnStay LnkDead = LnkDead
fnStay (LnkSig lu) = (LnkSig lu)

-- sendB, if active (not dead code), will
--
--   * create partitions if they don't already exist
--   * batch multiple updates to one partition (across signals)
--   * delay operation to runTCSend phase
--
-- sendB will also flag work as urgent or non-urgent, which allows
-- simple choking when a batch consists fully of non-urgent updates.
--
sendB :: (Partition p1, Partition p2) 
      => PCX w -> p1 -> p2 -> B (S p1 x) (S p2 x)
sendB cw p1 p2 = B_mkLnk tr_fwd lnkSend
    where lnkSend = mkSend cw p1 p2

mkSend :: (Partition p1, Partition p2) 
       => PCX w -> p1 -> p2 -> Lnk (S p2 x) -> IO (Lnk (S p1 x))
mkSend _ _ _ LnkDead = return LnkDead
mkSend cw p1 p2 (LnkSig lu) = 
    initPartition p1 cw >> -- create partition thread p1 (idempotent)
    initPartition p2 cw >> -- create partition thread p2 (idempotent)
    newIORef (StableT tAncient) >>= \ rfChoke -> -- track stability to compute urgency
    mkOBSend cw p1 p2 >>= \ obSend ->
    let touch = return () in
    let idle tS = 
            readIORef rfChoke >>= \ tS0 ->
            when (urgentStability tS0 tS) $
                writeIORef rfChoke tS >>
                obSend (ln_idle lu tS)
    in
    let update tS tU su =
            writeIORef rfChoke tS >>
            obSend (ln_update lu tS tU su)
    in
    let luSend = LnkUp touch update idle in
    return (LnkSig luSend)    

-- | stability updates are pretty much all or nothing; a minor
-- stability update can be ignored, but a big one needs to be
-- processed to support GC.
urgentStability :: StableT -> StableT -> Bool
urgentStability DoneT _ = False
urgentStability _ DoneT = True
urgentStability (StableT t0) (StableT tf) =
    (tf > (t0 `addTime` dtInsigStabilityUp))

{-
-- | every update must be delivered, but we aren't always in a rush
-- to deliver updates. If the update only applies to some distant
-- future, we're free to delay the update until it is closer to an
-- appropriate time. By regulating this behavior, it is possible to
-- keep most temporal-recursive cycles in Sirea from running too far
-- ahead of real-time (which costs memory).
updateUrgency :: StableT -> T -> T
updateUrgency DoneT _ = ASAP
updateUrgency (StableT tS) tU =
    let tCut = tU `subtractTime` dtFutureChoke in
    if (tS >= tCut) then tCut else
    let dt = tCut `diffTime` tS in
    assert (dt > 0) $
    (tS `addTime` (dt*0.25))
-}

-- | phaseDelayB delays updates a phase within runStepper. The idea
-- is to touch a bunch of updates in one run before executing them 
-- later run, potentially eliminating redundant updates. Use of 
-- phaseDelayB may also accumulate multiple updates, which is useful
-- if phaseDelayB runs in the runStepper receive phase.
phaseDelayB :: (Partition p) => PCX w -> B (S p x) (S p x)
phaseDelayB cw = fix $ \ b -> 
    let (p,_) = getPartitions b in
    let mkPQ =
            getPCX p cw >>= \ cp ->
            getTC cp >>= \ tc ->
            return (addTCWork tc)
    in
    phaseUpdateB mkPQ

getTC :: (Partition p) => PCX p -> IO TC
getTC = findInPCX 

getPCX :: (Typeable p) => p -> PCX w -> IO (PCX p)
getPCX _ = findInPCX

-- | the GobStopper is collection of all the Stopper values in an
-- application; it is used to shut down the individual partitions
-- during graceful shutdown. (The P0 Stopper is excluded.) 
newtype GobStopper = Gob { unGob :: IORef [Stopper] }
    deriving (Typeable)

instance Resource w GobStopper where
    locateResource _ _ = Gob <$> newIORef []

-- | runGobStopper will:
--     halt all active threads (at instant of runGobStopper)
--     register an event to call when all threads fully halt.
-- If new stoppers are later added, they'll be missed. Usually
-- this would be called only at shutdown, when nothing new should
-- or could be created.
runGobStopper :: GobStopper -> IO () -> IO ()
runGobStopper gs ev =
    readIORef (unGob gs) >>= \ lStoppers ->
    if (null lStoppers) then ev else
    newIORef (length lStoppers) >>= \ rfCD ->
    let onStop = readIORef rfCD >>= \ cd ->
                 writeIORef rfCD (cd - 1) >>
                 when (0 == cd) ev
    in 
    mapM_ (flip addStopperEvent onStop) lStoppers >>
    mapM_ runStopper lStoppers

-- initPartition is performed when building behaviors (or dynamic 
-- behaviors) when we cross into another partition. It ensures the
-- partition exists before we step into it. Idempotent and atomic.
-- (Must be atomic if dynamic behaviors may declare partitions).
initPartition :: (Partition p) => p -> PCX w -> IO ()
initPartition p cw =
    getPCX p cw >>= \ cp ->
    getTC cp >>= \ tc ->
    atomicModifyIORef (tc_init tc) (\x->(True,x)) >>= \ bInit ->
    unless bInit $ 
        newPartitionThread cp (tcToStepper tc) >>= \ s0 ->
        let stopInStepper = addTCRecv tc (runStopper s0) in
        let stopper = s0 { runStopper = stopInStepper } in 
        getPCX0 cw >>= \ cp0 ->
        initPulseListener cp0 cp >>
        unGob <$> getGS cw >>= \ gs ->
        atomicModifyIORef gs (\stoppers->(stopper:stoppers,()))
        -- TODO: add support to receive pulse.

getPCX0 :: PCX w -> IO (PCX P0) 
getPCX0 = findInPCX

getGS :: PCX w -> IO (GobStopper)
getGS cw = findInPCX cw

-- | OutBox - output from Sirea thread.
--
-- * Each partition has one outbox for each destination partititon.
-- * The `TC` object has one task for all non-empty outboxes.
-- * At the end of each round, all outboxes are emptied.
-- * Semaphore for bounded-buffer parallelism.
--
-- Partition threads should communicate only by use of RDP behavior.
-- Other forms of waits - e.g. communicating MVars - could result in
-- deadlock due to the bounded buffers. Bounds control resource use
-- and improve fairness.
--
-- Outboxes are modeled as resources under the sender's PCX. It is
-- manipulated single-threaded by the sender's thread, except for a
-- semaphore. Work to perform remotely is simply queued as an IO
-- task. Releasing the semaphore is added as a task to each batch, to
-- model the bounded buffer.
-- 
data OutBox p = OutBox 
    { ob_sem   :: !Semaphore -- bounds number of in-flight batches.
    , ob_state :: !(IORef [Work])
    } deriving (Typeable)

instance (Partition p0, Partition p) => Resource p0 (OutBox p) where
    locateResource _ _ = OutBox <$> newSemaphore batchesInFlight
                                <*> newIORef []

-- obSend - send work FROM p1 TO p2 (given the world PCX).
--   (not mt-safe, should be called only from p1's thread.)
--
-- obSend also becomes a choke-point for cyclic relationships. This
-- is achieved at the whole-batch level: if every update in a batch
-- may be delayed, then the whole batch is delayed. (Delaying at a
-- batch level enables  
--
type OBSend = Work -> IO ()
mkOBSend :: (Partition p1, Partition p2) => PCX w -> p1 -> p2 -> IO OBSend
mkOBSend cw p1 p2 = mkOBSend' <$> tc1 <*> ob <*> tc2 where    
    tc1 = getPCX p1 cw >>= getTC
    ob  = getOB cw p1 p2
    tc2 = getPCX p2 cw >>= getTC

mkOBSend' :: TC -> OutBox p2 -> TC -> OBSend
mkOBSend' tc1 ob tc2 = obwAddWork where
      obwAddWork work =
        readIORef (ob_state ob) >>= \ lw ->
        let bFirst = null lw in
        let lw' = work:lw in
        writeIORef (ob_state ob) lw' >>
        when bFirst (addTCSend tc1 deliver)
      deliver =
        readIORef (ob_state ob) >>= \ lw ->
        writeIORef (ob_state ob) [] >>
        let work = (sequence_ . reverse) lw in
        let sem = ob_sem ob in
        sem_wait sem >> -- wait for batch to become available
        addTCRecv tc2 (sem_signal sem >> work) -- send batch

-- | obtain the outbox resource p1->p2 starting from the world context
--   (Each partition has a set of outboxes for each other partition)
getOB :: (Partition p1, Partition p2) => PCX w -> p1 -> p2 -> IO (OutBox p2)
getOB cw p1 _ = getPCX p1 cw >>= findInPCX

-- | a simple semaphore to model bounded-buffers between partitions. 
-- 
-- State is either a count of available resources or a list of tasks
-- each waiting on a resource. Signal either releases one thread or 
-- adds one to the count. Wait either decrements count or adds one
-- event to the list.
--
-- Sirea is designed to be wait-free modulo the batch-level pushback
-- buffers between threads. The semaphore is used to model pushback. 
-- Semaphore should not be used by any Sirea code outside this file. 
--
type Semaphore = IORef (Either Int [Event])

-- | create a new semaphore with a given initial count
newSemaphore :: Int -> IO Semaphore
newSemaphore nInit = 
    assert (nInit >= 0) $
        newIORef (Left nInit)

-- signal that a resource is available; will call an event if any
-- thread is waiting, otherwise will increment number available
sem_signal :: Semaphore -> IO ()
sem_signal s = join $ atomicModifyIORef s inc
    where inc (Left n) = (Left (succ n), return())
          inc (Right []) = error "invalid state for semaphore"
          inc (Right (op:[])) = (Left 0, op)
          inc (Right (op:ops)) = (Right ops, op)

-- sem_wait_event will set the semaphore to call an event when
-- the resource is available, and grants the resource with the
-- same call (must signal to release)
sem_wait_event :: Semaphore -> Event -> IO ()
sem_wait_event s ev = join $ atomicModifyIORef s dec
    where dec (Left 0) = (Right [ev], return ())
          dec (Left n) = assert (n > 0) $ (Left (pred n), ev)
          dec (Right ops) = (Right (ops++[ev]), return ())

-- synchronous wait for resource
sem_wait :: Semaphore -> IO ()
sem_wait s = 
    newEmptyMVar >>= \ mv ->
    sem_wait_event s (putMVar mv ()) >>
    takeMVar mv

