{-# LANGUAGE GADTs #-}

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
-- To achieve this promise, a batching mechanism is used between
-- partitions. Updates for a remote partition from a given step are
-- delivered only when the step is complete. This is modeled via an
-- outbox between any two partitions, managed by `bcross` behaviors.
--
-- BCross is also a potential point of choke for updates. In Sirea,
-- every update has an arrive-by date. If that date is far in the
-- future (several seconds) then that update is not very urgent and
-- may be delayed a little. These occasional delays, even if only a
-- heartbeat, are useful to amortize computation for cyclic feedback
-- relationships. To achieve snapshot consistency, choking must be
-- performed at the granularity of a full batch.
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
    , stepDelayB
    , phaseDelayB
    , GobStopper(..)
    , runGobStopper
    , OutBox
    ) where

import Data.Typeable
import Data.Function (fix)
import Data.IORef
import Data.Maybe (isNothing)
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
import Sirea.Internal.Tuning (dtInsigStabilityUp, dtFutureChoke, batchesInFlight)
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
    initPartition p1 cw >> -- create partition p1 (idempotent)
    initPartition p2 cw >> -- create partition p2 (idempotent)
    newIORef Nothing >>= \ rfChoke -> -- track stability to compute urgency
    let send = obSend cw p1 p2 in
    let touch' = return () in
    let update' su = 
            readIORef rfChoke >>= \ t0 ->
            let bUrgent = isUrgentUpdate t0 su in
            let bDropUpdate = isNothing (su_state su) && not bUrgent in
            unless bDropUpdate $
                let tf = su_stable su in
                tf `seq` writeIORef rfChoke tf >>
                send bUrgent (ln_update lu su)
    in
    let lu' = LnkUp { ln_touch = touch', ln_update = update' } in
    return (LnkSig lu')    

-- isUrgentUpdate is a heuristics computation to decide whether
-- an update is urgent. (Note: If a pure stability update isn't
-- urgent, it may be dropped entirely.)
isUrgentUpdate :: Maybe T -> SigUp e -> Bool
isUrgentUpdate Nothing _ = True -- initial updates always urgent!
isUrgentUpdate (Just t0) su =
    let bUrgentDueToStability = 
            -- we care most about LARGE stability updates
            case su_stable su of
                Nothing -> True -- final updates always urgent!
                Just tf -> (tf > (t0 `addTime` dtInsigStabilityUp))
    in
    let bUrgentDueToState =
            -- we care most about NEAR-TERM state updates
            case su_state su of
                Nothing -> False -- pure stability update
                Just (_,tu) -> (tu < (t0 `addTime` dtFutureChoke))
    in
    bUrgentDueToStability || bUrgentDueToState
           

-- | phaseDelayB delays updates a phase within runStepper. The idea
-- is to touch a bunch of updates in one run before executing them 
-- later run, potentially eliminating redundant updates. Use of 
-- phaseDelayB may also accumulate multiple updates, which is useful
-- if phaseDelayB runs in the runStepper receive phase.
phaseDelayB :: (Partition p) => PCX w -> B (S p x) (S p x)
phaseDelayB cw = fix $ \ b -> 
    let (p,_) = getPartitions b in
    let cp = getPCX p cw in
    let tc = getTC cp in
    let doLater = addTCWork tc in
    phaseUpdateB (return doLater)

-- | stepDelayB is for use within a partition, but delays actions to
-- the next runStepper operation (cf. Sirea.Partition.onNextStep).
-- This should be combined with phaseDelayB in most cases.
--   i.e. stepDelayB cw >>> phaseDelayB cw
-- This composition works similar to an internal bcross.
--
-- The motivation for stepDelayB is to process updates provided
-- between steps, while ensuring the snapshot consistency within a
-- partition. Can also be used for worker threads. But it may prove
-- more useful in some cases to use `onNextStep` with phaseDelayB,
-- e.g. to ensure updates are grouped.
--
stepDelayB :: (Partition p) => PCX w -> B (S p x) (S p x)
stepDelayB cw = fix $ \ b ->
    let (p,_) = getPartitions b in
    let cp = getPCX p cw in
    let tc = getTC cp in
    let doSend = addTCRecv tc in
    let mkLn = return . fnStepDelay doSend in
    B_mkLnk tr_fwd mkLn

fnStepDelay :: (IO () -> IO ()) -> Lnk (S p x) -> Lnk (S p x)
fnStepDelay _ LnkDead = LnkDead
fnStepDelay stepDelay (LnkSig lu) = LnkSig lu'
    where lu' = LnkUp { ln_touch = return (), ln_update = update }
          update = stepDelay . ln_update lu

getTC :: (Partition p) => PCX p -> TC
getTC = findInPCX 

getPCX :: (Typeable p) => p -> PCX w -> PCX p
getPCX _ = findInPCX

-- | the GobStopper is collection of all the Stopper values in an
-- application; it is used to shut down the individual partitions
-- during graceful shutdown. (The P0 Stopper is excluded.) 
newtype GobStopper = Gob { unGob :: IORef [Stopper] }
instance Typeable GobStopper where
    typeOf _ = mkTyConApp tycGS []
        where tycGS = mkTyCon3 "Sirea" "Sirea.Partition.Internal" "GobStopper"
instance Resource GobStopper where
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
    let cp = getPCX p cw in
    let tc = getTC cp in
    atomicModifyIORef (tc_init tc) (\x->(True,x)) >>= \ bInit ->
    unless bInit $ 
        newPartitionThread cp (tcToStepper tc) >>= \ s0 ->
        let stopInStepper = addTCRecv tc (runStopper s0) in
        let stopper = s0 { runStopper = stopInStepper } in 
        let gs = unGob $ findInPCX cw in
        let cp0 = findInPCX cw :: PCX P0 in -- pulses are initiated by P0
        initPulseListener cp0 cp >>
        atomicModifyIORef gs (\stoppers->(stopper:stoppers,()))
        -- TODO: add support to receive pulse.
    


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
    , ob_state :: !(IORef OBW)
    }
data OBWork = OBW 
    { obw_urgent :: {-# UNPACK #-} !Bool -- are there any urgent updates in this batch? 
    , obw_sched  :: {-# UNPACK #-} !Bool -- scheduled? (due to choke, can't rely on empty `work`)
    , obw_work   :: [Work]               -- work to perform (reverse-ordered)
    }

obwZero :: OBW
obwZero = OBW False False []

instance Typeable1 OutBox where
    typeOf1 _ = mkTyConApp tycOB []
        where tycOB = mkTyCon3 "Sirea" "Sirea.Partition.Internal" "OutBox"
instance (Partition p) => Resource (OutBox p) where
    locateResource _ _ = OutBox <$> newSemaphore batchesInFlight
                                <*> newIORef obwZero

-- obSend - send work FROM p1 TO p2 (given the world PCX).
--   (not mt-safe, should be called only from p1's thread.)
--
-- obSend includes simplistic support for choke based on urgency at
-- granularity of the full batch. The `choke` mechanism is very 
-- simple: if not delivered in the current round, we'll deliver
-- on the next heartbeat. Choke is thus never longer than one
-- heartbeat. 
--
obSend :: (Partition p1, Partition p2) 
       => PCX w -> p1 -> p2 -> Bool -> Work -> IO ()
obSend cw p1 p2 = assert (typeOf p1 /= typeOf p2) $ obwAddWork
    where cp1 = getPCX p1 cw
          tc1 = getTC cp1
          cp2 = getPCX p2 cw
          tc2 = getTC cp2
          ob  = getOB cw p1 p2
          later = eventually cp1 
          obwAddWork bUrgent work =
            readIORef (ob_state ob) >>= \ obw ->
            let bWasSched = obw_sched obw in
            let urgent' = obw_urgent obw || bUrgent in
            let work' = work:(obw_work obw) in
            let obw' = OBW { obw_urgent = urgent', obw_sched = True, obw_work = work' } in
            writeIORef (ob_state ob) >>
            unless bWasSched obwSchedule
          obwSchedule = addTCSend tc1 deliverNowOrLater 
          deliverNowOrLater =
            readIORef (ob_state ob) >>= \ obw ->
            if (obw_urgent obw) 
                then let work = (sequence_ . reverse) (obw_work obw) in
                     writeIORef (ob_state ob) obwZero >>
                     deliverNow work
                else let obw' = obw { obw_sched = False } in
                     writeIORef (ob_state ob) obw' >>
                     deliverLater
          deliverNow work =
            let sem = ob_sem ob in
            sem_wait sem >> -- semaphore models a bounded buffer
            addTCRecv tc2 (sem_signal sem >> work)
          deliverLater = later $ obwAddWork True (return ())

-- | obtain the outbox resource p1->p2 starting from the world context
--   (Each partition has a set of outboxes for each other partition)
getOB :: (Partition p1, Partition p2) => PCX w -> p1 -> p2 -> OutBox p2
getOB cw p1 _ = findInPCX (getPCX p1 cw)

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

