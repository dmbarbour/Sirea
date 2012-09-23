{-# LANGUAGE GADTs #-}

-- | BCross is the implementation of cross for B and BCX.
--
-- It also contains types associated with partitions and
-- crossB, i.e. used in BCX.
--
-- Organization:
--   Each Partition has a `TC` (thread context).
--      This includes inbox, outbox, working tasks.

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
-- import Data.Maybe (isNothing)
import Control.Applicative
import Control.Exception (assert)
import Control.Concurrent.MVar 
import Control.Monad (join, unless, when)

import Sirea.Internal.BTypes
import Sirea.Internal.STypes
import Sirea.Internal.LTypes
import Sirea.Internal.PTypes
import Sirea.Internal.BImpl (phaseUpdateB)
import Sirea.Partition
import Sirea.PCX
import Sirea.Behavior

type Event = IO ()
type Work = IO ()

-- crossB will:
--    add the update as a task to perform in another partition.
--    allow updates on input to buffer and batch (bounded buffer)
-- crossB is eliminated if typeOf p1 == typeOf p2
--
crossB :: (Partition p1, Partition p2) => PCX w -> B w (S p1 x) (S p2 x)
crossB cw = fix $ \ b ->
    let (p1,p2) = getPartitions b in
    if (typeOf p1 == typeOf p2) 
      then jumpB
      else sendB cw p1 p2 >>> phaseDelayB cw

-- this is a total hack in haskell to access typesystem data
getPartitions :: B w (S p1 x) (S p2 x) -> (p1,p2)
getPartitions _ = (undefined,undefined)

-- jumpB is used if we're moving to the same partition we started in
-- It simply ignores the partitions and forwards the signal. Jump is
-- free after compilation. It would be unsafe to use this if p1 != p2.
jumpB :: (Partition p1, Partition p2) => B w (S p1 x) (S p2 x)
jumpB = fix $ \ b ->
    let (p1,p2) = getPartitions b in
    assert(typeOf p1 == typeOf p2) $
    B_mkLnk tr_fwd lnkJump
    where lnkJump = return . fnJump

fnJump :: Lnk (S p x) -> Lnk (S p' x)
fnJump LnkDead = LnkDead
fnJump (LnkSig lu) = (LnkSig lu)

-- sendB, if active (not dead code), will
--   * create partitions if they don't already exist
--   * batch multiple updates to one partition (across signals)
--   * delay operation to runTCSend phase
sendB :: (Partition p1, Partition p2) 
      => PCX w -> p1 -> p2 -> B w (S p1 x) (S p2 x)
sendB cw p1 p2 = B_mkLnk tr_fwd lnkSend
    where lnkSend = mkSend cw p1 p2

mkSend :: (Partition p1, Partition p2) 
       => PCX w -> p1 -> p2 -> Lnk (S p2 x) -> IO (Lnk (S p1 x))
mkSend _ _ _ LnkDead = return LnkDead
mkSend cw p1 p2 (LnkSig lu) = 
    initPartition p1 cw >>
    initPartition p2 cw >>
    let addToBatch = tcSend cw p1 p2 in
    let touch'  = return () in
    let update' = addToBatch . (ln_update lu) in
    let lu' = LnkUp { ln_touch = touch', ln_update = update' } in
    return (LnkSig lu')

-- | phaseDelayB delays updates a phase within runStepper. The idea
-- is to touch a bunch of updates in one run before executing them 
-- later run, potentially eliminating redundant updates. Use of 
-- phaseDelayB may also accumulate multiple updates, which is useful
-- if phaseDelayB runs in the runStepper receive phase.
phaseDelayB :: (Partition p) => PCX w -> B w (S p x) (S p x)
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
stepDelayB :: (Partition p) => PCX w -> B w (S p x) (S p x)
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
    locateResource _ = Gob <$> newIORef []

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
        atomicModifyIORef gs (\stoppers->(stopper:stoppers,()))
    

-- tcSend - send work from p1 to p2 in world w.
-- actual send happens at end of p1's round.
-- actual receive happens at beginning of p2's round.
-- outbox (modeled as a resource) controls communication with semaphore.
tcSend :: (Partition p1, Partition p2) 
       => PCX w -> p1 -> p2 -> Work -> IO ()
tcSend cw p1 p2 work = 
    assert (typeOf p1 /= typeOf p2) $
    let tc1 = getTC (getPCX p1 cw) in
    let ob  = getOB cw p1 p2 in
    let tc2 = getTC (getPCX p2 cw) in
    ob_addWork ob work >>= \ bFirstWorkInBox ->
    when bFirstWorkInBox $
        let deliverOb = ob_deliver ob tc2 in
        addTCSend tc1 deliverOb

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
    { ob_sem   :: Semaphore -- bounds number of in-flight batches.
    , ob_state :: IORef (Maybe Work)
    }

-- | how many batches in flight?
-- 
--     at least 1 to communicate
--     at least 2 to combine batches
--
-- Higher values can improve parallelism, but can cost memory and 
-- increase latency between an update becoming available and being
-- processed. I'm choosing a value that should be decent for most
-- programs. 
--
-- I don't want configuration to be a major aspect of Sirea. But I
-- might make this configurable via command line at some point (as a
-- resource). 
ob_max_in_flight :: Int
ob_max_in_flight = 6

instance Typeable1 OutBox where
    typeOf1 _ = mkTyConApp tycOB []
        where tycOB = mkTyCon3 "Sirea" "Sirea.Partition.Internal" "OutBox"
instance (Partition p) => Resource (OutBox p) where
    locateResource _ = OutBox <$> newSemaphore ob_max_in_flight
                              <*> newIORef Nothing

-- | obtain the outbox resource p1->p2 starting from the world context
getOB :: (Partition p1, Partition p2) => PCX w -> p1 -> p2 -> OutBox p2
getOB cw p1 _ = findInPCX (getPCX p1 cw)

-- | add some work to the OutBox. Returns true if work was added to
-- an empty outbox, so you can setup the delivery.
ob_addWork :: OutBox p -> Work -> IO Bool
ob_addWork ob op =
    let st = ob_state ob in
    readIORef st >>= \ mbW ->
    case mbW of
        Nothing ->
            writeIORef st (Just op) >>
            return True
        Just ops ->
            writeIORef st (Just (ops >> op)) >>
            return False

-- | deliver work from the outbox; results in an empty outbox.
-- Uses the counting semaphore for bounded buffers.
ob_deliver :: OutBox p -> TC -> IO ()
ob_deliver ob dst =
    let st = ob_state ob in
    readIORef st >>= \ mbW ->
    writeIORef st Nothing >>
    maybe (return ()) doDeliver mbW
    where doDeliver work =
            let sem = ob_sem ob in
            sem_wait sem >>
            addTCRecv dst (sem_signal sem >> work)

-- | a simple semaphore to model bounded-buffers between partitions. 
-- 
-- State is either a count of available resources or a list of tasks
-- each waiting on a resource. Signal either releases one thread or 
-- adds one to the count. Wait either decrements count or adds one
-- event to the list.
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

