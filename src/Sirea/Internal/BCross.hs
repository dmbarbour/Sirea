{-# LANGUAGE GADTs #-}

-- | BCross is the implementation of cross for B and BCX.
--
-- It also contains types associated with partitions and
-- crossB, i.e. used in BCX.
module Sirea.Internal.BCross 
    ( crossB
    , GobStopper(..)
    , runGobStopper
    , TC(..)
    , runTCStep
    , addTCRecv, addTCEvent
    , addTCWork, addTCSend
    , tcToStepper
    , OutBox
    ) where

import Data.Typeable
import Data.Function (fix)
import Data.IORef
import Data.Maybe (isNothing)
import Control.Applicative
import Control.Exception (mask_, assert)
import Control.Concurrent.MVar 
import Control.Monad (join, unless, when)

import Sirea.Internal.BTypes
import Sirea.Internal.STypes
import Sirea.Internal.LTypes
import Sirea.Internal.BImpl (phaseUpdateB)
import Sirea.Partition
import Sirea.PCX
import Sirea.Behavior

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
      else sendB cw p1 p2 >>> receiveB cw p2

-- this is a total hack in haskell to access typesystem data
getPartitions :: B w (S p1 x) (S p2 x) -> (p1,p2)
getPartitions _ = (undefined,undefined)

-- jumpB is used if we're moving to the same partition we started in
-- It simply ignores the partitions and forwards the signal. Jump is
-- free after compilation.
jumpB :: B w (S p1 x) (S p2 x)
jumpB = B_mkLnk tr_fwd lnkJump
    where lnkJump = MkLnk { ln_build = return . fnJump
                          , ln_tsen = False, ln_peek = 0 }

fnJump :: Lnk (S p a) -> Lnk (S p' a) 
fnJump LnkDead = LnkDead
fnJump (LnkSig lu) = LnkSig lu

-- sendB, if active (not dead code), will
--   * create partitions if they don't already exist
--   * batch multiple updates to one partition (across signals)
--   * delay operation to runTCSend phase
sendB :: (Partition p1, Partition p2) 
      => PCX w -> p1 -> p2 -> B w (S p1 x) (S p2 x)
sendB cw p1 p2 = B_mkLnk tr_fwd lnkSend
    where lnkSend = MkLnk { ln_build = mkSend cw p1 p2
                          , ln_tsen = False, ln_peek = 0 }

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

-- receiveB is used on the receive side of bcross. Serves two roles:
--   * combines updates from multiple in-flight batches
--   * touch to efficiently process multiple updates
receiveB :: (Partition p) => PCX w -> p -> B w (S p x) (S p x)
receiveB cw p = 
    let tc = getTC p cw in
    let doLater = addTCWork tc in
    phaseUpdateB (return doLater)

getTC :: (Typeable p) => p -> PCX w -> TC p
getTC _ = findInPCX

getPCX :: (Typeable p) => p -> PCX w -> PCX p
getPCX _ = findInPCX

-- | the GobStopper is collection of all the Stopper values in an
-- application; it is used to shut down the individual partitions
-- during graceful shutdown. 
newtype GobStopper = Gob { unGob :: IORef [Stopper] }
instance Typeable GobStopper where
    typeOf _ = mkTyConApp tycGS []
        where tycGS = mkTyCon3 "Sirea" "Partition.Internal" "GobStopper"
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
-- partition exists before we step into it. Idempotent.
initPartition :: (Partition p) => p -> PCX w -> IO ()
initPartition p cw =
    let tc = getTC p cw in
    atomicModifyIORef (tc_init tc) (\x->(True,x)) >>= \ bInit ->
    unless bInit $ 
        let cp = getPCX p cw in
        newPartitionThread cp (tcToStepper tc) >>= \ s0 ->
        let stopInStepper = addTCRecv tc (runStopper s0) in
        let stopper = s0 { runStopper = stopInStepper } in 
        let gs = unGob $ findInPCX cw in
        atomicModifyIORef gs (\stoppers->(stopper:stoppers,()))
    

-- | TC is the thread context, which is basically a couple IORefs 
-- with some metadata about the thread. 
--    tc_init :: for initialization; atomic
--    tc_recv :: either event or work; atomic
--    tc_work :: phased tasks, repeats until empty; not atomic
--    tc_send :: tasks to perform at end of round; not atomic
-- These are not heavily optimized; they don't need to be, since
-- there are a bounded number of tasks in any queue at once, and
-- received operations are pre-grouped in batches.
data TC p = TC 
    { tc_init :: IORef Bool
    , tc_recv :: IORef (Either Event Work)
    , tc_work :: IORef (Maybe Work)
    , tc_send :: IORef Work
    }
type Event = IO ()
type Work = IO ()

instance Typeable1 TC where
    typeOf1 _ = mkTyConApp tycTC []
        where tycTC = mkTyCon3 "Sirea" "Partition.Internal" "TC"
instance (Typeable p) => Resource (TC p) where
    locateResource _ = newTC

newTC :: IO (TC p)
newTC = TC <$> newIORef False
           <*> newIORef (Left (return ()))
           <*> newIORef Nothing
           <*> newIORef (return ())

-- | In each runStepper round:
--    recv tasks are emptied (atomically) then processed
--    work tasks are created by recv, then handled in group
--    send tasks performed at end of stepper round.
-- The `work` phase might run multiple rounds if creates more work.
-- However, `recv` and `send` are once per round. 
tcToStepper :: TC p -> Stepper
tcToStepper tc = Stepper 
    { runStepper = runTCStep tc
    , addStepperEvent = addTCEvent tc
    }
 
runTCStep :: TC p -> IO ()
runTCStep tc = mask_ $
    runTCRecv (tc_recv tc) >>
    runTCWork (tc_work tc) >>
    runTCSend (tc_send tc)

-- TCRecv has either event or work.
runTCRecv :: IORef (Either Event Work) -> IO ()
runTCRecv rfRecv =
    atomicModifyIORef rfRecv swapZero >>=
    either ignoreEvent performWork
    where swapZero x = (Left (return ()),x)
          ignoreEvent _ = return ()
          performWork work = work

-- TCWork will execute multiple phases.
-- Usually there is only one phase.
runTCWork :: IORef (Maybe Work) -> IO ()
runTCWork rfw = 
    readIORef rfw >>= \ mbWork ->
    writeIORef rfw Nothing >>
    maybe done doWork mbWork
    where doWork work = work >> runTCWork rfw
          done = return ()

-- TCSend will empty non-empty outboxes for the round. Usually a 
-- small task, since fan-out between partitions is limited by type.
-- Updates in each outbox will be sent as one atomic batch.
--
-- This operation may wait: each outbox has a semaphore with limited
-- number of in-flight batches. If a fast producer sends to a slower
-- consumer, the producer may end up waiting.
runTCSend :: IORef Work -> IO ()
runTCSend rfEmit = 
    readIORef rfEmit >>= \ doEmit ->
    writeIORef rfEmit (return ()) >>
    doEmit

-- add work to a partition; will execute at start of next round
addTCRecv :: TC p -> Work -> IO ()
addTCRecv tc op = join $ atomicModifyIORef (tc_recv tc) putOpTakeEvent
    where putOpTakeEvent (Left event) = (Right op, event)
          putOpTakeEvent (Right work) = (Right (work >> op), return ())

addTCEvent :: TC p -> Event -> IO ()
addTCEvent tc ev = join $ atomicModifyIORef (tc_recv tc) addOrExecEvent
    where addOrExecEvent (Left event) = (Left (event >> ev), return ())
          addOrExecEvent (Right work) = (Right work, ev)

-- work is not modified atomically.
addTCWork :: TC p -> Work -> IO ()
addTCWork tc newWork = modifyIORef (tc_work tc) addWork
    where addWork Nothing = Just newWork
          addWork (Just oldWork) = Just (oldWork >> newWork)

addTCSend :: TC p -> Work -> IO ()
addTCSend tc newWork = modifyIORef (tc_send tc) addWork
    where addWork oldWork = (oldWork >> newWork)

-- tcSend - send work from p1 to p2 in world w.
-- actual send happens at end of p1's round.
-- actual receive happens at beginning of p2's round.
-- outbox (modeled as a resource) controls communication with semaphore.
tcSend :: (Partition p1, Partition p2) 
       => PCX w -> p1 -> p2 -> Work -> IO ()
tcSend cw p1 p2 = 
    let tc1 = getTC p1 cw in
    let ob  = getOB cw p1 p2 in
    let tc2 = getTC p2 cw in
    tcSend' tc1 ob tc2

tcSend' :: TC p1 -> OutBox p2 -> TC p2 -> Work -> IO ()
tcSend' tc1 ob tc2 work =
    ob_addWork ob work >>= \ bFirst -> 
    when bFirst prepSend
    where prepSend  = addTCSend tc1 doDeliver -- deliver at end of round
          doDeliver = ob_deliver ob recvInP2  -- performs delivery (may wait)
          recvInP2  = addTCRecv tc2           -- adds work to next round in p2

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
        where tycOB = mkTyCon3 "Sirea" "Partition.Internal" "OutBox"
instance (Typeable p) => Resource (OutBox p) where
    locateResource _ = OutBox <$> newSemaphore ob_max_in_flight
                              <*> newIORef Nothing

-- | obtain the outbox resource p1->p2 starting from the world context
getOB :: (Typeable p1, Typeable p2) => PCX w -> p1 -> p2 -> OutBox p2
getOB cw p1 _ = findInPCX (getPCX p1 cw)

-- | add some work to the OutBox. Returns true if work was added to
-- an empty outbox, so you can setup the delivery.
ob_addWork :: OutBox p -> Work -> IO Bool
ob_addWork ob op =
    let st = ob_state ob in
    readIORef st >>= \ mbW ->
    let result = isNothing mbW in
    let mbW' = Just $ maybe op (>> op) mbW in
    writeIORef st mbW' >>
    return result

-- | deliver work from the outbox; results in an empty outbox.
-- Uses the counting semaphore for bounded buffers.
ob_deliver :: OutBox p -> (Work -> IO ()) -> IO ()
ob_deliver ob deliver =
    let st = ob_state ob in
    readIORef st >>= \ mbW ->
    writeIORef st Nothing >>
    maybe (return ()) doDeliver mbW
    where doDeliver work =
            let sem = ob_sem ob in
            sem_wait sem >>
            deliver (sem_signal sem >> work)

-- | a simple semaphore to model bounded-buffers. 
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

