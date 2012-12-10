
module Sirea.Internal.PTypes
    ( Stepper(..)
    , Stopper(..)
    , TC(..)
    , runTCStep
    , addTCRecv, addTCEvent
    , addTCWork, addTCSend
    , getTCTime
    , tcToStepper
    ) where

import Sirea.PCX
import Sirea.Time
import Data.Typeable
import Data.IORef
import Control.Applicative
import Control.Exception (mask_)
import Control.Monad (join)

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
-- runStepper: process available updates, deliver outputs. Returns 
--   very quickly if there is nothing to do, so should wait on event
--   or a fast-pased periodic task (e.g. render frame) after a step.
--
-- addStepperEvent: add a callback to occur when work is available.
--   The callback must be wait free, and must not call runStepper
--   itself. Typically used with MVars to wait for available input.
--   The event is called from non-partition threads, and should not
--   call runStepper. Event is removed when called or by runStepper.
--
data Stepper = Stepper 
    { runStepper      :: IO () -- ^ synchronous incremental step
    , addStepperEvent :: IO () -> IO () -- ^ notify of work to do
    }

-- | Stopper should provide a way to gracefully halt Sirea threads.
-- For most threads (except for the main thread) the Stopper is
-- called only after stopping the application signal, so activity
-- should have already been halted when runStopper is called. Any
-- final cleanup tasks can be peformed and the thread dropped after
-- runStopper. For the main thread, the client calls Stopper to halt
-- the SireaApp as a whole (see Sirea.Build).
--
-- Stopping may take a while, so events provide feedback for when a
-- thread or application is done with all cleanup. 
--
-- Sirea shifts runStopper operation to occur as a runStepper task.
-- This ensures need only to wait on Stepper events, and simplifies
-- thread safety issues. 
data Stopper = Stopper
    { runStopper      :: IO () -- ^ asynchronous begin stop
    , addStopperEvent :: IO () -> IO () -- ^ notify when stopped
    }

-- | TC is the thread context - basically a small set of queues in
-- IORefs, and some initialization status (for partitions other than
-- P0, keep track to shutdown later).
--    tc_init :: for initialization; atomic
--    tc_recv :: either event or work; atomic
--    tc_work :: phased tasks, repeats until empty; not atomic
--    tc_send :: tasks to perform at end of round; not atomic
--    tc_time :: time of step; monotonic, not atomic 
-- These are not heavily optimized; they don't need to be, since
-- there are a bounded number of tasks in any queue at once, and
-- received operations are pre-grouped in batches.
data TC = TC 
    { tc_init :: !(IORef Bool)
    , tc_recv :: !(IORef (Either [Event] [Work]))
    , tc_work :: !(IORef [Work])
    , tc_send :: !(IORef [Work])  
    , tc_time :: !(IORef (T,T)) -- (tEff,tAct)
    }
type Event = IO ()
type Work = IO ()

runReverse :: [IO ()] -> IO ()
runReverse = sequence_ . reverse

newTC :: IO TC
newTC = TC <$> newIORef False
           <*> newIORef (Left [])
           <*> newIORef []
           <*> newIORef []
           <*> newIORef (tZero,tZero)
    where tZero = mkTime 0 0

instance Typeable TC where
    typeOf _ = mkTyConApp tycTC []
        where tycTC = mkTyCon3 "sirea-core" "Sirea.Partition.Internal" "TC"
instance Resource TC where
    locateResource _ _ = newTC

-- | In each runStepper round:
--    recv tasks are emptied (atomically) then processed
--    work tasks are created by recv, then handled in group
--    send tasks performed at end of stepper round.
-- The `work` phase might run multiple rounds if creates more work.
-- However, `recv` and `send` are once per round. 
tcToStepper :: TC -> Stepper
tcToStepper tc = Stepper 
    { runStepper = runTCStep tc
    , addStepperEvent = addTCEvent tc
    }
 
runTCStep :: TC -> IO ()
runTCStep tc = mask_ $
    updateTime tc >>
    runTCRecv (tc_recv tc) >>
    runTCWork (tc_work tc) >>
    runTCSend (tc_send tc)

-- TCRecv has either event or work. If an event is scheduled, it is
-- dropped when running a step (and must be explicitly rescheduled).
runTCRecv :: IORef (Either [Event] [Work]) -> IO ()
runTCRecv rfRecv =
    atomicModifyIORef rfRecv swapZero >>=
    either dropEvent runWork
    where swapZero x = (Left [],x)
          dropEvent _ = return ()
          runWork = runReverse

-- TCWork may execute multiple phases.
--
-- If Sirea resources are designed properly, there should be a small
-- bounded number of phases per step. Current design only needs one.
runTCWork :: IORef [Work] -> IO ()
runTCWork rfw = 
    readIORef rfw >>= \ lw ->
    writeIORef rfw [] >>
    runWork lw
    where runWork [] = return ()
          runWork lw = runReverse lw >> runTCWork rfw

-- TCSend will empty non-empty outboxes for the round. Usually a 
-- small task, since fan-out between partitions is limited by type.
-- Updates in each outbox will be sent as one atomic batch.
--
-- This operation may wait: each outbox has a semaphore with limited
-- number of in-flight batches. If a fast producer sends to a slower
-- consumer, the producer may end up waiting.
runTCSend :: IORef [Work] -> IO ()
runTCSend rfEmit = 
    readIORef rfEmit >>= \ lw ->
    writeIORef rfEmit [] >>
    runReverse lw

-- add work to a partition; will execute at start of next round
addTCRecv :: TC -> Work -> IO ()
addTCRecv tc op = join $ atomicModifyIORef (tc_recv tc) putOpTakeEvent
    where putOpTakeEvent (Left lEvents) = (Right [op], runReverse lEvents)
          putOpTakeEvent (Right lWork) = (Right (op:lWork), return ())

addTCEvent :: TC -> Event -> IO ()
addTCEvent tc ev = join $ atomicModifyIORef (tc_recv tc) addOrExecEvent
    where addOrExecEvent (Left lEvents) = (Left (ev:lEvents), return ())
          addOrExecEvent (Right lWork) = (Right lWork, ev)

-- work is not modified atomically.
addTCWork :: TC -> Work -> IO ()
addTCWork tc op = 
    let rf = tc_work tc in
    readIORef rf >>= \ lw ->
    writeIORef rf (op:lw)

addTCSend :: TC -> Work -> IO ()
addTCSend tc op =
    let rf = tc_send tc in
    readIORef rf >>= \ lw ->
    writeIORef rf (op:lw) 

getTCTime :: TC -> IO T
getTCTime tc = fst `fmap` readIORef (tc_time tc)

-- updateTime time, with heuristics to ensure monotonic update.
updateTime :: TC -> IO ()
updateTime tc =
    let rf = tc_time tc in
    readIORef rf >>= \ ts0 ->
    getTime >>= \ tNow ->
    let tEff = adjTime ts0 tNow in
    tEff `seq`  
    writeIORef rf (tEff,tNow) 

-- the monotonic update heuristics
adjTime :: (T,T) -> T -> T
adjTime (tEff0, tAct0) tNow =
    let dtAct = tNow `diffTime` tAct0 in -- difference in times.
    let dtEst = 0.96 * dtAct in -- heuristic to mitigate sudden shifts in OS clock
    let tEst = if (dtEst > 0) then tEff0 `addTime` dtEst 
                              else tEff0 `addTime` 0.0001
    in 
    max tEst tNow 


-- TO CONSIDER:
--   I could model stop external to the behaviors themselves, halting on a runStepper.
--   Idea is:
--      Users of thread can schedule onStop tasks, i.e. to clean up. 
--      We always stop on a runStepper operation.
--      We simply enter a terminating loop until we get the stoppedOnMVar 
--      When main thread gets StoppedOnMVar, it returns.
--   I don't believe this would be an improvement, though. What I actually need is a
--   clean model for switching threads when I switch plugins... (eventually, anyway).
--   Maybe it'd be better to design threads with something like this in mind? E.g. in
--   the plugins model itself.

