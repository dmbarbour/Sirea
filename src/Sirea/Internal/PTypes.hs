
module Sirea.Internal.PTypes
    ( Stepper(..)
    , Stopper(..)
    ) where

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



