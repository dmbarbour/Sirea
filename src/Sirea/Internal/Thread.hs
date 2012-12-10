
-- | simple threading support for Partitions and runSireaApp
module Sirea.Internal.Thread
    ( StopData(..)
    , emptyStopData
    , finiStopData
    , simplePartitionLoop
    , makeStopper
    ) where

import Sirea.Internal.PTypes

import Data.IORef
import Control.Monad (join)
import Control.Exception (assert)
import Control.Concurrent.MVar

data StopData = SD
    { shouldStop   :: !Bool
    , isStopped    :: !Bool
    , onStopped    :: !(IO ())
    }

makeStopper :: IORef StopData -> Stopper
makeStopper rf = Stopper 
    { runStopper = atomicStop rf
    , addStopperEvent = addStopDataEvent rf }

emptyStopData :: StopData
emptyStopData = SD False False (return ())

atomicStop :: IORef StopData -> IO ()
atomicStop rf = atomicModifyIORef rf doStop
    where doStop sd = 
            if shouldStop sd then (sd, ()) else
            let sd' = sd { shouldStop = True } in
            (sd', ())

addStopDataEvent :: IORef StopData -> IO () -> IO ()
addStopDataEvent rf ev = join $ atomicModifyIORef rf addEv
    where addEv sd = 
            if (isStopped sd) then (sd, ev) else
            let stopEvent = onStopped sd >> ev in
            let sd' = sd { onStopped = stopEvent } in
            (sd', return ())

-- fini should be used only once
finiStopData :: IORef StopData -> IO ()
finiStopData rf = join $ atomicModifyIORef rf fini
    where fini sd =
                assert (shouldStop sd) $
                assert (not $ isStopped sd) $
                let sd' = SD True True (return ()) in
                (sd', onStopped sd)

simplePartitionLoop :: IORef StopData -> Stepper -> IO ()
simplePartitionLoop rfStop stepper =
    readIORef rfStop >>= \ sd ->
    if (shouldStop sd) then stop 
      else wait >> run >>= \ _ -> loop
    where stop = finiStopData rfStop 
          wait = newEmptyMVar >>= \ mv ->
                 addStepperEvent stepper (putMVar mv ()) >>
                 takeMVar mv
          run  = runStepper stepper
          loop = simplePartitionLoop rfStop stepper



