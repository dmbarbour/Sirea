{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, FlexibleInstances #-}


-- | The PulseSensor module supports weakly periodic tasks, usually
-- for local garbage collection for resources like demand monitors,
-- potentially for low-priority polling. Developers don't have much
-- control over the timing of the pulse, except that it runs as part
-- of runStepper and during the `send` phase (after updates have 
-- been processed).
--
-- The pulse is initiated by the main partition P0 on its heartbeat
-- (that is, when it increases stability of the main signal). It 
-- propagates independently of the P0 outbox, so there may be some
-- scheduling inefficiency.
--
-- Pulse events will run in the same order they are added. 
--
module Sirea.Internal.PulseSensor
    ( initPulseListener
    , getPulseScheduler
    , getPulseRunner
    ) where

import Control.Applicative
import Control.Monad (join)
import Data.IORef
import Data.Typeable
--import Sirea.Partition
import Sirea.PCX
import Sirea.Internal.PTypes

-- TODO: Support exponential-backoff via layers of batches.
--   I.e. at heartbeat, 2*heartbeat, 4*heartbeat, etc.
-- This will make it easier to prevent future computations from
-- running too far ahead, support more robust looping.

-- | We're building a PulseReq list. This list is not intended for
-- heavy use, and so pays the expense for atomic updates.
type Work = IO () 
type OnWork = IO ()
newtype PulseReq = PulseReq (IORef (OnWork, [Work]))
    deriving (Typeable)

instance Resource p PulseReq where 
    locateResource _ _ = PulseReq <$> newIORef (return (), [])

runPulse :: PulseReq -> IO ()
runPulse (PulseReq rf) = 
    atomicModifyIORef rf takeWork >>= sequence_ . reverse
    where takeWork (onW,ls) = ((onW,[]),ls)

-- add work to pulse, maybe call the work available signal 
addWorkToPulse :: PulseReq -> Work -> IO ()
addWorkToPulse (PulseReq rf) w = 
    join (atomicModifyIORef rf putWork)
    where putWork (onW,[]) = ((onW,w:[]),onW)
          putWork (onW,ls) = ((onW,w:ls),return ())

-- note: setWorkAvailableSignal will call immediately if work is available
setWorkAvailableSignal :: PulseReq -> OnWork -> IO ()
setWorkAvailableSignal (PulseReq rf) onW = 
    join (atomicModifyIORef rf setSig)
    where setSig (_,[]) = ((onW,[]),return ())
          setSig (_,ls) = ((onW,ls),onW)

-- initPulseListener sets the OnWork task for a partition to
-- set an onNextStep callback on the next main partition (P0) 
-- heartbeat. Basically, we're round-tripping these tasks via
-- the main partition. The main partition heartbeat runs with
-- the maintenance task, though doesn't use the outbox/inbox
-- mechanism.
initPulseListener :: PCX p0 -> PCX p -> IO ()
initPulseListener cp0 cp = 
    findInPCX cp0 >>= \ pr0 ->
    findInPCX cp >>= \ prp ->
    findInPCX cp >>= \ tc ->
    let runW = addTCRecv tc (runPulse prp) in -- callback via onNextStep
    let onW = addWorkToPulse pr0 runW in      -- add one-time callback to P0's pulse 
    setWorkAvailableSignal prp onW


type PulseScheduler = Work -> IO ()
getPulseScheduler :: PCX p -> IO PulseScheduler
getPulseScheduler cp = addWorkToPulse <$> findInPCX cp

-- PulseRunner for a given partition.
type PulseRunner = IO ()
getPulseRunner :: PCX p -> IO PulseRunner
getPulseRunner cp = runPulse <$> findInPCX cp


