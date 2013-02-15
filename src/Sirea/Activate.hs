{-# LANGUAGE GADTs, Rank2Types, MultiParamTypeClasses, DeriveDataTypeable #-}

-- | This module contains the functions to run a Sirea application.
-- The main function you'll need is 'runSireaApp'. The rest can be
-- ignored unless you need to control the main thread loop or have
-- other special needs.
--
module Sirea.Activate
    ( runSireaApp, pulseSireaApp
    , SireaAppObject(..)
    , buildSireaApp, beginSireaApp
    , bUnsafeExit
    ) where

import Prelude hiding (catch)
import Data.IORef
import Data.Typeable
import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad (unless, when, void, liftM)
import Control.Exception (catch, assert, AsyncException)
import Control.Concurrent (myThreadId, forkIO, killThread, threadDelay)
import Sirea.Internal.B0Type
import Sirea.Internal.LTypes
import Sirea.Internal.BCompile(compileB)
import Sirea.Internal.PTypes
import Sirea.Internal.BCross
import Sirea.Internal.Thread
import Sirea.Internal.PulseSensor (getPulseRunner)
import Sirea.Internal.Tuning (dtRestart, dtStability, dtHeartbeat, dtGrace)
import Sirea.Behavior
import Sirea.Partition
import Sirea.UnsafeOnUpdate
import Sirea.PCX
import Sirea.BCX
import Sirea.Time
import Sirea.Signal

import Debug.Trace (traceIO)


-- | The typical use case for Sirea is to simply runSireaApp as the
-- main operation, with enough abstraction that the app itself is a
-- one-liner. The application behavior is activated for side-effects
-- and the response signal is ignored. 
--
--    main :: IO ()
--    main = runSireaApp $ foo |*| bar |*| baz
--
-- RDP supports multi-agent composition with the |*| operator, where
-- an agent is modeled as an RDP behavior that drops its response. A
-- suggestion is to limit main to a composition of agents behaviors.
--
-- runSireaApp will activate the behavior and keep it active until 
-- interrupted by any AsyncException in the initial thread. Ctrl+C
-- will cause such a exception. After interruption, runSireaApp will
-- run a few more heartbeats (a fraction of a second) while shutting
-- down gracefully, then return. (A second interrupt will abort the 
-- effort for graceful shutdown.)
--
runSireaApp :: B (S P0 ()) y -> IO ()
runSireaApp app = buildSireaApp (app >>> btrivial) >>= beginSireaApp

-- | pulseSireaApp will simply runSireaApp very briefly, a fraction
-- of a second. This is potentially useful for testing, and enables
-- RDP to be wielded much like a procedure call.
pulseSireaApp :: B (S P0 ()) y -> IO ()
pulseSireaApp app = 
    buildSireaApp (app >>> btrivial) >>= \ so ->
    (runStopper . sireaStopper) so >>
    beginSireaApp so

-- | SireaAppObject manages life cycle of an initialized SireaApp:
-- 
--     Stepper - for user-controlled event loops
--     Stopper - to halt the application gracefully
--     PCX P0  - integrate resources controlled by main thread (*) 
--
-- These types are defined in the Partition and PCX modules. The P0
-- partition is thus similar to other partitions, excepting Stopper
-- is provided to halt the whole app. Stopper is not instantaneous;
-- continue to runStepper until Stopper callback event is executed
-- to support graceful shutdown.
--
-- (*) It is not recommended to actually use sireaContext, since any
-- explicit use is difficult to abstract and reuse. It is included 
-- not for utility, but for completeness, matching other partitions.
data SireaAppObject = SireaAppObject 
    { sireaStepper :: Stepper
    , sireaStopper :: Stopper
    , sireaContext :: PCX P0
    }

-- AppPeriodic data is used internally on the man clock step.
data AppPeriodic = AppPeriodic 
    { ap_cw     :: !(PCX W)
    , ap_tc0    :: !(TC)
    , ap_gs     :: !(GobStopper)
    , ap_pulse  :: !(IO ())
    , ap_sd     :: !(IORef StopData)
    , ap_luMain :: !(LnkUp ())
    }


-- | Build the SireaApp behavior, generating a SireaAppObject. Access
-- to the Stepper allows the user to embed the SireaApp in another
-- event loop, and access to Context supports integrating resources
-- controlled by the main event loop. 
--
-- If you don't already have a main event loop, I suggest using the
-- runSireaApp function and shifting resources to dedicated threads.
-- (Explicitly hooking up resources detracts from the declarative 
-- programming experience, and is not readily extensible.)
--
buildSireaApp :: B (S P0 ()) S1 -> IO SireaAppObject
buildSireaApp app = 
    newPCX [] >>= \ cw -> -- new global resource context
    getPCX0 cw >>= \ cp0 -> -- partition context P0
    findInPCX cp0 >>= \ tc0 ->
    writeIORef (tc_init tc0) True >>
    -- compute behavior in the new context
    -- add a phase delay for consistency with bcross updates
    let bcx = wrapB phaseDelayB >>> app in
    let b   = unwrapB bcx cw in
    -- compile behavior, dropping response
    let (_, mkLn) = compileB b (LnkDUnit ldt_zero) LnkDead in
    mkLn >>= \ lnk0 ->
    let bDead = ln_dead lnk0 in
    let lu = ln_lnkup lnk0 in
    buildSireaBLU cw lu >>= \ sireaAppObj ->
    when bDead ((runStopper . sireaStopper) sireaAppObj) >> 
    return sireaAppObj

getPCX0 :: PCX W -> IO (PCX P0)
getPCX0 = findInPCX
  
-- Build from a LnkUp, meaning there is something listening to the
-- signal. This doesn't actually initialize the signal, but does set
-- the app to kickstart on the first runStepper operation.
buildSireaBLU :: PCX W -> LnkUp () -> IO SireaAppObject
buildSireaBLU cw lu =
    newIORef emptyStopData >>= \ rfSD ->
    getPCX0 cw >>= \ cp0 ->
    findInPCX cp0 >>= \ tc0 ->
    addTCRecv tc0 (beginApp cw rfSD lu) >> -- add kickstart
    let stepper = tcToStepper tc0 in
    let stopper = makeStopper rfSD in
    return $ SireaAppObject { sireaStepper = stepper
                            , sireaStopper = stopper
                            , sireaContext = cp0  }
          
-- task to initialize application (performed on first runStepper)
-- a slight delay is introduced before everything really starts.
--
-- Once you start, you're active for at least a period of:
--
--    dtStability - dtGrace
--
-- This period starts at Now+dtGrace, providing a grace period for
-- startup (to pre-load resources, files, etc.). After halting, the
-- system runs an additional dtGrace for final shutdown. 
--
-- pulseSireaApp depends on stop condition remaining unchecked until
-- maintenance phases, so beginApp will always activate the app.
--
beginApp :: PCX W -> IORef StopData -> LnkUp () -> IO ()
beginApp cw rfSD lu = 
    findInPCX cw >>= \ gs ->
    getPCX0 cw >>= \ cp0 ->
    findInPCX cp0 >>= \ tc0 ->
    getPulseRunner cp0 >>= \ pulse ->    
    let apw = AppPeriodic 
                { ap_cw = cw
                , ap_tc0 = tc0
                , ap_gs = gs
                , ap_pulse = pulse
                , ap_sd = rfSD
                , ap_luMain = lu }
    in
    getTime >>= \ tNow ->
    let tStart = tNow `addTime` dtGrace in
    let tStable = tNow `addTime` dtStability in
    let nextStep = maintainApp apw tStable in
    ln_update lu (StableT tStable) tStart (s_always ()) >> -- activation!
    pulse >> -- first heartbeat
    schedule dtHeartbeat (addTCRecv tc0 nextStep)

-- schedule will delay an event then perform it in another thread.
-- Sirea only does this with one thread at a time.
schedule :: DT -> IO () -> IO ()
schedule dt op = assert (usec > 0) $ void $ 
    forkIO (threadDelay usec >> op)
    where usec = fromInteger $ (999 + dtToNanos dt) `div` 1000

-- regular maintenance operation, simply increases stability of the
-- active signal on a regular basis; performed within main thread.
-- At any given time, one maintenance operation is either queued in
-- the main thread or delayed by a 'schedule' thread.
maintainApp :: AppPeriodic -> T -> IO ()
maintainApp apw tS0 = beginPulse where
    lu = ap_luMain apw 
    tc0 = ap_tc0 apw
    beginPulse =
        ap_pulse apw >> -- heartbeat
        readIORef (ap_sd apw) >>= \ sd ->
        if shouldStop sd then beginHalt else
        getTCTime tc0 >>= \ tNow ->
        let bNeedRestart = (tNow > (tS0 `addTime` dtRestart)) in
        if bNeedRestart then beginReset tNow else continueIdle tNow
    later = schedule dtHeartbeat . addTCRecv tc0
    continueIdle tNow =
        let tS = tNow `addTime` dtStability in
        assert (tS >= tS0) $ -- the clock runs forwards, right?
        later (maintainApp apw tS) >>
        ln_idle lu (StableT tS) 
    resetMessage = "*** SIREA APP RESET (Stopped for a few seconds) ***"
    beginReset tNow =
        let tRestart = tNow `addTime` dtHeartbeat in
        let tS = tNow `addTime` dtStability in
        let sigRestart = s_switch s_never tRestart (s_always ()) in
        assert (tS >= tS0) $
        traceIO resetMessage >>
        later (maintainApp apw tS) >>
        ln_update lu (StableT tS) tS0 sigRestart
    beginHalt =
        let tFinal = tS0 `addTime` dtGrace in
        later (haltingApp apw tFinal) >>
        ln_update lu DoneT tFinal s_never 

-- After we set the main signal to inactive, we must still wait for
-- real-time to catch up, and should run a final few heartbeats to
-- provide any pulse actions.
haltingApp :: AppPeriodic -> T -> IO ()
haltingApp apw tFinal = 
    ap_pulse apw >> -- heartbeat
    getTCTime tc0 >>= \ tNow ->
    if (tNow > tFinal) -- wait for real time to catch up to stability
        then let onStop = ap_pulse apw >> finiStopData (ap_sd apw) in
             let gs = ap_gs apw in
             runGobStopper gs (addTCRecv tc0 onStop) >>
             later (finalizingApp apw) 
        else later (haltingApp apw tFinal)
    where later = schedule dtHeartbeat . addTCRecv tc0
          tc0 = ap_tc0 apw

-- at this point we've run the all-stop for all other threads,
-- so we're just biding time until threads report completion.
-- There might not be any more heartbeats at this point.
finalizingApp :: AppPeriodic -> IO ()
finalizingApp apw =
    ap_pulse apw >> -- heartbeat (potentially last)
    readIORef (ap_sd apw) >>= \ sd ->
    unless (isStopped sd) $
        let nextStep = finalizingApp apw in
        let tc0 = ap_tc0 apw in
        schedule dtHeartbeat (addTCRecv tc0 nextStep)



-- | beginSireaApp activates a forever loop to process the SireaApp.
-- Stopped by asynchronous exception, such as killThread or ctrl+c 
-- user interrupt. (note: a double-kill will abort graceful kill)
beginSireaApp :: SireaAppObject -> IO ()
beginSireaApp so =
    let stepper = sireaStepper so in
    let stopper = sireaStopper so in
    newIORef True >>= \ rfContinue ->
    let onStop = writeIORef rfContinue False in
    addStopperEvent stopper onStop >>
    let loop = basicSireaAppLoop rfContinue stepper in
    let onSignal = runStopper stopper in
    loop `catch` \ e -> onAsyncException e (onSignal >> loop)

-- Haskell's exception mechanism requires a little help to derive
-- which exceptions are processed by which handlers.
onAsyncException :: AsyncException -> a -> a
onAsyncException = const id

-- the primary loop
basicSireaAppLoop :: IORef Bool -> Stepper -> IO ()
basicSireaAppLoop rfContinue stepper = 
    runStepper stepper >>
    readIORef rfContinue >>= \ bContinue ->
    when bContinue (wait >>= \ _ -> loop)
    where wait = newEmptyMVar >>= \ mvWait  ->
                 addStepperEvent stepper (putMVar mvWait ()) >>
                 takeMVar mvWait 
          loop = basicSireaAppLoop rfContinue stepper

-- | bUnsafeExit - used with runSireaApp or beginSireaApp; effect is
-- killThread on the main thread when first activated, initiating a
-- graceful shutdown.
--
-- The behavior of bUnsafeExit is not precise and not composable. If
-- developers wish to model precise shutdown behavior, they should
-- use dynamic behaviors and explicitly switch to shutdown behavior,
-- which could then perform this exit.
--
bUnsafeExit :: B (S P0 ()) (S P0 ())
bUnsafeExit = unsafeOnUpdateBCX $ \ cw -> 
    getPCX0 cw >>= \ cp0 ->
    inExitR <$> findInPCX cp0 >>= \ rfKilled ->
    let kill = readIORef rfKilled >>= \ bBlooded ->
               unless bBlooded $ void $ 
                   writeIORef rfKilled True >>
                   -- note: fork to treat as async exception
                   -- respects mask; completes current step.
                   myThreadId >>= \ tidP0 ->
                   forkIO (killThread tidP0) 
    in
    let op _ = maybe (return ()) (const kill) in
    return op

newtype ExitR = ExitR { inExitR :: IORef Bool } deriving (Typeable)
instance Resource P0 ExitR where
    locateResource _ _ = liftM ExitR $ newIORef False


