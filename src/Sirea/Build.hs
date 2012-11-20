{-# LANGUAGE GADTs, Rank2Types #-}

-- | Build a Sirea application for embedding in an external loop.
--
-- A Sirea application, SireaApp, is a behavior with trivial inputs
-- and outputs, but that will observe and influence its environment
-- through sensors, actuators, and shared state. Applications react
-- continuously to their environment, and may record information to
-- adjust future behavior. Each SireaApp is essentially an agent.
--
-- Build will compile an application into a (Stepper,Stopper) pair.
-- Those types are defined in Sirea.Partition. Stepper allows Sirea 
-- to be used with existing event loops, and Stopper allows halting
-- the application. If there is no need to hook Sirea into existing
-- event loops, use `runSireaApp`, which takes over calling thread.
--
-- The first runStepper operation activates the application, setting
-- the input signal (S P0 ()) to active at current wall-clock time.
-- Stability is several seconds, increased incrementally by later 
-- step operations. Stopper respects stability, so it takes seconds
-- to halt a SireaApp in that manner. 
--
module Sirea.Build
    ( buildSireaApp
    , runSireaApp
    , beginSireaApp
    , SireaApp
    , SireaAppObject(..)
    , bUnsafeExit
    , bStartTime
    ) where

import Prelude hiding (catch)
import Data.IORef
import Data.Typeable
import Control.Concurrent.MVar
import Control.Monad (unless, when, void, liftM)
import Control.Exception (catch, assert, AsyncException)
import Control.Concurrent (myThreadId, forkIO, killThread, threadDelay)
import Sirea.Internal.BTypes
import Sirea.Internal.LTypes
import Sirea.Internal.BCompile(compileB)
import Sirea.Internal.PTypes
import Sirea.Internal.BCross
import Sirea.Internal.Thread
import Sirea.Internal.PulseSensor (runPulseActions)
import Sirea.Behavior
import Sirea.Partition
import Sirea.Link
import Sirea.UnsafeOnUpdate
import Sirea.PCX
import Sirea.BCX
import Sirea.Time
import Sirea.Signal

-- | This is what an RDP application looks like in Sirea:
--
--     type SireaApp = forall w . BCX w (S P0 ()) (S P0 ())
--
-- Such a behavior is intended for side-effects. Internally, signals
-- orchestrate real-world resources: sensors, actuators, databases,
-- and user interfaces. Interaction with an application must occur
-- via shared resources. The BCX type provides a means to distribute 
-- shared resources (or proxies to them) through the app.
--
-- RDP applications are essentially agents. Agents can be composed
-- in parallel (&&&) to model multi-agent systems operating in a 
-- shared resource environment. Agents can be composed by switching
-- (+++ or bdynamic) to model modal behavior and process control. In
-- both cases, the agents can communicate and collaborate via shared
-- state, especially the blackboard metaphor. By adding resources to
-- the environment, agents can provide configuration and extension.
--
-- In most cases, you'll want just one SireaApp for the process, and
-- use composition to model concurrent or modal applications. For 
-- precise shutdown behavior, consider modeling a shutdown mode.
--
type SireaApp = forall w . BCX w (S P0 ()) (S P0 ())

unwrapSireaApp :: SireaApp -> BCX w (S P0 ()) (S P0 ())
unwrapSireaApp app = app

-- | SireaAppObject manages life cycle of an initialized SireaApp:
-- 
--     Stepper - for user-controlled event loops
--     Stopper - to halt the application gracefully
--     PCX P0  - integrate resources controlled by main thread (*) 
--
-- These types are defined in the Partition and PCX modules. The P0
-- partition is thus similar to other partitions, excepting Stopper
-- is provided to halt the whole app. Stopper is not instantaneous;
-- continue to runStepper until Stopper event is called.
--
-- If runStopper before runStepper, the app will activate for the
-- minimum amount of time that Sirea supports, currently a quarter
-- of a second. I'll preserve this behavior, as it seems useful for
-- unit-testing.
--
-- (*) It is not recommended to actually use sireaContext, since any
-- explicit use is difficult to reproduce in another application. P0
-- can have resources, but it's preferable that they're provided by 
-- typeclass and BCX types and don't require explicit integration.
data SireaAppObject = SireaAppObject 
    { sireaStepper :: Stepper
    , sireaStopper :: Stopper
    , sireaContext :: PCX P0
    }

-- AppPeriodic data is used internally on the man clock step.
data AppPeriodic w = AppPeriodic 
    { ap_cw     :: !(PCX w)
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
buildSireaApp :: SireaApp -> IO SireaAppObject
buildSireaApp app = 
    -- new generic context; fresh global space for the app
    newPCX >>= \ cw -> 
    -- special initialization for P0 thread
    let cp0 = findInPCX cw :: PCX P0 in
    let tc0 = findInPCX cp0 :: TC in
    writeIORef (tc_init tc0) True >>
    -- compute behavior in the new context
    -- add a phase delay for consistency with bcross updates
    let bcx = (wrapBCX phaseDelayB) >>> (unwrapSireaApp app) in
    let b   = unwrapBCX bcx cw in
    -- compile behavior, dropping response
    let dt0 = LnkDUnit ldt_zero in
    let (_, mkLn) = compileB b dt0 LnkDead in
    mkLn >>= \ lnk0 ->
    case lnk0 of 
        LnkDead     -> return $ zeroObject cw
        (LnkSig lu) -> buildSireaBLU cw lu

-- zeroStepper and zeroStopper are used if the dead-code
-- elimination happens to kill the whole application.
zeroStepper :: Stepper
zeroStepper = Stepper 
    { runStepper = return ()
    , addStepperEvent = const $ return ()
    }
zeroStopper :: Stopper
zeroStopper = Stopper
    { runStopper = return ()
    , addStopperEvent = id -- run stopper event immediately
    } 
zeroObject :: PCX w -> SireaAppObject
zeroObject cw = 
    SireaAppObject { sireaStepper = zeroStepper
                   , sireaStopper = zeroStopper
                   , sireaContext = findInPCX cw
                   }
  
-- Build from a LnkUp, meaning there is something listening to the
-- signal. This doesn't actually initialize the signal, but does set
-- the app to kickstart on the first runStepper operation.
buildSireaBLU :: PCX w -> LnkUp () -> IO SireaAppObject
buildSireaBLU cw lu =
    newIORef emptyStopData >>= \ rfSD ->
    let cp0     = findInPCX cw :: PCX P0 in
    let tc0     = findInPCX cp0 in
    addTCRecv tc0 (beginApp cw rfSD lu) >> -- add kickstart
    let stepper = tcToStepper tc0 in
    let stopper = makeStopper rfSD in
    return $ SireaAppObject { sireaStepper = stepper
                            , sireaStopper = stopper
                            , sireaContext = findInPCX cw }
          
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
-- This could be useful for unit testing or one-off apps.
beginApp :: PCX w -> IORef StopData -> LnkUp () -> IO ()
beginApp cw rfSD lu = 
    let gs  = findInPCX cw in
    let cp0 = findInPCX cw :: PCX P0 in
    let tc0 = findInPCX cp0 in
    let pulse = runPulseActions cp0 in
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
    let su = SigUp { su_state = Just (s_always (), tStart)
                   , su_stable = Just tStable } in
    let nextStep = maintainApp apw tStable in
    tStart `seq`
    setStartTime cp0 tStart >>
    ln_update lu su >> -- activation!
    pulse >> -- first heartbeat
    schedule dtStep (addTCRecv tc0 nextStep)

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
maintainApp :: AppPeriodic w -> T -> IO ()
maintainApp apw tStable =
    ap_pulse apw >> -- heartbeat
    readIORef (ap_sd apw) >>= \ sd ->
    let tc0 = ap_tc0 apw in
    if shouldStop sd 
      then -- SHUTDOWN APPLICATION
           let su = SigUp { su_state = Just (s_never, tStable)
                          , su_stable = Nothing } in
           let tFinal = tStable `addTime` dtGrace in
           let nextStep = haltingApp apw tFinal in
           ln_update (ap_luMain apw) su >> -- indicate signal inactive in future.
           schedule dtStep (addTCRecv tc0 nextStep)
      else getTCTime tc0 >>= \ tNow -> 
           if (tNow > (tStable `addTime` dtRestart))
                then -- RESTART APPLICATION (halt and restore activity)
                    let tRestart = tNow `addTime` dtStep in
                    let tStable' = tNow `addTime` dtStability in
                    let sig = s_switch s_never tRestart (s_always ()) in
                    let su = SigUp { su_state = Just (sig, tStable)
                                   , su_stable = Just tStable' } in
                    let nextStep = maintainApp apw tStable' in
                    tRestart `seq` 
                    setStartTime (findInPCX (ap_cw apw)) tRestart >>
                    ln_update (ap_luMain apw) su >>
                    schedule dtStep (addTCRecv tc0 nextStep)
                else -- NORMAL MAINTENANCE 
                    let tStable' = tNow `addTime` dtStability in
                    assert (tStable' > tStable) $ -- can't handle clock running backwards
                    let su = SigUp { su_state = Nothing, su_stable = Just tStable' } in
                    let nextStep = maintainApp apw tStable' in
                    ln_update (ap_luMain apw) su >>
                    schedule dtStep (addTCRecv tc0 nextStep)

-- After we set the main signal to inactive, we must still wait for
-- real-time to catch up, and should run a final few heartbeats to
-- provide any pulse actions.
haltingApp :: AppPeriodic w -> T -> IO ()
haltingApp apw tFinal = 
    ap_pulse apw >> -- heartbeat
    let tc0 = ap_tc0 apw in
    getTCTime tc0 >>= \ tNow ->
    if (tNow > tFinal) -- wait for real time to catch up to stability
        then let onStop = ap_pulse apw >> finiStopData (ap_sd apw) in
             let gs = ap_gs apw in
             runGobStopper gs (addTCRecv tc0 onStop) >>
             let nextStep = finalizingApp apw in
             schedule dtStep (addTCRecv tc0 nextStep) 
        else let nextStep = haltingApp apw tFinal in
             schedule dtStep (addTCRecv tc0 nextStep)

-- at this point we've run the all-stop for all other threads,
-- so we're just biding time until threads report completion.
-- There might not be any more heartbeats at this point.
finalizingApp :: AppPeriodic w -> IO ()
finalizingApp apw =
    ap_pulse apw >> -- heartbeat (potentially last)
    readIORef (ap_sd apw) >>= \ sd ->
    unless (isStopped sd) $
        let nextStep = finalizingApp apw in
        let tc0 = ap_tc0 apw in
        schedule dtStep (addTCRecv tc0 nextStep)
    

-- Tuning parameters for the stability maintenance tasks:
--
--   dtRestart   : how long frozen to cause restart
--   dtStability : how far ahead to stabilize
--   dtStep      : period between stability updates
--   dtGrace     : grace period at startup and shutdown
--
-- dtStep can also influence amount of computation per round due to
-- `btouch` and similar structures that compute based on stability.
-- These values also can have an impact on idling overheads.
--
dtRestart, dtStability, dtStep, dtGrace :: DT
dtRestart   = 1.20  -- how long a pause to cause a restart
dtStability = 0.30  -- stability of main signal (how long to halt)
dtStep      = 0.06  -- periodic event to increase stability
dtGrace     = dtStep -- time allotted for graceful start and stop

-- | If you don't need to run the stepper yourself (that is, if you
-- don't need full control of the main thread event loop), consider 
-- use of runSireaApp. 
--
-- This will run the application until killThread or AsyncException,
-- such as ctrl+c interrupt. At that point it will try to shutdown 
-- gracefully. Use of `bUnsafeExit` causes an async killThread from
-- within the application behavior.
--
--    runSireaApp app = buildSireaApp app >>= beginSireaApp
--
runSireaApp :: SireaApp -> IO ()
runSireaApp app = buildSireaApp app >>= beginSireaApp

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
bUnsafeExit :: BCX w (S P0 ()) (S P0 ())
bUnsafeExit = unsafeOnUpdateBCX $ \ cw -> 
    let cp0 = findInPCX cw :: PCX P0 in
    let rfKilled = inExitR $ findInPCX cp0 in
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

newtype ExitR = ExitR { inExitR :: IORef Bool }
instance Typeable ExitR where
    typeOf _ = mkTyConApp tycExitR []
        where tycExitR = mkTyCon3 "sirea-core" "Sirea.Build.Internal" "ExitR"
instance Resource ExitR where
    locateResource _ = liftM ExitR $ newIORef False

-- | bStartTime - obtain logical start time of current contiguous
-- period of activity. Usually, this corresponds to the application
-- start time, but if any long pause is detected in the main loop,
-- Sirea will "restart" the application by setting the main signal
-- to inactive for a period of time. In that case, the start time 
-- will change.
--
-- Use cases for start time: naming log files, special handling of 
-- continuity-sensitive resources (e.g. volatile state).
--
-- Bad Idea: scheduling relative to application startup. Just don't.
-- Always favor a design that is robust to pausing or restarting the
-- application, which means scheduling based on state or Clock.
--
bStartTime :: BCX w (S P0 ()) (S P0 T)
bStartTime = unsafeLinkBCX $ \ cw -> 
    let cp0 = findInPCX cw :: PCX P0 in
    let rfST = rfStartTime $ findInPCX cp0 in
    return . naivelyReportValue rfST

-- naivelyReportValue will simply report the value held by an
-- IORef whenever a signal update is received. This technique
-- is unsafe in most cases, but is okay for tStart/tStop due to
-- the guaranteed update (brief period of inactivity whenever
-- tStart changes). TODO: make this more precise, perhaps by
-- keeping a history of restart times. 
naivelyReportValue :: IORef v ->  Lnk (S p v) -> Lnk (S p ())
naivelyReportValue _ LnkDead = LnkDead
naivelyReportValue rf (LnkSig lu) = (LnkSig lu')
    where lu' = LnkUp { ln_touch = touch', ln_update = update' }
          touch' = ln_touch lu
          update' su = 
            readIORef rf >>= \ v ->
            let su' = su_fmap (s_const v) su in
            ln_update lu su'

newtype StartTime = StartTime { rfStartTime :: IORef T }
instance Typeable StartTime where
    typeOf _ = mkTyConApp tycStartTime []
        where tycStartTime = mkTyCon3 "sirea-core" "Sirea.Build.Internal" "StartTime"
instance Resource StartTime where
    locateResource _ = liftM StartTime trf
        where trf = newIORef t0
              t0  = timeFromDays 0

-- sets the start time for one SireaApp.
setStartTime :: PCX P0 -> T -> IO () 
setStartTime cp0 t = writeIORef (rfStartTime $ findInPCX cp0) t


