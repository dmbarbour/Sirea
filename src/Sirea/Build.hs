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
-- to halt a SireaApp in that manner. For precise shutdown use state 
-- and dynamic behavior to model halted, paused, disabled behavior 
-- for the application as a whole.
--
-- It is recommended that Sirea applications be designed resilient
-- against crash, power loss, or abrupt killing of the whole Haskell 
-- process. This requires attention to use of state.
--
module Sirea.Build
    ( buildSireaApp
    , runSireaApp
    , beginSireaApp
    , SireaApp
    , bUnsafeExit
    ) where

import Data.IORef
import Data.Typeable
import Control.Concurrent.MVar
import Control.Monad (unless, when, void, liftM)
import Control.Exception (finally, assert)
import Control.Concurrent (myThreadId, forkIO, killThread, threadDelay)
import Sirea.Internal.BTypes
import Sirea.Internal.LTypes
import Sirea.Internal.BCompile(compileB)
import Sirea.Internal.PTypes 
import Sirea.Internal.BCross
import Sirea.Internal.Thread
import Sirea.Behavior
import Sirea.Partition
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
-- via shared resources. 
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

-- | SireaObject is the result of compiling a SireaApp. It provides:
-- 
--     Stepper - for user-controlled event loops
--     Stopper - to halt the application gracefully
--     PCX P0  - to integrate main partition resources 
--
-- These types are defined in the Partition and PCX modules. The P0
-- partition is thus similar to other partitions, excepting Stopper
-- is provided to halt the whole app. Stopper is not instantaneous;
-- continue to runStepper until Stopper event is called.
-- 
data SireaObject = SireaObject 
    { sireaStepper :: Stepper
    , sireaStopper :: Stopper
    , sireaContext :: PCX P0
    }

-- | Build the SireaApp behavior, generating a SireaObject. Access
-- to the Stepper allows the user to embed the SireaApp in another
-- event loop, and access to Context supports integrating resources
-- controlled by the main event loop. 
--
-- If you don't already have a main event loop, I suggest using the
-- runSireaApp function and shifting resources to dedicated threads.
-- (Explicitly hooking up resources detracts from the declarative 
-- programming experience, and is not readily extensible.)
--
buildSireaApp :: SireaApp -> IO SireaObject
buildSireaApp app = 
    -- new generic context; fresh global space for the app
    newPCX >>= \ cw -> 
    -- indicate the initial thread already exists
    let cp0 = findInPCX cw :: PCX P0 in
    let tc0 = findInPCX cp0 :: TC in
    writeIORef (tc_init tc0) True >> 
    -- compute behavior in the new context
    let bcx = unwrapSireaApp app in
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
zeroObject :: PCX w -> SireaObject
zeroObject cw = 
    SireaObject { sireaStepper = zeroStepper
                , sireaStopper = zeroStopper
                , sireaContext = findInPCX cw
                }

-- Build from a LnkUp, meaning there is something listening to the
-- signal. This doesn't actually initialize the signal, but does set
-- the app to kickstart on the first runStepper operation.
buildSireaBLU :: PCX w -> LnkUp () -> IO SireaObject
buildSireaBLU cw lu =
    newIORef emptyStopData >>= \ rfSD ->
    let gs      = findInPCX cw in
    let cp0     = findInPCX cw :: PCX P0 in
    let tc0     = findInPCX cp0 in
    addTCRecv tc0 (beginApp tc0 gs rfSD lu) >> -- add kickstart
    let stepper = tcToStepper tc0 in
    let stopper = makeStopper rfSD in
    return $ SireaObject { sireaStepper = stepper
                         , sireaStopper = stopper
                         , sireaContext = findInPCX cw }
          
-- task to initialize application (performed on first runStepper)
-- a slight delay is introduced before everything really starts.
beginApp :: TC -> GobStopper -> IORef StopData -> LnkUp () -> IO ()
beginApp tc0 gs rfSD lu =
    readIORef rfSD >>= \ sd ->
    if shouldStop sd 
      then shutdownEvent tc0 gs rfSD
      else getTime >>= \ tNow ->
           let tStart = tNow `addTime` dtStep in
           let tStable = tNow `addTime` dtStability in
           let su = SigUp { su_state = Just (s_always (), tStart)
                          , su_stable = Just tStable } in
           let nextStep = maintainApp tc0 gs rfSD lu tStable in
           ln_update lu su >> -- activation!
           schedule dtStep (addTCRecv tc0 nextStep)

-- schedule will delay an event then perform it in another thread.
-- Sirea only does this with one thread at a time.
schedule :: DT -> IO () -> IO ()
schedule dt op = assert (usec > 0) $ void $ 
    forkIO (threadDelay usec >> op)
    where usec = fromInteger $ (999 + dtToNanos dt) `div` 1000

-- regular maintenance operation, simply increases stability of the
-- active signal on a regular basis; performed within main thread.
maintainApp :: TC -> GobStopper -> IORef StopData -> LnkUp () -> T -> IO ()
maintainApp tc0 gs rfSD lu tStable =
    readIORef rfSD >>= \ sd ->
    if shouldStop sd 
      then let su = SigUp { su_state = Just (s_never, tStable)
                          , su_stable = Nothing } in
           ln_update lu su >> -- indicate signal inactive in future.
           schedule dtStability (addTCRecv tc0 (shutdownEvent tc0 gs rfSD))
      else getTime >>= \ tNow ->
           let tStable' = tNow `addTime` dtStability in
           let su = SigUp { su_state = Nothing, su_stable = Just tStable' } in
           let nextStep = maintainApp tc0 gs rfSD lu tStable' in
           ln_update lu su >>
           schedule dtStep (addTCRecv tc0 nextStep)

-- this is the last phase of shutdown - actually stopping threads
-- and eventually calling the Stopper event for the main thread.
shutdownEvent :: TC -> GobStopper -> IORef StopData -> IO ()
shutdownEvent tc0 gs rfSD = runGobStopper gs finiStop
    where finiStop = addTCRecv tc0 (finiStopData rfSD)
    

-- Tuning parameters for the stability maintenance tasks:
--
--   dtStability : how far ahead to stabilize
--   dtStep      : period between stability updates
--
-- dtStep can also influence amount of computation per round.
dtStability, dtStep :: DT
dtStability = 0.32  -- stability of main signal
dtStep      = 0.04  -- periodic event to increase stability

-- | If you don't need to run the stepper yourself, consider use of
-- runSireaApp. This will simply run the application until the main
-- thread receives a killThread signal, at which point it will try
-- to shutdown gracefully. Use of `bUnsafeExit` will cause this 
-- signal.
--
--    runSireaApp app = buildSireaApp app >>= beginSireaApp
--
runSireaApp :: SireaApp -> IO ()
runSireaApp app = buildSireaApp app >>= beginSireaApp

-- | beginSireaApp activates a forever loop to process the SireaApp.
-- It is stopped by killing the thread. Once stopped, will continue 
-- to run up to a few seconds while everything halts.
beginSireaApp :: SireaObject -> IO ()
beginSireaApp so =
    let stepper = sireaStepper so in
    let stopper = sireaStopper so in
    newIORef True >>= \ rfContinue ->
    addStopperEvent stopper (writeIORef rfContinue False) >>
    let loop = basicSireaAppLoop rfContinue stepper in
    loop `finally` (runStopper stopper >> loop)

basicSireaAppLoop :: IORef Bool -> Stepper -> IO ()
basicSireaAppLoop rfContinue stepper = 
    readIORef rfContinue >>= \ bContinue ->
    when bContinue (wait >> run >>= \ _ -> loop)
    where wait = newEmptyMVar >>= \ mvWait  ->
                 addStepperEvent stepper (putMVar mvWait ()) >>
                 takeMVar mvWait 
          run  = runStepper stepper
          loop = basicSireaAppLoop rfContinue stepper 
    

-- | bUnsafeExit - use with runSireaApp, sends a killThread signal
-- once an active signal to bUnsafeExit stabilizes. Basically a way
-- to halt the application from within the application. Should not
-- be used if the client plans to halt the application externally.
--
-- bUnsafeExit does not stop the whole Haskell process, only the one
-- SireaApp in which it is used. bUnsafeExit doesn't cause immediate
-- halt; the application may continue to run for up to a few seconds
-- to halt gracefully.  
--
-- For more precise and composable shutdown behavior, model shutdown
-- explicitly using dynamic behaviors or asynchronous sums.
--
bUnsafeExit :: BCX w (S P0 ()) (S P0 ())
bUnsafeExit = unsafeOnUpdateBCX $ \ cw -> 
    let cp0 = findInPCX cw :: PCX P0 in
    let rfKilled = inExitR $ findInPCX cp0 in
    let kill = readIORef rfKilled >>= \ bBlooded ->
               unless bBlooded $ void $ 
                   writeIORef rfKilled True >>
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


