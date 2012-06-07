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
-- step operations. Stopper respects stability, so it takes several
-- seconds to halt a SireaApp in that manner. For precise shutdown,
-- use state and dynamic behavior to model halted, paused, disabled
-- behavior for the application as a whole.
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
    , bUnsafeKillP0
    ) where

import Data.IORef
import Control.Concurrent.MVar
import Control.Monad (unless, when, void)
import Control.Exception (finally)
import Control.Concurrent (myThreadId, forkIO, killThread)
import Sirea.Internal.BTypes
import Sirea.Internal.LTypes
import Sirea.Internal.BCompile(compileB)
import Sirea.Internal.BCross
import Sirea.Behavior
import Sirea.Partition
import Sirea.LinkUnsafeIO
import Sirea.PCX
import Sirea.BCX
import Sirea.Time

-- TUNING:
--   dt_app_stability : how far ahead to stabilize (e.g. 5s)
--   dt_app_step      : periodic increase in stability (e.g. 1s)
--   dt_app_border    : assumed startup time (e.g. 50 ms)
--
-- Stabilizing 5s ahead means that some values are computed that
-- far ahead; it also means at least 5s to shut down.
dt_app_stability, dt_app_step, dt_app_border :: DT
dt_app_stability = 3.0 -- stability of main signal
dt_app_step = 0.5 -- periodic event to increase stability
dt_app_border = 0.05 -- latency for startup, shutdown

-- | This is what an RDP application looks like in Sirea:
--
--     type SireaApp = forall w . BCX w (S P0 ()) (S P0 ())
--
-- Such a behavior is intended for side-effects. Internally, signals
-- orchestrate real-world resources: sensors, actuators, databases,
-- and user interfaces. Interaction with an application must occur
-- via shared resources. 
--
-- RDP applications are essentially agents. Composition in parallel
-- models multi-agent systems operating on a shared environment. Or
-- compose with choice ((:|:) or dynamic behaviors) to model modal
-- switching between behaviors, e.g. paused vs. active. Agents can
-- collaborate with shared state and blackboard metaphors. They can
-- represent plugins or extensions by publishing dynamic behaviors
-- to shared locations.
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
-- is provided and halts the whole app. 
data SireaObject = SireaObject 
    { sireaStepper :: Stepper
    , sireaStopper :: Stopper
    , sireaContext :: PCX P0
    }

-- | Build the SireaApp behavior, generating a Stepper for use in a 
-- user-controlled event loop, and a Stopper to gracefully halt the 
-- application. Nothing should be started after build, not until the
-- runStepper operation is called.
--
buildSireaApp :: SireaApp -> IO SireaObject
buildSireaApp app = 
    -- new generic context; fresh global space for the app
    newPCX >>= \ cw -> 
    -- indicate the initial thread already exists
    let tc0 = (findInPCX cw :: TC P0) in
    writeIORef (tc_init tc0) True >> 
    -- build behavior using context
    let bcx = unwrapSireaApp app in
    let b   = unwrapBCX bcx cw in
    let dt0 = LnkDUnit ldt_zero in
    -- dropping the response signal
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

-- Build from a LinkUp, meaning there is something listening to the
-- signal. What Sirea will do is set a signal to activate the LnkUp,
-- then periodically increase the stability of that signal. 
buildSireaBLU :: PCX w -> LnkUp () -> IO SireaObject
buildSireaBLU cw lu =
    newIORef False >>= \ rfStop -> 
    let tc0 = getTC0 cw in
    -- TODO: prepare the stopper
    --       prepare the periodic stability increase
    undefined
    {- addTCRecv tc0 -}
    
getTC0 :: PCX w -> TC P0
getTC0 = findInPCX



-- | If you don't need to run the stepper yourself, consider use of
-- runSireaApp. This will simply run the application until the main
-- thread receives a killThread signal, at which point it will try
-- to shutdown gracefully. The signal can be delivered internally by
-- use of unsafeKillP0. 
runSireaApp :: SireaApp -> IO ()
runSireaApp app = buildSireaApp app >>= beginSireaApp

-- | beginSireaApp activates a forever loop to process the SireaApp.
-- It is stopped by killing the main thread. Used by runSireaApp.
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
    when bContinue $ 
        newEmptyMVar >>= \ mvWait  ->
        addStepperEvent stepper (putMVar mvWait ()) >>
        takeMVar mvWait >>
        runStepper stepper >>= \ _ ->
        basicSireaAppLoop rfContinue stepper 
    

-- | bUnsafeKillP0 - use with runSireaApp. The first time the signal
-- stabilizes as active, unsafeKillP0 causes asynchronous killThread
-- on the main (P0) thread. For runSireaApp, this causes a graceful 
-- shutdown, but unsafeKillP0 should be unique in the app.
--
-- Note: even after kill, it may take a couple seconds to shut down.
-- If that is too long, consider modeling shutdown properly as stage
-- of RDP application - i.e. dynamic switch to a halt behavior. Then 
-- bUnsafeKillP0 would still take a few seconds, but during inactive 
-- period with few outward effects.
--
bUnsafeKillP0 :: BCX w (S P0 ()) (S P0 ())
bUnsafeKillP0 = unsafeOnUpdateBCX $ \ _ ->
    newIORef False >>= \ rfKilled ->
    let kill = readIORef rfKilled >>= \ bBlooded ->
               unless bBlooded $ void $ 
                   writeIORef rfKilled True >>
                   myThreadId >>= \ tidP0 ->
                   forkIO (killThread tidP0)
    in
    -- operation ignores T and pr
    let op _ = maybe (return ()) (const kill) in
    return op



