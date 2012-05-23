{-# LANGUAGE GADTs, EmptyDataDecls, Rank2Types #-}

-- | "Build" a Sirea behavior for embedding in an external loop.
module Sirea.Build
    ( buildSireaApp
    , SireaApp
    , runSireaApp
    , unsafeKillP0
    ) where

import Data.IORef
import Control.Concurrent.MVar
import Control.Exception (finally, mask_)
import Control.Concurrent (myThreadId, forkIO, killThread)
import Sirea.Internal.BTypes
import Sirea.Internal.LTypes
import Sirea.Internal.BCompile(compileB)
import Sirea.Internal.BCross(loadP0)
import Sirea.Behavior
import Sirea.Partition
import Sirea.LinkUnsafeIO
import Sirea.PCX
import Sirea.BCX


-- | This is what an RDP application looks like in Sirea:
--
--     type SireaApp = forall w . BCX w (S P0 ()) (S P0 ())
--
-- Such a behavior is intended for side-effects. The response signal
-- is not used. Effects are achieved by signals to partitions that
-- integrate with real-world resources - user interfaces, sensors,
-- actuators, and databases. BCX effectively provides a global space
-- per SireaApp to maintain local proxies and adapters to arbitrary
-- resources.
--
-- Even though the response signal is useless, Sirea behaviors are
-- still very composable. Compose in parallel with (&&&). Use choice
-- ((:|:) or dynamic behaviors) to model switching and modes based
-- on a stateful element. Use shared state and blackboard metaphor
-- for collaboration. Publish dynamic behaviors to common registries
-- to represent plugins, extensions, and icon-like applications. 
--
-- Most Sirea clients should just create one, big SireaApp, perhaps
-- leveraging dynamic behaviors, rather than a bunch of small ones.
--
type SireaApp = forall w . BCX w (S P0 ()) (S P0 ())

unwrapSireaApp :: SireaApp -> BCX w (S P0 ()) (S P0 ())
unwrapSireaApp app = app

-- | Build the "main" Sirea behavior, generating a Stepper for use
-- in a user-controlled event loop. The application receives a fresh
-- PCX partition. 
--
-- After construction, nothing happens immediately. The first call
-- to runStepper will actually start the behavior. After you begin
-- to runStepper, you should continue running it (periodically or on
-- the stepper event) until the Stopper event is received. Halting
-- the behavior might take a couple seconds during which runStopper
-- must still be executed.
--
-- NOTE: For best performance, model shutdown within Sirea behavior,
-- i.e. use state to set shutdown, and dynamic behavior switches to 
-- passive behavior. Result is better anticipation and more control
-- over logical shutdown time. 
--
buildSireaApp :: SireaApp -> IO (Stepper, Stopper)
buildSireaApp app = 
    -- new generic context; fresh global space for the app
    newPCX >>= \ cx -> 
    -- indicate the initial thread already exists
    let tc0 = (findInPCX pcx :: TC P0) in
    writeIORef (tc_init tc0) True >> 
    -- build behavior using context; response signal drop
    let bcx = unwrapSireaApp app in
    let b   = unwrapBCX bcx cx in
    let dt0 = LnkDUnit ldt_zero in
    let (_, mkLn) = compileB b dt0 LnkDead in
    mkLn >>= \ lnk0 ->
    -- prepare the stepper and stopper
    case lnk0 of 
        LnkDead -> return (zeroStepper, zeroStopper)
        (LnkSig lu) -> buildSireaBLU cx lu

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

-- Build from a LinkUp, meaning there is something listening to the
-- signal. What Sirea will do is set a signal to activate the LnkUp,
-- then periodically increase the stability of that signal. 
--
buildSireaBLU :: PCX w -> LnkUp () -> IO (Stepper, Stopper)
buildSireaBLU pcx lu = undefined

-- note: mask_ the runStepper operation.


-- | If you don't need to run the stepper yourself, consider use of
-- runSireaApp. This will simply run the application until the main
-- thread receives a killThread signal, at which point it will try
-- to shutdown gracefully. The signal can be delivered internally by
-- use of unsafeKillP0. 
runSireaApp :: SireaApp -> IO ()
runSireaApp app = buildSireaApp app >>= beginSireaApp

beginSireaApp :: (Stepper, Stopper) -> IO ()
beginSireaApp (stepper,stopper) = 
    newIORef True >>= \ rfContinue ->
    addStopperEvent stopper (writeIORef rfContinue False) >>
    let loop = basicSireaAppLoop rfContinue stepper in
    loop `finally` (runStopper stopper >> loop)

basicSireaAppLoop :: IORef Bool -> Stepper -> IO ()
basicSireaAppLoop rfContinue stepper = 
    readIORef rfContinue >>= \ bContinue ->
    if (not bContinue) then return () else
    newEmptyMVar >>= \ mvWait  ->
    addStepperEvent stepper (putMVar mvWait ()) >>
    takeMVar mvWait >>
    runStepper stepper >>
    basicSireaAppLoop rfContinue stepper 
    

-- | unsafeKillP0 - use with runSireaApp. The first time the signal 
-- stabilizes as active, unsafeKillP0 causes asynchronous killThread
-- on the main (P0) thread. For runSireaApp, this causes a graceful 
-- shutdown, but unsafeKillP0 should be unique in the app.
--
-- Note: even after kill, it may take a couple seconds to shut down.
-- If that is too long, consider modeling shutdown properly as stage
-- of RDP application - i.e. dynamic switching to shutdown behavior.
--
unsafeKillP0 :: BCX w (S P0 ()) (S P0 ())
unsafeKillP0 = (wrapBCX . const) $ unsafeOnUpdateB $
    newIORef False >>= \ rfKilled ->
    let kill = readIORef rfKilled >>= \ bBlooded ->
               if bBlooded then return () else
               writeIORef rfKilled True >>
               myThreadId >>= \ tidP0 ->
               forkIO (killThread tidP0) >>
               return () 
    in
    let op _ = maybe (return ()) (const kill) in
    return op



