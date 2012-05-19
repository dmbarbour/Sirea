{-# LANGUAGE GADTs, EmptyDataDecls, Rank2Types #-}

-- | "Build" a Sirea behavior for embedding in an external loop.
module Sirea.Build
    ( buildSireaApp
    , runSireaApp
    , SireaApp
    ) where

import Sirea.Internal.BTypes
import Sirea.Internal.LTypes
import Sirea.Internal.BCompile
import Sirea.Behavior
import Sirea.Partition
import Sirea.PCX
import Sirea.BCX


-- | This is what an RDP application looks like in Sirea:
--
--     type SireaApp = BCX (S P0 ()) (S P0 ())
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
type SireaApp = forall w . B w (S P0 ()) (S P0 ())

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
buildSireaApp b = 
    newPCX >>= \ appCX -> 
    let dt0 = LnkDUnit ldt_zero in
    let (_, mkLn) = compileB b dt0 LnkDead in
    mkLn >>= \ lnk0 ->
    case lnk0 of 
        LnkDead -> return (zeroStepper, zeroStopper)
        (LnkSig lu) -> buildSireaBLU appCX lu

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

buildSireaBLU :: PCX w -> LnkUp () -> IO (Stepper, Stopper)
buildSireaBLU mcx lu = undefined
    

-- | If you don't need to run the stepper yourself, consider use of
-- runSireaApp. It will build the application and run it until the
-- thread receives a kill signal, at which point it will gracefully
-- shut down (unless killed again).
runSireaApp :: SireaApp -> IO ()
runSireaApp b = buildSireaApp b >>= basicSireaAppLoop

basicSireaAppLoop :: (Stepper, Stopper) -> IO ()
basicSireaAppLoop = undefined
    




