{-# LANGUAGE GADTs, EmptyDataDecls #-}

-- | "Build" a Sirea behavior for embedding in an external loop.
module Sirea.Build
    ( buildSireaApp
    , SireaApp
    ) where

import Sirea.Internal.BTypes
import Sirea.Internal.LTypes
import Sirea.Internal.BCompile
import Sirea.Behavior
import Sirea.Partition
import Sirea.PCX
import Data.Typeable


-- | This is what an RDP application looks like in Sirea:
--
--     type SireaApp = BCX (S P0 ()) (S P0 ())
--
-- Such a behavior is obviously intended for side-effects, since the
-- response signal is useless. The use of `BCX` allows the behavior
-- to be parameterized by a partition context (PCX), which acts as a
-- fresh global space for application state and resources.
--
-- RDP is very composable. Even SireaApp behaviors are composable:
--   * products - main behaviors represent independent agents active
--     in parallel. They interact through shared state and services. 
--   * sums or dynamic behavior can model staging, cycles, modality,
--     switching based on state - e.g. do X then Y then Z, loop.
--   * by publishing dynamic behaviors to a shared registry, apps
--     can also act as non-invasive `plugins` or `extensions`.
--
-- Shared state and blackboard architecture supports collaboration
-- between independent agents in RDP.
--
type SireaApp = B (S P0 ()) (S P0 ())

-- | Build the "main" Sirea behavior, generating a Stepper for use
-- in a user-controlled event loop. 
--
-- This requires some resources from the root PCX (the one just
-- above the partitions). 
--
-- requires a few resources that might have been used elsewhere in
-- the behavior (in particular, anything that crosses back into the
-- main partition). Nothing starts until the Stepper is called, so
-- Sirea should not interfere with other intializations
-- should be called regularly (waiting on events if necessary) until
-- the Haskell process is killed or the machine halts. (Crash-only
-- software design is recommended.) 
--
-- There 
--
-- 
--
-- To change behavior at runtime, or manage multiple behaviors at
-- runtime, create a dynamic behavior! Don't build and halt multiple
-- Sirea behaviors in one Haskell process.
-- 
buildSireaApp :: PCX () -> SireaApp -> IO (Stepper, Stopper)
buildSireaApp mcx b = 
    let dt0 = LnkDUnit ldt_zero in
    let (_, mkLn) = compileB b dt0 LnkDead in
    mkLn >>= \ lnk0 ->
    case lnk0 of 
        LnkDead -> return (zeroStepper, zeroStopper)
        (LnkSig lu) -> buildSireaBLU mcx lu

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

buildSireaBLU :: PCX () -> LnkUp () -> IO (Stepper, Stopper)
buildSireaBLU mcx lu = undefined
    



