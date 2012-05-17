
-- | "Build" the main behavior for embedding in an external loop.
--
-- This is a rather `raw` form, since developers must thread the PCX
-- argument directly. Sirea should eventually provide an alternative
-- interface to support type-driven programming, which will thread 
-- the PCX arguments implicitly. 
module Sirea.Build
    ( buildSireaB
    , runSireaApp
    , Main, MainB
    ) where

import Sirea.Internal.BTypes
import Sirea.Internal.LTypes
import Sirea.Internal.BCompile
import Sirea.Behavior
import Sirea.Partition

-- | The Main partition corresponds to whichever thread will be
-- responsible for the Stepper from buildSireaBehavior (usually
-- the main thread in Haskell).
type Main = ()

-- | The main behavior is what an application looks like in Sirea:
--
--     type MainB = B (S Main ()) (S Main ())
--
-- Such a behavior obviously is for its side-effects rather than for
-- its response. Indeed, the response is dropped. 
--
-- RDP is very composable. Even main behaviors can be composed:
--   * products - main behaviors represent independent agents active
--     in parallel. They interact through shared state and services. 
--   * sums or dynamic behavior can model staging, cycles, modality,
--     switching based on state - e.g. do X then Y then Z, loop.
--   * by publishing dynamic behaviors to a shared registry, apps
--     can also act as non-invasive `plugins` or `extensions`.
-- 
-- Consider use of blackboard metaphors for collaboration between
-- multiple agents, each represented as a Main Behavior.
--
-- If you need a quick and dirty way to integrate effects, e.g. for
-- simple unit tests or debugging, use LinkUnsafeIO.
--
type MainB = B (S Main ()) (S Main ())

-- | Build the "main" Sirea behavior, generating a Stepper for use
-- in a user-controlled event loop (or runSireaApp, if you don't
-- need an event loop). 
--
-- The `PCX Main` argument provides resources related to the event
-- loop - it must be the same PCX as passed to crossB. The best way
-- to achieve this is actually to skip 
--
-- 
Constructing the Main behavior
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
buildSireaB :: PCX Main -> MainB -> IO (Stepper, Stopper)
buildSireaB mcx mainB = 
    let (_, mkLn) = compileB mainB ldt_zero LnkDead in
    mkLn >>= \ lnk0 ->
    case lnk0 of 
        LnkDead -> return (zeroStepper, zeroStopper)
        (LnkSig lu) -> buildSireaBLU lu

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

-- hmm. At the moment this is a bit ugly, mostly with regards to
-- integrating `bcross` into Main. Some resources must be shared
-- based on the type parameters - in particular, the thread's 
-- input queue. 

This is a bit difficult, I currently don't have any
-- way to obtain 
buildSireaBLU :: LnkUp () -> IO (Stepper, Stopper)
buildSireaBLU lu =
    

-- | If you don't need to control the main event loop, i.e. if the
-- entire application is written for Sirea, use runSireaApp. It is
-- just a convenient way to provide a simple loop. 
-- (It will eventually provide a few nice features like handling a
-- kill signal properly.)
--
-- Example usage::
--    mainB :: MainB
--    mainB = bvoid $ ...
--
--    main :: IO ()
--    main = buildSireaB >>= 
--           runSireaApp
-- 



