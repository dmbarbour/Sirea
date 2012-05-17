
-- | "Build" the main behavior for embedding in an external loop.
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
--
type MainB = B (S Main ()) (S Main ())


-- | Build the "main" Sirea behavior, generating a Stepper for use
-- in a user-controlled event loop. 
--
-- Nothing starts until the Stepper is first called, after which it
-- should be called regularly (waiting on events if necessary) until
-- the Haskell process is killed or the machine halts. (Crash-only
-- software design is recommended.) Or you can run the Stopper to 
-- cleanly halt the behavior (you'll need to keep running until the
-- Stopper event activates.)
--
-- To change behavior at runtime, or manage multiple behaviors at
-- runtime, create a dynamic behavior! Don't build and halt multiple
-- Sirea behaviors in one Haskell process.
-- 
buildSireaB :: MainB -> IO (Stepper, Stopper)
buildSireaB mainB = 
    let (_, mkLn) = compileB mainB ldt_zero LnkDead in
    mkLn >>= \ lnk0 ->
    case lnk0 of 
        LnkDead -> return (
    buildSireaB' (ln


-- | If you don't need to control the main event loop, i.e. if the
-- entire application is written for Sirea, use runSireaApplication.
-- (It will eventually provide a few nice features like handling a
-- kill signal properly.)
--
-- Runs forever. Meant to be used as:
--    main :: IO ()
--    main = runSireaApp $ bvoid $ 
--      -- build the behavior here.
-- 
runSireaApp :: MainB -> IO ()
runSireaApp mainB = undefined mainB


