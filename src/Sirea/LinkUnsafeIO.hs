{-# LANGUAGE TypeOperators, GADTs #-}

-- | LinkUnsafeIO is a simple behavior that executes IO from a Link
-- as an RDP behavior stabilizes. The IO action will be executed for
-- every unique value observed in the signal, and executes once 
-- there are no more takebacks. The motivation for LinkUnsafeIO is 
-- easy support for debugging, HUnit tests - stuff that is intended 
-- for easy removal. The signal itself is passed along untouched 
-- after performing the IO.
--
-- In terms of RDP semantics, LinkUnsafeIO is unsafe because IO is
-- not spatially idempotent or commutative. The IO actions might be
-- duplicated if developers take advantage of RDP's semantics for
-- refactoring and abstraction. (The proper way to integrate IO will
-- leverage external services, e.g.  a blackboard metaphor to enable
-- RDP agents and IO agents to collaborate declaratively.)
--
-- In practice, developers can generally ensure that the unsafe IO 
-- won't be duplicated (uniqueness), or that it would be safe enough
-- to do so (idempotence), and that collectively such operations are 
-- insensitive to order (commutative, or close enough). So this can 
-- be used safely, with careful discipline. 
--
-- The IO is not used to load input back into the behavior; doing so
-- would interfere with anticipation. Also, the IO should be locally
-- stateless. 
--
module Sirea.LinkUnsafeIO 
    ( unsafeOnUpdateB
    , unsafeOnUpdateBL
    , unsafeOnUpdateBLN
    ) where

import Data.IORef
import Control.Applicative
import Sirea.Behavior
import Sirea.Link
import Sirea.Signal
import Sirea.Time
import Sirea.Internal.BImpl (undeadB, keepAliveB)

import Control.Exception (assert)

-- dtFinal provides the upper bound for how far to execute when 
-- stability increases to infinity, measured against the last 
-- update or the prior stability.
dtFinal :: DT
dtFinal = 3.0 -- seconds

-- | unsafeOnUpdateB - perform an IO action for every unique value
-- in a signal as it becomes stable, then forward the update (after
-- performing the IO). 
-- 
-- unsafeOnUpdateB qualifies as an effectful sink, i.e. it will keep
-- a behavior alive. This will interfere with dead code elimination,
-- and so may be unsuitable for debugging purposes.
unsafeOnUpdateB :: (Eq a) => (T -> Maybe a -> IO ()) -> B (S p a) (S p a)
unsafeOnUpdateB op = unsafeOnUpdateBL op >>> undeadB

-- | unsafeOnUpdateBL - a very lazy variation of unsafeOnUpdateB.
-- This variation allows dead-code elimination of the behavior when
-- the tapped signal is not used later in the pipeline.
--
-- Only suitable for signals you'll need for other reasons.
--
unsafeOnUpdateBL :: (Eq a) => (T -> Maybe a -> IO ()) -> B (S p a) (S p a)
unsafeOnUpdateBL op = unsafeLnkB blLnk
    where blLnk = MkLnk { ln_build = build
                        , ln_tsen = True
                        , ln_peek = 0     }
          build LnkDead = return LnkDead
          build (LnkSig lu) = 
            newIORef (s_never,Nothing) >>= \ rfSig ->
            newIORef Nothing >>= \ rfA ->
            let update su =
                    runToStability rfSig rfA op su >>
                    ln_update lu su                  
            in
            return (LnkSig $ lu { ln_update = update })

runToStability :: (Eq a) => IORef (Sig a, Maybe T) -> IORef (Maybe a) 
               -> (T -> Maybe a -> IO ()) -> SigUp a -> IO ()
runToStability rfSig rfA op su =
    -- update the stored signal so we don't miss any updates.
    readIORef rfSig >>= \ (s0,t0) -> 
    let tf  = su_stable su in
    let sf  = maybe s0 (\(s,t) -> s_switch s0 t s) (su_state su) in
    let sfc = maybe s_never (s_trim sf) tf in -- cleaned up signal.
    sfc `seq` writeIORef rfSig (sfc,tf) >>
    
    -- now for effects. For simplicity, I just keep an IORef that
    -- tracks the last operation performed. Only signal values in
    -- range of (t0 <= t < tf) may be executed, 
    let tUpd   = snd <$> su_state su in
    let tLower = t0 <|> tUpd in
    let tUpper = tf <|> ((`addTime` dtFinal) <$> (tUpd <|> t0)) in
    case (,) <$> tLower <*> tUpper of
        Nothing -> return ()
        Just (tL,tU) ->
            if (tL == tU) then return () else
            assert (tL < tU) $
            let lsigs = sigToList sf tL tU in
            let action (t,a) =
                    if (t == tU) then return () else
                    assert ((tL <= t) && (t < tU)) $
                    readIORef rfA >>= \ a0 ->
                    if (a == a0) then return () else
                    writeIORef rfA a >> 
                    op t a
            in 
            mapM_ action lsigs

-- | unsafeOnUpdateBLN - perform IO effects if any of many signals
-- are used later in the pipeline. A Goldilocks solution:
--      
--      unsafeOnUpdateB - too eager!
--      unsafeOnUpdateBL - too lazy!
--      unsafeOnUpdateBLN - just right.
--
-- This allows tapping a signal for debugging that would otherwise
-- be dropped by the behavior, but allows dead code elimination to
-- remove the behavior - based on whether nearby signals are dead.
--
-- In addition to the processed signal, BLN forwards an arbitrary 
-- complex signal without processing it (not even synchronizing it)
-- but will check to see whether it's all dead code. If not, the 
-- IO effect is activated.
unsafeOnUpdateBLN :: (Eq a) => (T -> Maybe a -> IO ()) 
                    -> B (S p a :&: x) (S p a :&: x)
unsafeOnUpdateBLN op = bfirst (unsafeOnUpdateBL op) >>> keepAliveB


