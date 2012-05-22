{-# LANGUAGE TypeOperators, GADTs #-}

-- | LinkUnsafeIO provides quick and dirty behaviors to integrate 
-- IO. Such behaviors can support easy debugging or prototyping of
-- resources. They in some cases will be sufficient for integration
-- of effects. Though, there are `cleaner` ways to integrate effects
-- with RDP (blackboard metaphor, shared services).
--
-- The onUpdate operations are unsafe because they are not generally
-- idempotent or commutative. Sirea won't replicate behaviors, but
-- developers should feel free to duplicate behaviors to abstract or
-- refactor code, leveraging RDP's spatial idempotence properties. 
--
-- In practice, developers can ensure that unsafe IO is used in safe 
-- ways. It just takes a little discipline. 
--
-- LinkUnsafeIO does not actually provide a way to pipe input back 
-- into the RDP behavior, at least not directly. 
--
-- On the input side, I am still designing a behavior - perhaps to
-- inject a signal into RDP from IO, which would then be masked by
-- the demand signal. For thread-safety, such a signal would first
-- go through an intermediate, dedicated partition. It should be
-- feasible to combine these updates into batches (using a monoid).
--
module Sirea.LinkUnsafeIO 
    ( unsafeOnUpdateB
    , unsafeOnUpdateBL
    , unsafeOnUpdateBLN
    ) where

import Data.IORef
import Control.Applicative
import Sirea.Link
import Sirea.Signal
import Sirea.Time
import Sirea.Behavior
import Sirea.B
import Sirea.Internal.BImpl (undeadB, keepAliveB)

import Control.Exception (assert)

-- dtFinal provides the upper bound for how far to execute when 
-- stability increases to infinity, measured against the last 
-- update or the prior stability.
dtFinal :: DT
dtFinal = 3.0 -- seconds

-- | unsafeOnUpdateB - perform an IO action for every unique value
-- in a signal as it becomes stable, then forward the update (after
-- performing the IO). The IO action is executed when the value is
-- fully stable, i.e. so you won't receive two updates for the same
-- time. The motivation for unsafeOnUpdateB is easy support for 
-- debugging, unit tests, auditing, etc.
--
-- The outermost `IO` is called once, when the link is first built.
-- It allows setting up any volatile resources, as necessary. This
-- might be called from a different thread than `p`.
--
-- unsafeOnUpdateB qualifies as an effectful sink, i.e. it will keep
-- a behavior alive. This will interfere with dead code elimination,
-- and so may be unsuitable for debugging purposes.
unsafeOnUpdateB :: (Eq a) => IO (T -> Maybe a -> IO ()) -> B w (S p a) (S p a)
unsafeOnUpdateB mkOp = unsafeOnUpdateBL mkOp >>> undeadB

-- | unsafeOnUpdateBL - a very lazy variation of unsafeOnUpdateB.
-- This variation allows dead-code elimination of the behavior when
-- the tapped signal is not used later in the pipeline.
--
-- Only suitable for signals you'll need for other reasons.
--
unsafeOnUpdateBL :: (Eq a) => IO (T -> Maybe a -> IO ()) -> B w (S p a) (S p a)
unsafeOnUpdateBL mkOp = unsafeLinkB blLnk
    where blLnk = MkLnk { ln_build = build
                        , ln_tsen = True
                        , ln_peek = 0     }
          build LnkDead = return LnkDead
          build (LnkSig lu) = 
            mkOp >>= \ op ->
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
unsafeOnUpdateBLN :: (Eq a) => IO (T -> Maybe a -> IO ()) 
                    -> B w (S p a :&: x) (S p a :&: x)
unsafeOnUpdateBLN mkOp = bfirst (unsafeOnUpdateBL mkOp) >>> keepAliveB



-- TODO:
--
-- A simple mechanism will also be provided to inject updates into a
-- behavior. For thread safety, this channels the updates through a
-- dedicated partition. 


