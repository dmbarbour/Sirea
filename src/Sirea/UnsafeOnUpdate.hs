{-# LANGUAGE TypeOperators, GADTs #-}

-- | UnsafeOnUpdate provides quick and dirty behaviors to integrate 
-- IO. Such behaviors can support easy debugging or prototyping of
-- resources. They in some cases will be sufficient for integration
-- of effects. There are cleaner ways to integrate effects with RDP
-- via blackboard metaphor, shared services.
--
-- The onUpdate operations are unsafe because they are not generally
-- idempotent or commutative. They also are difficult to anticipate,
-- and only weakly tied to real-time. They are suitable for simple
-- logging tasks for debugging, e.g. console output.
--
-- With discipline, developers can ensure unsafe IO is used in safe
-- ways, either make it idempotent and commutative or ensure unique
-- use within a behavior.
--
-- On the input side, I am still designing a behavior - perhaps to
-- inject a signal into RDP from IO, which would then be masked by
-- the demand signal. For thread-safety, such a signal would first
-- go through an intermediate, dedicated partition. It should be
-- feasible to combine these updates into batches (using a monoid).
--
module Sirea.UnsafeOnUpdate 
    ( unsafeOnUpdateB, unsafeOnUpdateBCX
    , unsafeOnUpdateBL, unsafeOnUpdateBCXL
    , unsafeOnUpdateBLN, unsafeOnUpdateBCXLN
    ) where

import Data.IORef
import Data.Typeable
import Control.Applicative
import Control.Monad (unless)
import Sirea.Link
import Sirea.Signal
import Sirea.Time
import Sirea.Behavior
import Sirea.B
import Sirea.BCX
import Sirea.PCX
import Sirea.Internal.BImpl (undeadB, keepAliveB)

import Control.Exception (assert)

-- dtFinal provides the upper bound for how far to execute when 
-- stability increases to infinity, measured against the last 
-- update or the prior stability. This only happens on shutdown,
-- so there should be a time at which we hit `Nothing` and there
-- are no further updates. But we'll only look for that time up 
-- to a few seconds into the signal. How far?
dtFinal :: DT
dtFinal = 6.0 -- seconds

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
unsafeOnUpdateBL mkOp = unsafeLinkB build
    where build LnkDead = return LnkDead
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
        Just (tL,tU) ->
            unless (tL == tU) $ assert (tL < tU) $
                let lsigs = sigToList sf tL tU in
                let action (t,a) =
                        unless (t == tU) $ assert ((tL <= t) && (t < tU)) $
                            readIORef rfA >>= \ a0 ->
                            unless (a == a0) $
                                writeIORef rfA a >> 
                                op t a
                in 
                mapM_ action lsigs
        Nothing -> return ()

-- | unsafeOnUpdateBLN - perform IO effects if any of many signals
-- are used later in the pipeline. A Goldilocks solution:
--      
--      unsafeOnUpdateB - too eager!
--      unsafeOnUpdateBL - too lazy!
--      unsafeOnUpdateBLN - just right.
--
-- This allows tapping a signal for debugging that would otherwise
-- be dropped by the behavior, but allows dead code elimination to
-- remove the behavior based on whether nearby signal `x` is dead.
--
-- In addition to the processed signal, BLN forwards an arbitrary 
-- complex signal without processing it (not even synchronizing it)
-- but will check to see whether it's all dead code. If not, the 
-- IO effect is activated.
unsafeOnUpdateBLN :: (Eq a) => IO (T -> Maybe a -> IO ()) 
                    -> B w (S p a :&: x) (S p a :&: x)
unsafeOnUpdateBLN mkOp = bfirst (unsafeOnUpdateBL mkOp) >>> keepAliveB


-- | the BCX variations of unsafeOnUpdateB, BL, BLN
--
-- Access to partition context allows effects from different parts
-- of one application to combine in shared state, and also to filter
-- for duplicate effects or enforce uniqueness.
--
-- Note that these operate on a specific partition (PCX p) rather
-- than the world context (PCX w). Each partition has a distinct set
-- of resources, which it can manipulate with the partition thread.
unsafeOnUpdateBCX   :: (Eq a, Typeable p) => (PCX p -> IO (T -> Maybe a -> IO ())) 
                                          -> BCX w (S p a) (S p a)
unsafeOnUpdateBCXL  :: (Eq a, Typeable p) => (PCX p -> IO (T -> Maybe a -> IO ())) 
                                          -> BCX w (S p a) (S p a)
unsafeOnUpdateBCXLN :: (Eq a, Typeable p) => (PCX p -> IO (T -> Maybe a -> IO ())) 
                                          -> BCX w (S p a :&: x) (S p a :&: x)

--unsafeOnUpdateBCX mkOp = wrapBCX $ \ cw -> unsafeOnUpdateB (mkOp (findInPCX cw))
unsafeOnUpdateBCX mkOp = wrapBCX $ unsafeOnUpdateB . mkOp . findInPCX
unsafeOnUpdateBCXL mkOp = wrapBCX $ unsafeOnUpdateBL . mkOp . findInPCX
unsafeOnUpdateBCXLN mkOp = wrapBCX $ unsafeOnUpdateBLN . mkOp . findInPCX




-- TODO:
--
-- A simple mechanism will also be provided to inject updates into a
-- behavior. For thread safety, this channels the updates through a
-- dedicated partition. 


