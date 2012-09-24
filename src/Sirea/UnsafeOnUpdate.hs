{-# LANGUAGE TypeOperators, GADTs #-}

-- | UnsafeOnUpdate provides quick and dirty behaviors to integrate 
-- output effects. While the name says `unsafe` (and it is, for RDP
-- invariants), this is far safer than using unsafeLinkB! Commands 
-- are activated when the input signal stabilizes on a new value -
-- a simple semantic that developers can easily grok and leverage.
--
-- IO actions are not generally idempotent or commutative. Thus, the
-- unsafeOnUpdate actions may be unsafe to duplicate or rearrange in
-- manners that should be safe for RDP. These safety concerns can be
-- mitigated with some simple disciplines:
--
--   * favor resources insensitive to concurrent update events
--   * consider enforcing uniqueness of unsafeOnUpdate per resource
--
-- The Sirea.AgentResource module would help for the latter option,
-- while message-passing is often suitable for concurrent events.
--
-- A weakness of UnsafeOnUpdate is that it is totally unsuitable for
-- input effects. Consequences of update events are not anticipated!
-- Sirea is blind to any feedback, which can lead to higher costs.
-- Developers should disfavor UnsafeOnUpdate if it causes any hidden
-- feedback. This is for unidirectional communication only: output.
--
-- UnsafeOnUpdate is useful for logging, debugging, and integration
-- with some imperative APIs. 
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
-- in a signal as it becomes stable, then forward the update. There
-- is also a one-time IO action on initial construction.
unsafeOnUpdateB :: (Eq a) => IO (T -> Maybe a -> IO ()) -> B (S p a) (S p a)
unsafeOnUpdateB mkOp = unsafeOnUpdateBL mkOp >>> undeadB


-- | unsafeOnUpdateBL - a very lazy variation of unsafeOnUpdateB.
-- This variation allows dead-code elimination of the behavior when
-- the tapped signal is not used later in the pipeline. A motivation
-- for this is logging a signal but only if it is active for another
-- reason.
unsafeOnUpdateBL :: (Eq a) => IO (T -> Maybe a -> IO ()) -> B (S p a) (S p a)
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
-- Runs the update event if any signal in `(S p a :&: x)` is alive.
-- This allows debugging without keeping a signal alive.
unsafeOnUpdateBLN :: (Eq a) => IO (T -> Maybe a -> IO ()) 
                    -> B (S p a :&: x) (S p a :&: x)
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


