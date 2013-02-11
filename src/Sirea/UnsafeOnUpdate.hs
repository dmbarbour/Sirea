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
import Control.Monad (unless)
import Control.Exception (assert)
import Sirea.Link
import Sirea.Signal
import Sirea.Time
import Sirea.Behavior
import Sirea.B
import Sirea.BCX
import Sirea.PCX
import Sirea.Internal.BImpl (undeadB, keepAliveB)
import Sirea.Internal.Tuning (dtFinalize, tAncient)

--import Debug.Trace

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
            newIORef (s_never, tAncient) >>= \ rfSig ->
            newIORef Nothing >>= \ rfA ->
            let lu' = luOnUpdate op rfSig rfA lu in
            return (LnkSig lu')

gcSig :: StableT -> Sig x -> Sig x
gcSig DoneT _ = s_never
gcSig (StableT tm) s0 = s_trim s0 tm

luOnUpdate  :: (Eq a) 
            => (T -> Maybe a -> IO ()) -- operation to execute
            -> IORef (Sig a, T) -- recorded signal; reported time
            -> IORef (Maybe a)  -- last reported value
            -> LnkUp a -- output sink (just forward input, but AFTER running)
            -> LnkUp a -- input source
luOnUpdate op rfSig rfA lu = LnkUp touch update idle where
    touch = ln_touch lu
    idle tS =
        readIORef rfSig >>= \ (s0,tLo) ->
        let sCln = gcSig tS s0 in
        let tHi = case tS of
                DoneT -> tLo `addTime` dtFinalize
                StableT tm -> tm
        in
        assert (tHi >= tLo) $
        sCln `seq` tHi `seq` 
        writeIORef rfSig (sCln,tHi) >>
        runUpdates tLo tHi s0 >>
        ln_idle lu tS 
    update tS tU su =
        readIORef rfSig >>= \ (s0,tLo) ->
        let sf = s_switch s0 tU su in
        let sCln = gcSig tS sf in
        let tHi = case tS of
                DoneT -> (max tLo tU) `addTime` dtFinalize
                StableT tm -> tm
        in
        assert (tHi >= tLo) $
        sCln `seq` tHi `seq` 
        writeIORef rfSig (sCln,tHi) >>
        runUpdates tLo tHi sf >>
        ln_update lu tS tU su
    runUpdates tLo tHi sig =
        let xs = sigToList sig tLo tHi in
        let xsSettled = filter ((< tHi) . fst) xs in
        mapM_ runOp xsSettled
    runOp (t,a) =
        readIORef rfA >>= \ a0 ->
        unless (a0 == a) $
            writeIORef rfA a >>
            op t a

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
unsafeOnUpdateBCX mkOp = wrapBCX $ \ cw -> unsafeOnUpdateB (findInPCX cw >>= mkOp)
unsafeOnUpdateBCXL mkOp = wrapBCX $ \ cw -> unsafeOnUpdateBL (findInPCX cw >>= mkOp)
unsafeOnUpdateBCXLN mkOp = wrapBCX $ \ cw -> unsafeOnUpdateBLN (findInPCX cw >>= mkOp)


