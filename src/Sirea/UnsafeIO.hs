{-# LANGUAGE TypeOperators, GADTs #-}

-- | UnsafeIO provides a few quick and dirty behaviors to integrate 
-- Haskell IO with Sirea. These are 'unsafe' in the sense that they
-- can violate RDP's invariants if not used carefully, but they are
-- safe with respect to Haskell's IO monad (no unsafePerformIO).
--
-- The issue with Haskell IO in RDP is that it is rarely idempotent
-- or commutative. They also have very poor support for speculation,
-- which may increase latencies and rework. Discipline and caution,
-- and knowledge of larger context.
--
-- This module supports many simple IO integration models based on
-- different assumptions and focuses. Each model may also have a few
-- variants.
--
--   OnUpdate - perform output action when input changes.
--   ReadOnce - perform input action when input changes.
--   OnEvents - ReadOnce + schedule reads with event API.
--   IOPolled - perform ad-hoc actions as stability updates.
--   IOAction - fusion of OnUpdate and Polled.
--
-- AgentResource and PCX may help lift these into proper APIs. 
--
module Sirea.UnsafeIO
    ( unsafeOnUpdateB
    , unsafeOnUpdateBL
    , unsafeOnUpdateBLN
    -- , unsafeReadOnceB
    -- , unsafeOnEventsB
    ) where

import Data.IORef
import Data.Maybe (mapMaybe)
import Control.Monad (unless)
import Control.Exception (assert)
import Sirea.UnsafeLink
import Sirea.Signal
import Sirea.Time
import Sirea.Behavior
import Sirea.B
import Sirea.PCX
import Sirea.Partition
import Sirea.Internal.Tuning (tAncient)

-- | unsafeOnUpdateB - perform an IO action for every unique value
-- in a signal as it becomes stable, then forward the update. There
-- is also a one-time IO action on initial construction. 
--
-- The IO operations are performed at the end of the step.
unsafeOnUpdateB :: (Eq a, Partition p) 
                => (PCX p -> IO (T -> a -> IO ()))
                -> B (S p a) (S p a)
unsafeOnUpdateB = unsafeLinkB . mkOnUpdate

-- | unsafeOnUpdateBL - a variation of unsafeOnUpdateB that does not
-- prevent dead-code elimination. The behavior will be dropped if 
-- the `S p a` signal is not used downstream. 
unsafeOnUpdateBL :: (Eq a, Partition p) 
                 => (PCX p -> IO (T -> a -> IO ())) 
                 -> B (S p a) (S p a)
unsafeOnUpdateBL = unsafeLinkBL . mkOnUpdate

-- | unsafeOnUpdateBLN - perform IO effects on the first signal if
-- any of the signals are used in the pipeline. This is useful to
-- debug a behavior without preventing dead-code elimination. 
unsafeOnUpdateBLN :: (Eq a, Partition p)  
                  => (PCX p -> IO (T -> a -> IO ())) 
                  -> B (S p a :&: x) (S p a :&: x)
unsafeOnUpdateBLN = unsafeLinkBLN . mkOnUpdate

mkOnUpdate :: (Eq a, Partition p) 
           => (PCX p -> IO (T -> a -> IO ()))
           -> PCX W -> LnkUp a -> IO (LnkUp a)
mkOnUpdate mkOp cw lu =
    findInPCX cw >>= \ cp ->
    getPSched cp >>= \ pd ->
    mkOp cp >>= \ op ->
    newIORef (P s_never (StableT tAncient)) >>= \ rfSig ->
    let lu' = luOnUpdate pd op rfSig lu in
    return lu'

-- simple strict pair
--
-- The recorded signal actually includes some values from before the
-- StableT value such that we can filter duplicates. No other record
-- of signal history is needed.
data P z = P !(Sig z) {-# UNPACK #-} !StableT

luOnUpdate  :: (Eq a) 
            => PSched -- to run actions at end of step
            -> (T -> a -> IO ()) -- operation to execute
            -> IORef (P a) -- recorded signal; reported time
            -> LnkUp a -- output sink (just forward input, but AFTER running)
            -> LnkUp a -- input source
luOnUpdate pd op rfSig lu = LnkUp touch update idle cyc where
    touch = ln_touch lu
    cyc = ln_cycle lu
    idle tS =
        readIORef rfSig >>= \ (P s0 tS0) ->
        runUpdates tS0 tS s0 >>
        ln_idle lu tS
    update tS tU su =
        readIORef rfSig >>= \ (P s0 tS0) ->
        assert (tU >= inStableT tS0) $
        let s' = s_switch' s0 tU su in
        runUpdates tS0 tS s' >>
        ln_update lu tS tU su
    lessOneNano tm = tm `subtractTime` nanosToDt 1
    record tS sig = 
        let p = P sig tS in 
        p `seq` writeIORef rfSig p
    runUpdates tS0 tS s =
        assert (tS >= tS0) $
        if (tS0 == tS) then record tS s else
        let tLo  = inStableT tS0 in
        let tLoR = lessOneNano tLo in -- for equality filter
        let tHi  = inStableT tS in
        let sGC = s_trim s (lessOneNano tHi) in
        record tS sGC >>
        let ops = mapMaybe seconds $
                  takeWhile ((< tHi) . fst) $
                  dropWhile ((< tLo) . fst) $
                  sigToList (s_adjeqf (==) s) tLoR tHi 
        in
        unless (null ops) (schedOps ops)
    schedOps = onStepEnd pd . mapM_ runOp
    runOp (t,a) = op t a

seconds :: (a,Maybe b) -> Maybe (a,b)
seconds (a,Just b) = Just (a,b)
seconds _ = Nothing


-- | unsafeReadOnceB



 
