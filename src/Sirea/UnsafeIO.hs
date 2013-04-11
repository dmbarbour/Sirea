{-# LANGUAGE TypeOperators, GADTs #-}

-- | UnsafeIO provides a few quick and dirty behaviors to integrate 
-- Haskell IO with Sirea. These are 'unsafe' in the sense that they
-- can violate RDP's invariants if not used carefully, but they are
-- safe with respect to Haskell's IO monad (no unsafePerformIO).
--
-- Haskell IO in Sirea is rarely idempotent or commutative, and has
-- poor support for speculation, increasing latencies and rework. In
-- Sirea, it is essential to avoid blocking IO within a partition.
-- And sequential operations might not interleave the way one might
-- expect (due to bursty and non-deterministic stability updates).
--
-- This module supports currently supports one IO adapter model, in 
-- a few variations:
--
--   OnUpdate - perform output action whenever input signal changes.
--
-- Eventually, I would like to support more models for ease of use:
-- 
--   ReadOnce - perform input action when input changes.
--     perhaps some minimal support for speculation.
--   OnEvents - ReadOnce + schedule reads via event API.
--   IOPolled - perform ad-hoc actions as stability updates.
--   IOAction - fusion of OnUpdate and Polled.
--
-- AgentResource and PCX may help lift these into proper APIs. 
--
module Sirea.UnsafeIO
    ( unsafeOnUpdateB
    , unsafeOnUpdateBL
    , unsafeOnUpdateBLN
    ) where

import Data.IORef
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
           -> PCX p -> LnkUp a -> IO (LnkUp a)
mkOnUpdate mkOp cp lu =
    mkOp cp >>= \ op ->
    newIORef (P s_never (StableT tAncient)) >>= \ rfSig ->
    let lu' = luOnUpdate op rfSig lu in
    return lu'

-- simple strict pair
--
-- The recorded signal actually includes some values from before the
-- StableT value such that we can filter duplicates. No other record
-- of signal history is needed.
data P z = P !(Sig z) {-# UNPACK #-} !StableT

luOnUpdate  :: (Eq a) 
            => (T -> a -> IO ()) -- operation to execute
            -> IORef (P a) -- recorded signal; reported time
            -> LnkUp a -- output sink (just forward input, but AFTER running)
            -> LnkUp a -- input source
luOnUpdate op rfSig lu = LnkUp touch update idle cyc where
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
        let ops = takeWhile ((< tHi) . fst) $
                  dropWhile ((< tLo) . fst) $
                  sigToList (s_adjeqf (==) s) tLoR tHi 
        in
        mapM_ runOp ops
    runOp (a,Just b) = op a b
    runOp _ = return ()

{-
-- | unsafeReadOnceB is suitable for resources that are constant or
-- can reasonably be assumed constant. Such resources are rare, but
-- do exist (e.g. environment variables). One-time construction can
-- support caches and similar. 
--
-- Note: unsafeReadOnce is lazy. It will not execute if there is no
-- consumer for the generated value. It also runs in the middle of a
-- step, so there should be no assumptions about whether updates are
-- consistent.
unsafeReadOnceB :: (Eq a, Partition p) 
                => (PCX p -> IO (T -> a -> IO b)) 
                -> B (S p a) (S p b)
unsafeReadOnceB = unsafeOnEventsB . const

-- | unsafeReadOnceBI 


-- | unsafeOnEventsB allows a reader to hook a simple events API.
-- This is exactly the same as unsafeReadOnceB, except that a notify
-- operation is provided. The notify operation informs Sirea that
-- more should be read for a given time. Notification is idempotent,
-- and accumulative. 

-}



 
