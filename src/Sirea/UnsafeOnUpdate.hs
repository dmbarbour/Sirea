{-# LANGUAGE TypeOperators, GADTs #-}

-- | UnsafeOnUpdate provides quick and dirty behaviors to integrate 
-- output effects. While the name says `unsafe` (and it is, for RDP
-- invariants), this is far safer than using unsafeLinkB. Commands 
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
-- Weaknesses of UnsafeOnUpdate:
--   
--   * output only, no speculation of consequences, poor feedback
--   * driven by stability, bursty, unsuitable for real-time control
--   * neither idempotent nor commutative, unsafe for RDP invariants
--
-- UnsafeOnUpdate is useful for logging, debugging, and integration
-- with some imperative APIs. It's convenient for hacking.
--
module Sirea.UnsafeOnUpdate 
    ( unsafeOnUpdateB
    , unsafeOnUpdateBL
    , unsafeOnUpdateBLN
    ) where

import Data.IORef
import Control.Monad (unless, when)
import Control.Exception (assert)
import Sirea.UnsafeLink
import Sirea.Signal
import Sirea.Time
import Sirea.Behavior
import Sirea.B
import Sirea.PCX
import Sirea.Partition
import Sirea.Internal.Tuning (tAncient)

import Debug.Trace (traceIO) 

-- | unsafeOnUpdateB - perform an IO action for every unique value
-- in a signal as it becomes stable, then forward the update. There
-- is also a one-time IO action on initial construction. 
--
-- The IO operations are performed at the end of the step.
unsafeOnUpdateB :: (Eq a, Partition p) 
                => (PCX p -> IO (T -> Maybe a -> IO ()))
                -> B (S p a) (S p a)
unsafeOnUpdateB = unsafeLinkB . mkOnUpdate

-- | unsafeOnUpdateBL - a variation of unsafeOnUpdateB that does not
-- prevent dead-code elimination. The behavior will be dropped if 
-- the `S p a` signal is not used downstream. 
unsafeOnUpdateBL :: (Eq a, Partition p) 
                 => (PCX p -> IO (T -> Maybe a -> IO ())) 
                 -> B (S p a) (S p a)
unsafeOnUpdateBL = unsafeLinkBL . mkOnUpdate

-- | unsafeOnUpdateBLN - perform IO effects on the first signal if
-- any of the signals are used in the pipeline. This is useful to
-- debug a behavior without preventing dead-code elimination. 
unsafeOnUpdateBLN :: (Eq a, Partition p)  
                  => (PCX p -> IO (T -> Maybe a -> IO ())) 
                  -> B (S p a :&: x) (S p a :&: x)
unsafeOnUpdateBLN = unsafeLinkBLN . mkOnUpdate

mkOnUpdate :: (Eq a, Partition p) 
           => (PCX p -> IO (T -> Maybe a -> IO ()))
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
            -> (T -> Maybe a -> IO ()) -- operation to execute
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
        let ops = takeWhile ((< tHi) . fst) $
                  dropWhile ((< tLo) . fst) $
                  sigToList (s_adjeqf (==) s) tLoR tHi 
        in
        unless (null ops) (schedOps ops)
    schedOps = onStepEnd pd . mapM_ runOp
    runOp (t,a) = op t a

 
