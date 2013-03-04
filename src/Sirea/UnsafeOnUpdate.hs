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
-- A weakness of UnsafeOnUpdate is that it is totally unsuitable for
-- input effects. Consequences of update events are not anticipated!
-- Developers should disfavor UnsafeOnUpdate if it causes feedback
-- through other IO observations. UnsafeOnUpdate is for output only.
--
-- UnsafeOnUpdate is useful for logging, debugging, and integration
-- with some simple imperative APIs. It's convenient for hacking.
--
module Sirea.UnsafeOnUpdate 
    ( unsafeOnUpdateB
    , unsafeOnUpdateBL
    , unsafeOnUpdateBLN
    ) where

import Data.IORef
import Control.Monad (unless)
import Control.Exception (assert)
import Sirea.UnsafeLink
import Sirea.Signal
import Sirea.Time
import Sirea.Behavior
import Sirea.B
import Sirea.PCX
import Sirea.Partition
import Sirea.Internal.Tuning (dtFinalize, tAncient)

--import Debug.Trace

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
    newIORef (s_never, tAncient) >>= \ rfSig ->
    newIORef Nothing >>= \ rfA ->
    let lu' = luOnUpdate pd op rfSig rfA lu in
    return lu'

luOnUpdate  :: (Eq a) 
            => PSched -- to run actions at end of step
            -> (T -> Maybe a -> IO ()) -- operation to execute
            -> IORef (Sig a, T) -- recorded signal; reported time
            -> IORef (Maybe a)  -- last reported value
            -> LnkUp a -- output sink (just forward input, but AFTER running)
            -> LnkUp a -- input source
luOnUpdate pd op rfSig rfA lu = LnkUp touch update idle cyc where
    touch = ln_touch lu
    cyc = ln_cycle lu
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
        schedRunUpdates tLo tHi s0 >>
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
        schedRunUpdates tLo tHi sf >>
        ln_update lu tS tU su
    schedRunUpdates tLo tHi sig =
        let xs = sigToList sig tLo tHi in
        let xsOp = takeWhile ((< tHi) . fst) xs in
        unless (null xsOp) (onStepEnd pd (mapM_ runOp xsOp))
    runOp (t,a) =
        readIORef rfA >>= \ a0 ->
        unless (a0 == a) $
            writeIORef rfA a >>
            op t a


gcSig :: StableT -> Sig x -> Sig x
gcSig DoneT _ = s_never
gcSig (StableT tm) s0 = s_trim s0 tm

