{-# LANGUAGE GADTs, TypeOperators #-}


-- | New behavior primitives for Sirea.
--
-- These shouldn't be necessary often, since it will only take a few
-- common abstractions to support most new ideas and resources. But 
-- `bUnsafeLnk` ensures that any corner cases can be handled.
--
module Sirea.Link 
    ( unsafeLnkB
    -- the following are re-exported from LTypes
    , MkLnk(..), LnkUp(..), Lnk, LnkW(..), SigUp(..)
    , ln_zero, ln_lnkup, ln_fst, ln_snd, ln_left, ln_right, ln_dead
    , ln_sumap, ln_lumap, su_fmap
    , su_apply
    ) where

import Sirea.Internal.LTypes
import Sirea.Internal.BTypes
import Sirea.Behavior
import Sirea.B()

import Control.Exception (assert)

-- | bUnsafeLnk allows developers to extend Sirea with new primitive 
-- behaviors. This supports FFI, foreign services, legacy adapters.
--
-- bUnsafeLnk is, as the name indicates, unsafe. It can violate RDP 
-- properties: spatial commutativity, spatial idempotence, duration
-- coupling, locally stateless and eventless behavior. Caution is
-- suggested. The basic rules are:
--
--   * commutative - order of signal updates or link creation does 
--       not affect behavior. 
--   * idempotent - equivalent signals have equivalent response; 
--       no additional effect from duplicate signals 
--   * no delay - Sirea must statically compute delays, so all delay
--       must be through bdelay or bsynch. Signals are synchronized
--       implicitly for bUnsafeLnk (in case of more than one input). 
--   * duration coupling - cannot create or destroy signals; output
--       signal must have same periods of active duration as input.
--   * locally stateless - caches allowed, but nothing that couldn't
--        be regenerated if we stopped the link and created it new.
--        All real state should be external, preferably persistent.
--   * eventless - no instantaneous signals
--   * cleanup - if a signal is in a final, terminal state (s_term)
--        then the LnkUp must be released by any element that holds
--        onto it. This is important for garbage collection. Dynamic
--        behaviors might create and destroy links many times in one
--        application.
--
-- For shared resources or shared state, use PCX then embed behavior
-- in type BCX to hide the context parameter. Use of global state is
-- NOT recommended. Use PCX anywhere you might want global state.
--
-- Construction of links should have no observable side effects. Any
-- effects should wait for an active input signal. IO to build links
-- is intended for building local resources: caches, resource hooks.
--
unsafeLnkB :: MkLnk w x y -> B w x y
unsafeLnkB ln = bsynch >>> B_tshift xBarrier >>> B_mkLnk tr_unit ln
    where xBarrier dts =
            assert (ldt_valid dts) $
            assert (ldt_minGoal dts == ldt_maxGoal dts) $ 
            let bNeedBarrier = ldt_minCurr dts /= ldt_maxCurr dts in
            if ln_tsen ln || bNeedBarrier
                then flip lnd_fmap dts $ \ x -> x { ldt_curr = (ldt_goal x) }
                else dts -- no change; all or nothing (for now)
    -- xBarrier actually applies the updates (sets ldt_curr) if necessary.


