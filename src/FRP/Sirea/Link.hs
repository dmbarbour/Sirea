{-# LANGUAGE GADTs, TypeOperators #-}

module FRP.Sirea.Link 
    ( unsafeLnkB
    -- the following are re-exported from LTypes
    , MkLnk(..), LnkUp(..), Lnk, LnkW(..), SigUp(..)
    , ln_zero, ln_lnkup, ln_fst, ln_snd, ln_left, ln_right, ln_dead
    , ln_sumap, ln_lumap, su_fmap
    , su_apply
    ) where

import FRP.Sirea.Internal.LTypes
import FRP.Sirea.Internal.BTypes
import FRP.Sirea.Behavior

import Control.Exception (assert)

-- | bUnsafeLnk allows developers to extend Sirea with new primitive 
-- behaviors. This supports FFI, foreign services, legacy adapters.
--
-- bUnsafeLnk is, as the name indicates, unsafe. It can violate RDP 
-- properties: spatial commutativity, spatial idempotence, duration
-- coupling, locally stateless and eventless behavior. Caution is
-- suggested. The basic rules are:
--   * commutative - order of link creation does not affect behavior
--   * idempotent - equivalent signals have equivalent response; 
--       no additional effect from duplicate signals 
--   * no delay - Sirea must statically compute delays, so all delay
--       must be through bdelay or bsynch. Signals are synchronized
--       implicitly for bUnsafeLnk (in case of more than one input). 
--   * duration coupling - cannot create or destroy signals; output
--       signal must have same periods of active duration as input.
--   * locally stateless - caches allowed, but nothing that couldn't
--        be regenerated if we stopped the link and created it new.
--        A link may represent access to external state. For Sirea,
--        I suggest external state resources be persistent.
--   * eventless - a link might experience multiple updates for the
--        same logical times. Only the last one counts. Brief views
--        should not affect the behavior of a system. Though, if the
--        updates are straggling, some inconsistency is acceptable.
--   * cleanup - if a signal is in a final, terminal state (s_term)
--        then the LnkUp must be released by any element that holds
--        onto it. This is important for garbage collection. Dynamic
--        behaviors might create and destroy links many times in one
--        application.
--
-- While bUnsafeLnk does have access to global state via IO, its use
-- is not recommended. Consider supplying context as an argument to
-- MkLnk to better support configuration, isolation of subprograms,
-- and garbage collection.
--
-- Further the developer must ensure that the created links properly
-- detach when the signal is in a final state (s_fini) so that the
-- behavior can be garbage collected. This is especially important
-- when using dynamic behaviors!
--
-- Each instance of bUnsafeLnk results in construction of one link.
-- Some links might be dead on input - never invoked with an active
-- signal. There should be no lasting side-effects just for creating
-- a link. Only when an active signal is received is it time to do
-- something. But it is okay to prepare some resources if they can
-- be GC'd if later unused. Primary use of IO is to build caches.
--
unsafeLnkB :: MkLnk x y -> B x y
unsafeLnkB ln = bsynch >>> B_tshift xBarrier >>> B_mkLnk tr_unit ln
    where xBarrier dts =
            assert (ldt_valid dts) $
            assert (ldt_minGoal dts == ldt_maxGoal dts) $ 
            let bNeedBarrier = ldt_minCurr dts /= ldt_maxCurr dts in
            if ln_tsen ln || bNeedBarrier
                then flip lnd_fmap dts $ \ x -> x { ldt_curr = (ldt_goal x) }
                else dts -- no change; all or nothing (for now)
    -- xBarrier actually applies the updates (sets ldt_curr) if necessary.


