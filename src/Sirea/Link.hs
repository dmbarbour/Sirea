{-# LANGUAGE GADTs, TypeOperators #-}


-- | New behavior primitives for Sirea.
--
-- These shouldn't be necessary often, since it will only take a few
-- common abstractions to support most new ideas and resources. But 
-- `unsafeLinkB` ensures that any corner cases can be handled.
--
module Sirea.Link 
    ( unsafeLinkB
    , unsafeLinkBCX
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
import Sirea.BCX
import Sirea.PCX

import Control.Exception (assert)

-- | unsafeLinkB can represent new primitive behaviors. Often, it is
-- unnecessary; even for FFI and legacy adapters, support for a few
-- common patterns would be sufficient.
--
-- unsafeLinkB is, as the name indicates, unsafe. It can violate RDP 
-- properties: spatial commutativity, spatial idempotence, duration
-- coupling, locally stateless and eventless behavior. Caution is
-- recommended. 
--
-- Construction of links should have no observable side effects. Any
-- effects should wait for an active input signal. IO to build links
-- is intended for building local resources: caches, resource hooks.
--
unsafeLinkB :: MkLnk w x y -> B w x y
unsafeLinkB ln = bsynch >>> B_tshift xBarrier >>> B_mkLnk tr_unit ln
    where xBarrier dts =
            assert (ldt_valid dts) $
            assert (ldt_minGoal dts == ldt_maxGoal dts) $ 
            let bNeedBarrier = ldt_minCurr dts /= ldt_maxCurr dts in
            if ln_tsen ln || bNeedBarrier
                then flip lnd_fmap dts $ \ x -> x { ldt_curr = (ldt_goal x) }
                else dts -- no change; all or nothing (for now)
    -- xBarrier actually applies the updates (sets ldt_curr) if necessary.


-- | unsafeLinkBCX provides access to PCX, which is a good place to
-- keep any "shared" state associated with a link. If otherwise you 
-- would need global state, please use unsafeLinkBCX instead and put
-- state in the PCX. 
--
-- Each partition has a dedicated child PCX for state. Obtain that 
-- PCX as a resource  (i.e. `findIn pcx :: PCX p`). Resources in a
-- partition should only be manipulated by local signals, i.e. after
-- bcross to that partition. That supports snapshot consistency. So
-- you should always be able to find the partition ID in the signal.
--
unsafeLinkBCX :: (PCX w -> MkLnk w x y) -> BCX w x y
unsafeLinkBCX = wrapBCX . (unsafeLinkB .)





