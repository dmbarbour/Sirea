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
    , MkLnk, Lnk, LnkW(..), SigUp(..), LnkUp(..)
    , ln_zero, ln_lnkup, ln_fst, ln_snd, ln_left, ln_right, ln_dead
    , ln_sumap, ln_lumap
    , su_fmap, su_apply
    ) where

import Sirea.Internal.LTypes -- includes MkLnk, etc.
import Sirea.Internal.BTypes
import Sirea.Internal.BImpl (forceDelayB)
import Sirea.Behavior
import Sirea.B()
import Sirea.BCX
import Sirea.PCX

-- | unsafeLinkB can represent new primitive behaviors. Often, it is
-- unnecessary; even for FFI and legacy adapters, support for a few
-- common patterns would be sufficient.
--
-- unsafeLinkB is unsafe. Discipline is necessary to avoid violating 
-- RDP invariants: spatial commutativity and idempotence, duration
-- coupling, locally stateless and eventless behavior. 
--
-- Construction of links should have no observable side effects. Any
-- observable effect should wait for active signal. At construction,
-- IO is used to create local state for caches or connections with
-- external resources. (Proxy resources should be accessed via PCX.)
--
-- Note: unsafeLinkB might be called from any thread, potentially at
-- any time (due to compilation of dynamic behaviors). IO must not
-- be specific to any partition. (Partition-specific IO should wait 
-- for a link update.)
unsafeLinkB :: MkLnk x y -> B x y
unsafeLinkB ln = bsynch >>> forceDelayB >>> B_mkLnk tr_unit ln


-- | unsafeLinkBCX provides access to PCX, which models resources
-- that may be shared between links. If otherwise you would need 
-- global state, please use unsafeLinkBCX and put state in the PCX. 
unsafeLinkBCX :: (PCX w -> MkLnk x y) -> BCX w x y
unsafeLinkBCX = wrapBCX . (unsafeLinkB .)





