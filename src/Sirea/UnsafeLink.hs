{-# LANGUAGE TypeOperators, GADTs #-}

-- | New behavior primitives for Sirea.
--
-- These shouldn't be necessary often, since it will only take a few
-- common abstractions to support most new ideas and resources. But 
-- unsafeLinkB ensures that unforseen corner cases can be handled.
--
module Sirea.UnsafeLink 
    ( unsafeLinkB
    , Lnk, LnkW(..), LnkUp(..), StableT(..)
    , ln_zero, ln_lnkup, ln_fst, ln_snd, ln_left, ln_right, ln_dead
    , ln_sfmap, ln_lumap, ln_append
    ) where

import Sirea.Internal.LTypes
import Sirea.Internal.B0Type (B0_mkLnk)
import Sirea.Internal.B0Impl (forceDelayB)
import Sirea.Internal.B0
import Sirea.Behavior
import Sirea.B
import Sirea.PCX
import Sirea.Partition (W)

-- | unsafeLinkB supports development of new primitive behaviors. It
-- should very rarely be necessary, since a few patterns can cover
-- many FFI adapters. Careful discipline is necessary to ensure that
-- unsafeLinkB preserves RDP's invariants (spatial commutativity and 
-- idempotence, duration coupling, locally stateless, eventless).
unsafeLinkB :: (SigInP p x, SigInP p y) 
            => (PCX W -> Lnk IO y -> IO (Lnk IO x)) -> B x y
unsafeLinkB = wrapB . (unsafeLinkB0 .)

unsafeLinkB0 :: (Monad m, SigInP p x, SigInP p y) 
             => (Lnk m y -> m (Lnk m x)) -> B0 m x y
unsafeLinkB0 ln = bsynch >>> forceDelayB >>> B0_mkLnk lc_dupCaps (const ln)


