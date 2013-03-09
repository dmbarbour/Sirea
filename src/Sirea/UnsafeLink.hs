{-# LANGUAGE TypeOperators, GADTs #-}

-- | Simple support for new behavior primitives in Sirea, requires
-- the processing be isolated to one signal.
--
-- These shouldn't be necessary often, since it will only take a few
-- common abstractions to support most new ideas and resources. But 
-- unsafeLinkB ensures that unforseen corner cases can be handled.
-- 
-- Processing multiple signals will require deeper access to Sirea's
-- representations.
--
module Sirea.UnsafeLink 
    ( unsafeFmapB
    , unsafeLinkB, unsafeLinkB_, unsafeLinkBL, unsafeLinkBLN
    , LnkUpM(..), LnkUp, StableT(..)
    , ln_zero, ln_sfmap, ln_lumap, ln_append
    ) where

import Control.Applicative
import Control.Exception (assert)
import Sirea.Internal.LTypes
import Sirea.Internal.B0Impl (mkLnkB0, mkLnkPure, forceDelayB0
                             ,undeadB0, keepAliveB0)
import Sirea.Internal.B0
import Sirea.Behavior
import Sirea.Signal
import Sirea.B
import Sirea.PCX
import Sirea.Partition (W)

-- | pure signal transforms, but might not respect RDP invariants.
unsafeFmapB :: (Sig a -> Sig b) -> B (S p a) (S p b)
unsafeFmapB = wrapB . const . unsafeFmapB0

unsafeFmapB0 :: (Monad m) => (Sig a -> Sig b) -> B0 m (S p a) (S p b)
unsafeFmapB0 = mkLnkPure lc_fwd . ln_lumap .ln_sfmap

-- | unsafeLinkB is used when the link has some side-effects other
-- than processing the signal, and thus needs to receive a signal
-- even if it is not going to pass one on.
unsafeLinkB :: (PCX W -> LnkUp y -> IO (LnkUp x)) -> B (S p x) (S p y)
unsafeLinkB fn = unsafeLinkBL fn >>> (wrapB . const) undeadB0

-- | unsafeLinkB_ describes a sink, cases where the response signal
-- is unused. Usually, this is used by wrapping it with `bvoid`.
unsafeLinkB_ :: (PCX W -> IO (LnkUp x)) -> B (S p x) S1
unsafeLinkB_ fn = wrapB b0 where
    b0 cw = forceDelayB0 >>> mkLnkB0 lc_dupCaps (ul cw) 
    ul cw _ ln = assert (ln_dead ln) $ LnkSig <$> fn cw  

-- | unsafeLinkBL is the lazy form of unsafeLinkB; it is inactive 
-- unless the response signal is necessary downstream.
unsafeLinkBL :: (PCX W -> LnkUp y -> IO (LnkUp x)) -> B (S p x) (S p y)
unsafeLinkBL fn = wrapB b0 where
    b0 cw = forceDelayB0 >>> mkLnkB0 lc_fwd (ul cw)
    ul cw _ (LnkSig lu) = LnkSig <$> fn cw lu
    ul _ _ LnkDead = return LnkDead

-- | unsafeLinkBLN is a semi-lazy form of unsafeLinkB; it is active
-- if any of the input signals are needed downstream, but operates
-- only on the first input (even if its particular output is not
-- used downstream).
unsafeLinkBLN :: (PCX W -> LnkUp y -> IO (LnkUp x)) 
              -> B (S p x :&: z) (S p y :&: z)
unsafeLinkBLN fn = bfirst (unsafeLinkBL fn) >>> (wrapB . const) keepAliveB0


