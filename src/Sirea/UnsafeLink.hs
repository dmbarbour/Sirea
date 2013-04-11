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
    , unsafeLinkWB, unsafeLinkWB_, unsafeLinkWBL, unsafeLinkWBLN
    , LnkUpM(..), LnkUp, StableT(..), inStableT
    , ln_zero, ln_sfmap, ln_lumap, ln_append
    ) where

import Data.Function (fix)
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
import Sirea.Partition (W, Partition)

-- | pure signal transforms, but might not respect RDP invariants.
unsafeFmapB :: (Sig a -> Sig b) -> B (S p a) (S p b)
unsafeFmapB = wrapB . const . unsafeFmapB0

unsafeFmapB0 :: (Monad m) => (Sig a -> Sig b) -> B0 m (S p a) (S p b)
unsafeFmapB0 = mkLnkPure lc_fwd . ln_lumap . ln_sfmap

lpcx1 :: (Partition p) => B (S p x) z -> PCX W -> IO (PCX p)
lpcx1 _ = findInPCX

lpcx2 :: (Partition p) => B (S p x :&: y) z -> PCX W -> IO (PCX p)
lpcx2 _ = findInPCX


-- | unsafeLinkB is used when the link has some side-effects other
-- than processing the signal, and thus needs to receive a signal
-- even if it is not going to pass one on.
unsafeLinkB :: (Partition p) => (PCX p -> LnkUp y -> IO (LnkUp x)) -> B (S p x) (S p y)
unsafeLinkB fn = fix $ unsafeLinkWB . fn' where
    fn' b cw lu = lpcx1 b cw >>= \ cp -> fn cp lu

-- | unsafeLinkB_ describes a sink, cases where the response signal
-- is unused. Usually, this is used by wrapping it with `bvoid`.
unsafeLinkB_ :: (Partition p) => (PCX p -> IO (LnkUp x)) -> B (S p x) S1
unsafeLinkB_ fn = fix $ unsafeLinkWB_ . fn' where
    fn' b cw = lpcx1 b cw >>= fn

-- | unsafeLinkBL is the lazy form of unsafeLinkB; it is inactive 
-- unless the response signal is necessary downstream.
unsafeLinkBL :: (Partition p) => (PCX p -> LnkUp y -> IO (LnkUp x)) -> B (S p x) (S p y)
unsafeLinkBL fn = fix $ unsafeLinkWBL . fn' where
    fn' b cw lu = lpcx1 b cw >>= \ cp -> fn cp lu

-- | unsafeLinkBLN is a semi-lazy form of unsafeLinkB; it is active
-- if any of the input signals are needed downstream, but operates
-- only on the first input (even if its particular output is not
-- used downstream).
unsafeLinkBLN :: (Partition p) => (PCX p -> LnkUp y -> IO (LnkUp x)) -> B (S p x :&: z) (S p y :&: z)
unsafeLinkBLN fn = fix $ unsafeLinkWBLN . fn' where
    fn' b cw lu = lpcx2 b cw >>= \ cp -> fn cp lu

-- | same as unsafeLinkB, but with world context
unsafeLinkWB :: (PCX W -> LnkUp y -> IO (LnkUp x)) -> B (S p x) (S p y)
unsafeLinkWB fn = unsafeLinkWBL fn >>> (wrapB . const) undeadB0

-- | same as unsafeLinkB_, but with world context
unsafeLinkWB_ :: (PCX W -> IO (LnkUp x)) -> B (S p x) S1
unsafeLinkWB_ fn = wrapB b0 where
    b0 cw = forceDelayB0 >>> mkLnkB0 lc_dupCaps (ul cw) 
    ul cw _ ln = assert (ln_dead ln) $ LnkSig <$> fn cw  

-- | same as unsafeLinkBL, but with world context
unsafeLinkWBL :: (PCX W -> LnkUp y -> IO (LnkUp x)) -> B (S p x) (S p y)
unsafeLinkWBL fn = wrapB b0 where
    b0 cw = forceDelayB0 >>> mkLnkB0 lc_fwd (ul cw)
    ul cw _ (LnkSig lu) = LnkSig <$> fn cw lu
    ul _ _ LnkDead = return LnkDead

-- | same as unsafeLinkBLN, but with world context
unsafeLinkWBLN :: (PCX W -> LnkUp y -> IO (LnkUp x)) -> B (S p x :&: z) (S p y :&: z)
unsafeLinkWBLN fn = bfirst (unsafeLinkWBL fn) >>> (wrapB . const) keepAliveB0



