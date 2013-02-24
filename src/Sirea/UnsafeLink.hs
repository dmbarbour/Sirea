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
    , unsafeLinkB
    , unsafeLinkBL
    , LnkUp(..), StableT(..)
    , ln_zero, ln_sfmap, ln_lumap, ln_append
    , isDoneT, fromStableT, maybeStableT
    ) where

import Control.Applicative ((<$>))
import Control.Exception (assert)
import Sirea.Internal.LTypes
import Sirea.Internal.B0Impl (mkLnkB0, mkLnkPure, forceDelayB0)
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
-- than processing the signal.
unsafeLinkB :: (PCX W -> Maybe (LnkUp IO y) -> IO (LnkUp IO x))
            -> B (S p x) (S p y)
unsafeLinkB fn = unsafeLinkB' fn' where
    fn' cw (LnkSig lu) = LnkSig <$> fn cw (Just lu)
    fn' cw ln = assert (ln_dead ln) $ LnkSig <$> fn cw Nothing

-- | unsafeLinkBL is the lazy form of unsafeLinkB; returns dead link
-- when there is no need for the result signal. 
unsafeLinkBL :: (PCX W -> LnkUp IO y -> IO (LnkUp IO x))
             -> B (S p x) (S p y)
unsafeLinkBL fn = unsafeLinkB' fn' where
    fn' cw (LnkSig lu) = LnkSig <$> fn cw lu
    fn' _ ln = assert (ln_dead ln) $ return LnkDead


unsafeLinkB' :: (PCX W -> Lnk IO (S p y) -> IO (Lnk IO (S p x))) 
            -> B (S p x) (S p y)
unsafeLinkB' fn = wrapB $ \ cw ->
    forceDelayB0 >>> mkLnkB0 lc_fwd (const (fn cw))


