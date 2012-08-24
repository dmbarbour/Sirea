{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}

-- | Bypass is a rather silly behavior model that allows you to skip
-- out of or into any computation by use of a given signal. Bypass
-- does not exist to be useful (though I'd be interested in hearing
-- of any discovered applications). Bypass exists for symmetry with
-- another, more traditional transformer model: State.
--
-- NOTE: Similar to State, BypassB fails to respect laws of RDP. It
-- does not respect decision commutativity or dead source elimination.
module Sirea.Trans.Bypass
    ( BypassB
    , liftBypass, wrapBypass, unwrapBypass
    ) where

import Prelude hiding (id,(.))
import Control.Category
import Sirea.Behavior
import Sirea.Partition

newtype BypassB k b x y = BypassB (b (k :|: x) (k :|: y))

wrapBypass :: b (k :|: x) (k :|: y) -> BypassB k b x y
wrapBypass = BypassB

unwrapBypass :: BypassB k b x y -> b (k :|: x) (k :|: y)
unwrapBypass (BypassB f) = f

liftBypass :: (BSum b) => b x y -> BypassB k b x y
liftBypass = wrapBypass . bright

-- legal for an Arrow, not legal for RDP!
leftBypass :: (BSum b) => BypassB k b x x' -> BypassB k b (x :|: y) (x' :|: y)
leftBypass (BypassB f) = BypassB $ bassocls >>> bleft f >>> bassocrs

instance (BSum b) => Category (BypassB k b) where
    id = liftBypass id
    (BypassB g) . (BypassB f) = BypassB (g . f)
instance (BSum b, BFmap b) => BFmap (BypassB k b) where
    bfmap   = liftBypass . bfmap
    bconst  = liftBypass . bconst
    bstrat  = liftBypass bstrat
    btouch  = liftBypass btouch
    badjeqf = liftBypass badjeqf
instance (BTemporal b, BSum b) => BTemporal (BypassB k b) where
    bdelay  = liftBypass . bdelay
    bsynch  = liftBypass bsynch
instance (BPeek b, BSum b) => BPeek (BypassB k b) where
    bpeek   = liftBypass . bpeek
instance (BCross b, BSum b) => BCross (BypassB k b) where
    bcross  = liftBypass bcross


