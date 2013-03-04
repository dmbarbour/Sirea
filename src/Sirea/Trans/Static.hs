{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | Static - augment a behavior with static information, applying an
-- applicative when building the behavior. 
--
-- This is probably the most useful basis for behavior transforms in
-- RDP, e.g. for configuration and dependency injection, and staged
-- programming.
module Sirea.Trans.Static
    ( StaticB
    , liftStatic, wrapStatic, unwrapStatic
    ) where

import Prelude hiding (id,(.))
import Control.Category
import Control.Applicative
import Sirea.Behavior
import Sirea.Partition

-- | StaticB is a behavior built by an applicative. 
newtype StaticB f b x y = SB (f (b x y))

wrapStatic :: f (b x y) -> StaticB f b x y
wrapStatic = SB

unwrapStatic :: StaticB f b x y -> f (b x y)
unwrapStatic (SB fbxy) = fbxy

liftStatic :: (Applicative f) => b x y -> StaticB f b x y
liftStatic = SB . pure

-- from Sirea.Behavior
instance (Category b, Applicative f) => Category (StaticB f b) where
    id = liftStatic id
    (SB f) . (SB g) = SB $ (.) <$> f <*> g
instance (BFmap b, Applicative f) => BFmap (StaticB f b) where
    bfmap   = liftStatic . bfmap
    bconst  = liftStatic . bconst
    bstrat  = liftStatic bstrat
    btouch  = liftStatic btouch
    badjeqf = liftStatic badjeqf
instance (BProd b, Applicative f) => BProd (StaticB f b) where
    bfirst (SB f) = SB (bfirst <$> f)
    bdup    = liftStatic bdup
    b1i     = liftStatic b1i
    b1e     = liftStatic b1e
    btrivial= liftStatic btrivial
    bswap   = liftStatic bswap
    bassoclp= liftStatic bassoclp
instance (BSum b, Applicative f) => BSum (StaticB f b) where
    bleft  (SB f) = SB (bleft <$> f)
    bmirror = liftStatic bmirror
    bmerge  = liftStatic bmerge
    b0i     = liftStatic b0i
    b0e     = liftStatic b0e
    bvacuous= liftStatic bvacuous
    bassocls= liftStatic bassocls
instance (BDisjoin b, Applicative f) => BDisjoin (StaticB f b) where
    bdisjoin= liftStatic bdisjoin
instance (BZip b, Applicative f) => BZip (StaticB f b) where
    bzap    = liftStatic bzap
instance (BSplit b, Applicative f) => BSplit (StaticB f b) where
    bsplit  = liftStatic bsplit
instance (BTemporal b, Applicative f) => BTemporal (StaticB f b) where
    bdelay  = liftStatic . bdelay
    bsynch  = liftStatic bsynch
instance (Behavior b, Applicative f) => Behavior (StaticB f b)

-- Static cannot evaluate itself, in general, but may evaluate any
-- dynamic behavior that the original behavior could evaluate.
instance (BDynamic b b', Applicative f) => BDynamic (StaticB f b) b' where
    bevalx  = liftStatic . bevalx

-- from Sirea.Partition
instance (BCross b, Applicative f) => BCross (StaticB f b) where
    bcross  = liftStatic bcross






