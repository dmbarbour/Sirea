{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | Static - augment a behavior with static information, applying an
-- applicative when building the behavior.  
module Sirea.Trans.Static
    ( StaticB
    , wrapStatic, unwrapStatic
    ) where

import Prelude hiding (id,(.))
import Control.Category
import Control.Applicative
import Sirea.Behavior
import Sirea.Partition

newtype StaticB f b x y = SB (f (b x y))

wrapStatic :: f (b x y) -> StaticB f b x y
wrapStatic = SB

unwrapStatic :: StaticB f b x y -> f (b x y)
unwrapStatic (SB fbxy) = fbxy

toSB :: (Applicative f) => b x y -> StaticB f b x y
toSB = (SB . pure)

-- from Sirea.Behavior
instance (Applicative f) => BEmbed b (StaticB f b) where
    bembed = toSB
instance (Category b, Applicative f) => Category (StaticB f b) where
    id = toSB id
    (SB f) . (SB g) = SB $ (.) <$> f <*> g
instance (BFmap b, Applicative f) => BFmap (StaticB f b) where
    bfmap   = toSB . bfmap
    bconst  = toSB . bconst
    bstrat  = toSB bstrat
    btouch  = toSB btouch
    badjeqf = toSB badjeqf
instance (BProd b, Applicative f) => BProd (StaticB f b) where
    bfirst (SB f) = SB (bfirst <$> f)
    bdup    = toSB bdup
    bfst    = toSB bfst
    bswap   = toSB bswap
    bassoclp= toSB bassoclp
instance (BSum b, Applicative f) => BSum (StaticB f b) where
    bleft  (SB f) = SB (bleft <$> f)
    bmirror = toSB bmirror
    bmerge  = toSB bmerge
    binl    = toSB binl
    bassocls= toSB bassocls
instance (BDisjoin b, Applicative f) => BDisjoin (StaticB f b) where
    bdisjoin= toSB bdisjoin
instance (BZip b, Applicative f) => BZip (StaticB f b) where
    bzip    = toSB bzip
    bzap    = toSB bzap
instance (BSplit b, Applicative f) => BSplit (StaticB f b) where
    bsplit  = toSB bsplit
instance (BTemporal b, Applicative f) => BTemporal (StaticB f b) where
    bdelay  = toSB . bdelay
    bsynch  = toSB bsynch
instance (BPeek b, Applicative f) => BPeek (StaticB f b) where
    bpeek   = toSB . bpeek
instance (BDynamic b b', Applicative f) => BDynamic (StaticB f b) b' where
    beval   = toSB . beval
    bexec   = toSB bexec
instance (Behavior b, Applicative f) => Behavior (StaticB f b)

-- from Sirea.Partition
instance (BCross b, Applicative f) => BCross (StaticB f b) where
    bcross  = toSB bcross
instance (BScope b, Applicative f) => BScope (StaticB f b) where
    bpushScope = toSB bpushScope
    bpopScope  = toSB bpopScope




