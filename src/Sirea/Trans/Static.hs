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

-- from Sirea.Behavior
instance (Applicative f) => BEmbed b (StaticB f b) where
    bembed = SB . pure
instance (Category b, Applicative f) => Category (StaticB f b) where
    id = (SB . pure) id
    (SB f) . (SB g) = SB $ (.) <$> f <*> g
instance (BFmap b, Applicative f) => BFmap (StaticB f b) where
    bfmap   = (SB . pure) . bfmap
    bconst  = (SB . pure) . bconst
    bstrat  = (SB . pure) bstrat
    btouch  = (SB . pure) btouch
    badjeqf = (SB . pure) badjeqf
instance (BProd b, Applicative f) => BProd (StaticB f b) where
    bfirst (SB f) = SB (bfirst <$> f)
    bdup    = (SB . pure) bdup
    bfst    = (SB . pure) bfst
    bswap   = (SB . pure) bswap
    bassoclp= (SB . pure) bassoclp
instance (BSum b, Applicative f) => BSum (StaticB f b) where
    bleft  (SB f) = SB (bleft <$> f)
    bmirror = (SB . pure) bmirror
    bmerge  = (SB . pure) bmerge
    binl    = (SB . pure) binl
    bassocls= (SB . pure) bassocls
instance (BDisjoin b, Applicative f) => BDisjoin (StaticB f b) where
    bdisjoin= (SB . pure) bdisjoin
instance (BZip b, Applicative f) => BZip (StaticB f b) where
    bzip    = (SB . pure) bzip
    bzap    = (SB . pure) bzap
instance (BSplit b, Applicative f) => BSplit (StaticB f b) where
    bsplit  = (SB . pure) bsplit
instance (BTemporal b, Applicative f) => BTemporal (StaticB f b) where
    bdelay  = (SB . pure) . bdelay
    bsynch  = (SB . pure) bsynch
instance (BPeek b, Applicative f) => BPeek (StaticB f b) where
    bpeek   = (SB . pure) . bpeek
instance (BDynamic b b', Applicative f) => BDynamic (StaticB f b) b' where
    beval   = (SB . pure) . beval
    bexec   = (SB . pure) bexec
instance (Behavior b, Applicative f) => Behavior (StaticB f b)

-- from Sirea.Partition
instance (BCross b, Applicative f) => BCross (StaticB f b) where
    bcross  = (SB . pure) bcross
instance (BScope b, Applicative f) => BScope (StaticB f b) where
    bpushScope = (SB . pure) bpushScope
    bpopScope  = (SB . pure) bpopScope




