{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}

-- Writer - extends a behavior with an implicit output, which must
-- be written on every step. Developers must specify the behavior to
-- combine parallel write operations, e.g. at zip and zap.
--
-- RDP really doesn't often need the writer monad; it can write as a
-- side-effect easily enough. But it may be useful, in some cases,
-- to encapsulate the writing behavior in a pure structure. 
module Sirea.Trans.Writer
    ( WriterB
    , wrapWriter, unwrapWriter
    ) where

import Prelude hiding (id,(.))
import Control.Category
import Sirea.Behavior
import Sirea.Partition

newtype WriterB w b x y = WriterB (b x (w :&: y))

wrapWriter :: b x (w :&: y) -> WriterB w b x y
wrapWriter = WriterB

unwrapWriter :: WriterB w b x y -> b x (w :&: y)
unwrapWriter (WriterB b) = b

-- It seems that `WriterB` is impossible to express in Sirea RDP;
-- there is no sensible `id` function 

{-

-- from Sirea.Behavior
instance (Category b) => Category (WriterB w b) where
    id = toSB id
    (SB f) . (SB g) = SB $ (.) <$> f <*> g
instance (BFmap b) => BFmap (WriterB w b) where
    bfmap   = toSB . bfmap
    bconst  = toSB . bconst
    bstrat  = toSB bstrat
    btouch  = toSB btouch
    badjeqf = toSB badjeqf
instance (BProd b) => BProd (WriterB w b) where
    bfirst (SB f) = SB (bfirst <$> f)
    bdup    = toSB bdup
    b1i     = toSB b1i
    b1e     = toSB b1e
    btrivial= toSB btrivial
    bswap   = toSB bswap
    bassoclp= toSB bassoclp
instance (BSum b) => BSum (WriterB w b) where
    bleft  (SB f) = SB (bleft <$> f)
    bmirror = toSB bmirror
    bmerge  = toSB bmerge
    b0i     = toSB b0i
    b0e     = toSB b0e
    bvacuous= toSB bvacuous
    bassocls= toSB bassocls
instance (BDisjoin b) => BDisjoin (WriterB w b) where
    bdisjoin= toSB bdisjoin
instance (BZip b) => BZip (WriterB w b) where
    bzap    = toSB bzap
instance (BSplit b) => BSplit (WriterB w b) where
    bsplit  = toSB bsplit
instance (BTemporal b) => BTemporal (WriterB w b) where
    bdelay  = toSB . bdelay
    bsynch  = toSB bsynch
instance (BPeek b) => BPeek (WriterB w b) where
    bpeek   = toSB . bpeek
instance (Behavior b) => Behavior (WriterB w b)

-- from Sirea.Partition
instance (BCross b) => BCross (WriterB w b) where
    bcross  = toSB bcross

-}



