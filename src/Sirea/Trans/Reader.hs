{-# LANGUAGE TypeOperators #-}


module Sirea.Trans.Reader
    ( ReaderB
    , liftReader, wrapReader, unwrapReader
    ) where

import Prelude hiding (id,(.))
import Control.Category
import Sirea.Behavior
import Sirea.Partition


newtype ReaderB r b x y = RB (b (r :&: x) y)

wrapReader :: b (r :&: x) y -> ReaderB r b x y
wrapReader = RB

unwrapReader :: ReaderB r b x y -> b (r :&: x) y
unwrapReader (RB b) = b

liftReader :: (BProd b) => b x y -> ReaderB r b x y
liftReader = wrapReader . (bsnd >>>)

-- from Sirea.Behavior
instance (BProd b) => Category (ReaderB r b) where
    id = liftReader id
    (RB g) . (RB f) = RB $
        bfirst bdup >>> bassocrp >>> bsecond f >>> g
instance (BFmap b, BProd b) => BFmap (ReaderB r b) where
    bfmap   = liftReader . bfmap
    bconst  = liftReader . bconst
    bstrat  = liftReader bstrat
    bseq    = liftReader bseq
    badjeqf = liftReader badjeqf
instance (BProd b) => BProd (ReaderB r b) where
    -- bfirst :: (r :&: (x :&: y)) ~> (x' :&: y) 
    bfirst (RB f) = RB $ bassoclp >>> bfirst f
    bdup    = liftReader bdup
    b1i     = liftReader b1i
    b1e     = liftReader b1e
    btrivial= liftReader btrivial
    bswap   = liftReader bswap
    bassoclp= liftReader bassoclp
instance (BZip b) => BZip (ReaderB r b) where
    bzap    = liftReader bzap
instance (BProd b, BTemporal b) => BTemporal (ReaderB r b) where
    bdelay  = liftReader . bdelay
    bsynch  = liftReader bsynch
instance (BProd b, BPeek b) => BPeek (ReaderB r b) where
    bpeek   = liftReader . bpeek

-- from Sirea.Partition
instance (BProd b, BCross b) => BCross (ReaderB r b) where
    bcross  = liftReader bcross


-- Note: BSum is not supported for ReaderB due to a general inability 
-- to disjoin to apply bleft. This also blocks BSplit, BDisjoin, BDynamic, 







