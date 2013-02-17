{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | PureB - restrict a behavior to the pure subset. PureB does not
-- export a `wrap` function, thus developers cannot inject effectful
-- behaviors into PureB. Instead, one can only compose or unwrap a
-- pure behavior.
--
-- The advantage of pure behaviors over just using Haskell functions
-- is their ability to model distribution, delay, synchronization, 
-- and independent update of complex signals. `beval` and `bcross`
-- are both accessible for pure behaviors.
--
module Sirea.Trans.Pure
    ( PureB, unwrapPure
    ) where

import Prelude hiding (id,(.))
import Control.Category
import Sirea.Behavior
import Sirea.Partition

-- | PureB is a behavior that has no side-effects and no access to
-- external resources. This can be useful to enforce purity on RDP
-- subprograms, e.g. to isolate a dynamic behavior.
newtype PureB b x y = PB (b x y)

liftPure :: b x y -> PureB b x y
liftPure = PB

-- | Pure behaviors can be unwrapped and treated as impure behaviors
unwrapPure :: PureB b x y -> b x y
unwrapPure (PB f) = f

-- from Sirea.Behavior
instance (Category b) => Category (PureB b) where
    id = liftPure id
    (PB f) . (PB g) = PB (f . g)
instance (BFmap b) => BFmap (PureB b) where
    bfmap   = liftPure . bfmap
    bconst  = liftPure . bconst
    bstrat  = liftPure bstrat
    btouch  = liftPure btouch
    badjeqf = liftPure badjeqf
instance (BProd b) => BProd (PureB b) where
    bfirst (PB f) = PB (bfirst f)
    bdup    = liftPure bdup
    b1i     = liftPure b1i
    b1e     = liftPure b1e
    btrivial= liftPure btrivial
    bswap   = liftPure bswap
    bassoclp= liftPure bassoclp
instance (BSum b) => BSum (PureB b) where
    bleft  (PB f) = PB (bleft f)
    bmirror = liftPure bmirror
    bmerge  = liftPure bmerge
    b0i     = liftPure b0i
    b0e     = liftPure b0e
    bvacuous= liftPure bvacuous
    bassocls= liftPure bassocls
instance (BDisjoin b) => BDisjoin (PureB b) where
    bdisjoin= liftPure bdisjoin
instance (BZip b) => BZip (PureB b) where
    bzap    = liftPure bzap
instance (BSplit b) => BSplit (PureB b) where
    bsplit  = liftPure bsplit
instance (BTemporal b) => BTemporal (PureB b) where
    bdelay  = liftPure . bdelay
    bsynch  = liftPure bsynch
    bfchoke = liftPure bfchoke
instance (BPeek b) => BPeek (PureB b) where
    bpeek   = liftPure . bpeek
instance (Behavior b) => Behavior (PureB b)

instance (BDynamic b b') => BDynamic b (PureB b') where
    beval dt = bfirst (bfmap unwrapPure) >>> beval dt
instance (BDynamic b b') => BDynamic (PureB b) (PureB b') where
    beval = liftPure . beval

-- from Sirea.Partition
instance (BCross b) => BCross (PureB b) where
    bcross  = liftPure bcross






