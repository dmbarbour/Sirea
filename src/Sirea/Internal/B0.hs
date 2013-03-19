
{-# LANGUAGE MultiParamTypeClasses #-}

-- | `B0 m x y` is the raw, primitive behavior type in Sirea. 
module Sirea.Internal.B0 
    ( B0
    ) where

import Sirea.Behavior
import Sirea.Internal.B0Type (B0)
import Sirea.Internal.B0Impl
import Sirea.Internal.B0Dynamic
import Data.Typeable

instance (Typeable1 m) => Typeable2 (B0 m) where
    typeOf2 b = mkTyConApp tcB0 [typeOf1 (getM b)]
        where tcB0 = mkTyCon3 "sirea-core" "Sirea.Internal.B0" "B0"

getM :: B0 m x y -> m ()
getM _ = undefined

alwaysEq :: a -> b -> Bool
alwaysEq = (const . const) True

instance (Monad m) => BFmap (B0 m) where 
    bfmap    = fmapB0
    bconst c = constB0 c >>> unsafeEqShiftB0 alwaysEq
    bstrat   = stratB0 
    btouch   = touchB0
    badjeqf  = adjeqfB0 >>> unsafeEqShiftB0 (==)
instance (Monad m) => BProd (B0 m) where
    bfirst   = firstB0
    bdup     = dupB0
    b1i      = s1iB0
    b1e      = s1eB0
    btrivial = trivialB0
    bswap    = swapB0
    bassoclp = assoclpB0
instance (Monad m) => BSum (B0 m) where
    bleft    = leftB0
    bmirror  = mirrorB0
    bmerge   = mergeB0
    b0i      = s0iB0
    b0e      = s0eB0
    bvacuous = vacuousB0
    bassocls = assoclsB0
instance (Monad m) => BZip (B0 m) where
    bzap     = zapB0
instance (Monad m) => BSplit (B0 m) where
    bsplit   = splitB0
instance (Monad m) => BDisjoin (B0 m) where 
    bdisjoin = disjoinB0
instance (Monad m) => BTemporal (B0 m) where
    bdelay   = delayB0
    bsynch   = synchB0
instance (Monad m) => Behavior (B0 m) 

-- Unfortunately, we can't have dynamic behaviors for type B
-- due to update scheduling issues. evalB without scheduler 
-- access must perform touches in the update phase, which is
-- a problem (since it can lead to premature updates).
--
instance (Monad m) => BDynamic (B0 m) (B0 m) where
    bevalx = evalB0



