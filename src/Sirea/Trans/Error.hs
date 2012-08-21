{-# LANGUAGE TypeOperators, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

-- | Error - augment a behavior with error information, which is
-- merged automatically. The error information can be accessed or
-- manipulated explicitly. Errors are of a single type, but we can
-- exit and enter error models at will.
module Sirea.Trans.Error
    ( ErrorB
    , liftError, wrapError, unwrapError
    , raiseError, handleError, tryInUnless, newError
    ) where

import Prelude hiding (id,(.))
import Control.Category
import Sirea.Behavior
import Sirea.Partition

newtype ErrorB e b x y = ErrorB (b x (e :|: y))

-- | Wrap a behavior for the Error transform
wrapError :: b x (e :|: y) -> ErrorB e b x y
wrapError = ErrorB

-- | Expose the underlying behavior
unwrapError :: ErrorB e b x y -> b x (e :|: y)
unwrapError (ErrorB b) = b

-- | Lift an error-free behavior into the Error transform
liftError :: (BSum b) => b x y -> ErrorB e b x y
liftError = wrapError . (>>> binr)

-- | Force an error
raiseError :: (BSum b) => ErrorB e b e y
raiseError = wrapError binl

-- | handleError will process errors from a particular operation.
handleError :: (BDisjoin b, SigInP p x) 
            => ErrorB e b x (S p () :&: y)
            -> ErrorB e b (x :&: e) y
            -> ErrorB e b x y
handleError b0 bF = tryInUnless b0 (liftError bsnd) bF

-- | Try to catch errors raised by a primary comptuation.
--
-- This offers a lot more context than handleError. It is a somewhat
-- awkward construct, but is borrowed from the arrows package.
-- 
tryInUnless :: (BDisjoin b, SigInP p x)
            => ErrorB e b x (S p () :&: y) -- computation with errors 
            -> ErrorB e b (x :&: y) z -- on success
            -> ErrorB e b (x :&: e) z -- on failure
            -> ErrorB e b x z
tryInUnless (ErrorB b0) (ErrorB bS) (ErrorB bF) = wrapError $
    bdup >>> bsecond b0 >>> -- @ (x :&: (e :|: (S p () :&: y)))
    bsecond bmirror >>> bdisjoin >>> -- @ ((x :&: y) :|: (x :&: e))
    (bS +++ bF) >>> bmerge -- @ (e :|: z)

-- | newError makes errors associated with an operation observable
newError :: (BSum b) => ErrorB e b x y -> ErrorB e b x (e :|: y)
newError = liftError . unwrapError

instance (BSum b) => Category (ErrorB e b) where
    id = liftError id
    g . f = wrapError $
        unwrapError f >>> bright (unwrapError g) >>> 
        bassocls >>> bleft bmerge

instance (BSum b, BFmap b) => BFmap (ErrorB e b) where
    bfmap   = liftError . bfmap
    bconst  = liftError . bconst
    bstrat  = liftError bstrat
    btouch  = liftError btouch
    badjeqf = liftError badjeqf
instance (BSum b) => BSum (ErrorB e b) where
    bleft f = wrapError $ bleft (unwrapError f) >>> bassocrs
    bmirror = liftError bmirror
    bmerge  = liftError bmerge
    b0i     = liftError b0i
    b0e     = liftError b0e
    bvacuous= liftError bvacuous
    bassocls= liftError bassocls
instance (BSplit b) => BSplit (ErrorB e b) where
    bsplit  = liftError bsplit
instance (BTemporal b, BSum b) => BTemporal (ErrorB e b) where
    bdelay  = liftError . bdelay
    bsynch  = liftError bsynch
instance (BPeek b, BSum b) => BPeek (ErrorB e b) where
    bpeek   = liftError . bpeek

-- from Sirea.Partition
instance (BCross b, BSum b) => BCross (ErrorB e b) where
    bcross  = liftError bcross


-- BProd seems to be infeasible. Basically, the issue is that I 
-- cannot model `bfirst` without a disjoin, and I cannot model a
-- disjoin for a generic type. So, ErrorB is not a product behavior.
--
--instance (BProd b, BSum b) => BProd (ErrorB e b) where
--    bfirst (ErrorB f) = 
-- ALSO BLOCKS: BDisjoin, BZip, Behavior, BDynamic





