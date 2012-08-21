{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}

-- | Skipper - augment a behavior with the ability to leap to a 
-- later evaluation, assuming you can generate the k type input.
--
-- This was developed for symmetry with the Error transformer, which
-- has been part of the Arrows package for some time. Skipper does
-- seem like a promising pattern in its own right.
module Sirea.Trans.Skipper
    ( SkipperB
    , wrapSkipper, unwrapSkipper
    ) where

import Prelude hiding (id,(.))
import Control.Category
import Sirea.Behavior
import Sirea.Partition

newtype SkipperB k b x y = SkipperB (b (k :|: x) y)

wrapSkipper :: b (k :|: x) y -> SkipperB k b x y
wrapSkipper = SkipperB

unwrapSkipper :: SkipperB k b x y -> b (k :|: x) y
unwrapSkipper (SkipperB b) = b




