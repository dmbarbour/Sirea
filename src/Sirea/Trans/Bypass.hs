{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}

-- | Bypass is a rather silly behavior model that allows you to skip
-- out of or into any computation by use of a given signal. Bypass
-- does not exist to be useful (though I'd be interested in hearing
-- of any discovered applications). Bypass exists for symmetry with
-- another, more traditional transformer model: State.
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
unwrapBypass (BypassB b) = b

liftBypass :: (BSum b) => b x y -> BypassB k b x y
liftBypass = wrapBypass . bright

