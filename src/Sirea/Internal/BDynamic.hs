{-# LANGUAGE TypeOperators #-}

-- | Implementation of the Dynamic behavior type for B.
module Sirea.Internal.BDynamic 
    ( evalB
    ) where


import Sirea.Internal.BTypes
import Sirea.Internal.STypes
import Sirea.Time
--import Sirea.Internal.BImpl

-- THE GOAL!
-- beval :: (SigInP p x) => DT -> b (S p (b x y) :&: x) (y :|: S p ())

evalB :: (SigInP p x) => DT 
      -> B w (S p (B w x y) :&: x) (y :|: S p ())
evalB = undefined



-- The basic technique:
--  Will maintain multiple dynamic behaviors at once - i.e. past, present, future.
--  Will generate a new receiver Lnk for each dynamic behavior. 
--  New receiver nodes primarily provide identity for merges.
--  Will eventually clear `old` dynamic behaviors, i.e. after fully stable.



