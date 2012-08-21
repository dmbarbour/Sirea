{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}

-- | State - basically the fusion of reader and writer, this is the
-- 'pure' model of state that is achieved by keeping a summary of
-- state between steps. 
--
-- RDP really doesn't often need the writer monad; it can write as a
-- side-effect easily enough. But it may be useful, in some cases,
-- to encapsulate the writing behavior.
module Sirea.Trans.State
    ( StateB
    , liftState, wrapState, unwrapState
    ) where

import Prelude hiding (id,(.))
import Control.Category
import Sirea.Behavior
import Sirea.Partition

newtype StateB s b x y = StateB (b (s :&: x) (s :&: y))

wrapState :: b (s :&: x) (s :&: y) -> StateB s b x y
wrapState = StateB

unwrapState :: StateB s b x y -> b (s :&: x) (s :&: y)
unwrapState (StateB b) = b

liftState :: (BProd b) => b x y -> StateB s b x y
liftState = wrapState . bsecond



