{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}

-- | State - propagate an extra value through a computation, and
-- manipulate it programmatically. This isn't really about "state",
-- but the naming convention harkens to State monad and State arrow
-- transformers. (The name made sense for the monad, not so much for 
-- continuous, real-time reactive arrows.)
--
-- In RDP, true state is persistent and external to the RDP logic.
--
-- NOTE: It seems State cannot be modeled with valid RDP. Primarily,
-- it disrespects commutativity of first and second operations.
-- Spatial commutativity requires: 
--   first f >>> second g = second g >>> first f
-- But it is not clear how to thread state to make this valid.
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

-- This is legal as an arrow, but not valid for RDP!
firstState :: (BProd b) => StateB s b x x' -> StateB s b (x :&: y) (x' :&: y)
firstState (StateB f) = StateB $ bassoclp >>> bfirst f >>> bassocrp

-- from Sirea.Behavior
instance (BProd b) => Category (StateB s b) where
    id = liftState id
    (StateB g) . (StateB f) = StateB (g . f)
instance (BFmap b, BProd b) => BFmap (StateB s b) where
    bfmap   = liftState . bfmap
    bconst  = liftState . bconst
    bstrat  = liftState bstrat
    btouch  = liftState btouch
    badjeqf = liftState badjeqf
-- No legal `BProd`
-- No legal `BZip`
instance (BProd b, BTemporal b) => BTemporal (StateB s b) where
    bdelay  = liftState . bdelay
    bsynch  = liftState bsynch
instance (BProd b, BPeek b) => BPeek (StateB s b) where
    bpeek   = liftState . bpeek
instance (BProd b, BCross b) => BCross (StateB s b) where
    bcross  = liftState bcross




