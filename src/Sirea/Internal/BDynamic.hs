{-# LANGUAGE TypeOperators #-}

-- | Implementation of the Dynamic behavior type for B.
-- Evaluation will logically sync the inputs, then 
-- physically sync the outputs (y). The inputs are not
-- immediately 
module Sirea.Internal.BDynamic 
    ( evalB
    ) where

import Prelude hiding(id,(.))
import Control.Category
import Sirea.Internal.BTypes
import Sirea.Internal.STypes
import Sirea.Internal.LTypes
import Sirea.Internal.BImpl (synchB, forceDelayB)
import Sirea.Time
--import Sirea.Internal.BImpl

-- THE GOAL!
-- beval :: (SigInP p x) => DT -> b (S p (b x y) :&: x) (y :|: S p ())
-- The basic technique:
--  Will maintain multiple dynamic behaviors at once - i.e. past, present, future.
--     Start with fixed amount above stability (e.g. 3 seconds). Return to problem.
--  Will generate a new receiver-side `Lnk y` for each dynamic behavior. 
--     Each receiver can add or remove self from collection (array?) 
--     based on input signal. (May only add/remove self upon receiving signal,
--     since `y` is not constrained to one partition).
--
--     Need to handle touch properly. Multiple `y` signals might be touched, need
--     to process all of them before forwarding updates.
--
--  Sender has 'time periods' mapped to different Lnk elements. Must also keep 
--  copy of every x signal in case of change in target behavior. Signals always
--  switch to term at upper boundary, except for last known future signal.
--
--  GC is achieved after a Lnk element has fully stabilized sender-side. Just release
--  the reference.
--
--  Dead `Lnk x` elements are not propagated backwards since they might be necessary in 
--  future dynamic behavior. Could be a problem, but some idioms can avoid it.
--     
--  New receiver nodes primarily provide identity for merges.
--  Will eventually clear `old` dynamic behaviors, i.e. after fully stable.
--  Note: need to take proper advantage of `touches` to only send updates after
--        full merge of every available input.

evalB :: (SigInP p x) => DT -> B w (S p (B w x y) :&: x) (y :|: S p ())
evalB dt = synchB >>> forceDelayB >>> undefined 



-- RESULTS LINK FACTORY.
--   One result link for each dynamic behavior.
--   All links are merged into target `Lnk y`.
--   Dead outputs are preserved. 
--   Touches on multiple merge targets:
--     clear touch status on update
--     propagate only if no touched elements remain
--     GC from collection only on propagation
mkMergeLnkFactory :: Lnk y -> IO (Lnk y)
mkMergeLnkFactory = undefined
-- CONCERN:
--   At moment, don't have any way to produce an `Lnk x` from
--   knowledge `SigInP x`. 
-- Probably need a typeclass for constructing Lnk x.
--   some sort of membrane class similar to SigInP



