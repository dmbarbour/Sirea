{-# LANGUAGE GADTs, TypeOperators #-}

-- | Type B0 is the primitive behavior type in Sirea. It operates in
-- a hidden applicative monad of kind 'm'. Hiding the monad ensures 
-- effects are performed with secure capabilities.  
module Sirea.Internal.B0Type
    ( B0(..)
    ) where

import Sirea.Internal.STypes
import Sirea.Internal.LTypes 

-- | B0 m x y describes an RDP behavior that is implemented in a
-- hidden, applicative monad m to help control effects. Access to
-- effects is instead regulated through capabilities.
--
-- RDP behaviors operate under many constraints: spatial idempotence
-- and commutativity, duration coupling, eventless, no accumulation
-- of state over time (i.e. no incremental folds over signals). The
-- limitations make RDP very compositional, but require new idioms
-- and state models to control systems.
--
-- Behaviors compose much like arrows (from Control.Arrow), but are
-- more constrained due to partitioning, asynchrony, and duration
-- coupling. Developers cannot apply Haskell functions at arbitrary
-- points, nor are all functions on signals valid for RDP.
data B0 m x y where
  B0_mkLnk   :: (LCaps m x -> LCaps m y) 
             -> (LCaps m x -> Lnk m y -> m (Lnk m x))
             -> B0 m x y
  B0_pipe    :: B0 m x y -> B0 m y z -> B0 m x z
  B0_first   :: B0 m x x' -> B0 m (x :&: y) (x' :&: y)
  B0_left    :: B0 m x x' -> B0 m (x :|: y) (x' :|: y)
  --B0_latent  :: (LCaps m x -> B0 m x y) -> B0 m x y

-- Thoughts: 
--   Could rewriting improve optimizations for piping B0_mkLnk elements?


