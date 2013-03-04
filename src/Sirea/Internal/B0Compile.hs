{-# LANGUAGE TypeOperators, GADTs #-}

module Sirea.Internal.B0Compile
    ( compileB0
    , compileB0s1
    , B0s1(..)
    , compileB0s2
    ) where

import Prelude hiding ((.),id)
import Sirea.Internal.STypes
import Sirea.Internal.B0Type
import Sirea.Internal.LTypes
import Control.Exception (assert)
import Control.Category


-- | Compilation of Sirea `B0` type behaviors. 
--
-- INPUTS:
--   B0 m x y    - behavior to compile
--   LCaps m x   - tracks time, common capabilities
--   LnkM m y     - where the output signal goes
-- OUTPUTS:
--   LCaps m y   - timings of outputs
--   m (LnkM m x) - constructor for input signal
--
-- Note: there is no integration with the Stepper at this point. Any
-- behaviors that need staging via the Stepper should have achieved 
-- it via MkLnk.
compileB0 :: (Monad m) => B0 m x y -> LCapsM m x -> LnkM m y -> (LCapsM m y, m (LnkM m x))
compileB0 bxy lcx lny =
    assert (lc_valid lcx) $
    let (bxy', lcy) = compileB0s1 bxy lcx in
    assert (lc_valid lcy) $
    let lnx = compileB0s2 bxy' lny in
    (lcy, lnx)

-- | This is an initial left-to-right compile within a behavior. It
-- computes the timing properties of the resulting signal, applies 
-- time-dependent transforms (B0_latent).
compileB0s1 :: (Monad m) => B0 m x z -> LCapsM m x -> (B0s1 m x z, LCapsM m z)
compileB0s1 (B0_pipe bxy byz) lcx =
    let (bxy', lcy) = compileB0s1 bxy lcx in
    assert (lc_valid lcy) $
    assert ((not . ln_dead) lcy) $
    let (byz', lcz) = compileB0s1 byz lcy in
    (B0s1_pipe bxy' byz', lcz)
compileB0s1 (B0_first bef) lcx =
    let lce = ln_fst lcx in
    let (bef', lcf) = compileB0s1 bef lce in
    let lcz = LnkProd lcf (ln_snd lcx) in
    (B0s1_first bef', lcz)
compileB0s1 (B0_left bef) lcx =
    let lce = ln_left lcx in
    if (ln_dead lce) -- dead-code elimination for :|: input
        then (B0s1_left deadOnInputB0s1, LnkSum LnkDead (ln_right lcx))
        else let (bef',lcf) = compileB0s1 bef lce in
             assert (not (ln_dead lcf)) $
             let lcz = LnkSum lcf (ln_right lcx) in
             (B0s1_left bef', lcz)
--compileB0s1 (B0_latent fn) lcx =
--    compileB0s1 (fn lcx) lcx
compileB0s1 (B0_mkLnk fn mkLnk) lcx =
    (B0s1_mkLnk (mkLnk lcx), fn lcx)


-- | B0s1 is basically B0 after the first stage compile. The LCaps
-- are applied and processed already, so only Lnk is left. 
data B0s1 m x y where
  B0s1_mkLnk :: (LnkM m y -> m (LnkM m x)) -> B0s1 m x y
  B0s1_pipe  :: B0s1 m x y -> B0s1 m y z -> B0s1 m x z
  B0s1_first :: B0s1 m x x' -> B0s1 m (x :&: y) (x' :&: y)
  B0s1_left  :: B0s1 m x x' -> B0s1 m (x :|: y) (x' :|: y)

instance (Monad m) => Category (B0s1 m) where
    id = B0s1_mkLnk return
    (.) = flip B0s1_pipe

-- | This is the right-to-left pass to build the behavior.
compileB0s2 :: (Monad m) => B0s1 m x z -> LnkM m z -> m (LnkM m x)
compileB0s2 (B0s1_pipe bxy byz) lnz = 
    compileB0s2 byz lnz >>= compileB0s2 bxy 
compileB0s2 (B0s1_first bef) lnz =
    compileB0s2 bef (ln_fst lnz) >>= \ lne ->
    return (LnkProd lne (ln_snd lnz))
compileB0s2 (B0s1_left bef) lnz =
    compileB0s2 bef (ln_left lnz) >>= \ lne ->
    return (LnkSum lne (ln_right lnz))
compileB0s2 (B0s1_mkLnk mkLnk) lnz = mkLnk lnz

-- | deadOnInputB0 simply returns LnkDead. Assumption: already have
-- proven the input is dead. Injected by compileB0s1 when B0_left is
-- dead on input; goal is to prevent unnecessary construction of 
-- resources (such as partition threads).
deadOnInputB0s1 :: (Monad m) => B0s1 m x y
deadOnInputB0s1 = B0s1_mkLnk (const (return LnkDead))



-- TODO (Maybe):
--  Compute maximum internal latency for a behavior.
--  Could be used for shutdown behavior.




