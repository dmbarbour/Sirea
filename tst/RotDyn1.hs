{-# LANGUAGE TypeOperators #-}

-- | Test of long-life for dynamics, via three rotations:
--    rotate 5 dynamic behaviors
--    rotate 7 inputs
-- for a total 35 combinations based on clock. 
module Main 
    ( main 
    ) where

import Sirea.Prelude
import Sirea.Clock
import Sirea.Time


rotate57 :: BCX w (S p ()) (S p Int :&: S p Int)
rotate57 = bclockOfFreq 0.5 >>> bfmap tmNanos >>> bfmap (`div` 2000000000) >>>
            (bfmap (`mod` 5) &&& bfmap (`mod` 7)) >>>
            (bfmap fromInteger *** bfmap fromInteger)

add, sub, mul, pow, ssq :: BCX w (S p Int) (S p Int)
add = bfmap (+ 6)
sub = bfmap (flip (-) 6)
pow = bfmap (flip (^) (6 :: Int))
mul = bfmap (* 6)
ssq = bfmap (\ x -> x*x + 6*6)

nToF :: Int -> BCX w (S p Int) (S p Int)
nToF 0 = add
nToF 1 = sub
nToF 2 = pow
nToF 3 = mul
nToF 4 = ssq
nToF _ = error "illegal behavior"

nToFEval :: BCX w (S p Int :&: S p Int) (S p Int)
nToFEval = bfirst (bfmap nToF) >>> bevalb 0 >>> bright (bconst 999) >>> bmerge

main :: IO ()
main = runSireaApp $ bvoid $
    rotate57 >>> (bzip &&& nToFEval) >>> bzip >>> 
    bprint show

