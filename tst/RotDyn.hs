{-# LANGUAGE TypeOperators #-}

-- | Test of long-life for dynamics, via three rotations:
--    rotate 5 dynamic behaviors
--    rotate 6 inputs on left
--    rotate 7 inputs on right
-- for a total 210 combinations based on clock. 
module Main 
    ( main 
    ) where

import Sirea.Prelude
import Sirea.Clock
import Sirea.Time


rotate567 :: BCX w (S p ()) (S p Int :&: S p Int :&: S p Int)
rotate567 = bclockOfFreq 3 >>> bfmap tmNanos >>> bfmap (`div` 333333333) >>>
            (bfmap (`mod` 5) &&& bfmap (`mod` 6) &&& bfmap (`mod` 7)) >>>
            (bfmap fromInteger *** bfmap fromInteger *** bfmap fromInteger)

add, sub, mul, pow, ssq :: BCX w (S p Int :&: S p Int) (S p Int)
add = bzipWith (+)
sub = bzipWith (-)
pow = bzipWith (^)
mul = bzipWith (*)
ssq = bzipWith $ \ b c -> (b*b) + (c*c)

nToF :: Int -> BCX w (S p Int :&: S p Int) (S p Int)
nToF 0 = add
nToF 1 = sub
nToF 2 = pow
nToF 3 = mul
nToF 4 = ssq
nToF _ = error "illegal behavior"

nToFEval :: BCX w (S p Int :&: S p Int :&: S p Int) (S p Int)
nToFEval = bfirst (bfmap nToF) >>> bevalb 0 >>> bright (bconst 999) >>> bmerge

zipAll :: BCX w (S p a :&: S p b :&: S p c) (S p (a,(b,c)))
zipAll = bsecond bzip >>> bzip

main :: IO ()
main = runSireaApp $ bvoid $
    rotate567 >>> (zipAll &&& nToFEval) >>> bzip >>> 
    bprint show

