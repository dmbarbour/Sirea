{-# LANGUAGE TypeOperators #-}

-- | Test that computes fibonacci values in a naive, recursive manner
-- using RDP structure. This is more a proof-of-ability and a test for
-- dynamic behaviors. Dynamic behaviors should be used sparingly in 
-- practical RDP.
module Main 
    ( main 
    ) where

import Sirea.Prelude
import Sirea.Clock
import Sirea.Time

fib :: BCX w (S P0 Int) (S P0 Int)
fib = (bfmap dynFib &&& (bfmap pred >>> (bfwd &&& bfmap pred))) >>> 
       bevalb 0 >>> bright (bconst (-1)) >>> bmerge

dynFib :: Int -> BCX w (S P0 Int :&: S P0 Int) (S P0 Int)
dynFib n = if (n < 2) then bfst >>> bconst n
                      else (fib *** fib) >>> bzipWith (+)

fibPrint :: BCX w (S P0 Int) (S P0 ())
fibPrint = (bfwd &&& fib) >>> bzipWith showFib >>> bprint_

showFib :: Int -> Int -> String
showFib n fibn = "Fib(" ++ show n ++ ") = " ++ show fibn

-- rotate numbers from 0..9 repeatedly, at 1 Hz 
rotateI :: BCX w (S p ()) (S p Int)
rotateI = bclockOfFreq 1 >>> bfmap tkI
    where tkI = fromInteger . (`div` nInnerPeriod) . (`mod` nOuterPeriod) . tmNanos
          nOuterPeriod = 10000000000  -- 10 seconds
          nInnerPeriod =  1000000000  -- 1 second

rotateFib :: BCX w (S P0 ()) (S P0 ())
rotateFib = rotateI >>> fibPrint

main :: IO ()
main = runSireaApp $ bconst 15 >>> fibPrint >>> bUnsafeExit

