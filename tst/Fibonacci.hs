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

fib :: B (S P0 Int) (S P0 Int)
fib = (bfmap dynFib &&& (bfmap pred >>> (bfwd &&& bfmap pred))) >>> 
       bbeval 0 >>> bright fibFail >>> bmerge
    where fibFail = bfmap $ const (-999999)

dynFib :: Int -> B (S P0 Int :&: S P0 Int) (S P0 Int)
dynFib n = if (n < 2) then bfst >>> bconst n
                      else (fib *** fib) >>> bzipWith (+)

fibPrint :: B (S P0 Int) S1
fibPrint = (bfwd &&& fib) >>> bzipWith showFib >>> bprint >>> btrivial

showFib :: Int -> Int -> String
showFib n fibn = "Fib(" ++ show n ++ ") = " ++ show fibn

-- rotate numbers from 0..9 repeatedly, at 1 Hz 
rotateI :: (Partition p) => B (S p ()) (S p Int)
rotateI = bclockOfFreq 1 >>> bfmap tkI
    where tkI = fromInteger . (`div` nInnerPeriod) . (`mod` nOuterPeriod) . tmNanos
          nOuterPeriod = 10000000000  -- 10 seconds
          nInnerPeriod =  1000000000  -- 1 second

rotateFib :: B (S P0 ()) S1
rotateFib = rotateI >>> fibPrint

main :: IO ()
main = do
    putStrLn "Hit Ctrl+C to exit" 
    runSireaApp $ bconst 11 >>> fibPrint

