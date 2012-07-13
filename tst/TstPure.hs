-- some tests for behaviors.
{-# LANGUAGE TypeOperators #-}
module Main 
    ( main 
    ) where

import Sirea.Prelude
import Sirea.UnsafeOnUpdate
import Sirea.Clock
import Sirea.Time
import Control.Monad (unless)

-- assertions
assertb :: (Eq a) => (a -> Bool) -> (a -> String) -> BCX w (S p a) (S p a)
assertb test failMsg = wrapBCX . const $ unsafeOnUpdateB mkAssert 
    where mkAssert = return doAssert
          doAssert _ = maybe (return ()) $ \ a ->
                           unless (test a) (doFail a)
          doFail a = ioError (userError ("FAILURE: " ++ (failMsg a)))



-- rotate from 0..99 then back again, quickly (every 1/10th second)
-- this is intended to serve as a simple variable for tests.
rotateI :: BCX w (S p ()) (S p Int)
rotateI = bclockOfFreq 10 >>> bfmap tkI
    where tkI = fromInteger . (`div` sTenth) . (`mod` sTen) . tmNanos
          sTen = 10000000000   -- 10 seconds
          sTenth = 100000000   -- 100 milliseconds

-- split integers to (Evens :|: Odds) 
splitEvensOdds :: BCX w (S p Int) (S p Int :|: S p Int)
splitEvensOdds = bfmap eitherEvenOrOdd >>> bsplit
    where eitherEvenOrOdd n = if (even n) then Left n else Right n

-- some active tests 
rtst0 = bvoid $ rotateI >>> bprint show
rtst1 = bvoid $ rotateI >>> (bfmap (+1) &&& bfwd) >>> bzipWith (*) >>> bfmap (`div` 2) >>> bprint show
rtst2 = bvoid $ rotateI >>> splitEvensOdds >>> bleft (bfmap (+10)) >>> bmerge >>> bprint show
-- tests to perform:
--  delay and synch
--  
--  anticipation (bpeek)
--  multi-threaded computation - cross, return
--  dead-code elimination
--    on target (bfst/bsnd)
--    on source (binl/binr)
--  split... just test twice with Left and Right?
--  
--  Might be nice to add a few utilities to sirea-core:
--    logging to console, runtime assertions
--  

main :: IO ()
main = runSireaApp rtst0
   

-- HOW will I make it convenient to write tests
-- for Sirea? Perhaps use some sort of `expect`
-- model? use unsafeOnUpdateB to push updates
-- to a channel for unit testing?


-- TESTS TO WRITE:
--  fmap
--  bzip
--  bsplit
--  bsynch
--  

