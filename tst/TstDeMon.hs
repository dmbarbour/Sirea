-- some tests for behaviors.
{-# LANGUAGE TypeOperators #-}
module Main 
    ( main 
    ) where

import Sirea.Prelude
import Sirea.UnsafeLink
import Sirea.UnsafeIO
import Sirea.Clock
import Sirea.Time
import Sirea.DemandMonitor
import Control.Monad (unless)
import qualified Data.Set as S
import Debug.Trace

-- assertions
assertb :: (Show a) => String -> (a -> Bool) -> B (S P0 a) (S P0 a)
assertb tstName test = bvoid $ bfmap (\a -> (test a,show a)) >>> unsafeOnUpdateB mkAssert
    where mkAssert _ = return doAssert
          doAssert t (b,a) =
            if b then putStrLn ("PASS: " ++ tstName ++ " @ " ++ show t ++ " (" ++ a ++ ")")
                 else putStrLn ("FAIL: " ++ tstName ++ " @ " ++ show t ++ " (" ++ a ++ ")")

tstInactive = snd amon >>> assertb "tstInactive" (== False)
    where amon = activityMonitor "tstInactive"
tstActive1 = fst amon >>> snd amon >>> assertb "tstActive1" (== True)
    where amon = activityMonitor "tstActive1"
tstActive2 = (fst amon &&& snd amon) >>> bsnd >>> assertb "tstActive2" (== True)
    where amon = activityMonitor "tstActive2"
tstInactive2 = snd dmon >>> assertb "tstInactive2" (S.null)
    where dmon = demandMonitor "tstInactive2"
          inject = bconst (0 :: Int) >>> fst dmon -- to infer type, not used

tstNums = bvoid $ inject >>> monitor >>> assertb "tstNums" ((== [3,4,5,7]) . S.toAscList)
    where inject = input 7 |*| input 3 |*| input 5 |*| input 4          
          input n = bconst (int n) >>> fst deMon
          monitor = snd deMon
          deMon = demandMonitor "tstNums"

allTests = tstInactive |*| tstActive1 |*| tstActive2 |*| tstNums |*| tstInactive2

main :: IO ()
main = runSireaApp $ allTests

tstCycle :: B (S P0 ()) (S P0 ())
tstCycle = snd dm >>> bdelay 0.1 >>> bfmap addOne >>> bprint >>> fst dm
     where dm = demandMonitor "tstCycle"
           addOne = succ . S.findMax . S.insert (0 :: Int)

-- 'int' is just a type annotation to help inference
int :: Int -> Int
int = id

