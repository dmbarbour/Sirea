-- some tests for behaviors.
{-# LANGUAGE TypeOperators #-}
module Main 
    ( main 
    ) where

import Sirea.Prelude
import Sirea.UnsafeLink
import Sirea.UnsafeOnUpdate
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
          doAssert t = maybe (return ()) $ \ (b,a) ->
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

printNums = bvoid $ inject >>> monitor >>> bprint
    where inject = input 12 |*| input 60 |*| input 42 |*| input 108          
          input n = bconst (int n) >>> fst deMon
          monitor = snd deMon
          deMon = demandMonitor "tstNums"

printTimes = bvoid $ inject >>> snd deMon >>> bprint
    where inject = input 1 |*| input 4 |*| input 2 |*| input 3
          input dt = bclockSeconds >>> bfmap (`addTime` dt) >>> fst deMon
          deMon = demandMonitor "times"

printInactive = snd deMon >>> {- pStable >>> -} bprint
    where input n = bconst (int n) >>> fst deMon
          deMon = demandMonitor "pi"

pStable = bvoid (unsafeLinkB_ (const (return p))) where
    p = LnkUp touch update idle cyc 
    touch = return () -- traceIO "touch"
    update tS tU _ = traceIO ("p update " ++ show tS ++ " " ++ show tU) 
    idle tS = traceIO ("p idle   " ++ show tS) 
    cyc _ = traceIO ("cyc") 

main :: IO ()
main = runSireaApp $ allTests

tstCycle :: B (S P0 ()) (S P0 ())
tstCycle = snd dm >>> bdelay 1.0 >>> bfmap addOne >>> bprint >>> fst dm
     where dm = demandMonitor "tstCycle"
           addOne = succ . S.findMax . S.insert (0 :: Int)

-- 'int' is just a type annotation to help inference
int :: Int -> Int
int = id

