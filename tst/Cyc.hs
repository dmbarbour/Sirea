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

main :: IO ()
main = runSireaApp $ tstCycle |*| (bconst (int 0) >>> bdemand dm)

dm :: String 
dm = "TestCycle"

tstCycle :: B (S P0 ()) (S P0 ())
tstCycle = bmonitor dm >>> bdelay 0.1 >>> bfmap addOne >>> bprint >>> bdemand dm
     where addOne = succ . S.findMax . S.insert (minBound :: Int)

-- 'int' is just a type annotation to help inference
int :: Int -> Int
int = id

