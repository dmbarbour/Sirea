-- clock (on console, for now)
-- todo: add a UI clock.
module Main 
    ( main 
    ) where

import Sirea.Prelude
import Sirea.Clock
import Sirea.Time
import Sirea.TimeTrigger
import Control.Exception (assert)

-- a better way to show the clock...
timeString :: T -> String
timeString t =
    let nDay = tmNanos t in
    let sDay     = nDay `div` 1000000000 in
    let (mDay,s) = sDay `divMod` 60 in
    let (hDay,m) = mDay `divMod` 60 in
    s2 hDay ++ ":" ++ s2 m ++ ":" ++ s2 s 
    where s2 x = assert ((x >= 0) && (x < 100)) $
                 if x < 10 
                    then "0" ++ show x
                    else show x

ttshow (t,b) = timeString t ++ " -> " ++ show b
bttshow = bprintWith ttshow

bTT :: DT -> B (S P0 ()) (S P0 ())
bTT dt = bvoid $ bclockSeconds >>> bfmap (`addTime` dt) >>> (bfwd &&& btimeTrigger) >>> bzip >>> bttshow

bTT0 = bTT 0
bTT1 = bTT 1
bTT2 = bTT 0.5


main :: IO ()
main = 
    print "before clock app" >>
    runSireaApp bTT2 >>
    print "after clock app"
   


