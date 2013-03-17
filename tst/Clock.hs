-- clock (on console, for now)
-- todo: add a UI clock.
module Main 
    ( main 
    ) where

import Sirea.Prelude
import Sirea.Clock
import Sirea.Time
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

-- using clock, printing based on stability. (Only works for low rate
-- clocks... up to ~20Hz. Higher rate will update in bursts.)
bCC :: B (S P0 ()) (S P0 ())
bCC = bvoid $ bclockSeconds >>> bseq >>> bprintWith timeString

main :: IO ()
main = 
    print "before clock app" >>
    runSireaApp bCC >>
    print "after clock app"
   


