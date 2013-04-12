
module Main where

import Sirea.Prelude

main = runSireaApp $ record |*| report 

record = btickOfFreq 3 >>> bsplitOn even >>> bleft (bconst () >>> btimeStamp "r")
report = btimeStampMon "r" >>> bprint


