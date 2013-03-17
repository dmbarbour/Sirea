-- some tests for behaviors.
module Main 
    ( main 
    ) where

import Sirea.Prelude

bHelloWorld = bconst "Hello, World!" >>> bprint

main :: IO ()
main = 
    print "before hw app (ctrl+c to halt)" >>
    runSireaApp bHelloWorld >>
    print "after hw app"
   


