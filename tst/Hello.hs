-- some tests for behaviors.
module Main 
    ( main 
    ) where

import Sirea.Prelude

bHelloWorld :: BCX w (S P0 ()) (S P0 ())
bHelloWorld = bvoid $ bconst "Hello, World!" >>> bprint id

main :: IO ()
main = 
    print "before hw app" >>
    runSireaApp bHelloWorld >>
    print "after hw app"
   


