-- some tests for behaviors.
module Main 
    ( main 
    ) where

import Sirea.Prelude
import Sirea.UnsafeOnUpdate
import Control.Exception

bprint :: (Show a, Eq a) => BCX w (S P0 a) (S P0 a)
bprint = unsafeOnUpdateBCX mkPrinter
    where mkPrinter _ = return printer
          printer _ = maybe (return ()) print

bHelloWorld :: BCX w (S P0 ()) (S P0 ())
bHelloWorld = bvoid $ bconst "Hello, World!" >>> bprint 

main :: IO ()
main = 
    print "before hw app" >>
    runSireaApp (bHelloWorld >>> bUnsafeExit) >>
    print "after hw app"
   


