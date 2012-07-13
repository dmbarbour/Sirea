
-- | Utility behaviors that lack a better home.
module Sirea.Utility
    ( bprint
    ) where

import Sirea.Behavior
import Sirea.BCX (BCX)
import Sirea.Partition (P0)
import Sirea.UnsafeOnUpdate (unsafeOnUpdateBCX)
import Sirea.PCX 
import Sirea.Time
import Data.Typeable
import Data.IORef
import Control.Monad (when, liftM)


-- | For debugging, it's often useful to dump information to the
-- console. The developer provides the show function (a -> String),
-- and each unique message is printed on a line. Redundant messages
-- will generally not be reprinted unless a fair amount of time has
-- passed, so each message should be complete in its meaning.
-- 
bprint :: (a -> String) -> BCX w (S P0 a) (S P0 a)
bprint showFn = bvoid $ bfmap showFn >>> unsafeOnUpdateBCX mkPrinter
    where mkPrinter = return . mbPrint . pb_list . findInPCX
          mbPrint _ _ Nothing = return ()
          mbPrint pbL t (Just msg) = addToPrinter pbL t msg

addToPrinter :: IORef [(T,String)] -> T -> String -> IO ()
addToPrinter pbl t msg = 
    readIORef pbl >>= \ lRC ->
    let (lRC', bPrint) = updatelRC t msg lRC in
    writeIORef pbl lRC' >>
    when bPrint (putStrLn msg)

-- reprint messages if they are repeated after a while.
-- this allows GC of old messages, and provides simple 
-- idempotence.
dt_print_expire :: DT
dt_print_expire = 6.0 

updatelRC :: T -> String -> [(T,String)] -> ([(T,String)],Bool)
updatelRC t msg lRC = 
    let (lRC',bPrint) = foldr fn ([],True) lRC in
    if bPrint then ((t,msg):lRC',True)
              else (lRC',False)
    where tExpire = subtractTime t dt_print_expire
          fn (tx,mx) (l,b) =
            if (tx < tExpire) then (l,b) else
            if (mx == msg) then (l,False) else
            ((tx,mx):l,b)

newtype PrintBuffer = PrintBuffer { pb_list  :: IORef [(T,String)] }
instance Typeable PrintBuffer where
    typeOf _ = mkTyConApp typb []
        where typb = mkTyCon3 "sirea-core" "Sirea.Utility.Internal" "PrintBuffer"
instance Resource PrintBuffer where
    locateResource _ = liftM PrintBuffer $ newIORef [] 
           
-- TODO:
--   assertions
--   maybe some sort of quickcheck or search-like signal for coverage testing



