{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, TypeOperators, GADTs #-}

-- | Utility behaviors that lack a better home. 
module Sirea.Utility
    ( bprint
    , bundefined
    ) where

import Sirea.Behavior
import Sirea.B (B)
import Sirea.Internal.B0
import Sirea.Partition (P0)
import Sirea.UnsafeOnUpdate 
import Sirea.PCX 
import Sirea.Time
import Sirea.Signal
import Sirea.UnsafeLink

import Sirea.Internal.Tuning (dtPrintExpire)
import Data.Typeable
import Data.IORef
import Control.Monad (when, liftM)

{- IDEA: a more useful, more declarative console?
     Rejected: Console input isn't suitable for persistent, reactive
     models like Sirea. A user-input file is much more promising as
     primitive input models go.
 -}

-- TODO: a TimeStamp state behavior:
--    String -> B (S p ()) (S p T)
--   Returns a timestamp for the start of a period of contiguous activity.
--   Inherently volatile. No need for persistence.


-- | Print allows developer to provide show function (a -> String)
-- and preserves the type of the input. This makes it easier to
-- inject bprint into a behavior for debugging.
bprint :: (Show a) => B (S P0 a) (S P0 a)
bprint showFn = bvoid $ bfmap show >>> bprint_

  -- TODO: switch to demand monitor + agent resource 
  -- for console printing
bprint_ :: B (S P0 String) (S P0 String)
bprint_ = unsafeOnUpdateBCX mkPrinter >>> bconst ()
    where mkPrinter cw = findInPCX cw >>= return . mbPrint . pb_list
          mbPrint _ _ Nothing = return ()
          mbPrint pbL t (Just msg) = addToPrinter pbL t msg

addToPrinter :: IORef [(T,String)] -> T -> String -> IO ()
addToPrinter pbl t msg = 
    readIORef pbl >>= \ lRC ->
    let (lRC', bPrint) = updatelRC t msg lRC in
    writeIORef pbl lRC' >>
    when bPrint (putStrLn msg)


updatelRC :: T -> String -> [(T,String)] -> ([(T,String)],Bool)
updatelRC t msg lRC = 
    let (lRC',bPrint) = foldr fn ([],True) lRC in
    if bPrint then ((t,msg):lRC',True)
              else (lRC',False)
    where tExpire = t `subtractTime` dtPrintExpire
          fn r@(tx,mx) (l,b) =
            if (tx < tExpire) then (l,b) else
            let b' = b && (msg /= mx) in
            (r:l,b')

newtype PrintBuffer = PrintBuffer { pb_list  :: IORef [(T,String)] } deriving (Typeable)
instance Resource P0 PrintBuffer where
    locateResource _ _ = liftM PrintBuffer $ newIORef [] 
           

-- | bundefined - exploratory programming often involves incomplete
-- behaviors. `bundefined` serves a similar role to `undefined` in
-- pure Haskell functions, but can work with RDP's compilation and
-- anticipation models.
bundefined :: (SigInP p y) => B (S p x) y
bundefined = bconst () >>> undefinedB

-- undefinedB is only live code if there is demand on `y`.
-- This would be unsafe without `y` being entirely in p.
-- 
-- Here `undefinedB` fails if it ever stabilizes on an active input
-- signal, but can accept temporary activity so long as it's in the
-- unstable future. undefinedB will not block dead code elimination.
-- 
undefinedB :: (SigInP p y) => B (S p ()) y 
undefinedB = unsafeOnUpdateBL (const $ return undefinedIO) >>> 
             (wrapB . const) nullB0
    where undefinedIO _ Nothing = return () -- inactive signals are okay
          undefinedIO _ (Just _) = fail "undefined behavior activated!"

-- nullB0 will idle instead of update, promising inactivity to all
-- downstream components. Since undefinedB will fail before this
-- promise is ever broken, it's actually a valid promise.
nullB0 :: (Monad m, SigInP p y) => B0 m (S p ()) y
nullB0 = mkLnkB0 lc_dupCaps (return . sendNothing)

-- undefinedB is a bit more sophisticated than just dropping signal.
-- Instead, it forwards an idling operation to downstream clients.
sendNothing :: (Monad m) => Lnk m y -> Lnk m (S p ())
sendNothing LnkDead = LnkDead
sendNothing (LnkProd x y) = (sendNothing x) `lnPlus` (sendNothing y)
sendNothing (LnkSum x y)  = (sendNothing x) `lnPlus` (sendNothing y)
sendNothing (LnkSig lu) = LnkSig (LnkUp touch update idle cycle) where
    touch = ln_touch lu 
    update tS _ _ = ln_idle lu tS
    idle = ln_idle lu
    cycle = ln_cycle lu

lnPlus :: (Monad m) => Lnk m (S p a) -> Lnk m (S p a) -> Lnk m (S p a)
lnPlus LnkDead y = y
lnPlus x LnkDead = x
lnPlus x y = LnkSig (ln_lnkup x `ln_append` ln_lnkup y)





