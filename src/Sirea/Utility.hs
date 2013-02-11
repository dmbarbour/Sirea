{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, TypeOperators, GADTs #-}

-- | Utility behaviors that lack a better home. 
module Sirea.Utility
    ( bprint, bprint_
    , BUndefined(..), bundefined
    -- , BUnit(..)
    ) where

import Sirea.Behavior
import Sirea.B (B)
import Sirea.BCX (BCX, wrapBCX)
import Sirea.Partition (P0)
import Sirea.UnsafeOnUpdate 
import Sirea.PCX 
import Sirea.Time
import Sirea.Link
import Sirea.Signal

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
bprint :: (a -> String) -> BCX w (S P0 a) (S P0 a)
bprint showFn = bvoid $ bfmap showFn >>> bprint_
  -- TODO: switch to demand monitor + agent resource 
  -- for console printing
bprint_ :: BCX w (S P0 String) (S P0 ())
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
           

-- | BUndefined - exploratory programming often involves incomplete
-- behaviors. `bundefined` serves a similar role to `undefined` in
-- Haskell. Direct use of `undefined` might fail when compiling the
-- behavior even if embedded in dead code. bundefined fails only if
-- there is stable, active input signal, and a consumer for the 
-- undefined result.
-- 
class BUndefined b where bundefined_ :: (SigInP p y) => b (S p ()) y

bundefined :: (BUndefined b, BFmap b, SigInP p y) => b (S p x) y
bundefined = bconst () >>> bundefined_

instance BUndefined B where bundefined_ = undefinedB
instance BUndefined (BCX w) where bundefined_ = (wrapBCX . const) undefinedB

-- undefinedB is only live code if there is demand on `y`.
-- This would be unsafe without `y` being entirely in p.
-- 
-- Here `undefinedIO` basically kills the process if we stabilize on an
-- active input signal, but it's okay so long as we don't stabilize an
-- active signal to the undefined behavior. (It's also okay if we never
-- 
undefinedB :: (SigInP p y) => B (S p ()) y 
undefinedB = unsafeOnUpdateBL (return undefinedIO) >>> unsafeLinkB mkKeepAlive
    where mkKeepAlive = return . sendNothing
          undefinedIO _ Nothing = return () -- inactive signal is okay
          undefinedIO _ (Just _) = undefined {- Haskell's undefined IO op -}

-- undefinedB is a bit more sophisticated than just dropping signal.
-- Need to send an inactive signal, properly, in order to perform 
-- any merges further on. 
sendNothing :: Lnk y -> Lnk (S p ())
sendNothing LnkDead = LnkDead
sendNothing (LnkProd x y) = (sendNothing x) `lnPlus` (sendNothing y)
sendNothing (LnkSum x y)  = (sendNothing x) `lnPlus` (sendNothing y)
sendNothing (LnkSig lu) = LnkSig (LnkUp touch update idle) where
    touch = ln_touch lu 
    update tS tU _ = ln_update lu tS tU s_never
    idle tS = ln_idle lu tS

lnPlus :: Lnk (S p a) -> Lnk (S p a) -> Lnk (S p a)
lnPlus LnkDead y = y
lnPlus x LnkDead = x
lnPlus x y = LnkSig (ln_lnkup x `ln_append` ln_lnkup y)


