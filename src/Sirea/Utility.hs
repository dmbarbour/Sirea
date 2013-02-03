{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, TypeOperators, GADTs #-}

-- | Utility behaviors that lack a better home. 
module Sirea.Utility
    ( bprint, bprint_
    , BUndefined(..), bundefined
    , BFChoke(..)
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


-- | Print allows developer to provide show function (a -> String)
-- and preserves the type of the input. This makes it easier to
-- inject bprint into a behavior for debugging.
bprint :: (a -> String) -> BCX w (S P0 a) (S P0 a)
bprint showFn = bvoid $ bfmap showFn >>> bprint_
  -- TODO: switch to demand monitor + agent resource 
  -- for console printing
bprint_ :: BCX w (S P0 String) (S P0 ())
bprint_ = unsafeOnUpdateBCX mkPrinter >>> bconst ()
    where mkPrinter = return . mbPrint . pb_list . findInPCX
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
           

{- IDEA: a volatile, stateful `timestamp` behavior
          i.e. String -> BCX w (S p ()) (S p T)

   State is the time value when initially activated.
   Holds that state until deactivated.
   Basically reports time of contiguous activation.
   Since naturally volatile, no persistence needed.
   Could be useful for integrating tuple spaces, etc.
-}




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


-- | Developers in RDP cannot create closed loops, but they can make
-- open feedback loops. In some cases, these might anticipate far 
-- into the future - a waste of time and memory. Use of `bfchoke`
-- can address this by restricting updates that are too far ahead of
-- stability. (You can specify how far is too far.)
--
-- For example, a simple demonstration cycle for demand monitors is:
--   tstCycle :: BCX w (S P0 ()) (S P0 ())
--   tstCycle = snd dm >>> bdelay 1.0 >>> bfmap addOne >>> bprint show >>> fst dm
--       where dm = demandMonitor "tstCycle"
--             addOne = succ . maximum . ((0::Int):)
-- This will print 1,2,3,... and so on, one number per second.
--
-- However, it also aggregates memory, computing a deep future. Use
-- bfchoke to prevent this aggregation, e.g.:
--   snd dm >>> bdelay 1.0 >>> bfchoke 9.0 >>> bfmap addOne >>> bprint show >>> fst dm
-- This will only compute nine seconds ahead then wait for real-time to catch up.
--
class BFChoke b where bfchoke :: DT -> b (S p a) (S p a)
instance BFChoke B where bfchoke = fchokeB
instance BFChoke (BCX w) where bfchoke = wrapBCX . const . bfchoke


fchokeB :: DT -> B (S p a) (S p a)
fchokeB dt = unsafeLinkB mkln where
    mkln LnkDead = return LnkDead
    mkln (LnkSig lu) = 
        newIORef Nothing >>= \ rf ->
        return (LnkSig (lnFChoke rf dt lu))

lnFChoke :: IORef (Maybe (T,Sig a)) -> DT -> LnkUp a -> LnkUp a
lnFChoke rf dt lu = LnkUp touch update idle where
    touch = ln_touch lu
    update tS tU su =
        readIORef rf >>= \ mbS ->
        case mbS of
            Nothing -> maybeDeliver tS tU su
            Just (tU0,su0) ->
                if (tU0 >= tU) then maybeDeliver tS tU su
                else maybeDeliver tS tU0 (s_switch su0 tU su)
    maybeDeliver tS tU su =
        let bDeliver = case tS of
                DoneT -> True
                StableT tm -> (tU < tm `addTime` dt)
        in
        if bDeliver then writeIORef rf Nothing >>
                         ln_update lu tS tU su
                    else writeIORef rf (Just (tU,su)) >>
                         ln_idle lu tS
    idle tS =
        readIORef rf >>= \ mbS ->
        case mbS of
            Nothing -> ln_idle lu tS 
            Just (tU0,su0) -> maybeDeliver tS tU0 su0    


