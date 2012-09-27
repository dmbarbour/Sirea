{-# LANGUAGE MultiParamTypeClasses, TypeOperators, GADTs #-}

-- | Utility behaviors that lack a better home. Anything here could
-- have been written by a Sirea client.
module Sirea.Utility
    ( HasPrinter(..), bprint
    , BUndefined(..), bundefined, bundefinedk
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
import Data.Typeable
import Data.IORef
import Control.Monad (when, liftM)

{- IDEA: a more useful, more declarative console.
    Inputs manage a set of values (X=Y) that may be accessed by users.
    Outputs manage a similar set of values. (X=A,B,C)
    Might unify these notions, so user is just acting as a printer?
    Also, some ability to query the console to replay old values.
-}


-- | For debugging, it's often useful to dump information to the
-- console. The basic printer will print each unique message to a 
-- line on the console. Behavior for redundant messages is up to
-- the printer - e.g. replay after a few seconds, or skip them.
class HasPrinter b p where bprint_ :: b (S p String) (S p ())

-- | Print allows developer to provide show function (a -> String)
-- and preserves the type of the input. This makes it easier to
-- inject bprint into a behavior for debugging.
bprint :: (HasPrinter b p, BFmap b, BProd b) 
       => (a -> String) -> b (S p a) (S p a)
bprint showFn = bvoid $ bfmap showFn >>> bprint_

-- BPrint for BCX P0. At the moment: not quite correct for the
-- reprint behavior. Could fix? but demand monitors would help.
-- Anyhow, idea is that it will reprint messages after a few 
-- seconds if they have not been printed recently. 
instance HasPrinter (BCX w) P0 where bprint_ = printB

printB :: BCX w (S P0 String) (S P0 ())
printB = unsafeOnUpdateBCX mkPrinter >>> bconst ()
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
    where tExpire = t `subtractTime` dt_print_expire
          fn r@(tx,mx) (l,b) =
            if (tx < tExpire) then (l,b) else
            let b' = b && (msg /= mx) in
            (r:l,b')

newtype PrintBuffer = PrintBuffer { pb_list  :: IORef [(T,String)] }
instance Typeable PrintBuffer where
    typeOf _ = mkTyConApp typb []
        where typb = mkTyCon3 "sirea-core" "Sirea.Utility.Internal" "PrintBuffer"
instance Resource PrintBuffer where
    locateResource _ = liftM PrintBuffer $ newIORef [] 
           
{- Don't really know how to extract signal at `p`. Maybe a simplified
   variation would work?

-- | BUnit - extract a unit signal. This is actually difficult to
-- achieve generically due to partition types and duration coupling,
-- i.e. can't just invent a unit signal, actually need to acquire it
-- and put it in the right partition. 
--
-- Trivia: Generalized Arrows (from Adam Megacz) require generic
-- ability to reduce signals to unit. Inability to express this is
-- why generalized arrows were eventually rejected for RDP.
--
-- possible interface?
class BUnit b x where
    bunit :: (SigInP p x) => b x (S p ())
-}

-- | BUndefined - exploratory programming often involves incomplete
-- behaviors. `bundefined` serves a similar role to `undefined` in
-- Haskell. Direct use of `undefined` might fail at compile time, 
-- even if embedded in dead code. `bundefined` will only fail if
-- there is stable, active demand for the undefined response.
--
-- Alternatively, one could use:
--
--   bconst undefined >>> bforce (`seq` ())
--
-- But this would only be useful for a single output signal, and it
-- might fail due to anticipation of a future that never stabilizes.
-- 
class BUndefined b where bundefined_ :: (SigInP p y) => b (S p ()) y

bundefined :: (BUndefined b, BFmap b, SigInP p y) => b (S p x) y
bundefined = bconst () >>> bundefined_

bundefinedk :: (BUndefined b, BFmap b, BProd b, SigInP p y) => b (S p x :&: x') y
bundefinedk = bfst >>> bundefined

instance BUndefined B where bundefined_ = undefinedB
instance BUndefined (BCX w) where bundefined_ = (wrapBCX . const) undefinedB

-- undefinedB is only live code if there is demand on `y`.
-- This would be unsafe without `y` being entirely in p.
undefinedB :: (SigInP p y) => B (S p ()) y 
undefinedB = unsafeOnUpdateBL (return undefinedIO) >>> unsafeLinkB mkKeepAlive
    where mkKeepAlive = return . sendNothing
          undefinedIO _ Nothing = return () -- inactive signal is okay
          undefinedIO _ (Just _) = undefined {- Haskell's undefined IO op -}

-- undefinedB is a bit more sophisticated than just dropping signal.
-- Need to send an inactive signal, properly, in order to perform 
-- any merges further on. (hmmm... a bit difficult is dealing with
-- the bcross case!)
sendNothing :: Lnk y -> Lnk (S p ())
sendNothing LnkDead = LnkDead
sendNothing (LnkProd x y) = (sendNothing x) `lnPlus` (sendNothing y)
sendNothing (LnkSum x y)  = (sendNothing x) `lnPlus` (sendNothing y)
sendNothing (LnkSig lu) = LnkSig (LnkUp { ln_touch = touch, ln_update = update })
    where touch = ln_touch lu
          update su = 
            let tStable = su_stable su in
            let st' = case su_state su of
                         Just (_,t) -> Just (s_never,t)
                         Nothing -> Nothing
            in
            let su' = SigUp { su_stable = tStable, su_state = st' } in
            ln_update lu su'

lnPlus :: Lnk (S p ()) -> Lnk (S p ()) -> Lnk (S p ())
lnPlus LnkDead y = y
lnPlus x LnkDead = x
lnPlus (LnkSig lux) (LnkSig luy) = LnkSig luz
    where luz = LnkUp { ln_touch = touch, ln_update = update }
          touch = ln_touch lux >> ln_touch luy
          update su = ln_touch luy >> (ln_update lux su >> ln_update luy su)

-- TODO:
--   bunit :: b x (S p ()).
--   assertions
--   maybe some sort of quickcheck or search-like signal for coverage testing
--     a bit hackish without an associated state model... but maybe we can
--       set up a state resource for this sort of purpose. Hmm. Maybe better
--       to build a proper testing-support library for Sirea. 


