{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, TypeOperators, GADTs #-}

-- | Utility behaviors that lack a better home. 
module Sirea.Utility
    ( bprint, bprintWith
    , bundefined
    ) where

import Sirea.Behavior
import Sirea.B
import Sirea.Internal.B0
import Sirea.Internal.B0Impl
import Sirea.Internal.LTypes
import Sirea.Partition (P0)
import Sirea.UnsafeOnUpdate 
--import Sirea.PCX 
--import Sirea.Time
import Sirea.Signal
import Sirea.UnsafeLink
import Data.IORef
import Data.Maybe (isNothing)
import Control.Monad (unless)
import Control.Exception (assert)

{- IDEA: a more useful, more declarative console?
     Rejected: Console input isn't suitable for persistent, reactive
     models like Sirea. A user-input file is much more promising as
     primitive input models go, and more broadly useful for configs.
 -}

{- TODO: a TimeStamp state behavior:
    String -> B (S p ()) (S p T)
   Returns a timestamp for the start of a period of contiguous activity.
   Inherently volatile. No need for persistence.
 -}

-- | Print uses the show function, and forwards the input unaltered.
--      bprint = bprintWith show
-- Note: at the moment, bprint isn't quite complete. It should act
-- as a resource, or upon a resource, to ensure precise logic. I'll
-- fix it up later, but the behavior will change subtly.
bprint :: (Show a) => B (S P0 a) (S P0 a)
bprint = bprintWith show

-- | Provide your own show function for printing.
bprintWith :: (a -> String) -> B (S P0 a) (S P0 a)
bprintWith fn = bvoid $ bfmap fn >>> bprint_

  -- TODO: switch to demand monitor + agent resource for console printing
bprint_ :: B (S P0 String) S1
bprint_ = unsafeOnUpdateB mkPrinter >>> btrivial
    where mkPrinter _ = return (const mbPrint)
          mbPrint Nothing = return ()
          mbPrint (Just msg) = putStrLn msg

-- | bundefined - exploratory programming often involves incomplete
-- behaviors. `bundefined` serves a similar role to `undefined` in
-- pure Haskell functions, but can work within RDP's compilation and
-- anticipation framework.
bundefined :: (SigInP p y) => B (S p x) y
bundefined = bconst () >>> undefinedB

-- might be nice to put some equivalent to 'assert' here, too.

-- undefinedB is only live code if there is demand on `y`.
-- This would be unsafe without `y` being entirely in p.
-- 
-- Here `undefinedB` fails if it ever stabilizes on an active input
-- signal, but can accept temporary activity so long as it's in the
-- unstable future.
--
-- undefinedB might be eliminated unless there is a valid consumer
-- of the signal downstream. 
-- 
undefinedB :: (SigInP p y) => B (S p ()) y 
undefinedB = unsafeLinkBL (const mkTestActivity) >>>
             (wrapB . const) nullB0

mkTestActivity :: LnkUp () -> IO (LnkUp ())
mkTestActivity lu =
    newIORef st_zero >>= \ rf -> 
    return (testActivity rf lu) 

testActivity :: IORef (SigSt ()) -> LnkUp () -> LnkUp ()
testActivity rf lu = LnkUp touch update idle cyc where
    touch = ln_touch lu
    cyc = ln_cycle lu
    idle tS = 
        process (st_idle tS) >>
        ln_idle lu tS
    update tS tU su =
        process (st_update tS tU su) >>
        ln_update lu tS tU su
    process fn =
        readIORef rf >>= \ st0 ->
        let st = fn st0 in
        let stCln = st_clear (st_stable st) st in
        stCln `seq` writeIORef rf stCln >>
        runTest (st_stable st0) (st_stable st) (st_signal st)
    runTest (StableT t0) (StableT tf) sig =
        assert (tf >= t0) $
        let lSigs = takeWhile ((< tf) . fst) $ sigToList sig t0 tf in
        let bInactive = all (isNothing . snd) lSigs in
        unless bInactive $
            fail "undefined behavior activated"
                
-- nullB0 will idle instead of update, promising inactivity to all
-- downstream components. Since undefinedB will fail before this
-- promise is ever broken, it's actually a valid promise.
nullB0 :: (Monad m, SigInP p y) => B0 m (S p ()) y
nullB0 = mkLnkB0 lc_dupCaps (const (return . sendNothing))

-- undefinedB is a bit more sophisticated than just dropping signal.
-- Instead, it forwards an idling operation to downstream clients.
sendNothing :: (Monad m) => LnkM m y -> LnkM m (S p ())
sendNothing LnkDead = LnkDead
sendNothing (LnkProd x y) = (sendNothing x) `lnPlus` (sendNothing y)
sendNothing (LnkSum x y)  = (sendNothing x) `lnPlus` (sendNothing y)
sendNothing (LnkSig lu) = LnkSig (LnkUp touch update idle cyc) where
    touch = ln_touch lu 
    update tS _ _ = ln_idle lu tS
    idle = ln_idle lu
    cyc = ln_cycle lu

lnPlus :: (Monad m) => LnkM m (S p a) -> LnkM m (S p a) -> LnkM m (S p a)
lnPlus LnkDead y = y
lnPlus x LnkDead = x
lnPlus x y = LnkSig (ln_lnkup x `ln_append` ln_lnkup y)


