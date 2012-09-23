{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}


-- | Behavior with Context.
--
-- BCX distributes a value representing an application's resource
-- context, a `PCX`, to every behavior component that needs it. The
-- resource context is for volatile resources: threads, connections, 
-- caches, demand monitors, stateless stability models. Persistent 
-- resources often need volatile proxies. (Application state should
-- be backed by external, persistent resources.)
--
-- Using PCX rather than Haskell global space is convenient via BCX,
-- and robust for multiple instances and plugins.
--
-- BCX and PCX enable declarative, type-driven resource acquisition
-- in Sirea. To have two behaviors observe and influence one GLUT
-- window, use the same type to identify the window resources. The
-- behaviors can access the PCX to hook elements together by type.
--
-- Types aren't essential to this design, but are convenient and
-- only weakly constrained by Haskell's module namespace. (You can
-- protect resources against casual access by hiding their types.)
-- 
-- NOTE: RDP is designed for secure programming in object capability
-- languages. A good object capability language will have convenient
-- syntax and module systems for distributing capabilities and for
-- controlling their distribution. This use of global state with BCX
-- irks me. But Haskell is not an object capability language. Sirea
-- needs to be convenient for Haskell.
-- 
module Sirea.BCX
    ( BCX
    , unwrapBCX
    , wrapBCX
    , onNextStepBCX
    ) where

import Prelude hiding ((.),id)
import Data.Typeable
import Control.Applicative
import Control.Category
--import Control.Arrow
import Sirea.Internal.BCross (crossB, stepDelayB, phaseDelayB)
import Sirea.Behavior
import Sirea.Trans.Static 
import Sirea.Partition
import Sirea.PCX
import Sirea.B

-- WithPCX is a functor and applicative of values that take a PCX.
type WithPCX w = WrappedArrow (->) (PCX w)


-- | The BCX type is essentially:
--
--       type BCX w x y = PCX w -> B w x y
--
-- But wrapped for manipulation as a Behavior.
--
-- BCX is a behavior that can work in any world w, given a context
-- (GCX w) to represent or proxy resources in that world. 
newtype BCX w x y = BCX { fromBCX :: StaticB (WithPCX w) B x y } 
    deriving ( Category, BFmap, BProd, BSum, BDisjoin
             , BZip, BSplit, BTemporal, BPeek, Behavior )
    -- NOT deriving: BDynamic, BCross

instance Typeable2 (BCX w) where
    typeOf2 _ = mkTyConApp tcBCX []
        where tcBCX = mkTyCon3 "sirea-core" "Sirea.BCX" "BCX"

unwrapBCX :: BCX w x y -> (PCX w -> B x y)
unwrapBCX = unwrapArrow . unwrapStatic . fromBCX

wrapBCX :: (PCX w -> B x y) -> BCX w x y
wrapBCX =  BCX . wrapStatic . WrapArrow

instance BCross (BCX w) where
    bcross = wrapBCX crossB

instance BDynamic (BCX w) B where
    beval = wrapBCX . const . beval

instance BDynamic (BCX w) (BCX w) where
    beval dt = wrapBCX $ \ cw -> 
        bfirst (bfmap (`unwrapBCX` cw)) >>> beval dt

-- | onNextStepBCX will delay processing until the next runStepper
-- event, and processes updates as though from a remote partition.
-- This can be useful for achieving snapshot consistency relative
-- even to a thread's internal updates, or for receiving updates
-- from worker threads.
-- 
onNextStepBCX :: (Partition p) => BCX w (S p x) (S p x)
onNextStepBCX = wrapBCX $ \ cw -> stepDelayB cw >>> phaseDelayB cw

{-

-- | getContextBCX will lift a partition context into an RDP signal.
-- This is probably a bad idea, since there is very little we can
-- do with most resources except via conventional IO. However, it
-- may be convenient when modeling dynamic behaviors.
--
getContextBCX :: (Typeable p) => BCX w (S p ()) (S p (PCX p))
getContextBCX = wrapBCX $ bconst . findInPCX

-- | loadResourceBCX will a partition resource into an RDP value.
-- The resource is selected by both `p` and `r`. 
--
--     loadResourceBCX = getContextBCX >>> bfmap findInPCX
--
loadResourceBCX :: (Resource r, Typeable p) => BCX w (S p ()) (S p r)
loadResourceBCX = wrapBCX $ bconst . findInPCX . loadPCX
    where loadPCX :: PCX w -> PCX p
          loadPCX = findInPCX

-- | getGlobalContextBCX will lift the global context into an RDP
-- signal. This is almost certainly a bad idea.
getGlobalContextBCX :: BCX w (S p ()) (S p (PCX w))
-}

-- Idea: Wrap the `PCX w` into a `GCX w` for global context. 
--   Idea is (1) to associate resources only with partition contexts.
--           (2) to hinder accidental use of toplevel where PCX is desired.
-- 

-- TODO:
--   BDynamic

-- phaseDelayBCX:
--   delay an operation until the next round of inputs.
--   useful for updates provided by the partition thread.


--
-- To consider and maybe do: something like
--
--   subContext :: (Typeable p) => BCX p x y -> BCX w x y
--   subApp     :: (Partition p) => BCX p (S p ()) (S p ()) -> BCX w (S p ()) (S p ())
--   pushWorld  :: (Partition p, SigInP p x, SigInP p y) => BCX p x y -> BCX w x y
--
-- Difficulties:
--   * world-crossing signals
--   * dynamic behaviors, allowing the sub context to access the parent 
--   * doesn't make a very good sandbox due to ambient authority in Haskell
--
-- Potential variation:
--   PCX w0 -> PCX w1 -> B w0 x y -> B w1 x y
--
-- This would apply a cross-world action on every signal in x and y. However, it 
-- doesn't readily rename the partitions. Would be a very `unsafeCrossWorldsB`.
-- To refine further, might want to rename partitions.
--
-- 


