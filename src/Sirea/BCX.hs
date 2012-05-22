{-# LANGUAGE GeneralizedNewtypeDeriving, Rank2Types #-}


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
    , unwrapBCX, wrapBCX
    ) where

import Prelude hiding ((.),id)
import Control.Applicative
import Control.Category
--import Control.Arrow
import Sirea.Behavior
import Sirea.Partition
import Sirea.Trans.Static 
import Sirea.B
import Sirea.PCX

-- WithPCX is a functor and applicative of values that take a PCX.
type WithPCX w = WrappedArrow (->) (PCX w)

-- | The BCX type is essentially:
--
--       type BCX w x y = PCX w -> B w x y
--
-- But wrapped for manipulation as a Behavior.
--
-- BCX is a behavior that can work in any world w, given a partition
-- context (PCX w) to represent or proxy resources in that world. 
newtype BCX w x y = BCX { fromBCX :: StaticB (WithPCX w) (B w) x y } 
    deriving ( Category, BFmap, BProd, BSum, BDisjoin
             , BZip, BSplit, BTemporal, BPeek, Behavior, BScope )

unwrapBCX :: BCX w x y -> PCX w -> B w x y
unwrapBCX = unwrapArrow . unwrapStatic . fromBCX

wrapBCX :: (PCX w -> B w x y) -> BCX w x y
wrapBCX =  BCX . wrapStatic . WrapArrow


-- Special Cases:
--   BDynamic - need BCX B, BCX BCX
--   BCross - initial instance
--   BEmbed

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


