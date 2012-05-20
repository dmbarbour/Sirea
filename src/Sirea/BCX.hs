{-# LANGUAGE GeneralizedNewtypeDeriving, Rank2Types #-}


-- | Behavior with Context.
--
-- BCX distributes a value representing an application's resource
-- context, a `PCX`, to every behavior component that needs it. The
-- resource context is for volatile resources: threads, connections, 
-- caches, demand monitors, stateless stability models. Persistent 
-- resources often need volatile proxies. (Application state should
-- be backed by persistent resources.)
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
    , unsafeLinkBCX
    ) where

import Prelude hiding ((.),id)
import Control.Applicative
import Control.Category
import Control.Arrow
import Sirea.Behavior
import Sirea.Partition
import Sirea.Trans.Static 
import Sirea.B
import Sirea.PCX
import Sirea.Link

-- WithPCX is a functor and applicative of values that take a PCX.
type WithPCX w = WrappedArrow (->) (PCX w)

-- BCXW is BCX in a particular world w.
newtype BCXW w x y = BCXW { fromBCXW :: StaticB (WithPCX w) (B w) x y } 
    deriving ( Category, BFmap, BProd, BSum, BDisjoin
             , BZip, BSplit, BTemporal, BPeek, Behavior, BScope )

unwrapBCXW :: BCXW w x y -> PCX w -> B w x y
unwrapBCXW = unwrapArrow . unwrapStatic . fromBCXW

wrapBCXW :: (PCX w -> B w x y) -> BCXW w x y
wrapBCXW =  BCXW . wrapStatic . WrapArrow

-- Special Cases:
--   BDynamic - need BCX B, BCX BCX
--   BCross - initial instance
--   BEmbed

-- possibility: only hide `w` at the toplevel SireaApp.

-- | The BCX type is essentially:
--
--       type BCX x y = forall w . PCX w -> B w x y
--
-- BCX is a behavior that can work in any world w, given a partition
-- context (PCX w) to represent or proxy resources in that world. 
type BCX x y = forall w . BCXW w x y

toBCXW :: BCX x y -> BCXW w x y
toBCXW b = b

--fromBCXW :: (forall w 

unwrapBCX :: BCX x y -> PCX w -> B w x y
unwrapBCX = unwrapBCXW . toBCXW

wrapBCX :: (forall w . PCX w -> B w x y) -> BCX x y
wrapBCX = undefined

unsafeLinkBCXW :: (PCX w -> MkLnk w x y) -> BCXW w x y
unsafeLinkBCXW = undefined

unsafeLinkBCX :: (forall w. (PCX w -> MkLnk w x y)) -> BCX x y
unsafeLinkBCX = undefined

-- Possibility:
--  support "sub-context" behaviors
--  sort of like running a separate application
--  (except you can provide some capabilities via 
--   dynamic behaviors).
--
-- Possibility:
--  need a way to get `values` in the current context?
--   i.e. build a BCX as a value to provide capability.
