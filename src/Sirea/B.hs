{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}


-- | Behaviors with IO.
--
-- BCX distributes a value representing an application's resource
-- context, a `PCX`, to every behavior component that needs it. The
-- resource context models volatile resources (threads, connections,
-- cache, demand monitors) and proxies to persistent resources.
--
-- Conceptually, BCX is behavior with ambient authority to external
-- resources and FFI, whereas type B should never be used for direct
-- access to a resource. (Type B is for computation only.)
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
    ) where

import Prelude hiding ((.),id)
import Data.Typeable
import Control.Applicative
import Control.Category
--import Control.Arrow
import Sirea.Internal.BCross (crossB)
import Sirea.Behavior
import Sirea.Trans.Static 
import Sirea.Partition
import Sirea.PCX
import Sirea.Internal.B0

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
newtype B m x y = B { from0 :: StaticB (WithPCX w) B0 m x y } 
    deriving ( Category, BFmap, BProd, BSum, BDisjoin
             , BZip, BSplit, BTemporal, BPeek, Behavior )
    -- NOT deriving: BDynamic, BCross

instance Typeable2 (B m) where
    typeOf2 _ = mkTyConApp tcBCX []
        where tcBCX = mkTyCon3 "sirea-core" "Sirea.BCX" "BCX"

unwrapBCX :: B m x y -> (PCX w -> B x y)
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


