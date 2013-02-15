{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module provides the B type behaviors, the primary behavior
-- kind exposed to clients of Sirea. These behaviors operate in IO 
-- and with a global resource context (WCX). The runSireaApp action
-- will begin executing a B type behavior. 
--
-- See Also:
--   Sirea.Activate - to activate a B type behavior
--   Sirea.PCX - for use of use of partitioned resource contexts
-- 
module Sirea.B
    ( B
    , wrapB
    , unwrapB
    ) where

import Prelude hiding ((.),id)
import Data.Typeable
import Control.Applicative
import Control.Category
import Sirea.Internal.BCross (crossB)
import Sirea.Behavior
import Sirea.Trans.Static 
import Sirea.Partition
import Sirea.ResourceContext
import Sirea.Internal.B0 -- B0 abstract type and instances

-- | The primary, concrete behavior implementation provided by Sirea.
newtype B x y = B { fromB0 :: StaticB (WrappedArrow (->) PCX W) (B0 IO) x y } 
    deriving ( Category, BFmap, BProd, BSum, BDisjoin
             , BZip, BSplit, BTemporal, BPeek, Behavior )
    -- NOT deriving: BDynamic, BCross

instance Typeable2 B where
    typeOf2 _b = mkTyConApp tcBCX [typeOf1 (getM _b)]
        where tcBCX = mkTyCon3 "sirea-core" "Sirea.BCX" "BCX"
              getM :: B m x y -> m ()
              getM _ = undefined

wrapB :: (PCX W -> B0 IO x y) -> B x y
wrapB =  B . wrapStatic . WrapArrow

unwrapB :: B x y -> (PCX W -> B0 IO x y)
unwrapB = unwrapArrow . unwrapStatic . fromB0

instance BCross B where
    bcross = wrapB crossB

instance BDynamic B (B0 IO) where
    beval = wrapB . const . beval

instance BDynamic B B where
    beval dt = wrapB $ \ cw -> 
        bfirst (bfmap (`unwrapB0` cw)) >>> beval dt


