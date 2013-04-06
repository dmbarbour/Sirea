{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls, NoMonomorphismRestriction #-}

module Main where

import Data.Function (fix)
import Data.Typeable
import Sirea.Prelude

data X deriving Typeable
data Y deriving Typeable
data Z deriving Typeable
type P1 = Pt X
type P2 = Pt Y
type P3 = Pt Z

tst = ini >>> xyz >>> bvoid r0 >>> yzxy >>> r0 where
    ini = bconst "" >>> touchP0
    xyz = x >>> y >>> z
    yzxy = y >>> z >>> x >>> y
    x = cross >>> touchP1
    y = cross >>> touchP2
    z = cross >>> touchP3
    r0 = cross >>> touchP0 >>> bprint

--cross :: (Partition p, Partition p') => B (S p String) (S p' String)
cross = bfmap (++ "->") >>> bcross

--touch :: (Typeable p) => B (S p String) (S p String)
touch = fix $ \ b -> bfmap (++ (pdesc b))

--pdesc :: (Typeable p) => B (S p x) y -> String
pdesc = show . typeOf . getP 

getP :: B (S p x) y  -> p
getP _ = undefined

touchP0 :: B (S P0 String) (S P0 String)
touchP0 = touch

touchP1 :: B (S P1 String) (S P1 String)
touchP1 = touch

touchP2 :: B (S P2 String) (S P2 String)
touchP2 = touch

touchP3 :: B (S P3 String) (S P3 String)
touchP3 = touch

main = runSireaApp tst




