{-# LANGUAGE TypeOperators #-}


module Sirea.Trans.Reader
    ( ReaderB
    , wrapReader, unwrapReader
    ) where

import Prelude hiding (id,(.))
import Control.Category
import Control.Applicative
import Sirea.Behavior
import Sirea.Partition


newtype ReaderB r b x y = ReaderB (b (r :&: x) y)

wrapReader :: b (r :&: x) y -> ReaderB r b x y
wrapReader = ReaderB

unwrapReader :: ReaderB r b x y -> b (r :&: x) y
unwrapReader (ReaderB b) = b

