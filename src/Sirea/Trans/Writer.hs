{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}

-- | Writer - extends a behavior with an implicit output, which must
-- be written on every step. Developers must specify the behavior to
-- combine parallel write operations, e.g. at zip and zap.
--
-- RDP really doesn't often need the writer monad; it can write as a
-- side-effect easily enough. But it may be useful, in some cases,
-- to encapsulate the writing behavior in a pure structure. 
module Sirea.Trans.Writer
    ( WriterB
    , wrapWriter, unwrapWriter
    ) where

import Prelude hiding (id,(.))
import Control.Category
import Sirea.Behavior
import Sirea.Partition

newtype WriterB w b x y = WriterB (b x (w :&: y))

wrapWriter :: b x (w :&: y) -> WriterB w b x y
wrapWriter = WriterB

unwrapWriter :: WriterB w b x y -> b x (w :&: y)
unwrapWriter (WriterB b) = b




