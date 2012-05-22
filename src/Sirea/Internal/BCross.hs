

-- | BCross is the implementation of cross for B and BCX.
--
-- It also contains types associated with partitions and
-- crossB, i.e. used in BCX.
module Sirea.Internal.BCross 
    ( crossB
    ) where

import Data.Typeable
import Sirea.Internal.BTypes
import Sirea.Internal.STypes
import Sirea.Partition
import Sirea.PCX

-- ugh, I hate using `typeOf` on the result types.
-- Need to try fixpoint again.

crossB :: (Partition p1, Partition p2) => PCX w -> B w (S p1 x) (S p2 x)
crossB pcx = undefined
{-
    let typ1 = typeOf (undefined :: p1) in
    let typ2 = typeOf (undefined :: p2) in 
    undefined
-}


