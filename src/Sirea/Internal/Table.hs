
-- Seems GHC Base is deprecating Data.HashTable at 7.6.
--
-- This module is to help prepare for the transition, avoiding
-- direct dependency on Data.HashTable until I'm bored enough
-- with other tasks to write a replacement.
--
-- With how cabal is, I'd rather avoid a package dependency.
-- So, here is my own simplified definition of a Table.
--
-- Table provides O(1) insert and delete. It is essentially
-- an IntMap in IO. 
--
module Sirea.Internal.Table 
    ( Table, Key
    , new
    , put
    , get
    , toList
    ) where

import Data.Int (Int64)
import qualified Data.HashTable as HT
import Control.Monad (void, liftM)

type Key = Int64
newtype Table v = TB { inTB :: HT.HashTable Key v }

new :: IO (Table v)
new = liftM TB newHT
    where newHT = HT.new (==) fromIntegral

put :: Table v -> Key -> Maybe v -> IO ()
put (TB ht) k Nothing = HT.delete ht k
put (TB ht) k (Just v) = void $ HT.update ht k v 

get :: Table v -> Key -> IO (Maybe v)
get = HT.lookup . inTB 

toList :: Table v -> IO [(Key,v)]
toList = HT.toList . inTB 


