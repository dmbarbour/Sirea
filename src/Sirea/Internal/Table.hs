
-- Seems GHC Base is deprecating Data.HashTable at 7.6.
--
-- This module provides a simple wrapper for a table-like 
-- structure where one can:
--
--   * allocate a `new` reference
--   * manipulate that reference to put or remove a value.
--   * observe the collection of references that have value.
--
-- This fulfills all need for tables in Sirea, and has an
-- additional effect of avoiding  
--
module Sirea.Internal.RSpace 
    ( Table, Key
    , new
    , newKey
    , put
    , get
    , toList
    , isEmpty
    ) where

import Data.Int (Int64)
import qualified Data.HashTable as HT
import Control.Monad (void, liftM)

-- Might a more convenient operation be to ge
newtype Key = Key Int64
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

-- might be able to later replace this with something efficient?
isEmpty :: Table v -> IO Bool
isEmpty = liftM null . toList


