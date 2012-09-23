
-- Seems GHC Base is getting rid of Data.HashTable at 7.6.
-- But, with how cabal is, I'd rather avoid another package dependency.
-- So, here is my own simplified definition of HashTable.
--
-- Since Sirea only needs int-keyed hashtables (since it provides its
-- own keys) this is specialized on Int32.
module Sirea.Internal.HashTable 
    ( HashTable
    , new
    , update
    , delete
    , toList
    ) where

data HashTable e = HT
    { ht_count :: IORef Int
    , 
    }


