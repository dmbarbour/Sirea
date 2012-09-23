
-- Seems GHC Base is getting rid of Data.HashTable at 7.6.
-- With how cabal is, I'd rather avoid a package dependency.
-- So, here is my own simplified definition of HashTable.
--
-- Since Sirea only needs int-keyed hashtables (since it provides its
-- own keys) this is specialized on Int.
module Sirea.Internal.HashTable 
    ( HashTable
    , new
    , update
    , delete
    , toList
    ) where



