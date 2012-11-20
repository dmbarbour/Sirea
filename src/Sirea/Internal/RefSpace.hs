
-- Seems GHC Base is deprecating Data.HashTable at 7.6.
--
-- This module provides a simple wrapper for a table-like 
-- structure where one can:
--
--   * allocate a `new` reference
--   * manipulate that reference to put or remove a value.
--   * observe the collection of references that have value.
--
-- There is no need to explicitly manage keys or addresses. The use
-- of references is more capability-secure, simple, and safe.
--
-- This fulfills need for tables in Sirea. It has a minor weakness, 
-- that each Ref will keep the full RefSpace in memory. But this is
-- not an issue for Sirea.
--
-- Nothing about a RefSpace is mt-safe. The RefSpace and all Refs
-- should be controlled by one partition.
--
module Sirea.Internal.RefSpace 
    ( RefSpace, Ref
    , newRefSpace
    , newRef
    , refPut, refGet
    , listActiveRefs
    ) where

import Data.Int (Int64)
import Data.IORef
import qualified Data.HashTable as HT
import Control.Applicative
import Control.Monad (void)

type Key = Int64
data RefSpace v = RS 
    { rs_store :: !(HT.HashTable Key v)
    , rs_kgen  :: !(IORef Key)
    }
data Ref v = RF 
    { rf_key   :: {-# UNPACKED #-} !Key 
    , rf_space :: !(RefSpace v)
    }

newRefSpace :: IO (RefSpace v)
newRefSpace = RS <$> newHT <*> newKGen
    where newHT = HT.new (==) fromIntegral
          newKGen = newIORef 60000

newKey :: RefSpace v -> IO Key
newKey rs = 
    let kg = rs_kgen rs in
    readIORef kg >>= \ n ->
    let n' = succ n in
    n' `seq` writeIORef kg n' >>
    return n'

rsPut :: RefSpace v -> Key -> Maybe v -> IO ()
rsPut rs k Nothing  = HT.delete (rs_store rs) k
rsPut rs k (Just v) = void $ HT.update (rs_store rs) k v

rsGet :: RefSpace v -> Key -> IO (Maybe v)
rsGet = HT.lookup . rs_store

newRef :: RefSpace v -> IO (Ref v) 
newRef rs = RF <$> newKey rs <*> pure rs

refPut :: Ref v -> Maybe v -> IO ()
refPut rf = rsPut (rf_space rf) (rf_key rf)

refGet :: Ref v -> IO (Maybe v)
refGet rf = rsGet (rf_space rf) (rf_key rf)

-- obtain all refs with active values, along with
-- those values at the time of call. not mt-safe.
listActiveRefs :: RefSpace v -> IO [(Ref v, v)]
listActiveRefs rs = map ktor <$> HT.toList (rs_store rs) 
    where ktor (k,v) = (RF k rs, v)



