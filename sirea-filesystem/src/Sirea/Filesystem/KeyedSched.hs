

-- Keyed scheduler, exists to ensure that no more than one task is
-- active per 'key' at a time. In this context, a key is likely a
-- filepath or directory path. Wraps another scheduler.
module Sirea.Filesystem.KeyedSched 
    ( newKeyedSched
    ) where

import Control.Monad (join, void)
import Control.Exception (finally, assert)
import Data.IORef
import qualified Data.Map as M

data KSched k = KS !(IORef (WorkMap k)) !Sched
type Sched = Work -> IO ()
type WorkMap k = M.Map k [Work]
type Work = IO ()

newKeyedSched :: (Ord k) => (Work -> IO ()) -> IO (k -> Work -> IO ())
newKeyedSched sched =
    newIORef M.empty >>= \ rf ->
    let ks = KS rf sched in
    return (addKeyedWork ks)

addKeyedWork :: (Ord k) => KSched k -> k -> Work -> IO ()
addKeyedWork ks@(KS rf _) k w = join $ atomicModifyIORef rf addw where
    addw m0 = addw' m0 (M.lookup k m0) 
    addw' m0 Nothing = (M.insert k [] m0, initKeyedWork ks k w)
    addw' m0 (Just ws) = (M.insert k (ws ++ [w]) m0, return ())

returnKey :: (Ord k) => KSched k -> k -> IO ()
returnKey ks@(KS rf _) k = join $ atomicModifyIORef rf rel where
    rel m0 = rel' m0 (M.lookup k m0) 
    rel' m0 Nothing = assert False $ (m0,return ()) -- illegal state
    rel' m0 (Just []) = (M.delete k m0, return ()) -- key released
    rel' m0 (Just (w:ws)) = (M.insert k ws m0, initKeyedWork ks k w)

initKeyedWork :: (Ord k) => KSched k -> k -> Work -> IO ()
initKeyedWork ks@(KS _ sched) k w = sched (w `finally` returnKey ks k)
    


