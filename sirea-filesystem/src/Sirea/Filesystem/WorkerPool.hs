
-- A pool of worker threads. Effectively a semaphore. But in this
-- case, new threads will spin up when necessary and self-destruct
-- when they run out of work. (Threads are cheap in Haskell.) The
-- main reason to limit concurrent work is to control resources,
-- e.g. number of open file descriptors and the amount of memory in
-- use but inaccessible due to only partial completion.
--
-- Intended for short-lived work, e.g. to read or write one file.
-- The IO operations should have their own way of calling home when
-- a result is needed. Workers will silently kill exceptions, but
-- the IO ops should catch them first (now asserted for debugging).
--
module Sirea.Filesystem.WorkerPool
    ( newWorkerPool
    ) where

import Data.IORef
import Control.Monad (join, void, liftM)
import Control.Exception (assert, try, SomeException)
import Control.Concurrent (forkIO)

type WPD = Either Int [IO ()]
type WPool = IORef WPD 

newWorkerPool :: Int -> IO (IO () -> IO ())
newWorkerPool n = assert (n > 0) $ liftM addWork $ newIORef (Left n)

addWork :: WPool -> IO () -> IO ()
addWork wp op = join $ atomicModifyIORef wp addw where
    addw (Left 0) = (Right [op],return ())
    addw (Left n) = assert (n > 0) $ (Left (pred n), forkWorker wp op)
    addw (Right ops) = (Right opsop, return ()) where
        opsop = ops ++ [op]

forkWorker :: WPool -> IO () -> IO ()
forkWorker wp op = void $ forkIO $ workerLoop wp op

workerLoop :: WPool -> IO () -> IO ()
workerLoop wp op = (try op >>= assertNoE) >> doMoreWork wp

assertNoE :: Either SomeException a -> IO ()
assertNoE (Left _) = assert False $ return ()
assertNoE _ = return ()
    
doMoreWork :: WPool -> IO ()
doMoreWork wp = join $ atomicModifyIORef wp takew where
    takew (Left n) = (Left (succ n), return ())
    takew (Right []) = error "invalid state for worker pool"
    takew (Right (op:[])) = (Left 0, workerLoop wp op)
    takew (Right (op:ops)) = (Right ops, workerLoop wp op)

