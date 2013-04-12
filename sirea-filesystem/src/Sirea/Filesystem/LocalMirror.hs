
-- | The Filesystem module will keep a local mirror of the actively
-- observed or manipulated portions of the Filesystem. Most of this
-- is done in IO, but asynchronously and with tight control over 
-- resource consumption (using workers and mt-safe operations).
--
-- LocalMirror 'belongs' to a particular partition, for which PSched
-- is provided.
--
-- Readers must re-subscribe after every notification (i.e. each
-- update is one-time only).
--
module Sirea.Filesystem.LocalMirror 
    ( FileDesc(..), FType(..)
    , fdIsFile, fdIsDir, fdModified, fdPath
    , LocalMirror
    , newLocalMirror
    , lmSchedWork
    ) where

import Prelude hiding (FilePath, catch)
import Filesystem.Path.CurrentOS(FilePath)
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FS
import Sirea.Time (T)
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import Data.Unique
import Data.IORef
import Control.Exception (catch, SomeException)
import Control.Monad.Fix (mfix)

import Sirea.Partition
import Sirea.Filesystem.Manager
import Sirea.Filesystem.OSManager
import Sirea.Filesystem.WorkerPool
import Sirea.Filesystem.KeyedSched

import Debug.Trace

-- TUNING
-- How many files shall we allow to read or write concurrently?
numFileLoaders :: Int
numFileLoaders = 6

-- | A FileDesc contains a simple description of a file.
data FileDesc = FD !FType !FilePath !T deriving (Show,Ord,Eq)
data FType = Dir | File deriving (Show,Ord, Eq)

fdIsFile, fdIsDir :: FileDesc -> Bool
fdModified :: FileDesc -> T
fdPath :: FileDesc -> FilePath

fdIsFile (FD ty _ _) = isFile ty
fdIsDir (FD ty _ _) = isDir ty
fdModified (FD _ _ t) = t
fdPath (FD _ p _) = p

isDir, isFile :: FType -> Bool
isDir Dir = True
isDir _ = False
isFile File = True
isFile _ = False

-- local mirror has a few schedulers and some mutable data
data LocalMirror = LocalMirror 
    { lm_wsched :: !(FilePath -> IO () -> IO ())
    , lm_psched :: !PSched
    , lm_fsm    :: !Manager
    , lm_data   :: !(IORef LMD)
    }

-- LMD models the filesystem as flat (no hierarchy)
--   there is a pool of active write signals
--   
data LMD = LMD

lmdZero :: LMD
lmdZero = LMD

newLocalMirror :: PSched -> IO LocalMirror
newLocalMirror pd = mfix $ \ lm ->
    newIORef lmdZero >>= \ rf ->
    newManager (eventsHandler lm) >>= \ fsm ->
    newWorkerPool numFileLoaders >>= \ wp ->
    newKeyedSched wp >>= \ ks ->
    return (LocalMirror ks pd fsm rf)

eventsHandler :: LocalMirror -> [Event] -> IO ()
eventsHandler _ [] = return ()
eventsHandler lm es = 
    onNextStep (lm_psched lm) $ 
        mapM_ (eventHandler lm) es

eventHandler :: LocalMirror -> Event -> IO ()
eventHandler _ e = traceIO ("TODO: handle event " ++ show e)

-- Schedule work regarding a particular file. Will serialize with
-- other work on the same file. Will be performed when a worker is
-- available.
lmSchedWork :: LocalMirror -> FilePath -> IO () -> IO ()
lmSchedWork lm fp = lm_wsched lm fp . catchAll fp

catchAll :: FilePath -> IO () -> IO ()
catchAll fp op = op `catch` reportE fp

reportE :: FilePath -> SomeException -> IO ()
reportE fp e = traceIO ("sirea-filesystem error: " ++ show fp ++ " - " ++ show e)


