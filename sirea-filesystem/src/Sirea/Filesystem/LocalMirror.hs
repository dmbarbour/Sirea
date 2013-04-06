
-- | The Filesystem module will keep a local mirror of the actively
-- observed or manipulated portions of the Filesystem. Most of this
-- is done in IO, but asynchronously and with tight control over 
-- resource consumption (using workers and mt-safe operations).

module Sirea.Filesystem.LocalMirror 
    ( FileDesc(..), FType(..)
    , fdIsFile, fdIsDir, fdModified, fdPath
    ) where

import Prelude hiding (FilePath)
import Filesystem.Path (FilePath)
import Filesystem.Path.CurrentOS()
import qualified Filesystem as FS
import qualified Filesystem.Path as FS
import Sirea.Time (T)
import qualified Data.Map.Strict as M
import Data.Unique
import qualified Data.ByteString as B

import Sirea.Filesystem.Manager


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


-- Resource: I'll keep a basic reflection of the filesystem being
-- observed or manipulated.
data FileRfl = FileRfl !FileSt !WriteMap !ObsMap
type FileSt = Maybe B.ByteString
type WriteMap = M.Map Unique (FileSt -> IO ())
type ObsMap = M.Map Unique Alert
type Alert = IO () -- alerts run once then must be rescheduled
data DirRfl = DirRfl !FileRflMap !DirList !ObsMap
type FileRflMap = M.Map FilePath FileRfl
type DirList = M.Map FilePath FileDesc


-- TUNING
-- How many files shall we allow to read or write concurrently?
-- (note: this doesn't limit the number of files accessed, just
-- the number of updates we can process at once, so even 1 or 2
-- should be sufficient.)
--numFileLoaders :: Int
--numFileLoaders = 6


