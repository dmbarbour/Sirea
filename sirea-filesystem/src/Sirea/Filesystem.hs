{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls, 
             FlexibleInstances, MultiParamTypeClasses,
             CPP
 #-} 

-- | Observe and influence the host filesystem through Sirea
--
-- This module provides a simple file-at-a-time ontology for file
-- observation and manipulation:
--
--   * read a file as binary or text
--   * list a directory
--   * write or remove a file with binary or text
--
-- These operations are continuous and reactive. When a file changes
-- a observer of that file will update automatically. Continuous
-- writes will only push updates when there is a change in the
-- target file state.
--
-- The current API is insufficient for operations on large files or
-- streams. I may consider partial, region-based file manipulations
-- in the future.
--
-- NOTE: Compared to other state models, filesystem is inexpressive,
-- difficult to speculate or retroactively correct, high latency,
-- low security, weak robustness, no history. Use the filesystem for 
-- integration with external tools or users. For user interaction,
-- files are persistent, declarative, and observed reactively. Files
-- are a much closer fit for RDP than console or command-line IO.
--
module Sirea.Filesystem 
    ( FS
    -- * Basic File Operations
    , breadFile
    , bwriteFile
    -- * Listing a directory
    , blistDirectory
    , FileDesc
    , fdIsFile
    , fdIsDir
    , fdModified
    , fdPath

    -- , loadConfFile
    -- , filePathDetails
    -- , listDirectoryDetailed

    -- * Text Operations
    , breadFileText
    , breadFileString
    , bwriteFileText
    , bwriteFileString
    ) where 

import Prelude hiding (FilePath)
import Filesystem.Path (FilePath) 
import qualified Filesystem.Path as FS
import qualified Filesystem as FS 
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Data.Function (on)
import Data.IORef
import Data.Unique
import Data.Typeable
import qualified Data.Map.Strict as M
import Control.Arrow (second)
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad (join,void)
import Control.Applicative
import Control.Exception (SomeException,assert,try)

import Sirea.Filesystem.Manager

import Sirea.Prelude
import Sirea.UnsafeLink
import Sirea.AgentResource
import Sirea.Partition
import Sirea.PCX

import Debug.Trace (traceIO)

-- TUNING
-- How many files shall we allow to read or write concurrently?
numFileLoaders :: Int
numFileLoaders = 6

-- | Sirea performs FileSystem operations in the FS partition.
type FS = Pt (Filesystem ()) -- simple loop partition.
data Filesystem x deriving (Typeable)

    

-- Resource: I'll keep a basic reflection of the filesystem being
-- observed or manipulated.
data FileRfl = FileRfl !FileSt !WriteMap !ObsMap
type FileSt = Maybe ByteString
type WriteMap = M.Map Unique (Signal FileSt)
type ObsMap = M.Map Unique Alert
type Alert = IO () -- alerts run once then must be rescheduled
data DirRfl = DirRfl !FileRflMap !DirList !ObsMap !SchedMap 
type FileRflMap = M.Map FilePath FileRfl
type DirList = M.Map FilePath FileDesc


--
-- sirea-filesystem leverages the events model of the underlying OS
-- where feasible. The OS is made aware of interest in a set of 
-- directories, and pumps all events back to the partition thread.
--
--
--     Notification Manager
--       | A  setWatchList up, events down
--       V |
--    Sirea FS Pt
--       ||||||
--       Loader Threads
--
-- 

-- | Read the current contents of a file. If the file does not exist
-- or there are errors (e.g. lack of permission), Nothing will be 
-- returned. Read cannot promise observation of every intermediate
-- state in the filesystem, but it will observe every state written
-- by this Sirea process.
breadFile :: B (S FS FilePath) (S FS (Maybe ByteString))
breadFile = bundefined -- might want to substitute something here for now

-- | Read a file as text. This simply maps a UTF-8 decode over the 
-- binary. Sequences that do not decode are dropped. If you need
-- something else, it's easy to implement:
--
--   breadFileText = breadFile >>> bfmap (fmap toText)
--     where toText = decodeUtf8With ignore
--
-- Sirea.Filesystem treats binary as the primary view to simplify
-- interaction between readers and writers of different kinds.
-- 
breadFileText :: B (S FS FilePath) (S FS (Maybe Text))
breadFileText = breadFile >>> bfmap (fmap toText) where
    toText = Text.decodeUtf8With Text.ignore

-- | Read a file as a string. This is not ideal for performance, but
-- is convenient. Note that this translates to Text first.
--
--   breadFileString = breadFileText >>> bfmap (fmap unpack)
--
breadFileString :: B (S FS FilePath) (S FS (Maybe String))
breadFileString = breadFileText >>> bfmap (fmap Text.unpack)


-- | Write a file, or remove it. When writing a file, intermediate
-- directory structure will be created if it doesn't already exist.
-- To remove a file, write Nothing. RDP's resource paradigm excludes
-- notions of creation or destruction, but 'does-not-exist' can be
-- understood as just another file state, distinct from empty file.
--
-- Writes may be choked. I.e. if you demand a dozen states over one
-- second, it may be that only one or two are actually written. Any
-- final state will be written, barring disruption. After any crash, 
-- you'll be depending on the OS and underlying filesystem for the
-- recovery. (Other state models for Sirea are more robust, using
-- the acid-state package or similar. It may be useful to leverage a
-- more robust state model to drive filesystem interactions.)
--
-- Developers should avoid write-conflicts. It isn't difficult; just
-- ensure by design that there is at most one writer for a given 
-- file. But if conflicts occur, Sirea favors keeping the lowest. In
-- this case: lowest in lexicographic byte order.
--
-- The response is simple boolean, with True being OK or success. A
-- failure, whether due to permissions or write conflict, is False.
--
bwriteFile :: B (S FS (FilePath, Maybe ByteString)) (S FS Bool)
bwriteFile = bundefined

-- | Write text to file as UTF-8.
bwriteFileText :: B (S FS (FilePath, Maybe Text)) (S FS Bool)
bwriteFileText = bfmap (second (fmap Text.encodeUtf8)) >>> bwriteFile

-- | Write a string to file as UTF-8.
bwriteFileString :: B (S FS (FilePath, Maybe String)) (S FS Bool)
bwriteFileString = bfmap (second (fmap Text.pack)) >>> bwriteFileText


-- | List contents of a directory, including relevant metadata.
blistDirectory :: B (S FS FilePath) (S FS [FileDesc])
blistDirectory = bundefined





{-
-- CONSIDER:
--  easy access to directories (home, etc.)?
--  fixed working directory

-- | Access ambient information about user directories or working
-- directory. Note: these values are constant during one run of the
-- Haskell process. Sirea does not allow runtime manipulation of 
-- working directory (it would be difficult to address reactively
-- due to potential use of local "." paths).
--
--    bworkingDir : directory from which app was started; "." path
--    bhomeDir : user's home directory
--    bdesktopDir : user directory, based on OS
--    bdocumentsDir : user directory, based on OS
--
--    bappDataDirectory : application data with label.
--    
--
-- See the system-fileio Filesystem module for more info.

bworkingDir, bhomeDir, bdesktopDir, bdocumentsDir :: B (S FS ()) (S FS FilePath)
bappDataDir, bappCacheDir, bappConfigDir :: Text -> B (S FS ()) (S FS FilePath)
-}



