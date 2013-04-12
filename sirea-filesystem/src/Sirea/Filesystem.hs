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
-- NOTE: Sirea.Filesystem does not make any effort to be savvy with
-- regards to symbolic links. This will not change. Sirea uses the
-- filesystem in a simplistic way
-- 
module Sirea.Filesystem 
    ( FS
    -- * Basic File Operations
    , breadFile
    , bwriteFile

    -- * Text Operations
    , breadFileText
    , bwriteFileText
    , breadFileString
    , bwriteFileString

    -- * Listing a directory
    , blistDirectory
    , FileDesc
    , fdIsFile
    , fdIsDir
    , fdModified
    , fdPath

    -- * Convenient Configuration Loading
    , bloadConfig
    , bloadConfigH
    , bloadConfigW

    -- * Quick access to directories.
    , bworkingDir
    , bhomeDir
    , bdesktopDir
    , bdocumentsDir    
    ) where 

import Prelude hiding (FilePath)
import Filesystem.Path (FilePath,(</>)) 
import qualified Filesystem as FS
import qualified Filesystem.Path as FS
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Data.IORef
import Data.Unique
import Data.Typeable
import Data.Maybe (fromMaybe)
import Control.Arrow (first, second)
import Control.Monad (unless)
import Control.Applicative

import Sirea.Filesystem.LocalMirror

import Sirea.Prelude
import Sirea.UnsafeLink
import Sirea.PCX
import Sirea.Partition
import Sirea.Signal

import Debug.Trace (traceIO)

-- | Sirea performs FileSystem operations in the FS partition.
type FS = Pt Filesystem -- simple loop partition.
data Filesystem deriving (Typeable)
    
-- local-mirror resource
newtype LM = LM LocalMirror deriving (Typeable)
instance Resource (Pt Filesystem) LM where
    locateResource _ cp =
        getPSched cp >>= \ pd ->
        newLocalMirror pd >>= \ lm ->
        return (LM lm) 

-- | Read the current contents of a file. If the file does not exist
-- or there are errors (e.g. lack of permission), Nothing will be 
-- returned. Read does not promise observation of every intermediate
-- state in the filesystem, but wil reliably provide a recent state
-- (assuming you aren't moving directories around and other corner
-- cases).
breadFile :: B (S FS FilePath) (S FS (Maybe ByteString))
breadFile = bfmap cleanFilePath >>> unsafeLinkBL mkFileReader 

-- any cleanup work I want to do on filepaths?
cleanFilePath, cleanDirPath :: FilePath -> FilePath
cleanFilePath = FS.collapse
cleanDirPath = (</> FS.empty) . FS.collapse

mkFileReader :: PCX W -> LnkUp (Maybe ByteString) -> IO (LnkUp FilePath)
mkFileReader cw ln = do
    cp <- getFSPCX cw
    pd <- getPSched cp
    k <- newUnique
    rf <- newIORef Nothing
    (LM lm) <- findInPCX cp 
    return (readLink pd k rf lm ln)

getFSPCX :: PCX W -> IO (PCX FS)
getFSPCX = findInPCX

-- read state
type ReadSt = Maybe (Sig FilePath, StableT)

readLink :: PSched -> Unique -> IORef ReadSt -> LocalMirror -> LnkUp (Maybe ByteString) -> LnkUp FilePath
readLink pd k rf lm ln = error "TODO: read files"

-- | Read a file as text. This simply maps a UTF-8 decode over the 
-- binary. Sequences that do not decode are replaced with U+FFFD,
-- rather than throwing an exception. 
--
--   breadFileText = breadFile >>> bfmap (fmap toText)
--     where toText = decodeUtf8With lenientDecode
--
-- Sirea.Filesystem treats binary as the primary view to simplify
-- interaction between readers and writers of different kinds.
-- 
breadFileText :: B (S FS FilePath) (S FS (Maybe Text))
breadFileText = breadFile >>> bfmap (fmap toText) where
    toText = Text.decodeUtf8With Text.lenientDecode

-- | Read a file as a string. This is not ideal for performance, but
-- is convenient. Note that this translates to Text first.
--
--   breadFileString = breadFileText >>> bfmap (fmap unpack)
--
-- A relevant concern is that strings are not compact or efficient,
-- and unless you're careful to process the string immediately with
-- bfmap, it is possible the expanded version will be kept in cache.
-- Text type is much better for efficient processing.
breadFileString :: B (S FS FilePath) (S FS (Maybe String))
breadFileString = breadFile >>> bfmap (fmap toString) where
    toString = Text.unpack . Text.decodeUtf8With Text.lenientDecode

-- | Write a file, or remove it. Intermediate directory structure is
-- created if necessary.
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
bwriteFile = bfmap (first cleanFilePath) >>> bvoid wf >>> vf where
    vf = (lf &&& bfmap snd) >>> bzipWith (==) -- verify by comparing read with write
    lf = bfmap fst >>> breadFile -- read the file
    wf = unsafeLinkB_ mkFileWriter -- try to write file (no direct return)

-- it might be worth just hacking something out for now, i.e. that
-- will not handle 
mkFileWriter :: PCX W -> IO (LnkUp (FilePath,Maybe ByteString))
mkFileWriter cw = do
    k <- newUnique
    cp <- getFSPCX cw
    (LM lm) <- findInPCX cp
    return (lnFileWriter k lm)

lnFileWriter :: Unique -> LocalMirror -> 

-- | Write text to file as UTF-8 (via Binary)
bwriteFileText :: B (S FS (FilePath, Maybe Text)) (S FS Bool)
bwriteFileText = bfmap (second (fmap fromText)) >>> bwriteFile where
    fromText = Text.encodeUtf8

-- | Write a string to file as UTF-8 (via Text)
bwriteFileString :: B (S FS (FilePath, Maybe String)) (S FS Bool)
bwriteFileString = bfmap (second (fmap fromString)) >>> bwriteFile where
    fromString = Text.encodeUtf8 . Text.pack

-- | List contents of a directory, including relevant metadata.
blistDirectory :: B (S FS FilePath) (S FS [FileDesc])
blistDirectory = bfmap cleanDirPath >>> unsafeLinkBL mkDirReader

mkDirReader :: PCX W -> LnkUp [FileDesc] -> IO (LnkUp FilePath)
mkDirReader = error "TODO: load directory info"

-- | For user interaction, it would often be convenient to create or
-- load a configuration file with default text. This is a utility
-- operation to support that common pattern. If the file does not
-- exist, it will be created in the filesystem with the given text.
-- In case of read error, the given text is returned.
bloadConfig :: IO FilePath -> Text -> B (S FS ()) (S FS Text)
bloadConfig getPath txt = lc where
    lc = unsafeLinkBL ini >>> breadFileText >>> bfmap (fromMaybe txt) 
    ini cw ln = do
        fp <- cleanFilePath <$> getPath
        cp <- getFSPCX cw
        (LM lm) <- findInPCX cp
        lmSchedWork lm fp (initFile fp) 
        return (ln_sfmap (s_const fp) ln)
    initFile fp =
        FS.isFile fp >>= \ bFile ->
        unless bFile $
            FS.createTree (FS.directory fp) >>
            FS.writeTextFile fp txt

-- load config relative to a directory.
bloadConfigRel :: IO FilePath -> FilePath -> Text -> B (S FS ()) (S FS Text)
bloadConfigRel getDirPath name txt = bloadConfig iodir txt where
    iodir = if FS.absolute name then return name else
            (</> name) <$> getDirPath

-- | bloadConfigH, bloadConfigW - load configs relative to home or
-- working directory, respectively. If absolute path is given, it is
-- used as an absolute path.
bloadConfigH, bloadConfigW :: FilePath -> Text -> B (S FS ()) (S FS Text)
bloadConfigH = bloadConfigRel FS.getHomeDirectory
bloadConfigW = bloadConfigRel FS.getWorkingDirectory 

-- | Access ambient information about user directories or working
-- directory. Note: these values are assumed constant during one 
-- run of the Haskell process. Developers should not manipulate the
-- working directory after starting a Sirea application.
--
--    bworkingDir : directory from which app was started; "." path
--    bhomeDir : user's home directory
--    bdesktopDir : user directory, based on OS
--    bdocumentsDir : user directory, based on OS
--
-- Application data should instead be kept using sirea-state. Files
-- are not well suited to RDP, but are useful for user interactions,
-- so only user directories are provided here (to subtly discourage
-- keeping app data in files).
--
bworkingDir, bhomeDir, bdesktopDir, bdocumentsDir :: B (S p ()) (S p FilePath)
bworkingDir = bioconst FS.getWorkingDirectory
bhomeDir = bioconst FS.getHomeDirectory
bdesktopDir = bioconst FS.getDesktopDirectory
bdocumentsDir = bioconst FS.getDocumentsDirectory

