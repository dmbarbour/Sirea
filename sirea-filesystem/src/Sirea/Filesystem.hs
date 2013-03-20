{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls #-} 

-- | observe and influence the host filesystem through Sirea
--
-- This module provides a simple file-at-a-time ontology for file
-- observation and manipulation:
--
--   * read a file as binary or text
--   * read a directory for discovery
--   * write a file as binary or text
--
-- In addition, there are a few convenience operations
--
--   * default or relative directories (user home, etc.)
--   * configuration loading with automatic default text
--
-- These operations are continuous and reactive. Directory structure
-- will be created implicitly on put. To delete a file, you will put
-- a Nothing state. (RDP's resource paradigm excludes the concepts
-- of creation or destruction. But we can treat 'does-not-exist' as
-- a state distinct from empty file.) 
--
-- The file-at-a-time philosophy is convenient, but performs poorly 
-- when processing very large files. Region-based access to files,
-- combined with indexes or stateful iterators, would be viable for
-- large file processing in RDP and Sirea. However, this package
-- does not currently support such techniques. 
--
-- At least for now, this module is only suitable for processing of
-- small or medium sized files. Anything less than a megabyte is 
-- probably okay on modern systems. Fortunately, that covers most
-- file processing very easily.
--
-- Sirea.Filesystem is very light on error details, for now. If an
-- API with more error information is needed, I'll add a parallel
-- set of behaviors to provide it.
--
-- The idea of 'user-input files' makes a great deal of sense in a
-- reactive model with orthogonal persistence, esp. when compared to
-- volatile, streaming, console input. 
module Sirea.Filesystem 
    ( FS
    , breadFile, breadTextFile
    , bwriteFile, bwriteTextFile
    --, loadConfFile
    , blistDirectory
    -- , filePathDetails
    -- , listDirectoryDetailed
    ) where 

import Prelude hiding (FilePath)
import Filesystem.Path (FilePath) -- cross platform paths
import qualified Filesystem as IOFS -- cross platform filesystem ops
import System.FSNotify -- cross platform filesystem events
import Data.ByteString (ByteString)
import Data.Text (Text)

import Sirea.Prelude
import Sirea.Behavior
import Sirea.UnsafeLink
import Sirea.AgentResource
import Sirea.Partition
import Sirea.PCX

import Data.Typeable

-- | Sirea performs FileSystem operations in the FS partition.
type FS = Pt FileSystem -- simple loop partition.
data FileSystem deriving (Typeable)

-- | Read the current contents of a file. If the file does not exist
-- or there are any other errors, Nothing will be returned.
breadFile :: B (S FS FilePath) (S FS (Maybe ByteString))
breadFile = bundefined

breadTextFile :: B (S FS FilePath) (S FS (Maybe Text))
breadTextFile = bundefined

bwriteFile :: B (S FS (Maybe ByteString)) (S FS Bool)
bwriteFile = bundefined

bwriteTextFile :: B (S FS (Maybe Text)) (S FS Bool)
bwriteTextFile = bundefined

blistDirectory :: B (S FS FilePath) (S FS [FilePath])
blistDirectory = bundefined

-- CONSIDER:
--  easy access to directories (home, etc.)?
--  fixed working directory

