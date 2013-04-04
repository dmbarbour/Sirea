
-- | Definition of Manager type used internally by Sirea.Filesystem,
-- and of the Event type handled by update actions. Implemented for
-- each OS.
--
-- The manager tracks a set of directories, reporting updates to a
-- single action passed to the manager upon construction. 
module Sirea.Filesystem.Manager
    ( FileDesc(..)
    , fdIsFile, fdIsDir, fdModified, fdPath
    , Event
    , EventsHandler
    , Manager(..)
    , MkManager
    ) where

import Prelude hiding (FilePath)
import Filesystem.Path (FilePath)
import Sirea.Time (T)

-- | A FileDesc contains a description of a file.
data FileDesc = FD
    { fd_type  :: !FType 
    , fd_path  :: !FilePath
    , fd_modT  :: {-# UNPACK #-} !T 
    -- , fd_size  :: !Int
    } deriving (Ord,Eq)
data FType = Dir | File deriving (Ord, Eq)

fdIsFile, fdIsDir :: FileDesc -> Bool
fdModified :: FileDesc -> T
fdPath :: FileDesc -> FilePath

fdIsFile = isFile . fd_type
fdIsDir = isDir . fd_type
fdModified = fd_modT
fdPath = fd_path

isDir, isFile :: FType -> Bool
isDir Dir = True
isDir _ = False
isFile File = True
isFile _ = False

-- | An event reports either the existence or non-existence of a file.
type Event = Either FileDesc FilePath
    
-- Handle an event, or a bulk set of events. This operation must be
-- mt-safe and non-blocking. Bulk sends will only be used if not
-- inconvenient for the particular Manager. The caller must be 
-- robust to hearing about files it already knows; the action is
-- assumed idempotent for events.
type EventsHandler = [Event] -> IO ()

-- Manager: track a time-varying set of directories.
--
--   setWatchList: specify active watch list. FilePaths that are in
--     previous 'setWatchList' actions but not in the current one
--     should be removed. 
--
-- The Manager is responsible for its own use of internal resources
-- or threads. Sirea.Filesystem will only create one manager per
-- Filesystem partition, and currently has only one such partition. 
--
-- The given watch list should use directory paths in canonical form.
--
data Manager = Manager 
    { setWatchList :: [FilePath] -> IO ()
    }

-- Create a manager. One handler is used for all events for all
-- active watches. Each manager module should export:
--   newManger :: MkManager
type MkManager = EventsHandler -> IO Manager


