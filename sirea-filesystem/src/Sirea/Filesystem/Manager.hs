
-- | Definition of Manager type used internally by Sirea.Filesystem,
-- and of the Event type handled by update actions. Implemented for
-- each OS.
--
-- The manager tracks a set of directories, reporting updates to a
-- single action passed to the manager upon construction. 
module Sirea.Filesystem.Manager
    ( Event(..)
    , evDirPath, evFileName, evFullPath, evTime
    , isExistsEvent, isDirectoryEvent
    , EventsHandler
    , Manager(..)
    , MkManager
    ) where

import Prelude hiding (FilePath)
import Filesystem.Path (FilePath,(</>))
import Filesystem.Path.CurrentOS()
import Sirea.Time (T)

-- | An event reports either the existence or non-existence of a file.
-- The time may be a best estimate. 
data Event = Event !Exists !Dir !Name !IsDir !T deriving (Show,Eq,Ord)
type Dir = FilePath  -- path of file in directory
type Name = FilePath -- name of file in directory
type Exists = Bool -- does the named file still exist?
type IsDir = Bool  -- does Name refer to a child directory

evDirPath, evFileName, evFullPath :: Event -> FilePath
evDirPath (Event _ dp _ _ _) = dp
evFileName (Event _ _ fn _ _) = fn
evFullPath (Event _ dp fn _ _) = dp </> fn
evTime :: Event -> T 
evTime (Event _ _ _ _ tm) = tm
isExistsEvent, isDirectoryEvent :: Event -> Bool
isExistsEvent (Event bExists _ _ _ _) = bExists
isDirectoryEvent (Event _ _ _ bDir _) = bDir
    
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
    { setWatchList :: [Dir] -> IO ()
    }

-- Create a manager. One handler is used for all events for all
-- active watches. Each manager module should export:
--   newManger :: MkManager
type MkManager = EventsHandler -> IO Manager


