
-- | Definition of Manager type used internally by Sirea.Filesystem,
-- and of the Event type handled by update actions. Implemented for
-- each OS.
--
-- The manager tracks a set of directories, reporting updates to a
-- single action passed to the manager upon construction. 
module Sirea.Filesystem.Manager
    ( Event(..)
    , Manager(..)
    , MkManager
    ) where

import Sirea.Time (T)

-- Event: the information I need to avoid performing FileSystem IO
-- from the FS partition when maintaining a directory list.
data Event = FSEvent 
    { ev_exist :: !Bool
    , ev_path  :: !FilePath
    , ev_isdir :: !Bool
    , ev_time  :: {-# UNPACK #-} !T
    } deriving (Show)

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


