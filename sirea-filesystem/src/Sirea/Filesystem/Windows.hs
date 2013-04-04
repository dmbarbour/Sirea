

module Sirea.Filesyste.Windows
    ( newManager
    ) where

import qualified System.Win32.Notify as W
import Sirea.Filesystem.Polling
import Debug.Trace

newManager :: MkManager 
newManager eh =
    traceIO ("TODO: Windows notifications using Win32-notify.") >>
    newPollingManager eh



{- code from FSNotify for guidance


import Prelude hiding (FilePath)

import Control.Concurrent.Chan
import Control.Monad (when)
import Data.IORef (atomicModifyIORef, readIORef)
import Data.Time (getCurrentTime, UTCTime)
import System.FSNotify.Listener
import System.FSNotify.Path (fp, canonicalizeDirPath)
import System.FSNotify.Types
import qualified System.Win32.Notify as WNo

type NativeManager = WNo.WatchManager

-- TODO: Need to ensure we use properly canonalized paths as
-- event paths. In Linux this required passing the base dir to
-- handle[native]Event.

void :: IO ()
void = return ()

-- Win32-notify has (temporarily?) dropped support for Renamed events.
fsnEvent :: UTCTime -> WNo.Event -> Maybe Event
fsnEvent timestamp (WNo.Created  False name) = Just $ Added    (fp name) timestamp
fsnEvent timestamp (WNo.Modified False name) = Just $ Modified (fp name) timestamp
fsnEvent timestamp (WNo.Deleted  False name) = Just $ Removed  (fp name) timestamp
fsnEvent _         _                         = Nothing
{-
fsnEvents timestamp (WNo.Renamed  False (Just oldName) newName) = [Removed (fp oldName) timestamp, Added (fp newName) timestamp]
fsnEvents timestamp (WNo.Renamed  False Nothing newName)        = [Added (fp newName) timestamp]
-}

handleWNoEvent :: ActionPredicate -> EventChannel -> DebouncePayload -> WNo.Event -> IO ()
handleWNoEvent actPred chan dbp inoEvent = do
  currentTime <- getCurrentTime
  let maybeEvent = fsnEvent currentTime inoEvent
  case maybeEvent of
    Just evt -> handleEvent actPred chan dbp evt
    Nothing  -> void

instance FileListener WNo.WatchManager where
  initSession = fmap Just WNo.initWatchManager
  killSession = WNo.killWatchManager
  listen db watchManager path actPred chan = do
    WNo.watchDirectory watchManager (fp path') False varieties (handler actPred chan dbp)
  listenRecursive db watchManager path actPred chan = do
    WNo.watchDirectory watchManager (fp path') True varieties (handler actPred chan dbp)

handler :: ActionPredicate -> EventChannel -> DebouncePayload -> WNo.Event -> IO ()
handler = handleWNoEvent

varieties :: [WNo.EventVariety]
varieties = [WNo.Create, WNo.Delete, WNo.Move, WNo.Modify]

-}


