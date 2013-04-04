

module Sirea.Filesyste.OSX
    ( newManager
    ) where

import qualified System.OSX.FSEvents as FSE
import Sirea.Filesystem.Polling
import Debug.Trace

newManager :: MkManager 
newManager eh =
    traceIO ("TODO: OSX notifications (using hfsevents).") >>
    newPollingManager eh


{- Code from FSNotify for guidance

--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--
module System.FSNotify.OSX
       ( newSession
       ) where

-- NOTE: this version of FSNotify has recently had a major overhaul.
-- If someone with access to OSX > 10.6 could cajole the following
-- code into working order, it'd be appreciated. Following is a best 
-- effort without compiling it.


import Prelude hiding (FilePath, catch)
import System.FSNotify.Polling (newPollingSession) -- fallback
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad hiding (void)
import Data.Bits
import Data.IORef (atomicModifyIORef, readIORef)
import Data.Map (Map)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Word
-- import Debug.Trace (trace)
import Filesystem (isFile)
import Filesystem.Path hiding (concat)
import System.FSNotify.Listener
import System.FSNotify.Path (fp, canonicalizeDirPath)
import System.FSNotify.Types
import qualified Data.Map as Map


-- TODO: We really should use something other than FilePath as a key to allow
-- for more than one listener per FilePath.
type WatchMap = Map FilePath FSE.EventStream
data OSXManager = OSXManager (MVar WatchMap)

newSession :: IO Session 
newSession = FSE.fileLevelEventsSupported >>= mkSession where
    mkSession True = fmap osxSession newManager
    mkSession False = newPollingSession

newManager :: IO OSXManager
newManager = fmap OSXManager (newMVar Map.empty)

osxSession :: OSXManager -> Session
osxSession mgr = Session (kill mgr) (clear mgr) (start mgr)

kill :: OSXManager -> IO ()
kill (OSXManager wm) = modifyMVar killAll >>= mapM_ FSE.eventStreamDestroy where
    killAll m0 = return (Map.empty, Map.elems m0)

clear :: OSXManager -> FilePath -> IO ()
clear (OSXManager wm) dir = join $ modifyMVar killDir where
    killDir m0 = return $ killDir' m0 (Map.lookup dir m0)
    killDir' m0 Nothing = (m0,return())
    killDir' m0 (Just es) = (Map.delete dir m0, FSE.eventStreamDestroy es)


start :: OSXManager -> FilePath -> Action -> IO () 
start (OSXManager wm) dir action = body where
    handler = handleFSEEvents dir action
    body = do
        es <- FSE.eventStreamCreate [fp dir] 0.0 True False True handler
        join $ modifyMVar wm (add es)
    add es m0 = return $ add' es m0 (Map.lookup dir m0)
    add' es m0 Nothing = (Map.insert dir es m0, return ())
    add' es m0 (Just es0) = (Map.insert dir es m0, FSE.eventStreamDestroy es0)

nil :: Word64
nil = 0x00

-- OS X reports the absolute (canonical) path without a trailing slash. Add
-- the trailing slash when the path refers to a directory
canonicalEventPath :: FSE.Event -> FilePath
canonicalEventPath event =
  if flags .&. dirFlag /= nil then path </> empty else path
  where
    flags = FSE.eventFlags event
    dirFlag = FSE.eventFlagItemIsDir
    path = fp $ FSE.eventPath event

fsnEvents :: UTCTime -> FSE.Event -> IO [Event]
fsnEvents timestamp fseEvent = liftM concat . sequence $ map (\f -> f fseEvent) (eventFunctions timestamp)
  where
    eventFunctions :: UTCTime -> [FSE.Event -> IO [Event]]
    eventFunctions t = [addedFn t, modifFn t, removFn t, renamFn t]
    addedFn t e = if hasFlag e FSE.eventFlagItemCreated        then return [Added    (path e) t] else return []
    modifFn t e = if (hasFlag e FSE.eventFlagItemModified
                   || hasFlag e FSE.eventFlagItemInodeMetaMod) then return [Modified (path e) t] else return []
    removFn t e = if hasFlag e FSE.eventFlagItemRemoved        then return [Removed  (path e) t] else return []
    renamFn t e = if hasFlag e FSE.eventFlagItemRenamed then
                    isFile (path e) >>= \exists -> if exists   then return [Added    (path e) t] else return [Removed (path e) t]
                  else
                    return []
    path = canonicalEventPath
    hasFlag event flag = FSE.eventFlags event .&. flag /= 0

-- Separate logic is needed for non-recursive events in OSX because the
-- hfsevents package doesn't support non-recursive event reporting.

handleNonRecursiveFSEEvent :: ActionPredicate -> EventChannel -> FilePath -> DebouncePayload -> FSE.Event -> IO ()
-- handleNonRecursiveFSEEvent _       _    dirPath _   fseEvent | trace ("OSX: handleNonRecursiveFSEEvent " ++ show dirPath ++ " " ++ show fseEvent) False = undefined
handleNonRecursiveFSEEvent actPred chan dirPath dbp fseEvent = do
  currentTime <- getCurrentTime
  events <- fsnEvents currentTime fseEvent
  handleNonRecursiveEvents actPred chan dirPath dbp events
handleNonRecursiveEvents :: ActionPredicate -> EventChannel -> FilePath -> DebouncePayload -> [Event] -> IO ()
-- handleNonRecursiveEvents actPred _    dirPath _   (event:_     ) | trace (   "OSX: handleNonRecursiveEvents "
--                                                                       ++ show dirPath ++ " " ++ show event
--                                                                       ++ "\n  " ++ fp (directory dirPath)
--                                                                       ++ "\n  " ++ fp (directory (eventPath event))
--                                                                       ++ "\n  " ++ show (actPred event)) False = undefined
handleNonRecursiveEvents actPred chan dirPath dbp (event:events)
  | directory dirPath == directory (eventPath event) && actPred event = do
    case dbp of
      (Just (DebounceData epsilon ior)) -> do
        lastEvent <- readIORef ior
        when (not $ debounce epsilon lastEvent event) (writeChan chan event)
        atomicModifyIORef ior (\_ -> (event, ()))
      Nothing                           -> writeChan chan event
    handleNonRecursiveEvents actPred chan dirPath dbp events
  | otherwise                                                         = handleNonRecursiveEvents actPred chan dirPath dbp events
handleNonRecursiveEvents _ _ _ _ []                                   = void

handleFSEEvent :: EventChannel -> FSE.Event -> IO ()
-- handleFSEEvent _       _    _   fseEvent | trace ("OSX: handleFSEEvent " ++ show fseEvent) False = undefined
handleFSEEvent chan fseEvent = do
  currentTime <- getCurrentTime
  events <- fsnEvents currentTime fseEvent
  handleEvents chan events


-}

