
-- Polling implementation. This isn't intended to be super efficient,
-- just a decent fallback in case there is no OS implementation.
--
-- To alleviate race conditions, files younger than ten seconds are
-- reported with existence events for any new subscription. 
--
module Sirea.Filesystem.Polling
    ( newPollingManager
    ) where 

import Prelude hiding (FilePath)
import Control.Monad (void,unless)
import Control.Concurrent
import Data.IORef
import Data.Maybe (catMaybes)
import Data.List (sort)
import Data.Function (on)
import qualified Data.Map as M
import qualified Filesystem.Path as FS
import qualified Filesystem as FS
import qualified Control.Exception as E
import qualified System.IO.Error as IOE
import Sirea.Filesystem.Manager
import Sirea.Time
import Debug.Trace (traceIO)

data P = P
    { p_dtPoll :: !DT
    , p_sigup  :: !(MVar ()) -- signal watchlist update.
    , p_watch  :: !(IORef [FilePath])
    , p_action :: !(EventsHandler)
    }

type FilePath = FS.FilePath

-- Polling keeps a simple memory for comparing results. The Event is
-- used as a file records, since it's close enough to what I need. A
-- 'Nothing' value indicates this is the first time we're polling a 
-- path, so no changes are reported for that initial effort. 
--
-- The lists in PMem will have been sorted by contents.
type PMem = M.Map FilePath (Maybe [FileRec])

-- for a FileRec, the FilePath is local to directory
data FileRec = FileRec
    { fr_path  :: !FilePath
    , fr_isdir :: !Bool
    , fr_mod   :: {-# UNPACK #-} !T
    }

-- if a file is younger than dtYoung, we'll report it for any new
-- subscriptions as a new file, i.e. to make sure the subscriber 
-- doesn't miss it due to a race condition.
dtYoung :: DT
dtYoung = 15 -- seconds

newPollingManager :: DT -> MkManager
newPollingManager dtPoll eh = 
    newEmptyMVar >>= \ w ->
    newIORef [] >>= \ rfL ->
    let p = P dtPoll w rfL eh in
    forkIO (pollInit p) >>
    return (Manager (setWatch p))

-- setWatch simply records the watchlist then signals the change.
-- This will be handled the next time the poll thread tests for
-- the signal. Does not block.
setWatch :: P -> [FilePath] -> IO ()
setWatch (P _ u w _) wl = void $ writeIORef w wl >> tryPutMVar u ()

-- pollInit is the state when we don't have any active watches.
pollInit :: P -> IO ()
pollInit p = takeMVar (p_sigup p) >> updateWatchList p M.empty

-- if we receive a watch-list update, we'll need to adjust the
-- memory and polling effort. 
updateWatchList :: P -> PMem -> IO ()
updateWatchList p m =
    readIORef (p_watch p) >>= \ wl ->
    if (null wl) then pollInit p else
    pollCycle p (pollMemTransfer wl m)

-- pollMemTransfer will keep information on paths in the original
-- map after a change in the watchlist. 
pollMemTransfer :: [FilePath] -> PMem -> PMem
pollMemTransfer wl mOrig = foldl addPath M.empty wl where
    addPath m dir = 
        case M.lookup dir mOrig of
            Nothing -> M.insert dir Nothing m 
            Just x -> M.insert dir x m

-- the main polling cycle
pollCycle :: P -> PMem -> IO ()
pollCycle p oldMem =
    tryTakeMVar (p_sigup p) >>= \ mbu -> 
    case mbu of
        Just () -> updateWatchList p oldMem
        Nothing -> do
            tNow <- getTime
            newMem <- mainPollingAction tNow p oldMem
            threadDelay (dtToUsec (p_dtPoll p))
            pollCycle p newMem

dtToUsec :: DT -> Int
dtToUsec = fromIntegral . (`div` 1000) . dtToNanos 

expectedError :: IOE.IOError -> Bool
expectedError ioe =
    IOE.isDoesNotExistError ioe ||
    IOE.isPermissionError ioe
    
printError :: FilePath -> IOE.IOError -> IO ()
printError d ioe = traceIO ("error @ " ++ show d ++ ": " ++ show ioe)

-- obtain a FilePath-sorted list of file records.
listDirectory :: FilePath -> IO [FileRec]
listDirectory dir = happyPath `E.catch` sadPath where
    sadPath ioe = 
        unless (expectedError ioe) (printError dir ioe) >>
        return []
    happyPath = do 
        dl <- sort `fmap` FS.listDirectory dir
        dlRec <- mapM pathToRecord dl
        return (catMaybes dlRec)

-- pathToRecord will return Nothing if there is any exception.
-- The input path should already be canonicalized.
pathToRecord :: FilePath -> IO (Maybe FileRec)
pathToRecord path = happyPath `E.catch` sadPath where
    sadPath ioe = printError path ioe >> return Nothing
    happyPath = do
        bDir <- FS.isDirectory path
        tMod <- fromUTC `fmap` FS.getModified path
        return $ Just (FileRec path bDir tMod)
            
mainPollingAction :: T -> P -> PMem -> IO PMem
mainPollingAction tNow p = M.traverseWithKey dirAction where
    dirAction dir Nothing = do 
        dlExists <- listDirectory dir
        let tY = tNow `subtractTime` dtYoung
        let dly = filter ((> tY) . fr_mod) dlExists
        p_action p (map (existsEvent dir) dly)
        return (Just dlExists)
    dirAction dir (Just dlOld) = do
        dlNew <- listDirectory dir
        let evs = diffEvents dir tNow dlOld dlNew
        p_action p evs
        return (Just dlNew)

-- diff of two sorted lists. The given tNow is necessary for file
-- removal events.
diffEvents :: FilePath -> T -> [FileRec] -> [FileRec] -> [Event]
diffEvents d _ [] newFiles = map (existsEvent d) newFiles
diffEvents d t oldFiles [] = map (removedEvent d t) oldFiles
diffEvents d t os@(o:os') ns@(n:ns') =
    case (compare `on` fr_path) o n of
      LT -> removedEvent d t o : diffEvents d t os' ns 
      GT -> existsEvent d n : diffEvents d t os ns'
      EQ -> let rest = diffEvents d t os' ns' in
            if (fr_isdir o /= fr_isdir n) 
                then let tRem = fr_mod n `subtractTime` 0.01 in
                     removedEvent d tRem o : existsEvent d n : rest
                else if (fr_mod n /= fr_mod o)
                        then existsEvent d n : rest
                        else rest

existsEvent :: FilePath -> FileRec -> Event
existsEvent dir (FileRec p d tMod) = Event True dir p d tMod

removedEvent :: FilePath -> T -> FileRec -> Event
removedEvent dir tNow (FileRec p d _) = Event False dir p d tNow


