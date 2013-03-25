
-- Polling implementation. This isn't intended to be super efficient,
-- just a decent fallback in case there is no OS implementation.
--
-- To alleviate race conditions, files younger than ten seconds are
-- reported with existence events for any new subscription. 
module Sirea.Filesystem.Polling
    ( newPollingManager
    ) where 

import Prelude hiding (FilePath)
import Control.Monad (void,unless)
import Control.Concurrent.MVar
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
import Sirea.Time (T,fromUTC)
import Debug.Trace (traceIO)

data P = P
    { p_sigup  :: !(MVar ()) -- signal watchlist update.
    , p_watch  :: !(IORef [FilePath])
    , p_action :: !(EventsHandler)
    }

-- Polling keeps a simple memory for comparing results. The Event is
-- used as a file records, since it's close enough to what I need. A
-- 'Nothing' value indicates this is the first time we're polling a 
-- path, so no changes are reported for that initial effort. 
--
-- The lists in PMem will have been sorted by contents.
type PMem = M.Map FilePath (Maybe [FileRec])

-- for a FileRec, the FilePath must be canonical.
data FileRec = FileRec
    { fr_path  :: !FilePath
    , fr_isdir :: !Bool
    , fr_mod   :: {-# UNPACK #-} !T
    }

pollPeriod :: Int -- microseconds
pollPeriod = 1 * 1000 * 1000

-- if a file is younger than dtYoung, we'll report it for any new
-- subscriptions as a new file, i.e. to make sure the subscriber 
-- doesn't miss it due to a race condition.
dtYoung :: DT
dtYoung = 60 -- seconds

newPollingManager :: MkManager
newPollingManager eh = 
    newEmptyMVar >>= \ w ->
    newIORef [] >>= \ rfL ->
    let p = P w rfL eh in
    newThread (pollInit p) >>
    return (Manager (setWatch p))

-- setWatch simply records the watchlist then signals the change.
-- This will be handled the next time the poll thread tests for
-- the signal. Does not block.
setWatch :: P -> [Event] -> IO ()
setWatch (P u w _) wl = void $ writeIORef w wl >> tryPutMVar u ()

-- pollInit is the state when we don't have any active watches.
pollInit :: P -> IO ()
pollInit p = takeMVar (p_sigup p) >> pUpdateWL p M.empty

-- if we receive a watch-list update, we'll need to adjust the
-- memory and polling effort. 
pUpdateWL :: P -> PMem -> IO ()
pUpdateWL p m =
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
pollCycle p m =
    tryTakeMVar (p_sigup p) >>= \ mbu -> 
    case mbu of
        Just () -> pUpdateWL p m
        Nothing -> do
            tNow <- getTime
            m' <- mainPollingAction tNow p m
            threadDelay pollPeriod
            pollCycle p m 

expectedError :: IOE.IOError -> Bool
expectedError ioe =
    isDoesNotExistErrorType ioe ||
    isPermissionErrorType ioe
    
printError :: FilePath -> IOE.IOError -> IO ()
printError d ioe =
    unless (expectedError ioe) $
        traceIO ("error @ " ++ show d ++ ": " ++ show ioe)

-- obtain a FilePath-sorted list of file records.
listDirectory :: FilePath -> IO [FileRec]
listDirectory dir = happyPath `E.catch` sadPath where
    sadPath ioe = printError dir ioe >> return []
    happyPath = do 
        canonDir <- FS.canonicalizePath dir
        dl <- sort `fmap` FS.listDirectory canonDir
        dlRec <- mapM pathToRecord $ map (canonDir </>) dl
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
        dlExists <- listDirectory 
        let tY = tNow `subtractTime` dtYoung
        let dly = filter (fr_mod > tY) dlExists
        p_action p (map existsEvent dly)
        return (Just dlExists)
    dirAction dir (Just dlOld) = do
        dlNew <- listDirectory dir
        let evs = diffEvents tNow dlOld dlNew
        p_action p evs
        return (Just dlNew)

-- diff of two sorted lists. The given tNow is necessary for file
-- removal events.
diffEvents :: T -> [FileRec] -> [FileRec] -> [Event]
diffEvents _ [] newFiles = map existsEvent newFiles
diffEvents tNow oldFiles [] = map (removedEvent tNow) oldFiles
diffEvents tNow os@(o:os') ns@(n:ns') =
    case (compare `on` fr_path) o n of
      LT -> removedEvent tNow o : diffEvents tNow os' ns 
      GT -> existsEvent n : diffEvent os ns'
      EQ -> let rest = diffEvents tNow os' ns' in
            if (fr_isdir o /= fr_isdir n) 
                then let tRem = fr_mod n `subtractTime` 0.001 in
                     removedEvent tRem o : existsEvent n : rest
                else if (fr_mod n /= fr_mod o)
                        then existsEvent n : rest
                        else rest

existsEvent :: FileRec -> Event
existsEvent (FileRec p d t) = Event True p d t

removedEvent :: T -> FileRec -> Event
removedEvent tNow (FileRec p d _) = Event False p d tNow


