
-- File notifications using Linux's hinotify.
--
-- The current design is that the inotify subsystem is created when
-- we have at least one active watch. From there, we add or remove
-- watches to match the current watch-list. Since all watches have
-- the same callback event, there is no trouble with updating the
-- watch action on a directory.
module Sirea.Filesystem.Linux
    ( newManager
    ) where 

import Prelude hiding (FilePath)
import Filesystem.Path (FilePath, (</>))
import Filesystem.Path.CurrentOS (encodeString, decodeString)
import qualified Filesystem as FS
import Control.Concurrent.MVar
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import qualified System.INotify as INo
import qualified System.IO.Error as IOE
import qualified Control.Exception as E

import Sirea.Time
import Sirea.Filesystem.Manager

import Debug.Trace (traceIO)


type WatchMap  = M.Map FilePath INo.WatchDescriptor
type WatchData = Maybe (INo.INotify, WatchMap)
data L = L 
    { l_watch  :: !(MVar WatchData)
    , l_action :: !EventsHandler
    }

newManager :: MkManager
newManager eh =
    newMVar Nothing >>= \ rfW ->
    let lm = (L rfW eh) in
    return (Manager (setWatch lm))
    
setWatch :: L -> [FilePath] -> IO ()
setWatch l wl = modifyMVar_ (l_watch l) setw where
    setw Nothing = 
        if (null wl) then return Nothing else do
        ino <- INo.initINotify 
        setw' ino M.empty
    setw (Just (ino,m0)) = 
        if (null wl) 
            then INo.killINotify ino >> return Nothing
            else setw' ino m0
    setw' ino m0 = do
        wds <- mapM (addw ino m0) wl
        let m' = M.fromList $ catMaybes wds
        mapM_ remw (M.toList (m0 `M.difference` m'))
        return (Just (ino,m'))
    addw ino m0 dir = addw' ino dir (M.lookup dir m0) `E.catch` addE dir
    addE dir ioe = printError dir ioe >> return Nothing
    addw' _ dir (Just wd) = return $ Just (dir,wd)
    addw' ino dir Nothing =
        inoAddWatch ino dir (l_action l) >>= \ wd ->
        return (Just (dir,wd))
    remw (d,wd) = INo.removeWatch wd `E.catch` printError d
    
printError :: FilePath -> IOE.IOError -> IO ()
printError d ioe = traceIO ("error @ " ++ show d ++ ": " ++ show ioe)

inoAddWatch :: INo.INotify -> FilePath -> EventsHandler -> IO INo.WatchDescriptor
inoAddWatch ino dir action = INo.addWatch ino v dir' (catchIOE . eh) where
    v = [INo.Create, INo.Delete, INo.MoveIn, INo.MoveOut, INo.CloseWrite]
    dir' = encodeString dir
    catchIOE op = op `E.catch` printError dir
    eh (INo.Created bd n) = touched bd n        
    eh (INo.Closed bd (Just n) True) = touched bd n
    eh (INo.MovedOut bd n _) = removed bd n
    eh (INo.MovedIn bd n _) = touched bd n        
    eh (INo.Deleted bd n) = removed bd n
    eh _ = action []
    touched bDir n =
        let name = decodeString n in
        let fullPath = dir </> name in
        FS.getModified fullPath >>= \ tMod ->
        let ev = Event True dir name bDir (fromUTC tMod) in
        action [ev]        
    removed bDir n =
        let name = decodeString n in
        getTime >>= \ tNow ->
        let tDel = tNow `subtractTime` 0.01 in
        let ev = Event False dir name bDir tDel in
        action [ev]


