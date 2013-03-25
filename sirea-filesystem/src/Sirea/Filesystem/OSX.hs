

module Sirea.Filesyste.OSX
    ( newManager
    ) where

import Sirea.Filesystem.Polling
import Debug.Trace

newManager :: MkManager 
newManager eh =
    traceIO ("TODO: OSX notifications (using hfsevents).") >>
    newPollingManager eh


