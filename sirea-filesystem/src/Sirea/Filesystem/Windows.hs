

module Sirea.Filesyste.Windows
    ( newManager
    ) where

import Sirea.Filesystem.Polling
import Debug.Trace

newManager :: MkManager 
newManager eh =
    traceIO ("TODO: Windows notifications using Win32-notify.") >>
    newPollingManager eh

