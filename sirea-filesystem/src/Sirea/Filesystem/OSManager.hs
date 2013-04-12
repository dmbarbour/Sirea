
{-# LANGUAGE CPP #-}

-- Select the OS-specific manager.
module Sirea.Filesystem.OSManager (newManager) where

#if defined(USE_POLLING)
import Sirea.Filesystem.Manager
import Sirea.Filesystem.Polling
#elif defined(OS_Linux)
import Sirea.Filesystem.Linux
#elif defined(OS_Windows)
import Sirea.Filesystem.Windows
#elif defined(OS_OSX)
import Sirea.Filesystem.OSX
#endif

#if defined(USE_POLLING)
dtPoll :: DT 
dtPoll = 3.0 -- seconds

newManager :: MkManager
newManager = newPollingManager dtPoll
#endif

