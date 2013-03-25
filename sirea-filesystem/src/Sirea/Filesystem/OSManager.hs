
{-# LANGUAGE CPP #-}

-- Select the OS-specific manager.
module Sirea.Filesystem.OSManager (newManager) where

#if defined(USE_POLLING)
import Sirea.Filesystem.Polling
#elif defined(OS_Linux)
import Sirea.Filesystem.Linux
#elif defined(OS_Windows)
import Sirea.Filesystem.Windows
#elif defined(OS_OSX)
import Sirea.Filesystem.OSX
#endif

#ifdef USE_POLLING
newManager :: MkManager
newManager = newPollingManager
#endif

