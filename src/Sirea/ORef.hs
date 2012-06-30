
{-# LANGUAGE MultiParamTypeClasses #-}

-- | ORef stands for 'observable reference' - a variable maintained
-- by a partition and observable by RDP behaviors. An ORef variable
-- is not sensitive to demand (not directly, anyway); it is one to
-- many communication. This is useful for simple sensory signals if
-- they are cheap enough to provide even when there is no demand.
module Sirea.ORef
    ( ORefType(..)
    , ProvidesORef
    , ORef -- abstract
    , loadORef
    , readORef
    , writeORef
    , tuneORefGC
    , observeORef
    ) where

-- TODO: Generalize ORef internals for use with
--   * Demand Monitors
--   * Reactive State Models
--   * Filesystem Observers (named by values other than types...)
--   

import Data.Typeable
import Data.IORef
import Control.Applicative

import Sirea.PCX
import Sirea.BCX
--import Sirea.Partition (onNextStep)
import Sirea.Signal
import Sirea.Link 
import Sirea.Behavior
import Sirea.Time 
import Sirea.Partition
import Sirea.Internal.LTypes
--import Sirea.Internal.BImpl

-- | ORefType: some parameters for a new ORef. An ORef type should
-- adequately explain its purpose - i.e. instead of (Int,Int), use:
--
-- >  {-# LANGUAGE DerivingDataTypeable #-}
-- >  newtype MousePos = MousePos (Int,Int) 
-- >    deriving (Typeable,Eq,Show)
-- 
-- This can then be documented at the type, and the type serves as a
-- name and documentation for the resource. 
--
class (Typeable x) => ORefType x where
    -- | Default value for an ORef, i.e. if not assigned or when the
    -- written signal is `Nothing`.
    --
    -- If no suitable default value, developers should explicitly
    -- model a constructor similar to Nothing. Avoid `undefined`.
    orefDefault :: x

-- | ProvidesORef is a simple declaration that a given ORefType is
-- provided and maintained by a given partition.
--
-- This is necessary because otherwise it would be too easy to ask
-- for an ORef that the thread doesn't maintain, by accident. All 
-- the ORefs provided by a partition should be declared in proximity 
-- to the partition type (and could be given specific names.)
class (Partition p, ORefType x) => ProvidesORef p x 

-- | loadORef - build an ORef from resources in the partition. The
-- ORef cannot be obtained directly with findInPCX to enforce that
-- ProvidesORef was declared.
loadORef :: (ProvidesORef p x) => PCX p -> ORef x
loadORef = ORef . findInPCX

-- ORef exists mostly to hide the ORef resource, forcing clients to
-- use loadORef and declare the resource.
newtype ORef x = ORef (ORefR x)

-- The ORef resource is just a named IORef. This is not a shared 
-- IORef; the actual updates should occur only in the owning thread.
newtype ORefR x = ORefR (IORef (ORefSt x))

instance Typeable1 ORefR where
    typeOf1 _ = mkTyConApp tycORefR []
        where tycORefR = mkTyCon3 "Sirea" "ORef.Internal" "ORefR"
instance (ORefType x) => Resource (ORefR x) where
    locateResource _ = ORefR <$> newIORef initORefSt

-- details for an active ORef:
--   oref_hist   : history relative to stability
--   oref_signal : current signal associated with ORef (*)
--   oref_stable : current stability of current signal
--   oref_lsubs  : list of subscriptions with identifiers
--
-- a new subscriber should be added not when the ORef is created,
-- but rather when its subscription becomes active in the ORef's
-- owning partition. Similarly, unsubscribe if the ORef is inactive
-- for the foreseeable future. 
--
-- The subscriber is responsible for masking the signal to preserve
-- duration coupling.
-- 
data ORefSt x = ORefSt
    { oref_hist     :: !DT
    , oref_signal   :: !(SigSt x)
    , oref_lsubs    :: [(Int,LnkUp x)]
    , oref_subid    :: !Int
    --, oref_step     :: !(IO () -> IO ())
    }

orefDefHist :: DT
orefDefHist = 3.0 -- seconds
   

initORefSt :: (ORefType x) => ORefSt x
initORefSt = ORefSt 
    { oref_hist     = orefDefHist
    , oref_signal   = st_zero { st_signal = s_always orefDefault }
    , oref_lsubs    = []
    , oref_subid    = 10000
    }

-- | writeORef will set the future of the observable variable. For
-- use in a specific partition thread associated with `p`. 
--
-- The associated behaviors will delay processing until the next
-- runStepper operation. While using writeORef there may always be
-- a new task ready. Do avoid creating a busy update loop, perhaps
-- runStepper at the end of each round.
--
writeORef :: (ORefType x) => ORef x -> SigUp x -> IO ()
writeORef (ORef (ORefR rfR)) = writeORef' rfR . withDefault
    where withDefault = su_fmap (<|> s_always orefDefault)

-- at writeORef', the signal update is already merged with default.
writeORef' :: IORef (ORefSt x) -> SigUp x -> IO ()
writeORef' rf su = undefined
{-    readIORef rf >>= \ st ->
    mapM 
-}


-- | tuneORefGC - modify the default and the amount of history kept
-- for a specific ORef. The default is a few seconds of history,
-- which should be good in most cases. But there may be concerns if
-- the ORef describes a large value that updates rapidly and fails
-- to share much structure. This should be called when starting the
-- partition thread.
--
-- Keeping some history is necessary for dynamic behaviors. When
-- new subscribers are added, a brief history for state and ORef
-- signals makes Sirea RDP robust and eventually consistent for
-- straggling updates, scheduling hiccups, and startup order. 
--
-- GC is specified by a positive delta time (DT). This value is 
-- subtracted from signal stability (after each writeORef) and the
-- signal is trimmed to that point. No GC is performed if stability
-- is forever (reported as Nothing), so in that case the signal
-- should be constant.
--
tuneORefGC :: ORef x -> DT -> IO ()
tuneORefGC = undefined



-- | readORef will obtain the signal recently written to the ORef.
-- Does not include the orefDefaults or fallbacks. 
--
-- The ORef signal is always active, with the default value if not
-- otherwise specified.
readORef :: ORef x -> IO (Sig x)
readORef = undefined

-- | observe the variable from within the RDP application. The 
-- result is the ORef signal masked by the demand signal.
observeORef :: (ProvidesORef p x) => BCX w (S p ()) (S p x)
observeORef = undefined
    -- implementation:
    --   manage subscription based on signal.
    --   phase delay updates from writeORef.
    --   mask the signals when combining them.
    -- roughly:
    --   b (S p ()) (S p () :&: S p ())
    --   unsafe behavior to receive writes
    --   unsafe behavior to mask signals (unsafeSigZipB s_mask)
    --   b y (S p x) -- unsafe behavior to receive writes.
    --   
    --   






