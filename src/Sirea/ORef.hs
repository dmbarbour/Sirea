
{-# LANGUAGE MultiParamTypeClasses #-}

-- | ORef stands for 'observable reference'.
--
-- The essential concept is:
--   a variable
--   describing environment
--   observed by many
--
-- ORef is simplistic. It doesn't support rich queries or multiple
-- views. But it can serve a useful role in resource adapters and 
-- simple signal sources such as framerates or mouse position.
--
-- Each ORef is identified by type and partition. In practice, this
-- means the set of ORefs must be known statically and are a very
-- small set of values. But ORefs are best used sparingly; they are
-- an impoverished, inflexible reactive concept compared to RDP's 
-- rich use of demand values and demand monitors.
--
-- For Sirea and RDP, the full future of the ORef is updated, not
-- just the current value. The main reasons for this are:
--   * easy modeling for models that change in stable ways
--   * shift updates slightly to the future, to avoid rework
--   * support anticipation (bpeek) like other RDP resources
--   * model precise, consistent, logical timing for signals
--
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

import Data.Typeable
import Data.IORef
import Control.Applicative

import Sirea.PCX
import Sirea.BCX
import Sirea.Signal
import Sirea.Link 
import Sirea.Behavior
import Sirea.Time (DT)

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
-- provided and maintained by a given partition type.
--
-- This is necessary because otherwise it would be too easy to ask
-- for an ORef that the thread doesn't maintain, by accident. All 
-- the ORefs provided by a partition should be declared in proximity 
-- to the partition type (and could be given specific names.)
class (Typeable p, ORefType x) => ProvidesORef p x 

-- | loadORef - build an ORef from resources in the partition. The
-- ORef cannot be obtained directly with findInPCX to enforce that
-- ProvidesORef was declared.
loadORef :: (ProvidesORef p x) => PCX p -> ORef x
loadORef = ORef . findInPCX

newtype ORef x = ORef { inORef :: ORefR x } 
newtype ORefR x = ORefR { inORefR :: IORef (ORefSt x) } 

instance Typeable1 ORefR where
    typeOf1 _ = mkTyConApp tycORefR []
        where tycORefR = mkTyCon3 "Sirea" "ORef.Internal" "ORefR"
instance (ORefType x) => Resource (ORefR x) where
    locateResource _ = ORefR <$> newIORef defaultORefSt

data ORefSt x = ORefSt

defaultORefSt :: (ORefType x) => ORefSt x
defaultORefSt = undefined


-- | writeORef will set the future of the observable variable. For
-- use in a specific partition thread associated with `p`. Updates
-- should respect stability values (i.e. be monotonic in stability,
-- and updates apply no earlier than prior stability.) 
--
-- The actual update is deferred until the next runStepper action.
-- Developers using writeORef should runStepper at the end of each
-- round - i.e. to clear the queue before waiting on new work.
--
writeORef :: ORef x -> SigUp x -> IO ()
writeORef = undefined


orefDefaultHistory :: DT
orefDefaultHistory = 3.0 -- seconds

-- | tuneORefGC - modify the default and the amount of history kept
-- for a specific ORef. The default is a few seconds of history,
-- which should be good in most cases. But there may be concerns if
-- the ORef describes a large value that updates rapidly and fails
-- to share much structure.
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



-- | readORef will obtain the recent values of the ORef, as of the
-- recent runStepper operation. For use in the specific partition
-- thread associated with `p`. 
--
-- The ORef signal is always active, with the default value if not
-- otherwise specified.
readORef :: ORef x -> IO (Sig x)
readORef = undefined

-- | observe the variable from within the RDP application. The 
-- result is the ORef signal masked by the demand signal.
observeORef :: (ProvidesORef p x) => BCX w (S p ()) (S p x)
observeORef = undefined




