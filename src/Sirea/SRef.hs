{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances, GADTs #-}

-- | This module provides observable, mutable references to support 
-- integration with effectful partitions. Two reference types are
-- provided - effectively, one for each half of a demand monitor.
--
--   an SRefO supports one-to-many communications from a partition
--   an SRefI supports many-to-one communication to a partition
--
-- Corresponding behaviors can access these values. The SRefO is
-- updated by procedure, while the SRefI causes an event to fire
-- when data is available.
--
-- This module assumes partitions use a static set of SRefs, so they
-- must be declared (using a typeclass) and accessed only by type.  
--
-- The SRef mechanisms are essentially the different halves of the
-- demand monitor made accessible to the partition thread.
--
-- In general, SRef instances should not be exposed to clients 
-- of a library. Hide them as an implementation detail.
--
-- (this module isn't tested yet)
module Sirea.SRef
    ( DeclareSRefO, SRefO, publishSRef, bObserveSRef
    , DeclareSRefI, SRefI, bSignalSRef, setHandlerSRef
    ) where

import Data.Maybe
import Data.IORef
import Data.Typeable
import Data.Function (fix)
import Control.Applicative
import Control.Exception (assert)

import Sirea.Signal
import Sirea.Behavior
import Sirea.Time
import Sirea.PCX
import Sirea.BCX
import Sirea.B
import Sirea.Partition
import Sirea.Link

import Sirea.Internal.Tuning (dtDaggrHist)
import Sirea.Internal.DemandMonitorData

-- | for partition-to-many communication references
newtype SRefO z = SRefO (MonitorDist (Maybe z), IORef (Maybe (SigUp (Maybe z)))) deriving (Typeable)

-- | for many-to-partition communication references
newtype SRefI z = SRefI (DemandAggr z [z], IORef (SigUp [z] -> IO ())) deriving (Typeable)


-- | Declare that an signal provided by a partition.
class (Partition p, Typeable z) => DeclareSRefO p z

-- | Declare that a signal is observed by a partition.
class (Partition p, Typeable z) => DeclareSRefI p z

instance (DeclareSRefO p z) => Resource p (SRefO z) where
    locateResource _ cp = newSRefO cp

instance (DeclareSRefI p z) => Resource p (SRefI z) where
    locateResource _ cp = newSRefI cp

newSRefO :: (DeclareSRefO p z) => PCX p -> IO (SRefO z)
newSRefO cp = 
    newIORef Nothing >>= \ rf ->
    newMonitorDist cp (pure Nothing) >>= \ md ->
    return (SRefO (md,rf))

newSRefI :: (DeclareSRefI p z) => PCX p -> IO (SRefI z)
newSRefI cp =
    newIORef (const (return ())) >>= \ rf ->
    let later = atEndOfStep cp in
    let update su = later $             
            readIORef rf >>= \ handleUpdate ->
            handleUpdate su
    in
    let lu = LnkUp { ln_touch = return (), ln_update = update } in
    newDemandAggr cp lu sigListZip >>= \ da ->
    return (SRefI (da,rf))            

sigListZip :: [Sig a] -> Sig [a]
sigListZip = foldr (s_full_zip jf) (s_always [])
  where jf (Just x) (Just xs) = Just (x:xs)
        jf _ xs = xs
 
-- | Observe an IO-thread maintained signal reference. If the value
-- is not specified, a Nothing value may be returned for the period.
bObserveSRef :: (DeclareSRefO p z) => BCX w (S p ()) (S p (Maybe z))
bObserveSRef = fix $ \ b -> wrapBCX $ \ cw -> 
    let cp = getPCX b cw in
    let SRefO (md,_) = findInPCX cp in
    monitorFacetB md

monitorFacetB :: MonitorDist z -> B (S p ()) (S p z)
monitorFacetB md = unsafeLinkB lnMon
    where lnMon LnkDead = return LnkDead -- dead code elim.
          lnMon (LnkSig lu) = LnkSig <$> newMonitorLnk md lu

getPCX :: (Partition p) => BCX w (S p x) (S p y) -> PCX w -> PCX p
getPCX = const findInPCX

-- | Update an observable reference. You can update present, future,
-- or even a little into the past (subject to silent truncation*).
-- By updating the future, you can reduce computation pressure on
-- downstream clients (and reduce rework), so that's preferable when
-- the domain allows for it. 
--
-- A published signal is distributed on the next runStepper action.
-- Publishing will also trigger the stepper event if any is set.
-- 
-- * retroactive corrections are not always possible, and the window
-- closes quickly (more for some domains than others, such as sound
-- or video). 
-- 
publishSRef :: (DeclareSRefO p z) => PCX p -> DT -> Sig z -> IO ()
publishSRef cp = publish where
    SRefO (md,rf) = findInPCX cp
    ln = primaryMonitorLnk md 
    nextStep = onNextStep cp
    nextPhase = phaseDelay cp
    publish dt sz =
        let dtUp = max (negate dtDaggrHist) dt in
        getStepTime cp >>= \ tNow ->
        let tUp = tNow `addTime` dtUp in
        let sz' = s_full_map Just sz in
        let su = SigUp { su_state = Just (sz',tUp), su_stable = Nothing } in
        readIORef rf >>= \ mbsu ->
        case mbsu of 
            Nothing -> writeIORef rf (Just su) >>
                       nextStep initUpdate
            Just su0 -> let su' = su_piggyback su0 su in
                        writeIORef rf (Just su')
    initUpdate = nextPhase update >> ln_touch ln
    update =
        readIORef rf >>= \ mbsu ->
        let su = fromMaybe (SigUp Nothing Nothing) mbsu in
        writeIORef rf Nothing >>
        ln_update ln su

-- | Influence state of a reference (via signal). Signals to the
-- SRef are observable after after the p
bSignalSRef :: (DeclareSRefI p z) => BCX w (S p z) (S p ())
bSignalSRef = fix $ \ b -> wrapBCX $ \ cw -> 
    let cp = getPCX b cw in
    let SRefI (da,_) = findInPCX cp in
    demandFacetB da

demandFacetB :: DemandAggr e z -> B (S p e) (S p ())
demandFacetB da = bvoid (unsafeLinkB lnDem) >>> bconst ()
    where lnDem ln = assert (ln_dead ln) $ LnkSig <$> newDemandLnk da


-- | An SRefI enables a partition to easily receive signals from the
-- RDP layer. This is modeled by setting an event handler for the
-- updates. If you're going to handle SRef updates, it is generally
-- necessary to set the handler before first runStepper event (or
-- you'll miss the initial values). If no handler is set, updates
-- are dropped. 
--
-- Events are handled in runStepper, but in the last phase. 
setHandlerSRef :: (DeclareSRefI p z) => PCX p -> (SigUp [z] -> IO ()) -> IO ()
setHandlerSRef cp = writeIORef rf
    where SRefI (_,rf) = findInPCX cp
        
-- TODO: consider setting an event instead of polling handleSRef?




