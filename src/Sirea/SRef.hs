{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances, GADTs #-}

-- | SRef is intended to be a bridge between RDP behaviors and IO
-- processes. Values produced in IO can be sent to RDP observers,
-- and signals from RDP can be observed in IO. 
--
--   SRefO provides one-to-many communication from a partition.
--   SRefI provides many-to-one communication to the partition.
--
-- 
module Sirea.SRef 
    ( SRefO, getSRefO, writeSRef, bwatchSRef
    , SRefI, getSRefI, readSRef, addSRefEvent, bsignalSRef
    ) where

import Data.Maybe
import Data.IORef
import Data.Typeable
import Data.Function (fix)
import Control.Applicative
import Control.Exception (assert)
import Control.Monad (when)

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
newtype SRefO z = SRefO (MonitorDist (Maybe z), DT -> Sig z -> IO())
    deriving (Typeable)

instance (Partition p, Typeable z) => Resource p (SRefO z) where 
    locateResource _ = newSRefO

newSRefO :: (Partition p) => PCX p -> IO (SRefO z)
newSRefO cp = 
    newMonitorDist cp (s_always Nothing) >>= \ md ->
    mkStepDelay cp (primaryMonitorLnk md) >>= \ lu ->
    let getT = getStepTime cp in
    let write dtReq sz =
            let dtAct = max (negate dtDaggrHist) dtReq in
            getT >>= \ tNow ->
            let tu = tNow `addTime` dtAct in
            let smz = s_full_map Just (s_trim sz tu) in
            let su = SigUp { su_stable = Nothing 
                           , su_state = Just (smz,tu) } 
            in
            ln_update lu su
    in
    return (SRefO (md,write))

-- create a LnkUp that delays processing to the next step.
mkStepDelay :: (Partition p) => PCX p -> LnkUp z -> IO (LnkUp z)
mkStepDelay cp lz = lnStepDelay <$> newIORef Nothing where
    lnStepDelay rf = LnkUp { ln_touch = return (), ln_update = update rf }
    update rf = later . receive rf -- delays processing of SigUp until next step
    later = onNextStep cp
    receive rf su = 
        readIORef rf >>= \ mbsu ->
        case mbsu of 
            Nothing ->
                writeIORef rf (Just su) >>
                nextPhase (deliver rf) >>
                ln_touch lz
            Just su0 -> -- combine multiple upates (last one wins)
                let su' = su_piggyback su0 su in
                writeIORef rf (Just su')
    nextPhase = phaseDelay cp
    deliver rf =
        readIORef rf >>= \ mbsu ->
        assert ((not . isNothing) mbsu) $
        writeIORef rf Nothing >>
        maybe (return ()) (ln_update lz) mbsu

-- | for many-to-partition communication references
newtype SRefI z = SRefI (DemandAggr z [z], IORef (SigUp [z] -> IO ())) deriving (Typeable)

instance (Partition p, Typeable z) => Resource p (SRefI z) where
    locateResource _ cp = newSRefI cp

newSRefI :: (Partition p) => PCX p -> IO (SRefI z)
newSRefI cp =
    newIORef dropUpdates >>= \ rf ->
    let reportUpdate su =
            readIORef rf >>= \ onUpdate ->
            writeIORef rf dropUpdates >>
            onUpdate su
    in
    let later = atEndOfStep cp in
    let update su =
            let bDoNotify = (not . isNothing . su_state) su in
            when bDoNotify (later $ reportUpdate su)
    in
    let lu = LnkUp { ln_touch = return (), ln_update = update } in
    newDemandAggr cp lu sigListZip >>= \ da ->
    return (SRefI (da,rf))            

dropUpdates :: SigUp [z] -> IO ()
dropUpdates = const (return ())

sigListZip :: [Sig a] -> Sig [a]
sigListZip = foldr (s_full_zip jf) (s_always [])
  where jf (Just x) (Just xs) = Just (x:xs)
        jf _ xs = xs

--------------------------------------
---- Partition side manipulations ----
--------------------------------------

-- | Obtain an input SRef within a partition. An SRef is uniquely 
-- identified by type and a string name. Developers should usually
-- wrap getSRef operations to avoid mistakes with names and types.
-- An SRef should only be used from the corresponding partition.
getSRefI :: (Partition p, Typeable z) => PCX p -> String -> SRefI z
getSRefI = flip findByNameInPCX

-- | Obtain an output SRef within a partition. An SRef is uniquely
-- identified by type and a string name. Developers should usually
-- wrap getSRef operations to avoid mistakes with names and types.
-- An SRef should only be used from the corresponding partition.
getSRefO :: (Partition p, Typeable z) => PCX p -> String -> SRefO z
getSRefO = flip findByNameInPCX

-- | Write to an output SRef. The DT offset enables writing to the
-- future (positive value) or very recent past. Writing the future
-- reduces rework, but also introduces latency for reactions. There
-- is a limit how far back the past can be written (~50ms) and reach
-- beyond it will be silently truncated, but writing a little of the
-- past can be useful for retroactive correction and consistency.
writeSRef :: SRefO z -> DT -> Sig z -> IO ()
writeSRef (SRefO (_,write)) = write 

-- | Read the current input signal for an input SRef. This enables
-- polling of the current value, including access to the recent
-- past (up to a couple heartbeats). This is essentially a polling
-- model, and may lose information. Suggest use of addSRefEvent if
-- you need reliable processing for the data.
--
-- NOTE: Developers must treat the list as a set, i.e. not depending
-- on order or duplicates. A list is used to avoid Ord constraints.
readSRef :: SRefI z -> IO (Sig [z])
readSRef (SRefI (da,_)) = pollDemandAggr da

-- | Add a callback to hear the next signal update to an input SRef.
-- The callbacks run at the end of the next runStepper step in which
-- a signal state (su_state) update occurs. 
--
-- The SRefEvent is a one-time callback. Developers must explicitly
-- reset it if they want to hear the next update, etc.. 
--
-- NOTE: Developers must treat the list as a set, i.e. not depending
-- on order or duplicates. A list is used to avoid Ord constraints.
addSRefEvent :: SRefI z -> (SigUp [z] -> IO ()) -> IO ()
addSRefEvent (SRefI (_,rf)) ev = modifyIORef rf addEv where
    addEv oldOps su = (oldOps su) >> ev su

-----------------------------------------
---- SRef access and update from RDP ----
-----------------------------------------

-- | Observe the signal from an output SRef managed by a partition.
-- If the signal is not set by the partition for a period, watch 
-- will report an active Nothing signal.
bwatchSRef :: (Partition p, Typeable z) => String -> BCX w (S p ()) (S p (Maybe z))
bwatchSRef nm = fix $ \ b -> wrapBCX $ \ cw ->
    let cp = getPCX b cw in    
    let SRefO (md,_) = getSRefO cp nm in
    monitorFacetB md

-- | Send a signal to an input SRef to be observed in partition.
-- Mutiple signals may be sent and processed concurrently.
bsignalSRef :: (Partition p, Typeable z) => String -> BCX w (S p z) (S p ())
bsignalSRef nm = fix $ \ b -> wrapBCX $ \ cw ->
    let cp = getPCX b cw in
    let SRefI (da,_) = getSRefI cp nm in
    demandFacetB da

getPCX :: (Partition p) => BCX w (S p a) z -> PCX w -> PCX p
getPCX _ = findInPCX

monitorFacetB :: MonitorDist z -> B (S p ()) (S p z)
monitorFacetB md = unsafeLinkB lnMon
    where lnMon LnkDead = return LnkDead -- dead code elim.
          lnMon (LnkSig lu) = LnkSig <$> newMonitorLnk md lu

demandFacetB :: DemandAggr e z -> B (S p e) (S p ())
demandFacetB da = bvoid (unsafeLinkB lnDem) >>> bconst ()
    where lnDem ln = assert (ln_dead ln) $ LnkSig <$> newDemandLnk da

