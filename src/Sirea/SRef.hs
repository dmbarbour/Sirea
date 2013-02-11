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
data SRefO z = SRefO !(MonitorDist (Maybe z)) !(T -> Sig z -> IO())
    deriving (Typeable)

instance (Partition p, Typeable z) => Resource p (SRefO z) where 
    locateResource _ = newSRefO

newSRefO :: (Partition p) => PCX p -> IO (SRefO z)
newSRefO cp = 
    getPSched cp >>= \ pd ->
    newMonitorDist cp (s_always Nothing) >>= \ md ->
    newIORef Nothing >>= \ rf ->
    let lu = primaryMonitorLnk md in
    return (newSRefO' pd md rf lu)

newSRefO' :: PSched -> MonitorDist (Maybe z) 
          -> IORef (Maybe (T,Sig z)) -> LnkUp (Maybe z)
          -> SRefO z
newSRefO' pd md rf lu = SRefO md write where
    write tU su = -- protect stability for low tU values.
        p_stepTime pd >>= \ tNow ->
        let tU' = max tU (tNow `subtractTime` dtDaggrHist) in
        write' tU' su
    write' tU su = -- record the update; activate for next step
        readIORef rf >>= \ mbup ->
        case mbup of
            Nothing ->
                writeIORef rf (Just (tU,su)) >>
                p_onNextStep pd activate
            Just (tU0,su0) ->
                if (tU0 >= tU) 
                    then writeIORef rf (Just (tU,su)) 
                    else writeIORef rf (Just (tU0, s_switch su0 tU su))
    activate =
        readIORef rf >>= \ mbup ->
        writeIORef rf Nothing >>
        case mbup of
            Nothing -> return ()
            Just (tU,su) -> 
                let smz = s_full_map Just su in
                let action = ln_update lu DoneT tU smz in
                p_onUpdPhase pd action >>
                ln_touch lu


-- | for many-to-partition communication references
data SRefI z = SRefI !(DemandAggr z [z]) !(IORef (T -> Sig [z] -> IO ()))
    deriving (Typeable)

instance (Partition p, Typeable z) => Resource p (SRefI z) where
    locateResource _ cp = newSRefI cp

newSRefI :: (Partition p) => PCX p -> IO (SRefI z)
newSRefI cp = 
    newIORef ((const . const . return) ()) >>= \ rf ->
    getPSched cp >>= \ pd ->
    newDemandAggr cp (lnkSRefI pd rf) sigListZip >>= \ da ->
    return (SRefI da rf) 

lnkSRefI :: PSched -> IORef (T -> Sig zs -> IO ()) -> LnkUp zs
lnkSRefI pd rf = LnkUp touch update idle where
    touch = return ()
    idle _ = return ()
    update _ tU su = p_onStepEnd pd $
        readIORef rf >>= \ eventHandler ->
        eventHandler tU su
    
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
getSRefI :: (Partition p, Typeable z) => PCX p -> String -> IO (SRefI z)
getSRefI = flip findByNameInPCX

-- | Obtain an output SRef within a partition. An SRef is uniquely
-- identified by type and a string name. Developers should usually
-- wrap getSRef operations to avoid mistakes with names and types.
-- An SRef should only be used from the corresponding partition.
getSRefO :: (Partition p, Typeable z) => PCX p -> String -> IO (SRefO z)
getSRefO = flip findByNameInPCX

-- | Write to an output SRef. Developers must specify at which time
-- their updates begin to apply, but this may be silently truncated
-- for deep retroactive updates (more than ~50ms). To reduce rework, 
-- it's preferable to update the future.
writeSRef :: SRefO z -> T -> Sig z -> IO ()
writeSRef (SRefO _ write) = write 

-- | Read the current input signal for an input SRef. This enables
-- polling of the current value, including access to the recent
-- past (up to a couple heartbeats). This is essentially a polling
-- model, and may lose information. Suggest use of addSRefEvent if
-- you need reliable processing for the data.
--
-- NOTE: Developers must treat the list as a set, i.e. not depending
-- on order or duplicates. A list is used to avoid Ord constraints.
readSRef :: SRefI z -> IO (Sig [z])
readSRef (SRefI da _) = pollDemandAggr da

-- | Add a callback to hear the next signal update to an input SRef.
-- The callbacks run at the end of the next runStepper step in which
-- a signal state (su_state) update occurs. 
--
-- The SRefEvent is a one-time callback. Developers must explicitly
-- reset it if they want to hear the next update, etc.. 
--
-- NOTE: Developers must treat the list as a set, i.e. not depending
-- on order or duplicates. A list is used to avoid Ord constraints.
addSRefEvent :: SRefI z -> (T -> Sig [z] -> IO ()) -> IO ()
addSRefEvent (SRefI _ rf) ev = modifyIORef rf addEv where
    addEv oldOps su = (oldOps su) >> ev su

-----------------------------------------
---- SRef access and update from RDP ----
-----------------------------------------

-- | Observe the signal from an output SRef managed by a partition.
-- If the signal is not set by the partition for a period, watch 
-- will report an active Nothing signal.
bwatchSRef :: (Partition p, Typeable z) => String -> BCX w (S p ()) (S p (Maybe z))
bwatchSRef nm = fix $ \ b -> wrapBCX $ \ cw ->
    let getMD = getPCX b cw >>= \ cp -> 
                getSRefO cp nm >>= \ (SRefO md _) ->
                return md
    in
    monitorFacetB getMD

-- | Send a signal to an input SRef to be observed in partition.
-- Mutiple signals may be sent and processed concurrently.
bsignalSRef :: (Partition p, Typeable z) => String -> BCX w (S p z) (S p ())
bsignalSRef nm = fix $ \ b -> wrapBCX $ \ cw ->
    let getDA = getPCX b cw >>= \ cp -> 
                getSRefI cp nm >>= \ (SRefI da _) -> 
                return da 
    in
    demandFacetB getDA 

getPCX :: (Partition p) => BCX w (S p a) z -> PCX w -> IO (PCX p)
getPCX _ = findInPCX

monitorFacetB :: IO (MonitorDist z) -> B (S p ()) (S p z)
monitorFacetB getMD = unsafeLinkB lnMon
    where lnMon LnkDead = return LnkDead -- dead code elim.
          lnMon (LnkSig lu) = 
            getMD >>= \ md ->
            LnkSig <$> newMonitorLnk md lu

demandFacetB :: IO (DemandAggr e z) -> B (S p e) (S p ())
demandFacetB getDA = bvoid (unsafeLinkB lnDem) >>> bconst ()
    where lnDem ln = assert (ln_dead ln) $
            getDA >>= \ da -> 
            LnkSig <$> newDemandLnk da

