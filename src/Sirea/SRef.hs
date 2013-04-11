{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances, GADTs #-}

-- | SRef is intended to be a bridge between RDP behaviors and IO
-- processes. Values produced in IO can be sent to RDP observers,
-- and signals from RDP can be observed in IO. 
--
--   SRefO provides one-to-many communication from a partition.
--   SRefI provides many-to-one communication to the partition.
--
-- SRefs are essentially observable variables, and are owned by one
-- partition. Naturally, the partition must be aware of the variable
-- for it to be useful, and must update or observe the variable as 
-- part of its common loop. This undermines their utility somewhat.
--
-- In cases where you might use SRef, consider AgentResource as an
-- alternative. Both enable centralized processing in one partition,
-- but AgentResource offers more freedom to express behavior in RDP
-- and without invasive edits of the Partition instance.
-- 
module Sirea.SRef 
    ( SRefO, getSRefO, writeSRef, writeSRefDT, bwatchSRef
    , SRefI, getSRefI, readSRef, addSRefEvent, bsignalSRef
    ) where

import Data.IORef
import Data.Typeable
import Control.Monad (unless,when)
import Control.Exception (assert)

import Sirea.Signal
import Sirea.Behavior
import Sirea.Time
import Sirea.PCX
import Sirea.B
import Sirea.Partition
import Sirea.UnsafeLink

import Sirea.Internal.Tuning (dtDaggrHist, tAncient)
import Sirea.Internal.DemandMonitorData

-- | for partition-to-many communication references
--
-- In addition to processing normal updates, SRefO will
-- periodically increase stability until the signal is
-- in a final state.
data SRefO z = SRefO
    { sro_mdist  :: !(MonitorDist (Maybe z))
    , sro_link   :: !(LnkUp (Maybe z))
    , sro_data   :: !(IORef (SROD z))
    , sro_psched :: !PSched
    } deriving (Typeable)

data SROD z = SROD
    { srod_signal :: !(Sig z)   -- current signal
    , srod_stable :: !StableT   -- last reported stability
    , srod_tmup   :: !(Maybe T) -- update time
    , srod_active :: !Bool      -- update scheduled
    , srod_flush  :: !Bool      -- flush scheduled
    }

srodZero :: SROD z
srodZero = SROD s_never (StableT tAncient) Nothing False False

instance (Partition p, Typeable z) => Resource p (SRefO z) where 
    locateResource _ = newSRefO
instance (Partition p, Typeable z) => NamedResource p (SRefO z)

newSRefO :: (Partition p) => PCX p -> IO (SRefO z)
newSRefO cp = 
    getPSched cp >>= \ pd ->
    newMonitorDist pd (s_always Nothing) >>= \ md ->
    newIORef srodZero >>= \ rf ->
    let ln = primaryMonitorLnk md in
    return (SRefO md ln rf pd)


-- | for many-to-partition communication references
data SRefI z = SRefI 
    { sri_daggr  :: !(DemandAggr z [z])
    , sri_data   :: !(IORef (SRID z))
    , _sri_psched :: !PSched
    } deriving (Typeable)
data SRID z = SRID
    { srid_event  :: !(Maybe (T -> Sig [z] -> IO ()))
    , srid_signal :: !(Sig [z])
    }

sridZero :: SRID z
sridZero = SRID Nothing s_never

instance (Partition p, Typeable z) => Resource p (SRefI z) where 
    locateResource _ = newSRefI
instance (Partition p, Typeable z) => NamedResource p (SRefI z)

newSRefI :: (Partition p) => PCX p -> IO (SRefI z)
newSRefI cp = 
    newIORef sridZero >>= \ rf -> 
    getPSched cp >>= \ pd ->
    let lu = lnkSRefI pd rf in
    newDemandAggr pd lu sigListZip >>= \ da ->
    return (SRefI da rf pd) 

lnkSRefI :: PSched -> IORef (SRID z) -> LnkUp [z]
lnkSRefI pd rf = LnkUp touch update idle cyc where
    touch = return ()
    cyc _ = return () 
    idle tS = -- GC recorded signal
        readIORef rf >>= \ srid ->
        let s' = s_trim (srid_signal srid) (inStableT tS) in
        let srid' = srid { srid_signal = s' } in
        srid' `seq` writeIORef rf srid' 
    update tS tU su = -- update, GC, trigger
        readIORef rf >>= \ srid ->
        let s1 = s_switch' (srid_signal srid) tU su in
        let s' = s_trim s1 (inStableT tS) in
        let srid' = SRID Nothing s' in
        srid' `seq` writeIORef rf srid' >>
        case srid_event srid of
            Nothing -> return ()
            Just ev -> onStepEnd pd $ ev tU su

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
-- for deep retroactive updates - less than about -50ms. To reduce 
-- rework, it's preferable to update the future. 
--
-- Note: while the input signal is inactive, active observers will
-- see a 'Nothing' value. The input signal starts inactive.
--
-- Note: Writes are safe from any thread, though writes to an SRefO
-- should be serialized.
writeSRef :: SRefO z -> T -> Sig z -> IO ()
writeSRef sro tU0 su = onNextStep (sro_psched sro) $
    readIORef (sro_data sro) >>= \ srod ->
    let s0 = srod_signal srod in
    let tS0 = srod_stable srod in
    let tU = max tU0 (inStableT tS0) in
    let sf = s_switch' s0 tU su in
    let tmup' = Just $! maybe tU (min tU) (srod_tmup srod) in
    let srod' = srod { srod_signal = sf, srod_tmup = tmup' } in
    srod' `seq` writeIORef (sro_data sro) srod' >>
    activateSRO sro

-- idempotent activation within a step
activateSRO :: SRefO z -> IO ()
activateSRO sro =
    readIORef (sro_data sro) >>= \ srod ->
    unless (srod_active srod) $
        let srod' = srod { srod_active = True } in
        writeIORef (sro_data sro) srod' >>
        onUpdPhase (sro_psched sro) (deliverSRO sro) >>
        ln_touch (sro_link sro)        

-- | Write to an output SRef using relative time for the update
-- based on the current clock value. This is sometimes convenient,
-- but does risk inconsistency with external change events.
writeSRefDT :: SRefO z -> DT -> Sig z -> IO ()
writeSRefDT sro dt su =
    stepTime (sro_psched sro) >>= \ tNow ->
    let tU = tNow `addTime` dt in
    writeSRef sro tU su


-- deliver update associated with SRO
deliverSRO :: SRefO z -> IO ()
deliverSRO sro =
    readIORef (sro_data sro) >>= \ srod ->
    assert (srod_active srod) $
    stepTime (sro_psched sro) >>= \ tNow ->
    let tCut = tNow `subtractTime` dtDaggrHist in
    let tS = StableT tCut in
    let s' = s_trim (srod_signal srod) tCut in
    let bNeedFlush = not (s_is_final s' tCut) in
    let bInitFlush = not (srod_flush srod) && bNeedFlush in
    let bFlushSched = srod_flush srod || bNeedFlush in
    let srod' = SROD { srod_signal = s'
                     , srod_stable = tS
                     , srod_tmup = Nothing
                     , srod_active = False
                     , srod_flush = bFlushSched
                     }
    in
    srod' `seq` writeIORef (sro_data sro) srod' >>
    when bInitFlush (eventually (sro_psched sro) (flushSRO sro)) >>
    case srod_tmup srod of
        Nothing -> ln_idle (sro_link sro) tS
        Just tU -> 
            let s0 = s_trim (srod_signal srod) tU in
            let su = s_full_map Just s0 in
            ln_update (sro_link sro) tS tU su    

-- An SRefO that is not in its final state may idle until it has
-- cleared existing state. This is scheduled by activate, and it
-- simply schedules a new activation on a later heartbeat.
flushSRO :: SRefO z -> IO ()
flushSRO sro =
    readIORef (sro_data sro) >>= \ srod ->
    assert (srod_flush srod) $
    let srod' = srod { srod_flush = False } in
    let s0 = srod_signal srod in
    let tS = srod_stable srod in
    let bDone = s_is_final s0 (inStableT tS) in
    writeIORef (sro_data sro) srod' >>
    unless bDone (activateSRO sro)


-- | Read the current input signal for an input SRef. This enables
-- polling of the current value, including access to the recent
-- past (up to a couple heartbeats). This is essentially a polling
-- model, and may lose information. Suggest use of addSRefEvent if
-- you need reliable processing for the data.
--
-- NOTE: the list of values [z] should be treated as a set, i.e. any
-- duplicate values and ordering should not affect behavior of the
-- application. It is provided as a list to avoid Ord constraints.
readSRef :: SRefI z -> IO (Sig [z])
readSRef = fmap srid_signal . readIORef . sri_data
    

-- | Add a callback to hear the next signal update to an input SRef.
-- The callbacks run at the end of the next runStepper step in which
-- a signal state update occurs. (Stability increases do not trigger
-- this event.)
--
-- The SRefEvent is a one-time callback. Developers must explicitly
-- reset it when they want to hear the next update. Usually, it is
-- best to reset upon receiving the event. SRefI is choked using the
-- same means as demand monitors, so updates may delay a little if
-- they don't apply right away.
--
-- NOTE: the list of values [z] should be treated as a set, i.e. any
-- duplicate values and ordering should not affect behavior of the
-- application. It is provided as a list to avoid Ord constraints.
addSRefEvent :: SRefI z -> (T -> Sig [z] -> IO ()) -> IO ()
addSRefEvent sri ev = modifyIORef (sri_data sri) addEvSRI where
    addEvSRI srid = 
        let ev' = addEv (srid_event srid) in
        srid { srid_event = ev' }
    addEv Nothing = Just ev
    addEv (Just ev0) = Just (jf ev0 ev)
    jf e1 e2 tU su = e1 tU su >> e2 tU su


-----------------------------------------
---- SRef access and update from RDP ----
-----------------------------------------

-- | Observe the signal from an output SRef managed by a partition.
-- If the signal is not set by the partition for a period, watch 
-- will report an active Nothing signal.
bwatchSRef :: (Partition p, Typeable z) => String -> B (S p ()) (S p (Maybe z))
bwatchSRef nm = monitorFacetB $ \ cp -> sro_mdist `fmap` getSRefO cp nm

-- | Send a signal to an input SRef to be observed in partition.
-- Mutiple signals may be sent and processed concurrently.
bsignalSRef :: (Partition p, Typeable z) => String -> B (S p z) (S p ())
bsignalSRef nm = demandFacetB $ \ cp -> sri_daggr `fmap` getSRefI cp nm

monitorFacetB :: (Partition p) => (PCX p -> IO (MonitorDist z)) -> B (S p ()) (S p z)
monitorFacetB getMD = unsafeLinkBL lnMon
    where lnMon cw lu = getMD cw >>= flip newMonitorLnk lu

demandFacetB :: (Partition p) => (PCX p -> IO (DemandAggr e z)) -> B (S p e) (S p ())
demandFacetB getDA = bvoid (unsafeLinkB_ lnDem) >>> bconst ()
    where lnDem cw = getDA cw >>= newDemandLnk


