{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances #-}

-- | TimeStamp is a simple state model to capture times associated
-- with eventful observations, such as when the application starts
-- or when the mouse enters a window. More precisely, TimeStamp will
-- capture the start of any continuous demand for the timestamp. The
-- timestamp forgets its value when the demand releases. Because it
-- is naturally volatile, TimeStamp is not persisted.
--
-- This is useful in conjunction with TimeTrigger to model delayed
-- action, or with state models to help distinguish events such as
-- button presses - by adding the timestamp. 
--
-- This can be used as an activity monitor with extra information on
-- the start time of the activity, via btimeStampMon.
--
module Sirea.TimeStamp 
    ( btimeStamp
    , btimeStampMon
    ) where

import Control.Applicative
import Data.IORef
import Data.Typeable
import Data.Maybe (fromMaybe)
import Sirea.Time
import Sirea.PCX
import Sirea.Partition
import Sirea.UnsafeLink
import Sirea.Behavior
import Sirea.B
import Sirea.Signal
import Sirea.Internal.DemandMonitorData
import Sirea.Internal.B0Impl (wrapLnEqShift, unsafeSigZipB0)
import Sirea.Internal.SigType

-- | Each partition can have multiple timestamps, distinguished by
-- string identifiers. The `btimeStamp` behavior will both activate
-- the timestamp and return its value. In case of dynamic behavior,
-- it is possible to handoff the timestamp without losing its value.
btimeStamp :: (Partition p) => String -> B (S p ()) (S p T)
btimeStamp nm = bvoid put >>> get where
    put = unsafeLinkB_ (tsPut nm)
    get = unsafeLinkBL (tsGet nm)

tsGet :: (Partition p) => String -> PCX p -> LnkUp T -> IO (LnkUp ())
tsGet nm cp lu =
    findByNameInPCX nm cp >>= \ ts ->
    newMonitorLnk (ts_md ts) lu

tsPut :: (Partition p) => String -> PCX p -> IO (LnkUp ())
tsPut nm cp =
    findByNameInPCX nm cp >>= \ ts ->
    newDemandLnk (ts_da ts)

-- | Passively observe the timestamp, reporting whether it is active
-- and maybe the time of activity. Does not activate the timestamp.
btimeStampMon :: (Partition p) => String -> B (S p ()) (S p (Maybe T))
btimeStampMon nm = bdup >>> bfirst get >>> remask where
    get = unsafeLinkBL (tsGet nm)
    remask = wrapB . const $ unsafeSigZipB0 zmon

-- needed to shift `Sig T` from MonitorDist to `Sig (Maybe T)` 
zmon :: Sig T -> Sig () -> Sig (Maybe T)
zmon sT s1 = s_mask (s_full_map Just sT) s1

data TS = TS 
    { ts_da  :: !(DemandAggr () ())
    , ts_md  :: !(MonitorDist T)
    } deriving (Typeable)

instance (Partition p) => Resource p TS where locateResource _ = newTS
instance (Partition p) => NamedResource p TS

newTS :: (Partition p) => PCX p -> IO TS
newTS cp =
    newIORef s_never >>= \ rf -> -- memory  
    getPSched cp >>= \ pd ->
    newMonitorDist pd s_never >>= \ md ->
    let luMon = primaryMonitorLnk md in
    let luTS = tsLink rf luMon in
    wrapLnEqShift (==) luTS >>= \ luEq ->
    newDemandAggr pd luEq sigActive >>= \ da ->
    return (TS da md)
    
sigActive :: [Sig a] -> Sig ()
sigActive [] = s_never
sigActive (s:ss) = s_const () $ foldl (<|>) s ss

-- the link will track the values up through stability, and will use
-- the current value to process the Sig () into a Sig T.    
tsLink :: IORef (Sig T) -> LnkUp T -> LnkUp ()
tsLink rf lu = LnkUp touch update idle cyc where
    cyc = ln_cycle lu
    touch = ln_touch lu
    idle tS = 
        readIORef rf >>= \ s0 ->
        let sCln = s_trim_prior s0 (inStableT tS) in
        sCln `seq` writeIORef rf sCln >>
        ln_idle lu tS
    update tS tU su =
        readIORef rf >>= \ s0 ->
        let tHist = fst (s_sample_prior s0 tU) in
        let tCandidate = fromMaybe tU tHist in
        let su' = tsMap tCandidate (s_trim su tU) in
        let s' = s_switch s0 tU su' in
        let sCln = s_trim_prior s' (inStableT tS) in
        sCln `seq` writeIORef rf sCln >>
        ln_update lu tS tU su'

tsMap :: T -> Sig a -> Sig T
tsMap t0 (Sig (Just _) s) = t0 `seq` Sig (Just t0) (tsMapSeq1 s)
tsMap _ (Sig Nothing s) = Sig Nothing (tsMapSeq0 s)

-- previously active, so ignore changes from active to active. 
tsMapSeq1 :: Seq (Maybe a) -> Seq (Maybe T)
tsMapSeq1 Done = Done
tsMapSeq1 (Step _ (Just _) s) = tsMapSeq1 s
tsMapSeq1 (Step tm Nothing s) = Step tm Nothing (tsMapSeq0 s)

-- previously inactive, looking for next activity
tsMapSeq0 :: Seq (Maybe a) -> Seq (Maybe T)
tsMapSeq0 Done = Done
tsMapSeq0 (Step tm (Just _) s) = Step tm (Just tm) (tsMapSeq1 s)
tsMapSeq0 (Step _ Nothing s) = tsMapSeq0 s


