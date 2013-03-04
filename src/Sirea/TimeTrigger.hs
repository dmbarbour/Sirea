
-- | In a reactive system, we often want to express reactions to
-- time itself. A question might be "Has it been less than three
-- seconds since the file was modified?" This module provides a
-- precise, logical mechanism to express such queries, assuming
-- you can provide a timestamp to test against:
-- 
--   btimeTrigger :: (Partition p) => B (S p T) (S p Bool)
--
-- This behavior will output True when logical time is greater than
-- or equal to the time indicated in signal. The time signal will
-- usually come from stateful resources (timestamp, calendar, etc.).
-- The logical time is from an implicit logical clock with infinite
-- precision (thus the Partition constraint), though implementation
-- is expected to cheat for performance.
-- 
module Sirea.TimeTrigger
    ( btimeTrigger
    , s_timeTrigger
    ) where

import Sirea.B
import Sirea.Time
import Sirea.UnsafeLink
import Sirea.Behavior
import Sirea.Signal
import Sirea.Partition

import Sirea.Internal.SigType
import Sirea.Internal.DiscreteTimedSeq

-- | btimeTrigger returns True whenever logical time is greater than
-- or equal to the signaled time. This supports time-based triggers.
-- It is more efficient and precise than use of Sirea.Clock with 
-- explicit comparisons.
btimeTrigger :: (Partition p) => B (S p T) (S p Bool)
btimeTrigger = unsafeFmapB s_timeTrigger

-- time trigger 0 assumes there is no pending trigger
dsTimeTrigger0 :: DSeq (Maybe T) -> DSeq (Maybe Bool)
dsTimeTrigger0 ds = DSeq $ dsTimeTriggerStep0 . dstep ds

dsTimeTriggerStep0 :: DStep (Maybe T) -> DStep (Maybe Bool)
dsTimeTriggerStep0 DSDone = DSDone
dsTimeTriggerStep0 (DSWait ds) = DSWait (dsTimeTrigger0 ds)
dsTimeTriggerStep0 (DSNext t0 Nothing ds) =
    DSNext t0 Nothing (dsTimeTrigger0 ds)
dsTimeTriggerStep0 (DSNext t0 (Just tx) ds) =
    if (t0 < tx) then DSNext t0 (Just False) (dsTimeTrigger1 tx ds)
                 else DSNext t0 (Just True) (dsTimeTrigger0 ds)

-- time trigger 1 has a pending trigger at a given instant
dsTimeTrigger1 :: T -> DSeq (Maybe T) -> DSeq (Maybe Bool)
dsTimeTrigger1 tx ds = DSeq $ \ tq -> dsTimeTriggerStep1 tx tq (dstep ds tq)

dsTimeTriggerStep1 :: T -> T -> DStep (Maybe T) -> DStep (Maybe Bool)
dsTimeTriggerStep1 tx _ DSDone = DSNext tx (Just True) ds_done
dsTimeTriggerStep1 tx tq (DSWait ds) =
    if (tq < tx) then DSWait (dsTimeTrigger1 tx ds)
                 else DSNext tx (Just True) (dsTimeTrigger0 ds)
dsTimeTriggerStep1 tx _ step@(DSNext t0 v ds) =
    if (tx < t0) then DSNext tx (Just True) (dsTimeTrigger0 (ds_first t0 v ds))
                 else dsTimeTriggerStep0 step

-- | Compute logical time trigger events on a signal. 
s_timeTrigger :: Sig T -> Sig Bool
s_timeTrigger sT = 
    case s_head sT of
        Nothing -> mkSig Nothing (dsTimeTrigger0 (s_tail sT))
        Just tx -> mkSig (Just False) (dsTimeTrigger1 tx (s_tail sT))


