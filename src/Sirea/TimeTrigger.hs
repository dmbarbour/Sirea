
-- | In a reactive system, we often want to express reactions to
-- time itself. A question might be "Has it been less than three
-- seconds since the file was modified?" This module provides a
-- precise, logical mechanism to express such queries, assuming
-- you can provide a timestamp to test against:
-- 
--   btimeTrigger :: b (S p T) (S p Bool)
--
-- This behavior will output True when logical time is greater than
-- or equal to the time indicated in signal. The time signal will
-- usually come from stateful resources (timestamp, calendar, etc.).
-- The logical time is from an implicit logical clock with infinite
-- precision (thus the HasClock constraint), though implementation
-- is expected to cheat for performance.
-- 
module Sirea.TimeTrigger
    ( HasTimeTrigger(..)
    , s_timeTrigger
    ) where

import Sirea.B
import Sirea.BCX
import Sirea.Time
import Sirea.Link
import Sirea.Behavior
import Sirea.Signal
import Sirea.Clock

import Sirea.Internal.SigType
import Sirea.Internal.DiscreteTimedSeq


-- | A btimeTrigger returns True if the real time is greater than or
-- equal to the time parameter. This can help control behaviors with
-- queries like: "Has it been less than three seconds since foo?"
--
-- btimeTrigger is precise and works on discrete varying signals.
-- 
class (HasClock b) => HasTimeTrigger b where
    btimeTrigger :: b (S p T) (S p Bool)

instance HasTimeTrigger (BCX w) where
    btimeTrigger = (wrapBCX . const) timeTriggerB

timeTriggerB :: B (S p T) (S p Bool)
timeTriggerB = unsafeLinkB mkLn where
    mkLn = return . (ln_lumap . ln_sumap . su_fmap) s_timeTrigger

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


