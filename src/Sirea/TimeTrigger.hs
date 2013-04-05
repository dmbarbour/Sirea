
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

-- | btimeTrigger returns True whenever logical time is greater than
-- or equal to the signaled time. This supports time-based triggers.
-- It is more efficient and precise than use of Sirea.Clock with 
-- explicit comparisons.
btimeTrigger :: (Partition p) => B (S p T) (S p Bool)
btimeTrigger = unsafeFmapB (s_adjeqf (==) . s_timeTrigger)

-- | Compute logical time trigger events on a signal. 
s_timeTrigger :: Sig T -> Sig Bool
s_timeTrigger (Sig Nothing ts) = Sig Nothing (tt0 ts)
s_timeTrigger (Sig (Just tm) ts) = Sig (Just False) (tt1 tm ts) 

-- tt0 - no active trigger
tt0 :: Seq (Maybe T) -> Seq (Maybe Bool)
tt0 Done = Done
tt0 (Step t Nothing s) = Step t Nothing (tt0 s)
tt0 (Step t (Just tx) s) =
    if (t < tx) then Step t (Just False) (tt1 tx s)
                else Step t (Just True) (tt0 s)

-- tt1 - pending trigger
tt1 :: T -> Seq (Maybe T) -> Seq (Maybe Bool)
tt1 tT Done = Step tT (Just True) Done
tt1 tT s@(Step t _ _) = 
    if (tT < t) then Step tT (Just True) (tt0 s) 
                else tt0 s


