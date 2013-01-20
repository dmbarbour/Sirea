
-- | In a reactive system, we often want to express reactions to
-- time itself. A question might be "Has it been less than three
-- seconds since the file was modified?" This module provides a
-- precise, logical mechanism to express such queries, assuming
-- you can provide a timestamp to test against:
-- 
--   btimeTrigger :: b (S p T) (S p Bool)
--
-- This behavior will output True when logical time is greater than
-- or equal to the time indicated by signal. The time signal will
-- usually come from stateful resources (timestamp, calendar, etc.).
-- The logical time is from an implicit logical clock with infinite
-- precision (thus the HasClock constraint), though implementation
-- is expected to cheat for performance.
-- 
module Sirea.TimeTrigger
    ( HasTimeTrigger(..)
    ) where

import Control.Exception (assert)
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
class (HasClock b) => HasTimeTrigger b where
    btimeTrigger :: b (S p T) (S p Bool)

-- TODO: BCX instance for HasTimeTrigger


    
{-
clockSig' :: ClockSpec -> Integer -> Integer -> Sig T
clockSig' cs nDays nNanos =
    let periodNanos = dtToNanos (clock_period cs) in
    let offsetNanos = dtToNanos (clock_offset cs) in
    let nInterval = (nNanos - offsetNanos) `div` periodNanos in
    assert (nInterval >= 0) $
    let clockNanos = offsetNanos + (nInterval * periodNanos) in
    assert (clockNanos < nanosInDay) $
    let t0 = mkTime nDays clockNanos in
    let ts = timeAfterTime cs t0 in
    let lUpd = map (\t->(t,Just t)) ts in
    listToSig (Just t0) lUpd

-- Times after a given time. 
-- The series of updates is repeated each day. 
timeAfterTime :: ClockSpec -> T -> [T]
timeAfterTime cs t0 = t0 `seq` (t1:timeAfterTime cs t1)
    where t1  = let t0' = addTime t0 (clock_period cs) in
                if (tmDay t0' > tmDay t0)
                  then mkTime (tmDay t0') (dtToNanos (clock_offset cs))
                  else t0'

-}

