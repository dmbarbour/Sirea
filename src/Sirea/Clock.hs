{-# LANGUAGE MultiParamTypeClasses #-}

-- | Clock is a behavior representing access to the current time via
-- a discrete clock resource. In Sirea, access to time is an ambient 
-- authority. (For secure RDP, access to clock should generally be a
-- protected capability. A clock is a stateful resource.) 
--
-- Time is continuous, but the clock is not. Instead, the clock will
-- simply update at a predictable rate: tick, tock, tick, tock based
-- on logical astronomical time.
--
-- Clocks are useful for synchronization and scheduling patterns. An
-- agent modeled in RDP can wait for a specific time, then change 
-- behavior. Clocks make it easy to model distributed behaviors of
-- collaborative agents as occurring in lockstep, assuming the plans
-- for action are distributed well enough in advance. 
--
-- Clock-based patterns are well known, as are the difficulties. One
-- difficulty is the tendency for all actions to be scheduled at the
-- same logical instants. To help counter this, clocks may be offset
-- a small amount in what times they report. (Though, there are also
-- disadvantages from having lots of small updates.)
-- 
module Sirea.Clock
    ( ClockSpec(..)
    , HasClock(..)
    ) where


-- | ClockSpec specifies when a clock ticks and tocks.
--
--     clock_period: period for clock updates. Positive. 
--     clock_offset: first clock update in each day. Non-negative.
--
-- Offset must be less than period. Period is no more than a day. 
-- The clock cycle restarts each day. It is best to choose a period
-- that fits evenly into a day (i.e. a fraction of a day) so that 
-- there are no uneven steps at the end of each day.
--
-- All clocks with the same ClockSpec will update at the same
-- logical instants. Each update contains the current time (MJD)
-- at the time of update. Timezones and local time will require
-- further computation.
data ClockSpec {
    clock_period :: DT
    clock_offset :: DT
}

class HasClock b where
    -- | Observe a logical clock of a given ClockSpec. 
    bclock :: ClockSpec -> b (S p ()) (S p T)


clockB :: ClockSpec -> B w (S p ()) (S p T)
clockB = undefined

clockBCX :: ClockSpec -> BCX w (S p ()) (S p T)
clockBCX = undefined

