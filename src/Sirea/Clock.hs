{-# LANGUAGE GADTs #-}

-- | Clock is a behavior representing access to the current time. In
-- Sirea, access to time is an ambient authority, available in every
-- partition. A clock should be considered a stateful resource. This
-- is because the zero time (midnight, November 17, 1858, Greenwich)
-- is completely arbitrary; RDP behaviors should generally behave in
-- relative time. (Astronomical time supports coordination of state
-- and shared resources across independent behaviors.)
--
-- The clock provides a view of logical time. Because Sirea has only
-- discrete signals, the Sirea.Clock signal is discrete, updating in
-- a tick tock tick tock rhythm. The clock's behavior is independent
-- of demand, and observers will experience logically simultaneous
-- updates (no propagation delay). Logical clocks support precise, 
-- deterministic reasoning about timing.
--
-- Clocks are excellent for synchronization and scheduling patterns
-- between collaborative agents. If developers can establish a plan
-- of action far enough in advance (so the plan propagates ahead of
-- time) then distributed agents can take coordinated, simultaneous
-- or interleaved actions. Cron jobs and similar are also easy to 
-- model with a clock.
-- 
-- An alternative to logical clocks is animated state models, which  
-- may change state according to logical timeouts or time debts. For
-- flexible or relative timing, use animated state. (Animated state 
-- models will be provided by another package.) I recommend animated
-- state in most cases that might use a clock; most behaviors need 
-- relative time, not universal time.
-- 
module Sirea.Clock
    ( ClockSpec(..)
    , HasClock(..)
    , bclockOfFreq, bclockOfFreqAndPhase
    , bclockHours, bclockMinutes, bclockSeconds
    , freqToClockSpec, freqAndPhaseToClockSpec
    , csHours, csMinutes, csSeconds
    ) where

import Control.Exception (assert)
import Sirea.B
import Sirea.BCX
import Sirea.Time
import Sirea.Link
import Sirea.Behavior
import Sirea.Signal

-- | ClockSpec specifies when a clock ticks and tocks.
--
--     clock_period: period for clock updates. Positive. 
--     clock_offset: first clock relative each day. Non-negative.
--
-- A new period starts at `midnight` (according to UTC) every day.
-- It is best to choose a clock period that evenly divides one day.
-- Clock period shouldn't be more than a day, and clock offset must
-- be less than the period. 
--
-- The offset can prevent clocks from all updating at one instant.
-- Simultaneous updates are more efficient, but can be problematic
-- if it focuses too much work at a single instant (i.e. causing a
-- system to pause while logical time catches up with real time).
-- Still, in most of cases, you'll want offset to be 0 and period 
-- to correspond to a simple rational frequency.
-- 
-- Caution: High rate clocks become expensive to compute and memory
-- intensive. If Sirea computes 300ms for history and anticipation,
-- a nanosecond clock signal in memory would consume over a gigabyte
-- of memory and a gigahertz CPU. If the rate was only 1000Hz, the
-- cost would measure in kilobytes and kilohertz of CPU. Don't use
-- high frequency clocks.
--
-- An alternative to clocks is use of animated state, which can be
-- flexible and efficient, though more difficult to maintain in a
-- robust manner (as state tends to be).
-- 
data ClockSpec = ClockSpec
    { clock_period :: !DT
    , clock_offset :: !DT
    } deriving (Show,Eq)

class HasClock b where
    -- | Observe a logical clock of a given ClockSpec. 
    bclock :: ClockSpec -> b (S p ()) (S p T)

instance HasClock (B w) where
    bclock = clockB

instance HasClock (BCX w) where
    bclock = wrapBCX . const . clockB

-- clockB will implement a clock via unsafe
clockB :: ClockSpec -> B w (S p ()) (S p T)
clockB cs =
    assert (clockSpecValid cs) $ 
    unsafeLinkB clockLnk
    where clockLnk = return . buildFn
          buildFn LnkDead = LnkDead
          buildFn (LnkSig lu) = LnkSig (clockFn cs lu)

clockFn :: ClockSpec -> LnkUp T -> LnkUp ()
clockFn cs lu = LnkUp { ln_touch = touch, ln_update = update }
    where touch = ln_touch lu
          update su =
            let tStable = su_stable su in
            case su_state su of
                Nothing ->
                    let su' = SigUp { su_state = Nothing
                                    , su_stable = tStable } 
                    in ln_update lu su'
                Just (s, tUpd) ->
                    let sClock = clockSig cs tUpd in
                    let s' = s_mask sClock s in
                    let su' = SigUp { su_state = Just (s',tUpd)
                                    , su_stable = tStable }
                    in ln_update lu su'

-- An observation of a clock signal starting near a given instant.
-- Note that the clock signal does not depend on the time we begin
-- observing, nor on real time, only on the logical time of day and
-- the ClockSpec.
--
-- The goal with clockSig is to ensure O(1) computation and simple
-- real-time computation of the clock signal. 
clockSig :: ClockSpec -> T -> Sig T
clockSig cs t0 =
    assert (clockSpecValid cs) $
    let offsetNanos = dtToNanos (clock_offset cs) in
    let nNanos = tmNanos t0 in
    let nDays = tmDay t0 in
    if (nNanos < offsetNanos) 
        then clockSig' cs (pred nDays) (pred nanosInDay)
        else clockSig' cs nDays nNanos
    


nanosInDay :: Integer
nanosInDay = 24   {- hr    -} * 
             60   {- mn/hr -} * 
             60   {- s/mn  -} * 
             1000 {- ms/s  -} * 
             1000 {- us/ms -} * 
             1000 {- ns/us -}

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

-- | It is often desirable to specify a clock based on frequency.
-- Frequency is ticks per second, but sub-second frequencies are
-- allowed. Frequency must still select a valid ClockSpec.
--
-- Note that very high frequencies (e.g. multi MHz) might become a
-- compute burden.  
bclockOfFreq :: (HasClock b) => Double -> b (S p ()) (S p T)
bclockOfFreq = bclock . freqToClockSpec

-- | Frequency is updates per second, and phase how far into each
-- period we perform the update. E.g. if frequency was 2 and phase
-- is 0.5, then we update twice per second at 0.25 and 0.75. 
-- Phase must be bounded by [0.0,1.0). 
bclockOfFreqAndPhase :: (HasClock b) => Double -> Double -> b (S p ()) (S p T)
bclockOfFreqAndPhase dFreq dPhase = bclock $ freqAndPhaseToClockSpec dFreq dPhase

-- | A few common low-frequency clocks. These each report the full time, 
-- but do so at a very slow rate (relative to processor speeds).
bclockHours, bclockMinutes, bclockSeconds :: (HasClock b) => b (S p ()) (S p T)
bclockHours   = bclock csHours 
bclockMinutes = bclock csMinutes
bclockSeconds = bclock csSeconds

csHours, csMinutes, csSeconds :: ClockSpec
csHours = ClockSpec { clock_period = nanosToDt (60 * 60 * 1000 * 1000 * 1000), clock_offset = 0 }
csMinutes = ClockSpec { clock_period = nanosToDt (60 * 1000 * 1000 * 1000), clock_offset = 0 }
csSeconds = ClockSpec { clock_period = nanosToDt (1000 * 1000 * 1000), clock_offset = 0 }

clockSpecValid :: ClockSpec -> Bool
clockSpecValid cs =
    (clock_period cs > 0) &&
    (clock_offset cs >= 0) &&
    (clock_period cs > clock_offset cs) &&
    (one_full_day >= clock_period cs)
    where one_full_day = nanosToDt nanosInDay

-- | Translate a frequency value to a ClockSpec.
freqToClockSpec :: Double -> ClockSpec
freqToClockSpec hz = freqAndPhaseToClockSpec hz 0.0

-- | Translate frequency and phase to a ClockSpec.
--
-- Frequency is obvious, ticks per second. In general, it is best to
-- choose a value that evenly divides a day. Also, this computation
-- may break down near the extremes, though it should work well 
-- enough for practical frequencies (e.g. 0.05 to 2000.0).
--
-- Here, phase is in the range [0.0,1.0), relative offset within one 
-- period of the clock. A clock with 0 phase will always tick at 
-- midnight (UTC). 
freqAndPhaseToClockSpec :: Double -> Double -> ClockSpec
freqAndPhaseToClockSpec dFreq dPhase = 
    assert ((minFreq <= dFreq) && (dFreq <= maxFreq)) $
    assert ((0.0 <= dPhase) && (dPhase < 1.0)) $
    let dPeriod = (1000000000.0 / dFreq) in
    let nPeriod = ceiling dPeriod in
    let nPhase  = floor (dPeriod * dPhase) in
    assert ((1 <= nPeriod) && (nPeriod <= nanosInDay)) $
    assert ((0 <= nPhase) && (nPhase < nPeriod)) $
    let dtPeriod = nanosToDt nPeriod in
    let dtPhase  = nanosToDt nPhase in
    ClockSpec { clock_period = dtPeriod
              , clock_offset = dtPhase }
    where minFreq = (1.0 / 86400.0)
          maxFreq = 1000000000.0



