{-# LANGUAGE GADTs #-}

-- | Clock is a behavior representing access to the current time. In
-- Sirea, access to time is an ambient authority, available in every
-- partition. A clock must be considered a stateful resource because
-- it logically involves access to an external clock (which provides
-- the 0 time) even though it's implemented as a simple transform of 
-- the input signal.
--
-- These clocks are logical. bclockSeconds will update every second,
-- on the second, starting from the 0 time. Time values in Sirea are
-- MJD, so 0 time is midnight, November 17, 1858, Greenwich. Clocks
-- are specified with a period and offset. In the resource discovery 
-- concept, you effectively can discover any clock you can specify.
-- (You aren't creating the clock, just finding one that fits your
-- needs.) Clocks can be accessed in terms of tick count or time.
--
-- Clocks are useful for various synchronization, scheduling, and
-- testing patterns. But high-frequency (low period) clocks are very
-- expensive to model. Computation grows directly with frequency. 
-- Alternatives to clocks may offer greater precision at lower cost. 
-- See Sirea.TimeTrigger, Sirea.TimeStamp, and animated state models
-- (in sirea-state) for possible alternatives.
-- 
module Sirea.Clock
    ( ClockSpec(..)
    , bclockTickTime, bclockTick, bclockTime
    , btickOfFreq, bclockOfFreq
    , bclockHours, bclockMinutes, bclockSeconds
    ) where

import Control.Arrow (first)
import Control.Exception (assert)
import Data.Ratio
import Data.IORef
import Data.Maybe (isNothing)
import Sirea.B
import Sirea.Time
import Sirea.UnsafeLink
import Sirea.Behavior
import Sirea.Signal
import Sirea.Internal.SigType
import Sirea.Partition (Partition)

-- | ClockSpec specifies when a clock ticks and tocks.
--
--     clock_period: period in seconds (recip of frequency)
--     clock_offset: 0 offset in seconds.
--
-- These logical clocks are MJD-relative, but use a simplified time
-- concept (e.g. excluding leap-seconds). Within their simplified
-- time model, they are perfect - never skipping or slowing, and 
-- always ticking on some exact value. Sirea will report the clock
-- within the limits of its own precision (i.e. up to once per nano
-- second, as of this writing).
--
-- All periods are valid, but a clock with period 0 will never tick
-- and one with a negative period will tick downwards (which is only
-- relevant if you're observing tick-count instead of time). All 
-- offsets are valid. 
-- 
data ClockSpec = ClockSpec
    { clock_period :: !Rational -- period in seconds
    , clock_offset :: !Rational -- offset from MJD 0 in seconds
    } deriving (Show,Eq)

-- | Observe a clock with both tick-count and associated time. When
-- you start observing will not affect the clock, i.e. logically the
-- clock is an external resource that behaves independently of any
-- observer.
bclockTickTime :: (Partition p) => ClockSpec -> B (S p ()) (S p (Integer,T))
bclockTickTime cs@(ClockSpec p o) = 
    if (p < 0) then bclockTickTime (ClockSpec (negate p) o) >>> bfmap (first negate) else
    if (p == 0) then bconst (0, tickToTime cs 0) else
    if (abs p < hfThresh) then unsafeLinkBL (mkClockHF cs) else
    unsafeLinkBL (mkClock cs)

-- high frequency clocks get special handling
hfThresh :: Rational
hfThresh = 3 % (1000*1000*1000)

-- | Observe a clock in terms of tick-count. The tick counts will be
-- quite large, unless you use a corresponding offset.
bclockTick :: (Partition p) => ClockSpec -> B (S p ()) (S p Integer)
bclockTick cs = bclockTickTime cs >>> bfmap fst

-- | Observe a clock in terms of current time.
bclockTime :: (Partition p) => ClockSpec -> B (S p ()) (S p T)
bclockTime cs = bclockTickTime cs >>> bfmap snd

-- find time associated with a particular tick count for a clock spec.    
tickToTime :: ClockSpec -> Integer -> T
tickToTime (ClockSpec p o) n =
    let rSecs = o + (p * fromInteger n) in
    let nanos = (numerator rSecs * nanosInSec) `div` denominator rSecs in
    mkTime 0 nanos

-- find tick associated with particular time for a clock spec.
timeToTick :: ClockSpec -> T -> Integer
timeToTick cs@(ClockSpec p o) tm =
    let nanos = (tmDay tm) * nanosInDay + tmNanos tm in
    let rSecs = nanos % nanosInSec in
    let nF = (rSecs - o) / p in
    floor nF

nanosInDay, nanosInSec :: Integer
nanosInDay = 24*60*60 * nanosInSec
nanosInSec = 1000*1000*1000

-- | For testing, it is often convenient to just obtain a signal
-- that increments periodically at a predictable rate. There may
-- be other uses for it.
btickOfFreq :: (Partition p) => Rational -> B (S p ()) (S p Integer)
btickOfFreq = bclockTick . freqToCS

-- | Obtain a clock in terms of frquency instead of period.
bclockOfFreq :: (Partition p) => Rational -> B (S p ()) (S p T)
bclockOfFreq = bclockTime . freqToCS

-- frequency to clockspeck
freqToCS :: Rational -> ClockSpec
freqToCS r = if (0 == r) then ClockSpec 0 0 else ClockSpec (recip r) 0

-- | A logical hour-clock. Updates every hour on the hour.
bclockHours :: (Partition p) => B (S p ()) (S p T)
bclockHours = bclockOfFreq (1 % 3600)

-- | A logical minute-clock. Updates every minute on the minute.
bclockMinutes :: (Partition p) => B (S p ()) (S p T)
bclockMinutes = bclockOfFreq (1 % 60)

-- | A logical second-clock. Updates every second on the second.
bclockSeconds :: (Partition p) => B (S p ()) (S p T)
bclockSeconds = bclockOfFreq 1

-- very high frequency clocks will need to compute ticks from times
-- rather than vice versa.
mkClockHF :: ClockSpec -> pcx -> LnkUp (Integer,T) -> IO (LnkUp ())
mkClockHF _ _ = error "TODO: support very high-frequency clocks"

-- Clock implementation. Logical clocks are external resources, but
-- their behavior is predictable so is computed locally. A concern
-- is that signals are spine-strict: we can't lazily compute the 
-- infinite future, and must turn some stability updates into real
-- updates. The last reported tick/time is recorded.
--
mkClock :: ClockSpec -> pcx -> LnkUp (Integer,T) -> IO (LnkUp ())
mkClock cs _ ln = error "TODO: finish implementing clocks"
{-    newIORef Nothing >>= \ rf -> -- track time to consider updates.
    return (lnClock cs rf ln)



lnClock :: ClockSpec -> IORef (Maybe T) -> LnkUp (Integer,T) -> LnkUp ()
lnClock cs rf lu = LnkUp touch update idle cycle where
    touch = ln_touch lu
    update tS tU su = 
        let tLo = min (inStableT tS) tU in
        let (tEnd,bActiveEnd) = sigEnd tSegStart su in
        let tHi = tEnd `addTime` dtClockStep in
        let mem = if bActiveEnd then Just tHi else Nothing in
        writeIORef rf mem >>
        let sClock = clockSig cs tLo tHi in
    
    
        remember bActiveEnd tHi >>

        if bActiveEnd then update1 tS tU su (tEnd `addTime` dtClockSeg)
                      else update0 tS tU su tEnd
    remember True tHi = writeIORef rf (Just tHi)
    remember 
        
        
-- Find the last values in a signal, given some
-- proposed initial values.
sigEnd :: T -> Sig a -> (T, Maybe a)
sigEnd t (Sig hd tl) = seqEnd t hd tl

seqEnd :: T -> a -> Seq a -> (T, a)
seqEnd t a Done = (t,a)
seqEnd _ _ (Step t a s) = seqEnd t a s


-- generate a segment of logical clock signal starting at given time
-- and stretching through another given time. This method is for low
-- frequency clocks.
clockSig :: ClockSpec -> T -> (T,Sig (Integer,T))

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
    


-- Times after a given time. 
-- The series of updates is repeated each day. 
timeAfterTime :: ClockSpec -> T -> [T]
timeAfterTime cs t0 = t0 `seq` (t1:timeAfterTime cs t1)
    where t1  = let t0' = addTime t0 (clock_period cs) in
                if (tmDay t0' > tmDay t0)
                  then mkTime (tmDay t0') (dtToNanos (clock_offset cs))
                  else t0'

-}
