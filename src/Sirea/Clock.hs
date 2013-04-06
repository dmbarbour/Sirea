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
timeToTick (ClockSpec p o) tm =
    let nanos = (tmDay tm) * nanosInDay + tmNanos tm in
    let rSecs = nanos % nanosInSec in
    let nF = (rSecs - o) / p in
    floor nF

-- clockList will return a limited sequence of (tick,time) pairs for
-- a logical clock, capturing the given low and high bounds. There
-- will be at least two elements in this list (one on or before low,
-- one on or after high).
clockList :: ClockSpec -> T -> T -> [(Integer,T)]
clockList cs tLo tHi = assert (tLo < tHi) $
    let nLo = timeToTick cs tLo in
    let tClockLo = tickToTime cs nLo in
    assert (tLo >= tClockLo) $ -- sanity check
    (nLo, tClockLo):clockListN cs tHi (nLo + 1)

-- increment until we include tHi. Always returns a non-empty list.
clockListN :: ClockSpec -> T -> Integer -> [(Integer,T)]
clockListN cs tHi n =
    let tN = tickToTime cs n in 
    let cl = if (tN >= tHi) then [] else clockListN cs tHi (n+1) in
    (n,tN):cl

-- obtain clock signal from clock list:
clockListToSig :: [(Integer,T)] -> Sig (Integer,T)
clockListToSig [] = assert False $ Sig Nothing Done -- expect non-empty list
clockListToSig (x:xs) = Sig (Just x) (seqFromList $ fmap cu xs) where
    cu nt = (snd nt, Just nt)


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
mkClock cs _ ln =     
    newIORef Nothing >>= \ rf -> -- track time to consider updates.
    return (lnClock cs rf ln)

dtClockStep, dtClockIdle :: DT
dtClockStep = 2.4 -- affects how much is computed per an update step
dtClockIdle = 0.6 -- how near end of last step before computing more

lnClock :: ClockSpec -> IORef (Maybe T) -> LnkUp (Integer,T) -> LnkUp ()
lnClock cs rf lu = LnkUp touch update idle cyc where
    touch = ln_touch lu
    cyc = ln_cycle lu
    idle tS = readIORef rf >>= idle' tS
    idle' tS Nothing = ln_idle lu tS -- dead signal...
    idle' tS (Just tLast) =
        let dt = diffTime tLast (inStableT tS) in
        if (dtClockIdle < dt) then ln_idle lu tS else
        let tLo = tLast in
        let tHi = tLo `addTime` dtClockStep in
        let lClock = tail (clockList cs tLo tHi) in
        let tU = (snd . head) lClock in
        let sClock = clockListToSig lClock in
        deliver tS tU sClock
    update tS@(StableT tm) tU su = 
        let tLo = min tm tU in 
        let (tEnd,_) = sigEnd tLo su in
        let tHi = (max tEnd $ max tm tU) `addTime` dtClockStep in
        let lClock = clockList cs tLo tHi in
        let sClock = clockListToSig lClock `s_mask` su in
        deliver tS tU sClock
    deliver tS tU sClock =
        let (t,m) = sigEnd tU sClock in
        let mem = if isNothing m then Nothing else Just t in
        writeIORef rf mem >>
        ln_update lu tS tU sClock        
        
-- Find the last values in a signal, given some
-- proposed initial values.
sigEnd :: T -> Sig a -> (T, Maybe a)
sigEnd t (Sig hd tl) = seqEnd t hd tl

seqEnd :: T -> a -> Seq a -> (T, a)
seqEnd t a Done = (t,a)
seqEnd _ _ (Step t a s) = seqEnd t a s


