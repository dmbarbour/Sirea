{-# LANGUAGE DeriveDataTypeable #-}
module Sirea.Time
    ( T
    , tmDay,tmNanos
    , mkTime,timeFromDays
    , getTime      
    , DT
    , dtToNanos,nanosToDt
    , addTime,subtractTime,diffTime
    , fromUTC
    ) where

import Data.Int (Int32,Int64)
import Data.Ratio ((%),numerator,denominator)
import qualified Data.Time.Clock as CW
import qualified Data.Time.Calendar as Cal
import Data.Function (on)
import Data.Typeable
import Control.Exception (assert)


-- | T - a fixpoint representation of time UTC with nanosecond
-- precision, as a pair of integers. Time in Sirea is modeled as  
-- continuous, but the actual implementation is limited precision.
--    tmDay   - Modified Julian Day (days since Nov 17, 1858)
--    tmNanos - Nanoseconds in the day. [0,86400*10^9)
-- Simplified. Strict. No leap seconds. Limited range, just over
-- plus or minus five million years. 
--
-- The choice of nanoseconds is so we can squeeze time-of-day into
-- a double value, for interaction with most scripting languages
-- (JavaScript, most importantly). 
-- 
-- Construct via mkTime, fromUTC, or getTime. 
data T = T {-# UNPACK #-} !Int32 {-# UNPACK #-} !Int64
    deriving (Typeable)

_tmDay :: T -> Int32
_tmDay (T d _) = d

_tmNanos :: T -> Int64
_tmNanos (T _ n) = n

tmDay :: T -> Integer
tmDay = toInteger . _tmDay

tmNanos :: T -> Integer
tmNanos = toInteger . _tmNanos

-- | `mkTime days nanos`
-- smart constructor for time 
mkTime :: Integer -> Integer -> T
mkTime days nanos =
    let (q,r) = nanos `divMod` nanosInDay in
    let d = fromInteger (days + q) in
    let n = fromInteger r in
    T d n

-- | timeFromDays will convert a Modified Julian Day, stored as a
-- rational, to a T value. 
timeFromDays :: Rational -> T
timeFromDays r = mkTime days (nanos + carry)
    where (days,dayFrac) = numerator r `divMod` denominator r
          (nanos,nanoFrac) = (dayFrac * nanosInDay) `divMod` denominator r
          carry = if (nanoFrac * 2 > denominator r) then 1 else 0

-- | Obtain estimate of current time from operating system.
getTime :: IO T
getTime = CW.getCurrentTime >>= return . fromUTC

fromUTC :: CW.UTCTime -> T
fromUTC utc =
    let d = Cal.toModifiedJulianDay (CW.utctDay utc)
        r = toRational (CW.utctDayTime utc)
        n = numerator r * nanosInSec `div` denominator r
    in mkTime d n

-- | DT - a representation of a difference in two times, accessible
--   as a distance in nanoseconds. 
newtype DT = DT { unDT :: T }

dtToNanos :: DT -> Integer
dtToNanos (DT tm) = (nanosInDay * tmDay tm) + tmNanos tm

nanosToDt :: Integer -> DT
nanosToDt = DT . mkTime 0 

-- | Add a difference in time to an absolute time.
addTime :: T -> DT -> T
addTime tm (DT dt) =
    let d = _tmDay tm + _tmDay dt in 
    let n = _tmNanos dt + _tmNanos tm in
    if (n < nnid) then T d n 
                  else T (d + 1) (n - nnid)
    where nnid = fromInteger nanosInDay

-- | Subtract a difference in time from an absolute time
subtractTime :: T -> DT -> T
subtractTime tm (DT dt) = unDT (diffTime tm dt)

-- | Find the difference in time, diffTime a b = a - b
diffTime :: T -> T -> DT
diffTime tm tm' =
    let d = _tmDay tm - _tmDay tm' in
    let n = _tmNanos tm - _tmNanos tm' in
    if (n < 0) then DT (T (d-1) (n+nnid))
               else DT (T d n)
    where nnid = fromInteger nanosInDay

nanosInDay, secondsInDay, nanosInSec :: Integer
nanosInDay = secondsInDay * nanosInSec
secondsInDay = 24 * 60 * 60
nanosInSec = 1000 * 1000 * 1000 

instance Eq T where
  (==) a b = eqNanos a b && eqMJD a b
    where eqNanos = (==) `on` _tmNanos
          eqMJD = (==) `on` _tmDay

instance Eq DT where
  (==) = (==) `on` unDT

instance Ord T where
  compare a b = 
     case cmpDays a b of
        EQ -> cmpNanos a b
        x -> x
     where cmpDays = compare `on` _tmDay
           cmpNanos = compare `on` _tmNanos

instance Ord DT where
  compare = compare `on` unDT

instance Num DT where
    (+) (DT a) b = DT (addTime a b)
    (-) = diffTime `on` unDT
    (*) a b = nanosToDt (q + c)
        where na = dtToNanos a
              nb = dtToNanos b
              (q,r) = (na * nb) `divMod` nanosInSec
              c = if (r > (nanosInSec `div` 2)) then 1 else 0
    negate (DT a) = 
        if (_tmNanos a == 0) 
            then DT (T (negate (_tmDay a)) 0) 
            else DT (T (negate (_tmDay a) - 1) (nnid - _tmNanos a))
        where nnid = fromInteger nanosInDay
    abs (DT a) = if (_tmDay a < 0) then negate (DT a) else (DT a)
    signum (DT a) = 
        if (_tmDay a < 0) 
            then -1 
            else  1
    fromInteger = nanosToDt . (*) nanosInSec

-- 'Fractional' is primarily for the 'fromRational' 
-- numeric conversions in seconds.
instance Fractional DT where
    (/) a b = nanosToDt (q + c) 
        where na = dtToNanos a
              nb = dtToNanos b
              (q,r) = (na * nanosInSec) `divMod` nb  -- 
              c = if (2 * r > nb) then 1 else 0  -- carry
    recip = (1 /) 
    fromRational rat = nanosToDt (q + c)
        where (q,r) = (numerator rat * nanosInSec) `divMod` denominator rat
              c = if (2 * r > denominator rat) then 1 else 0

-- show fixpoint days and seconds
instance Show T where
    show tm = showFrac 14 days -- 14 places for 86400s * 1000000000 ns
      where days = (tmDay tm * nanosInDay + tmNanos tm) % nanosInDay

instance Show DT where
    show dt = showFrac 9 (dtToNanos dt % nanosInSec)

-- represent the rational as a decimal string up to n places.
-- note that rounding is necessary to restore the data precisely.
showFrac :: Int -> Rational -> String
showFrac nPlaces rat = 
    assert (nPlaces > 0) $
    let (sign,posR) = if (rat < 0) then ("-",negate rat) else ("",rat) in
    let (q,r) = numerator posR `divMod` denominator posR in
    let (bcarry,sFrac) = showFrac' (denominator posR) r nPlaces in
    let c = if bcarry then 1 else 0 in
    sign ++ show (q + c) ++ "." ++ sFrac

showFrac' :: Integer -> Integer -> Int -> (Bool,String)
showFrac' den num nPlaces =
    if (nPlaces == 0) then ((num*2 > den),"") else
    let (q,r) = (num * 10) `divMod` den in
    let (bc,sRem) = showFrac' den r (nPlaces - 1) in
    let c = if bc then 1 else 0 in
    let q' = c + fromInteger q in
    if (q' == 10) then (True,  '0' : sRem) 
                  else (False, showDec q' : sRem)

showDec :: Int -> Char
showDec n = assert ((0 <= n) && (n <= 9)) $ toEnum (n + 48)

