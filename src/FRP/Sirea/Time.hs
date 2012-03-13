
module FRP.Sirea.Time
 (T
 ,tmDay,tmNanos
 ,mkTime,getTime      
 ,DT
 ,dtToNanos,nanosToDt
 ,addTime,diffTime
 ) where

import Data.Int (Int32,Int64)
import Data.Ratio ((%),numerator,denominator)
import qualified Data.Time.Clock as CW
import qualified Data.Time.Calendar as Cal
import Data.Function (on)

-- | T - a fixpoint representation of time UTC with nanosecond
-- precision, as a pair of integers. Time in Sirea is modeled as  
-- continuous, but the actual implementation is limited precision.
--    tmDay   - Modified Julian Day (days since Nov 17, 1858)
--    tmNanos - Nanoseconds in the day. [0,86400*10^9)
-- Simplified. Strict. No leap seconds. Limited range, but over
-- plus or minus five million years.
--
-- The choice of nanoseconds is so we can squeeze time-of-day into
-- a double value, for interaction with most scripting languages
-- (JavaScript, most importantly). 
-- 
-- Construct via mkTime, fromUTC, or getTime. 
data T = T { _tmDay :: !Int32, _tmNanos :: !Int64 }

tmDay :: T -> Integer
tmDay = toInteger . _tmDay

tmNanos :: T -> Integer
tmNanos = toInteger . _tmNanos

-- | mkTime days nanos, smart constructor; will convert
-- nanos to days.
mkTime :: Integer -> Integer -> T
mkTime days nanos =
    let (q,r) = nanos `divMod` nanosInDay in
    T { _tmDay = fromInteger (days + q)
      , _tmNanos = fromInteger r 
      }

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
    if (n < nnid) 
    then T { _tmDay = d, _tmNanos = n }
    else T { _tmDay = d+1, _tmNanos = n-nnid }
    where nnid = fromInteger nanosInDay

-- | Find the difference in time, diffTime a b = a - b
diffTime :: T -> T -> DT
diffTime tm tm' =
    let d = _tmDay tm - _tmDay tm' in
    let n = _tmNanos tm - _tmNanos tm' in
    if (n < 0)
    then DT $ T { _tmDay = (d-1), _tmNanos = (n+nnid) }
    else DT $ T { _tmDay = d, _tmNanos = n }
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
    negate = nanosToDt . negate . dtToNanos
    abs = nanosToDt . abs . dtToNanos
    signum = fromInteger . signum . dtToNanos
    fromInteger = nanosToDt . (*) nanosInSec

-- 'Fractional' is primarily for the 'fromRational' 
-- numeric conversions in seconds.
instance Fractional DT where
    (/) a b = nanosToDt (q + c) 
        where na = dtToNanos a
              nb = dtToNanos b
              (q,r) = (na * nanosInSec) `divMod` nb  -- 
              c = if (r > nb `div` 2) then 1 else 0  -- carry
    recip = (1 /) 
    fromRational r = nanosToDt ps
        where ps = (numerator r * nanosInSec) `div` denominator r

instance Show T where
    show tm = "MJD " ++ sDay ++ sFrac
      where dblFrac = fromRational (tmNanos tm % nanosInDay) :: Double
            sDay = show (tmDay tm)
            sFrac = tail $ show dblFrac

instance Show DT where
    show dt = show fsec ++ "s"
      where fsec = fromRational (dtToNanos dt % nanosInSec) :: Double


