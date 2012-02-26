
module FRP.Sirea.Time
 (T,tmMJD,tmPicos
 ,mkTime,getTime      
 ,DT
 ,dtToPicos,picosToDt
 ,addTime,diffTime
 ) where

import Data.Int (Int32,Int64)
import qualified Data.Time.Clock as CW
import Data.Time.Calendar (toModifiedJulianDay)
import Data.Function (on)

-- | T - a fixpoint representation of universal time with picosecond
-- precision, as a pair of integers. Time in Sirea is modeled as 
-- continuous, but the actual implementation is limited precision.
--    tmMJD   - Modified Julian Day (days since Nov 17, 1858)
--    tmPicos - Picoseconds in the day. (0..86400*10^12-1)
-- Simplified. Strict. No leap seconds. Limited range, but over
-- plus or minus five million years.
-- 
-- Construct via mkTime, fromUTC, or getTime. 
data T { _tmMJD :: !Int32, _tmPicos :: !Int64 }

tmMJD :: T -> Integer
tmMJD = toInteger . _tmMJD

tmPicos :: T -> Integer
tmPicos = toInteger . _tmPicos

-- | mkTime days picos, smart constructor; will convert
-- picos to days.
mkTime :: Integer -> Integer -> T
mkTime days picos =
    let (q,r) = picos `divMod` picosInDay in
    T { _tmMJD = fromInteger (days + q)
      , _tmPicos = fromInteger r 
      }

-- | Obtain estimate of current time from operating system.
getTime :: IO T
getTime = CW.getCurrentTime >>= return . fromUTC

fromUTC :: CW.UTCTime -> T
fromUTC utc =
    let d = Cal.toModifiedJulianDay (CW.utctDay utc)
        r = toRational (CW.utctDayTime utc)
        p = numerator r * picosInSec `div` denominator r
    in mkTime d p

-- | DT - a representation of a difference in two times, accessible
--   as a distance in picoseconds. 
newtype DT = DT T 

dtToPicos :: DT -> Integer
dtToPicos (DT tm) = (picosInDay * tmMJD tm) + tmPicos tm

picosToDt :: Integer -> DT
picosToDT = DT . mkTime 0 

-- | Add a difference in time to an absolute time.
addTime :: T -> DT -> T
addTime tm (DT dt) =
    let d' = _tmMJD tm + _tmMJD dt in 
    let p' = _tmPicos dt + _tmPicos tm in
    if (p < ppd) 
    then T { _tmMJD = d', _tmPicos = p' }
    else T { _tmMJD = d'+1, _tmPicos = p'-ppd }

-- | Find the difference in time, diffTime a b = a - b
diffTime :: T -> T -> DT

picosInDay, secondsInDay, picosInSec :: Integer
picosInDay = secondsInDay * picosInSec
secondsInDay = 24 * 60 * 60
picosInSec = 1000 * 1000 * 1000 * 1000

ppd :: Int64
ppd = fromInteger picosInDay 

instance Eq T where
  (==) a b = eqPicos a b && eqMJD a b
    where eqPicos = (==) `on` _tmPicos
          eqMJD = (==) `on` _tmMJD

instance Eq DT where
  (==) = (==) `on` unDT

instance Ord T where
  compare a b = 
     case cmpDays a b of
        EQ -> cmpPicos a b
        x -> x
     where cmpDays = compare `on` _tmMJD
           cmpPicos = compare `on` _tmPicos

instance Ord DT where
  compare = compare `on` unDT

unDT :: DT -> T
unDT (DT t) = t


instance Num DT where
    (+) a b = 
        let p = picos a + picos b in
        let d = days a + days b in
        if (p > picosInDay) 
        then UT { days = (d + 1), picos = (p - picosInDay) }
        else UT { days = d, picos = p }
    (-) a b = 
        let p = picos a - picos b in
        let d = days a - days b in
        if (p < 0) 
        then UT { days = (d - 1), picos = (p + picosInDay) }
        else UT { days = d, picos = p }
    (*) a b = picosToUT (q + c)
        where pa = utToPicos a
              pb = utToPicos b
              (q,r) = (pa * pb) `divMod` toInteger picosInSec
              c = if (r > toInteger (picosInSec `div` 2)) then 1 else 0 
    negate b = 
        if (picos b == 0)
        then UT { days = negate (days b), picos = 0 }
        else UT { days = negate (days b) - 1, picos = picosInDay - (picos b) }

    abs b = if (days b < 0) then negate b else b
    signum b =
        case compare (utToPicos b) 0 of
            LT -> fromInteger (-1)
            EQ -> fromInteger 0
            GT -> fromInteger 1
    fromInteger n = picosToUT (n * toInteger picosInSec)

-- 'Fractional' is primarily for the 'fromRational' 
-- numeric conversions in seconds.
instance Fractional DT where
    (/) a b = picosToUT (q + c) 
        where pa = utToPicos a
              pb = utToPicos b
              (q,r) = (pa * toInteger picosInSec) `divMod` pb
              c = if (r > pb `div` 2) then 1 else 0
    recip = (1 /) 
    fromRational r = picosToUT ps
        where ps = numerator r * toInteger picosInSec `div` denominator r

-- show instances for debugging, approximate.
instance Show T where
    show tm =
       let dblDay = fromInteger (tmDay tm) :: Double
           dblFrac = fromRational (tmPicos tm % picosInDay) :: Double
       in show (dblDay + dblFrac)

instance Show DT where
    show = show . unDT





