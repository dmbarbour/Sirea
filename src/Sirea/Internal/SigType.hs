
-- | Most developers don't need direct access to the representation
-- of signals. But for the few who do, here it is.
--
-- There are also a few operations on sequences. 
module Sirea.Internal.SigType
    ( Sig(..)
    , Seq(..)
    , seqFirst
    , seqFromList, seqToList, seqTakeList
    , seqConst0, seqConst1
    , seqMap
    , seqQuery, seqTrim
    , seqSigup
    , seqFilter
    , seqAdjeqf
    , seqAp
    , seqMerge
    , seqMask0, seqMask1
    , seqAdjn0, seqAdjn1
    ) where

import Sirea.Time (T)
import Control.Exception (assert)
import Control.Applicative
import Data.Maybe (isNothing)

-- | Sig is an abstract type for discrete-varying signals in Sirea.
-- A signal is defined for all times, but in practice the past is
-- dropped (collected) while the future is updated over time. 
data Sig a = Sig !(Maybe a) !(Seq (Maybe a))

-- seq is a spine-strict, compact sequence, with time in monotonic 
data Seq a = Step {-# UNPACK #-} !T a !(Seq a) | Done

seqDone :: Seq a
seqDone = Done

-- concatenate a stop or value
seqFirst :: T -> a -> Seq a -> Seq a
seqFirst t a s = assert (monoTime t s) $ Step t a s

-- monotonic time for assertions
monoTime :: T -> Seq a -> Bool
monoTime _ Done = True
monoTime t (Step t' _ _) = (t < t')

-- convert a list to a sequence. The list must be finite and ordered
-- in time. 
seqFromList :: [(T,a)] -> Seq a
seqFromList = foldr (uncurry seqFirst) seqDone

-- convert a sequence to a list.
seqToList :: Seq a -> [(T,a)] 
seqToList Done = []
seqToList (Step t v s) = (t,v):seqToList s

-- partial sequence to list (elements prior to the query time)
-- will include any element exactly at the queried time.
seqTakeList :: T -> Seq a -> [(T,a)]
seqTakeList _ Done = []
seqTakeList tq (Step t v s) = 
    if (tq < t) then [] else 
    (t,v):seqTakeList tq s

-- replace values in a sequence with a constant. 
--  seqConst0 assumes prior value was Nothing
--  seqConst1 assumes prior value was the constant.
seqConst0, seqConst1 :: c -> Seq (Maybe a) -> Seq (Maybe c)

seqConst0 c (Step _ Nothing  s) = seqConst0 c s
seqConst0 c (Step t (Just _) s) = Step t (Just c) (seqConst1 c s)
seqConst0 _ Done = Done

seqConst1 c (Step t Nothing s) = Step t Nothing (seqConst0 c s)
seqConst1 c (Step _ (Just _) s) = seqConst1 c s
seqConst1 _ Done = Done

-- map a function to every value in a sequence.
seqMap :: (a -> b) -> Seq a -> Seq b
seqMap f (Step t v s) = Step t (f v) (seqMap f s)
seqMap _ Done = Done

{-# RULES "seqMap/id" seqMap id = id #-}

-- query a signal to obtain its value at a particular time, and the
-- rest of the signal starting from that point.
seqQuery :: a -> T -> Seq a -> (a, Seq a)
seqQuery x _ Done = (x,Done)
seqQuery x tq s@(Step t v s') = 
    if (tq < t) then (x,s) else 
    seqQuery v tq s'

-- trim is same as query except with just the resulting sequence.
seqTrim :: T -> Seq a -> Seq a
seqTrim _ Done = Done
seqTrim tq s@(Step t _ s') =
    if (tq < t) then s else
    seqTrim tq s' 

-- switch from one signal to another at a particular time.
-- note the `Maybe a` value is considered the infinite 
-- history of the second signal.
seqSigup :: Seq a -> T -> a -> Seq a -> Seq a
seqSigup xs tU y ys = seqSigup_i ays tU xs where
    (y',ys') = seqQuery y tU ys
    ays = seqFirst tU y' ys'

seqSigup_i :: Seq a -> T -> Seq a -> Seq a
seqSigup_i ys tU (Step tx v xs) = 
    if (tx >= tU) then ys else 
    Step tx v (seqSigup_i ys tU xs)
seqSigup_i ys _ Done = ys

-- filter a sequence given a function.
seqFilter :: (a -> Bool) -> Seq a -> Seq a
seqFilter _ Done = Done
seqFilter f (Step t v s) = if (f v) then Step t v s' else s'
    where s' = seqFilter f s

-- filter adjacent values, given filter function and initial value.
-- (will also filter adjacent stops or Nothing values)
seqAdjeqf :: (a -> a -> Bool) -> a -> Seq a -> Seq a
seqAdjeqf _ _ Done = Done
seqAdjeqf eq a (Step t a' s) =
    if (eq a a') then seqAdjeqf eq a s
                 else Step t a' (seqAdjeqf eq a' s)

-- apply values in one signal to functions in another
seqAp :: (a -> b) -> a -> Seq (a -> b) -> Seq a -> Seq b
seqAp f _ Done xs = seqMap f xs
seqAp _ x fs Done = seqMap ($ x) fs
seqAp f x fs@(Step tf f' fs') xs@(Step tx x' xs') =
    case compare tf tx of
        LT -> Step tf (f' x ) (seqAp f' x  fs' xs )
        EQ -> Step tf (f' x') (seqAp f' x' fs' xs')
        GT -> Step tx (f  x') (seqAp f  x' fs  xs')

-- combine two signals, favoring values from the left side.
seqMerge :: Maybe a -> Maybe a -> Seq (Maybe a) -> Seq (Maybe a) -> Seq (Maybe a)
seqMerge Nothing _ Done ys = ys -- left signal done and transparent
seqMerge _ Nothing xs Done = xs -- right signal done and inactive
seqMerge (Just _) _ Done _ = Done --  left signal done and opaque
seqMerge _ y@(Just _) xs Done = seqMap (<|> y) xs -- constant backup
seqMerge Nothing y (Step _ Nothing xs) ys = seqMerge Nothing y xs ys -- false update left
seqMerge x Nothing xs (Step _ Nothing ys) = seqMerge x Nothing xs ys -- false update right
seqMerge x y xs@(Step tx x' xs') ys@(Step ty y' ys') =
    case compare tx ty of
        LT -> Step tx (x' <|> y ) (seqMerge x' y  xs' ys )
        EQ -> Step tx (x' <|> y') (seqMerge x' y' xs' ys')
        GT -> let s' = seqMerge x y' xs ys' in
              if isNothing x then Step ty y' s' else s'

-- mask one signal with another. The resulting signal is active only
-- if both signals are active, but has values only from first input.
--  mask0 assumes values are initially masked
--  mask1 assumes values are initially unmasked
seqMask0, seqMask1 :: (Maybe a) -> Seq (Maybe a) -> Seq (Maybe b_) -> Seq (Maybe a)
seqMask0 _ _ Done = Done -- masked from now on
seqMask0 Nothing Done _ = Done -- signal inactive from now on
seqMask0 (Just c) Done ys = seqConst0 c ys -- masking a constant
seqMask0 Nothing (Step _ Nothing xs) ys = seqMask0 Nothing xs ys -- false step left
seqMask0 x xs (Step _ Nothing ys) = seqMask0 x xs ys -- false step right (still masked)
seqMask0 x xs@(Step tx x' xs') ys@(Step ty (Just _) ys') =
    case compare tx ty of
        LT -> seqMask0 x' xs' ys -- still masked
        EQ -> seqUnmask x' tx xs' ys' -- unmask @ new x
        GT -> seqUnmask x ty xs ys' -- unmask @ old x

seqUnmask :: Maybe a -> T -> Seq (Maybe a) -> Seq (Maybe b_) -> Seq (Maybe a)
seqUnmask Nothing _ xs ys = seqMask1 Nothing xs ys -- unmasked Nothing, no change
seqUnmask x tm xs ys = Step tm x (seqMask1 x xs ys) -- unmasked something.

seqMask1 _ xs Done = xs -- unmasked from now on
seqMask1 Nothing Done _ = Done -- inactive from now on
seqMask1 (Just c) Done ys = seqConst1 c ys -- masking a constant
seqMask1 Nothing (Step _ Nothing xs) ys = seqMask1 Nothing xs ys -- false update left
seqMask1 x xs (Step _ (Just _) ys) = seqMask1 x xs ys -- false update right (still exposed)
seqMask1 x xs@(Step tx x' xs') ys@(Step ty Nothing ys') =
    case compare tx ty of
        LT -> Step tx x' (seqMask1 x' xs' ys) -- exposed update
        EQ -> let s' = seqMask0 x' xs' ys' in
              if isNothing x then s' else Step tx Nothing s'
        GT -> let s' = seqMask0 x xs ys' in
              if isNothing x then s' else Step tx Nothing s'

-- filter redundant 'Nothing' values. This is implicit for many
-- operations, so is not critical. adjn0 assumes previous value
-- is Nothing, adjn1 asumes otherwise.
seqAdjn0, seqAdjn1 :: Seq (Maybe a) -> Seq (Maybe a)
seqAdjn0 Done = Done
seqAdjn0 (Step _ Nothing s) = seqAdjn0 s
seqAdjn0 (Step t v@(Just _) s) = Step t v (seqAdjn1 s)

seqAdjn1 Done = Done
seqAdjn1 (Step t Nothing s) = Step t Nothing (seqAdjn0 s)
seqAdjn1 (Step t v@(Just _) s) = Step t v (seqAdjn1 s)


-- TODO:
-- Sig describes a discrete-varying signal. But continuous varying
-- signals might be modeled within it - preferably in a manner 
-- suitable for symbolic analysis. This likely means one of:
--   * trigonometric interpolation polynomial (sum of m = -n to n of c_m * e^(i*m*x))
--   * polynomial expressions (sum of i = 0..n of A_i * t^i)
-- Both would allow simple rep as vector of doubles for the coefficients.
-- (But there is a challenge of performing time-shifts on them. Maybe some sort
-- of matrix operation would be necessary.)
--
-- multi-dimensional curves would be desired anyway, i.e. vectors and 
-- matrices of curvatures. Some sort of time-varying bezier surface is 
-- also a possibility.
--
-- Will probably want to handle in separate modules. I don't have the 
-- mathematical knowledge for this at the moment, not even to efficiently


