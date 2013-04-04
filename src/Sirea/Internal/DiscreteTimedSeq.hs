
-- | A naive approach to modeling a discrete updating signal in 
-- continuous time is to use a list of [(t,v)] pairs in monotonic 
-- order. Unfortunately, this has a problem when we filter signals, 
-- e.g. to eliminate adjacent duplicate values. The basic issue is 
-- that there is no upper limit for how far we'll search into the 
-- future for the next value.
--
-- For Sirea, only need `T` and `Maybe v` types. Specialization can
-- cut out a lot of indirection for these computations. 
--
-- DiscreteTimedSeq enables the caller to control the search for the
-- next update, in terms of a simple time model (ordinal), and thus
-- ensures predictable computation. 
--
-- The advantage, then, is that clients of DiscreteTimedSeq won't be
-- exposed unnecessarily to `updates` that don't actually change
-- anything, and thus reduce redundant computation. However, action
-- to explicitly filter values is necessary (ds_adjeqf)
module Sirea.Internal.DiscreteTimedSeq
    ( DStep(..)
    , DSeq(..)
    , ds_done
    , ds_first
    , ds_fromList, ds_takeList
    , ds_const0, ds_const1
    , ds_map
    , ds_query
    , ds_sigup, ds_sigup'
    , ds_filter
    , ds_adjeqfx
    , ds_adjn0, ds_adjn1
    , ds_ap
    , ds_merge 
    , ds_mergeZip
    , ds_mask0, ds_mask1
    ) where

import Sirea.Time (T)
import Control.Exception (assert)
import Control.Applicative
import Data.Maybe (isNothing)

-- | DStep is the result of a search. 
data DStep a 
    = DSDone
    | DSWait !(DSeq a)
    | DSNext {-# UNPACK #-} !T a !(DSeq a)

-- | DSeq allows us to step through time. Each step gives us the
-- next update at or before the queried instant, along with the
-- subsequent sequence.
newtype DSeq a = DSeq { dstep :: (T -> DStep a) }

-- | ds_done is a basic constructor, equivalent to the empty list. 
ds_done :: DSeq a
ds_done = DSeq (const DSDone)

-- | ds_first adds an element to the beginning of a sequence.
ds_first :: T -> a -> DSeq a -> DSeq a
ds_first tm v ds' = DSeq $ \ tq -> 
   if (tq < tm) then DSWait (ds_first tm v ds')
                else DSNext tm v ds' 

-- | lazily transform a list into timed sequence. List should be in
-- strict monotonic order, with no divergence between steps (do not
-- filter the list until after you have it inside DSeq!).
ds_fromList :: [(T,v)] -> DSeq v
ds_fromList [] = ds_done
ds_fromList ((tx,x):xs) = ds_first tx x xs'
    where xs' = ds_fromList' tx xs

-- ds_fromList' applies an assertion for strict monotonic order.
ds_fromList' :: T -> [(T,v)] -> DSeq v
ds_fromList' _ [] = ds_done
ds_fromList' t0 ((tx,x):xs) = assert (t0 < tx) (ds_first tx x xs')
    where xs' = ds_fromList' tx xs

-- | ds_takeList - obtains a list from the sequence, up to a given
-- time. This is useful for testing and debugging (i.e. for 'Show').
ds_takeList :: T -> DSeq v -> [(T,v)]
ds_takeList tm ds =
    case dstep ds tm of
        DSDone -> []
        DSWait _ -> []
        DSNext tx x ds' -> (tx,x):(ds_takeList tm ds')

-- | ds_const0 replaces values with a constant under the assumption 
-- that the prior value was Nothing. It will automatically filter
-- out adjacent Nothing values.
ds_const0 :: c -> DSeq (Maybe a) -> DSeq (Maybe c)
ds_const0 c ds = DSeq $ \ tq -> ds_const0_step c tq (dstep ds tq)

-- | ds_const1 replaces values by a constant under the assumption 
-- that the prior value was already Just c. It will automatically
-- filter out adjacent Just c values.
ds_const1 :: c -> DSeq (Maybe a) -> DSeq (Maybe c)
ds_const1 c ds = DSeq $ \ tq -> ds_const1_step c tq (dstep ds tq)

ds_const0_step :: c -> T -> DStep (Maybe a) -> DStep (Maybe c)
ds_const0_step _ _  DSDone = DSDone
ds_const0_step c _  (DSWait ds) = DSWait (ds_const0 c ds)
ds_const0_step c tq (DSNext _ Nothing ds) = ds_const0_step c tq (dstep ds tq)
ds_const0_step c _  (DSNext tm (Just _) ds) = DSNext tm (Just c) (ds_const1 c ds)

ds_const1_step :: c -> T -> DStep (Maybe a) -> DStep (Maybe c)
ds_const1_step _ _  DSDone = DSDone
ds_const1_step c _  (DSWait ds) = DSWait (ds_const1 c ds)
ds_const1_step c _  (DSNext tm Nothing ds) = DSNext tm Nothing (ds_const0 c ds)
ds_const1_step c tq (DSNext _ (Just _) ds) = ds_const1_step c tq (dstep ds tq)

-- | ds_map applies a function to every element in the sequence.
ds_map :: (a -> b) -> DSeq a -> DSeq b
ds_map f ds = DSeq (ds_map_step f . dstep ds)

-- | ds_map_step applies a function to a particular step. 
ds_map_step :: (a -> b) -> DStep a -> DStep b
ds_map_step _ DSDone = DSDone
ds_map_step f (DSWait ds) = DSWait (ds_map f ds)
ds_map_step f (DSNext tx x ds) = DSNext tx (f x) (ds_map f ds)

-- a couple rules to help with ds_mergeZip
{-# RULES
"ds_map/id" ds_map id = id
"ds_map_step/id" ds_map_step id = id
"ds_map/fmap_id" ds_map (fmap id) = id
"ds_map_step/fmap_id" ds_map_step (fmap id) = id
  #-}

-- | ds_query obtains a value of a DSeq at a given time, treating
-- it like a signal. The previous value must be provided. The
-- future sequence is part of the result.
ds_query :: a -> T -> DSeq a -> (a, DSeq a)
ds_query x tq ds =
    case dstep ds tq of
        DSDone           -> (x, ds_done)
        DSWait ds'       -> (x, ds')
        DSNext _ x' ds'  -> ds_query x' tq ds'

-- | ds_sigup combines two signals at an intermediate instant.
--   The initial value of the second signal must be provided.
ds_sigup :: DSeq a -> T -> a -> DSeq a -> DSeq a
ds_sigup xs tm y ys = ds_sigup_i ays tm xs
    where (y',ys')  = ds_query y tm ys
          ays       = ds_first tm y' ys' 

-- internal part of ds_sigup.
ds_sigup_i :: DSeq a -> T -> DSeq a -> DSeq a
ds_sigup_i ys tm xs =
    case dstep xs tm of
        DSDone   -> ys
        DSWait _ -> ys
        DSNext tx x xs' -> if (tx < tm) 
            then ds_first tx x (ds_sigup_i ys tm xs')
            else ys

-- | ds_sigup' is a stricter version of ds_sigup.
-- This should help control space when number of updates is high.
ds_sigup' :: DSeq a -> T -> a -> DSeq a -> DSeq a
ds_sigup' xs tm y ys = ays `seq` ds_sigup_i' ays tm xs
    where (y',ys') = ds_query y tm ys
          ays      = ds_first tm y' ys'

-- internal function of ds_sigup'
ds_sigup_i' :: DSeq a -> T -> DSeq a -> DSeq a
ds_sigup_i' ys tm xs =
    case dstep xs tm of
        DSDone      -> ys
        DSWait _    -> ys
        DSNext tx x xs' -> if (tx < tm)
            then let xsu = ds_sigup_i' ys tm xs' in
                 xsu `seq` ds_first tx x xsu
            else ys

-- | filter a sequence
ds_filter :: (a -> Bool) -> DSeq a -> DSeq a
ds_filter fnKeep ds = DSeq $ \ tq ->
    case dstep ds tq of
        DSDone -> DSDone
        DSWait ds' -> DSWait (ds_filter fnKeep ds')
        DSNext tx x ds' -> 
            let rest = ds_filter fnKeep ds' in
            if (fnKeep x) then DSNext tx x rest
                else dstep rest tq

-- | filter adjacent entries by a given equality function, with an 
-- initial value. Will compare against the last reported value or
-- the initial value.
ds_adjeqfx :: (a -> a -> Bool) -> a -> DSeq a -> DSeq a
ds_adjeqfx eq x xs = DSeq $ \ tq -> ds_adjeqfx_step eq x tq (dstep xs tq)

ds_adjeqfx_step :: (a -> a -> Bool) -> a -> T -> DStep a -> DStep a
ds_adjeqfx_step _ _ _ DSDone = DSDone
ds_adjeqfx_step eq x _ (DSWait xs') = DSWait (ds_adjeqfx eq x xs')
ds_adjeqfx_step eq x tq (DSNext tx x' xs') =
    if (eq x x') then ds_adjeqfx_step eq x tq (dstep xs' tq)
                 else DSNext tx x' (ds_adjeqfx eq x' xs')

-- | apply elements of one sequence to another on matching times.
ds_ap :: (x -> y) -> x -> DSeq (x -> y) -> DSeq x -> DSeq y
ds_ap f x fs xs = DSeq $ \ tq ->
    let uf = dstep fs tq in
    let ux = dstep xs tq in
    ds_ap_step f x uf ux

-- internal operation for ds_ap
ds_ap_step :: (x -> y) -> x -> DStep (x -> y) -> DStep x -> DStep y
ds_ap_step _ x uf DSDone = ds_map_step ($ x) uf
ds_ap_step f _ DSDone ux = ds_map_step f ux
ds_ap_step f x (DSWait fs) (DSWait xs) = DSWait (ds_ap f x fs xs)
ds_ap_step _ x (DSNext tf f' fs') (DSWait xs) =
    DSNext tf (f' x) (ds_ap f' x fs' xs)
ds_ap_step f _ (DSWait fs) (DSNext tx x' xs') =
    DSNext tx (f x') (ds_ap f x' fs xs')
ds_ap_step f x (DSNext tf f' fs') (DSNext tx x' xs') =
    case compare tf tx of
        LT -> DSNext tf (f' x ) (ds_ap f' x  fs' xs )
        EQ -> DSNext tf (f' x') (ds_ap f' x' fs' xs')
        GT -> DSNext tx (f  x') (ds_ap f  x' fs  xs')
    where fs = ds_first tf f' fs'
          xs = ds_first tx x' xs'

-- | ds_merge treats the sequences as signals, and combines two
-- signals favoring the LHS while active. This is basically a
-- specialized zip for merging. In particular, it will avoid
-- introducing false updates except at transitions between
-- signal sources.
--
ds_merge :: (Maybe a) -> (Maybe a) -> DSeq (Maybe a) 
         -> DSeq (Maybe a) -> DSeq (Maybe a)
ds_merge = mg where
    mg x y xs ys = DSeq $ \ tq -> ms x y tq (dstep xs tq) (dstep ys tq)
    ms Nothing _ _ DSDone uy = uy -- left is done, so stay in right
    ms _ Nothing _ ux DSDone = ux -- right is done, so stay in left
    ms (Just _) _ _ DSDone _ = DSDone -- forever in left; left masks right
    ms _ y@(Just _) _ ux DSDone = ds_map_step (<|> y) ux -- flip-flop with y
    ms x y _ (DSWait xs) (DSWait ys) = DSWait (mg x y xs ys) -- both inputs waiting
    ms Nothing y tq (DSNext _ Nothing xs) uy = ms Nothing y tq (dstep xs tq) uy    -- false update left
    ms x Nothing tq ux (DSNext _ Nothing ys) = ms x Nothing tq ux (dstep ys tq)    -- false update right
    ms _ y _ (DSNext tx x' xs') (DSWait ys) = msL tx x' xs' y ys                   -- update left
    ms x _ tq ux@(DSWait xs) (DSNext ty y' ys') = msR x tq ux xs ty y' ys'         -- update right
    ms x y tq ux@(DSNext tx x' xs') (DSNext ty y' ys') =
        case compare tx ty of
            LT -> msL tx x' xs' y (ds_first ty y' ys')
            GT -> msR x tq ux (ds_first tx x' xs') ty y' ys'
            EQ -> DSNext tx (x' <|> y') (mg x' y' xs' ys')
    msL tx x' xs' y ys = DSNext tx (x' <|> y) (mg x' y xs' ys)         -- update on left
    msR Nothing _ _ xs ty y' ys' = DSNext ty y' (mg Nothing y' xs ys') -- update on right is exposed
    msR x@(Just _) tq ux _ _ y' ys' = ms x y' tq ux (dstep ys' tq)     -- update on right is masked

-- | ds_mergeZip will combine two signals when both are active, or
-- otherwise use just one of the two signals. The performance of
-- mergeZip is potentially intermediate to merge and zip, with the
-- ability to better handle inactivity of a signal. Inactive if both
-- inputs are inactive.
--
ds_mergeZip :: (a -> b -> c) -> (a -> c) -> (b -> c)
            -> (Maybe a) -> (Maybe b)
            -> DSeq (Maybe a) -> DSeq (Maybe b) -> DSeq (Maybe c)
ds_mergeZip zab ina inb = mz where
    mz a b as bs = DSeq $ \ tq -> mzs a b tq (dstep as tq) (dstep bs tq) -- merge zip query
    mzs Nothing _ _ DSDone bs = ds_map_step (fmap inb) bs -- transfer b values
    mzs _ Nothing _ as DSDone = ds_map_step (fmap ina) as -- transfer a values
    mzs (Just af) _ _ DSDone bs = ds_map_step (fmap (zab af)) bs -- zip with final a
    mzs _ (Just bf) _ as DSDone = ds_map_step (fmap (flip zab bf)) as -- zip with final b
    mzs a b _ (DSWait as) (DSWait bs) = DSWait (mz a b as bs) -- waiting, no updates yet
    mzs Nothing b tq (DSNext _ Nothing as') sb = mzs Nothing b tq (dstep as' tq) sb -- false update left
    mzs a Nothing tq sa (DSNext _ Nothing bs') = mzs a Nothing tq sa (dstep bs' tq) -- false update right
    mzs _ b _ (DSNext ta a' as') (DSWait bs) = DSNext ta (mbz a' b) (mz a' b as' bs) -- step left
    mzs a _ _ (DSWait as) (DSNext tb b' bs') = DSNext tb (mbz a b') (mz a b' as bs') -- step right
    mzs a b _ (DSNext ta a' as') (DSNext tb b' bs') =
        case compare ta tb of
            LT -> DSNext ta (mbz a' b ) (mz a' b  as' bs )
            GT -> DSNext tb (mbz a  b') (mz a  b' as  bs')
            EQ -> DSNext ta (mbz a' b') (mz a' b' as' bs')
        where as = ds_first ta a' as'
              bs = ds_first tb b' bs'
    mbz (Just a) (Just b) = Just (zab a b)
    mbz Nothing mb = fmap inb mb
    mbz ma Nothing = fmap ina ma

-- | use second signal to control the activity of the first.
-- | ds_mask0 assumes x hidden by y
ds_mask0 :: (Maybe a) -> DSeq (Maybe a) -> DSeq (Maybe b_) -> DSeq (Maybe a)
ds_mask0 x xs ys = DSeq $ \ tq ->
    let ux = dstep xs tq in
    let uy = dstep ys tq in
    ds_mask0_step x tq ux uy

-- | ds_mask1 assumes x not hidden by y (x might be 'Nothing' anyway)
ds_mask1 :: (Maybe a) -> DSeq (Maybe a) -> DSeq (Maybe b_) -> DSeq (Maybe a)
ds_mask1 x xs ys = DSeq $ \ tq ->
    let ux = dstep xs tq in
    let uy = dstep ys tq in
    ds_mask1_step x tq ux uy 

-- find next element, mask currently hiding x
ds_mask0_step :: Maybe a -> T -> DStep (Maybe a)
              -> DStep (Maybe b_) -> DStep (Maybe a)
ds_mask0_step _ _ _ DSDone = DSDone       -- masked from now on
ds_mask0_step Nothing _ DSDone _ = DSDone -- signal inactive
ds_mask0_step (Just c) tq DSDone uy = ds_const0_step c tq uy -- mask a constant
ds_mask0_step x _ (DSWait xs) (DSWait ys) = DSWait (ds_mask0 x xs ys)
ds_mask0_step Nothing tq (DSNext _ Nothing xs') uy =
    ds_mask0_step Nothing tq (dstep xs' tq) uy -- false step left
ds_mask0_step x tq ux (DSNext _ Nothing ys') =
    ds_mask0_step x tq ux (dstep ys' tq) -- false step right
ds_mask0_step _ tq (DSNext _ x' xs') uy@(DSWait _) =
    ds_mask0_step x' tq (dstep xs' tq) uy -- step left masked
ds_mask0_step x tq ux@(DSWait xs) (DSNext ty (Just _) ys') =
    ds_mask0_step_unmask x tq ux xs ty ys'
ds_mask0_step x tq ux@(DSNext tx x' xs') uy@(DSNext ty (Just _) ys') =
    case (compare tx ty) of
        LT -> ds_mask0_step x' tq (dstep xs' tq) uy
        GT -> ds_mask0_step_unmask x tq ux (ds_first tx x' xs') ty ys'
        EQ -> ds_mask0_step_unmask x' tq (dstep xs' tq) xs' ty ys'

-- unmask a signal, handle the case there is Nothing to unmask
ds_mask0_step_unmask :: Maybe a -> T -> DStep (Maybe a) -> DSeq (Maybe a) 
                     -> T -> DSeq (Maybe b_) -> DStep (Maybe a)
ds_mask0_step_unmask Nothing tq ux _ _ ys = ds_mask1_step Nothing tq ux (dstep ys tq)
ds_mask0_step_unmask x _ _ xs ty ys = DSNext ty x (ds_mask1 x xs ys)

-- find next element, mask exposes x
ds_mask1_step :: Maybe a -> T -> DStep (Maybe a) 
              -> DStep (Maybe b_) -> DStep (Maybe a)
ds_mask1_step _ _ ux DSDone = ux   -- unmasked from now on
ds_mask1_step Nothing _ DSDone _ = DSDone -- signal inactive from now on
ds_mask1_step (Just c) tq DSDone uy = ds_const1_step c tq uy -- mask a constant
ds_mask1_step x _ (DSWait xs) (DSWait ys) = DSWait (ds_mask1 x xs ys)
ds_mask1_step Nothing tq (DSNext _ Nothing xs') uy =
    ds_mask1_step Nothing tq (dstep xs' tq) uy -- false step left (still Nothing)
ds_mask1_step x tq ux (DSNext _ (Just _) ys') =
    ds_mask1_step x tq ux (dstep ys' tq) -- false step right (still exposed)
ds_mask1_step _ _ (DSNext tx x' xs') (DSWait ys) =
    DSNext tx x' (ds_mask1 x' xs' ys) -- step left exposed
ds_mask1_step x tq ux@(DSWait xs) (DSNext ty Nothing ys') =
    ds_mask1_step_right x tq ux xs ty ys'
ds_mask1_step x tq ux@(DSNext tx x' xs') (DSNext ty Nothing ys') =
    case (compare tx ty) of
        LT -> DSNext tx x' (ds_mask1 x' xs' (ds_first ty Nothing ys'))
        GT -> ds_mask1_step_right x tq ux (ds_first tx x' xs') ty ys'
        EQ -> if isNothing x then ds_mask0_step x' tq (dstep xs' tq) (dstep ys' tq) 
                             else DSNext tx Nothing (ds_mask0 x' xs' ys')

-- mask a signal; handle case where there is Nothing to mask
ds_mask1_step_right :: Maybe a -> T -> DStep (Maybe a) -> DSeq (Maybe a) 
                    -> T -> DSeq (Maybe b_) -> DStep (Maybe a)
ds_mask1_step_right Nothing tq ux _ _ ys = ds_mask0_step Nothing tq ux (dstep ys tq)
ds_mask1_step_right x _ _ xs ty ys = DSNext ty Nothing (ds_mask0 x xs ys)

-- | filter duplicate 'Nothing' values. 
-- assume prior element was 'Nothing'
ds_adjn0 :: DSeq (Maybe a) -> DSeq (Maybe a)
ds_adjn0 ds = DSeq $ \ tq -> ds_adjn0_step tq (dstep ds tq)

-- separated for smoother recursive case
ds_adjn0_step :: T -> DStep (Maybe a) -> DStep (Maybe a)
ds_adjn0_step _ DSDone = DSDone
ds_adjn0_step _ (DSWait ds') = DSWait (ds_adjn0 ds')
ds_adjn0_step tq (DSNext _ Nothing ds') = ds_adjn0_step tq (dstep ds' tq)
ds_adjn0_step _ (DSNext tx x ds') = DSNext tx x (ds_adjn1 ds')

-- | same as ds_adjn0, but assuming prior value was Just
ds_adjn1 :: DSeq (Maybe a) -> DSeq (Maybe a)
ds_adjn1 ds = DSeq $ \ tq -> 
    case dstep ds tq of
        DSDone -> DSDone
        DSWait ds' -> DSWait (ds_adjn1 ds')
        DSNext tm Nothing ds' -> DSNext tm Nothing (ds_adjn0 ds')
        DSNext tm v ds' -> DSNext tm v (ds_adjn1 ds')


