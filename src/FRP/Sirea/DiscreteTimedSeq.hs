
-- | A naive approach to modeling a discrete updating signal in 
-- continuous time is to use a list of [(t,v)] pairs in monotonic 
-- order. Unfortunately, this has a problem when we filter signals, 
-- e.g. to eliminate adjacent duplicate values. The basic issue is 
-- that there is no upper limit for how far we'll search into the 
-- future for the next value.
--
-- DiscreteTimedSeq enables the caller to control the search for the
-- next update, in terms of a simple time model (ordinal), and thus
-- ensures predictable computation. 
--
-- The advantage, then, is that clients of DiscreteTimedSeq won't be
-- exposed unnecessarily to `updates` that don't actually change
-- anything, and thus reduce redundant computation. However, action
-- to explicitly filter values is necessary (ds_adjeqf)
module FRP.Sirea.DiscreteTimedSeq
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
    , ds_mask0, ds_mask1
    ) where

import Control.Exception (assert)

-- | DStep is the result of a search. 
data DStep t a 
    = DSDone
    | DSWait !(DSeq t a)
    | DSNext !t a !(DSeq t a)

-- | DSeq allows us to step through time. Each step gives us the
-- next update at or before the queried instant, along with the
-- subsequent sequence.
newtype DSeq t a = DSeq { dstep :: (t -> DStep t a) }

-- | ds_done is a basic constructor, equivalent to the empty list. 
ds_done :: DSeq t a
ds_done = DSeq (const DSDone)

-- | ds_first adds an element to the beginning of a sequence.
ds_first :: (Ord t) => t -> a -> DSeq t a -> DSeq t a
ds_first tm v ds' = DSeq $ \ tq -> 
   if (tq < tm) then DSWait (ds_first tm v ds')
                else DSNext tm v ds' 

-- | lazily transform a list into timed sequence. List should be in
-- strict monotonic order, with no divergence between steps (do not
-- filter the list until after you have it inside DSeq!).
ds_fromList :: (Ord t) => [(t,v)] -> DSeq t v
ds_fromList [] = ds_done
ds_fromList ((tx,x):xs) = ds_first tx x xs'
    where xs' = ds_fromList' tx xs

-- ds_fromList' applies an assertion for strict monotonic order.
ds_fromList' :: (Ord t) => t -> [(t,v)] -> DSeq t v
ds_fromList' _ [] = ds_done
ds_fromList' t0 ((tx,x):xs) = assert (t0 < tx) (ds_first tx x xs')
    where xs' = ds_fromList' tx xs

-- | ds_takeList - obtains a list from the sequence, up to a given
-- time. This is useful for testing and debugging (i.e. for 'Show').
ds_takeList :: t -> DSeq t v -> [(t,v)]
ds_takeList tm ds =
    case dstep ds tm of
        DSDone -> []
        DSWait _ -> []
        DSNext tx x ds' -> (tx,x):(ds_takeList tm ds')

-- | ds_const0 replaces values with a constant under the assumption 
-- that the prior value was Nothing. It will automatically filter
-- out adjacent Nothing values.
ds_const0 :: c -> DSeq t (Maybe a) -> DSeq t (Maybe c)
ds_const0 c ds = DSeq $ \ tq -> ds_const0_step c tq (dstep ds tq)

-- | ds_const1 replaces values by a constant under the assumption 
-- that the prior value was already Just c. It will automatically
-- filter out adjacent Just c values.
ds_const1 :: c -> DSeq t (Maybe a) -> DSeq t (Maybe c)
ds_const1 c ds = DSeq $ \ tq -> ds_const1_step c tq (dstep ds tq)

ds_const0_step :: c -> t -> DStep t (Maybe a) -> DStep t (Maybe c)
ds_const0_step _ _  DSDone = DSDone
ds_const0_step c _  (DSWait ds) = DSWait (ds_const0 c ds)
ds_const0_step c tq (DSNext _ Nothing ds) = ds_const0_step c tq (dstep ds tq)
ds_const0_step c _  (DSNext tm (Just _) ds) = DSNext tm (Just c) (ds_const1 c ds)

ds_const1_step :: c -> t -> DStep t (Maybe a) -> DStep t (Maybe c)
ds_const1_step _ _  DSDone = DSDone
ds_const1_step c _  (DSWait ds) = DSWait (ds_const1 c ds)
ds_const1_step c _  (DSNext tm Nothing ds) = DSNext tm Nothing (ds_const0 c ds)
ds_const1_step c tq (DSNext _ (Just _) ds) = ds_const1_step c tq (dstep ds tq)

-- | ds_map applies a function to every element in the sequence.
ds_map :: (a -> b) -> DSeq t a -> DSeq t b
ds_map f ds = DSeq (ds_map_step f . dstep ds)

-- | ds_map_step applies a function to a particular step. 
ds_map_step :: (a -> b) -> DStep t a -> DStep t b
ds_map_step _ DSDone = DSDone
ds_map_step f (DSWait ds) = DSWait (ds_map f ds)
ds_map_step f (DSNext tx x ds) = DSNext tx (f x) (ds_map f ds)

-- | ds_query obtains a value of a DSeq at a given time, treating
-- it like a signal. The previous value must be provided. The
-- future sequence is part of the result.
ds_query :: a -> t -> DSeq t a -> (a, DSeq t a)
ds_query x tq ds =
    case dstep ds tq of
        DSDone           -> (x, ds_done)
        DSWait ds'       -> (x, ds')
        DSNext _ x' ds'  -> ds_query x' tq ds'

-- | ds_sigup combines two signals at an intermediate instant.
--   The initial value of the second signal must be provided.
ds_sigup :: (Ord t) => DSeq t a -> t -> a -> DSeq t a -> DSeq t a
ds_sigup xs tm y ys = ds_sigup_i ays tm xs
    where (y',ys')  = ds_query y tm ys
          ays       = ds_first tm y' ys' 

-- internal part of ds_sigup.
ds_sigup_i :: (Ord t) => DSeq t a -> t -> DSeq t a -> DSeq t a
ds_sigup_i ys tm xs =
    case dstep xs tm of
        DSDone   -> ys
        DSWait _ -> ys
        DSNext tx x xs' -> if (tx < tm) 
            then ds_first tx x (ds_sigup_i ys tm xs')
            else ys

-- | ds_sigup' is a stricter version of ds_sigup.
-- This should help control space when number of updates is high.
ds_sigup' :: (Ord t) => DSeq t a -> t -> a -> DSeq t a -> DSeq t a
ds_sigup' xs tm y ys = ays `seq` ds_sigup_i' ays tm xs
    where (y',ys') = ds_query y tm ys
          ays      = ds_first tm y' ys'

-- internal function of ds_sigup'
ds_sigup_i' :: (Ord t) => DSeq t a -> t -> DSeq t a -> DSeq t a
ds_sigup_i' ys tm xs =
    case dstep xs tm of
        DSDone      -> ys
        DSWait _    -> ys
        DSNext tx x xs' -> if (tx < tm)
            then let xsu = ds_sigup_i' ys tm xs' in
                 xsu `seq` ds_first tx x xsu
            else ys

-- | filter the sequence
ds_filter :: (a -> Bool) -> DSeq t a -> DSeq t a
ds_filter fnKeep ds = DSeq $ \ tq ->
    case dstep ds tq of
        DSDone -> DSDone
        DSWait ds' -> DSWait (ds_filter fnKeep ds')
        DSNext tx x ds' -> 
            let rest = ds_filter fnKeep ds' in
            if (fnKeep x) then DSNext tx x rest
                else dstep rest tq

-- | filter adjacent entries by a given equality function, with an 
-- initial value. The test value to the filter updates whenever a
-- value is accepted into the sequence (i.e. not equal).
ds_adjeqfx :: (a -> a -> Bool) -> a -> DSeq t a -> DSeq t a
ds_adjeqfx eq x ds = DSeq $ \ tq ->
    case dstep ds tq of
        DSDone      -> DSDone
        DSWait ds'  -> DSWait $ ds_adjeqfx eq x ds'
        DSNext tx x' ds' -> if (eq x x') 
            then dstep (ds_adjeqfx eq x ds') tq
            else DSNext tx x' (ds_adjeqfx eq x' ds')

-- | apply elements of one sequence to another on matching times.
ds_ap :: (Ord t) => (x -> y) -> x -> DSeq t (x -> y) -> DSeq t x -> DSeq t y
ds_ap f x fs xs = DSeq $ \ tq ->
    let uf = dstep fs tq in
    let ux = dstep xs tq in
    ds_ap_step f x uf ux

-- internal operation for ds_ap
ds_ap_step :: (Ord t) => (x -> y) -> x 
                      -> DStep t (x -> y) -> DStep t x -> DStep t y
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
-- signals favoring the RHS while active. This is basically a
-- specialized zip for merging. In particular, it will avoid
-- introducing duplicate updates except at transitions between
-- signal sources.
ds_merge :: (Ord t) => (Maybe a) -> (Maybe a) 
         -> DSeq t (Maybe a) -> DSeq t (Maybe a) -> DSeq t (Maybe a)
ds_merge x y xs ys = DSeq $ \ tq ->
    let ux = dstep xs tq in
    let uy = dstep ys tq in
    ds_merge_step x y tq ux uy

ds_merge_step :: (Ord t) => (Maybe a) -> (Maybe a) -> t
    -> DStep t (Maybe a) -> DStep t (Maybe a) -> DStep t (Maybe a)
ds_merge_step _ y _ ux DSDone = if kp y then DSDone else ux
ds_merge_step x _ _ DSDone uy = if kp x then uz else uy
    where uz = ds_map_step (\ z -> if kp z then z else x) uy
ds_merge_step x y _ (DSWait xs) (DSWait ys) = DSWait (ds_merge x y xs ys)
ds_merge_step x y tq (DSNext tx x' xs') uy@(DSWait ys) =
    ds_merge_step_left x y tq tx x' xs' uy ys
ds_merge_step x y tq ux@(DSWait xs) (DSNext ty y' ys') =
    ds_merge_step_right x y tq ux xs ty y' ys'
ds_merge_step x y tq ux@(DSNext tx x' xs') uy@(DSNext ty y' ys') =
    case (compare tx ty) of
        LT -> ds_merge_step_left x y tq tx x' xs' uy ys
        GT -> ds_merge_step_right x y tq ux xs ty y' ys'
        EQ -> let rest = ds_merge x' y' xs' ys' in
              if kp y' then DSNext ty y' rest
                       else DSNext tx x' rest
    where xs = ds_first tx x' xs'
          ys = ds_first ty y' ys'

-- refactored functions from ds_merge
ds_merge_step_left :: (Ord t) => Maybe a -> Maybe a -> t -> t 
    -> Maybe a -> DSeq t (Maybe a) -> DStep t (Maybe a) -> DSeq t (Maybe a)
    -> DStep t (Maybe a)
ds_merge_step_left _ y tq tx x' xs' uy ys =
    if kp y then ds_merge_step x' y tq (dstep xs' tq) uy
            else DSNext tx x' (ds_merge x' y xs' ys) 

ds_merge_step_right :: (Ord t) => Maybe a -> Maybe a -> t
    -> DStep t (Maybe a) -> DSeq t (Maybe a)
    -> t -> Maybe a -> DSeq t (Maybe a)
    -> DStep t (Maybe a)
ds_merge_step_right x y tq ux xs ty y' ys' =
    let rest = ds_merge x y' xs ys' in
    if kp y' then DSNext ty y' rest
      else if kp y then DSNext ty x rest
        else ds_merge_step x y' tq ux (dstep ys' tq) 

kp :: Maybe a -> Bool
kp Nothing  = False
kp _        = True


-- | use second signal to control the activity of the first.
-- The first two inputs are the current values in the signals.
-- Specialized to avoid introducing unnecessary changes.
ds_mask :: (Ord t) => (Maybe a) -> (Maybe b_) 
    -> DSeq t (Maybe a) -> DSeq t (Maybe b_) -> DSeq t (Maybe a)
ds_mask x y xs ys = if kp y then ds_mask1 x xs ys else ds_mask0 x xs ys

-- | ds_mask0 assumes x hidden by y
ds_mask0 :: (Ord t) => (Maybe a) 
    -> DSeq t (Maybe a) -> DSeq t (Maybe b_) -> DSeq t (Maybe a)
ds_mask0 x xs ys = DSeq $ \ tq ->
    let ux = dstep xs tq in
    let uy = dstep ys tq in
    ds_mask0_step x tq ux uy

-- | ds_mask1 assumes x not hidden by y (x might be 'Nothing' anyway)
ds_mask1 :: (Ord t) => (Maybe a) 
    -> DSeq t (Maybe a) -> DSeq t (Maybe b_) -> DSeq t (Maybe a)
ds_mask1 x xs ys = DSeq $ \ tq ->
    let ux = dstep xs tq in
    let uy = dstep ys tq in
    ds_mask1_step x tq ux uy 

-- find next element, mask currently hiding x
ds_mask0_step :: (Ord t) => Maybe a -> t -> DStep t (Maybe a)
    -> DStep t (Maybe b_) -> DStep t (Maybe a)
ds_mask0_step _ _ _ DSDone = DSDone       -- masked from now on
ds_mask0_step Nothing _ DSDone _ = DSDone -- signal inactive
ds_mask0_step (Just c) tq DSDone uy = ds_const0_step c tq uy
ds_mask0_step x _ (DSWait xs) (DSWait ys) = DSWait (ds_mask0 x xs ys)
ds_mask0_step _ tq (DSNext _ x' xs') uy@(DSWait _) =
    ds_mask0_step x' tq (dstep xs' tq) uy
ds_mask0_step x tq ux@(DSWait xs) (DSNext ty y' ys') =
    ds_mask0_step_right x tq ux xs ty y' ys'
ds_mask0_step x tq ux@(DSNext tx x' xs') uy@(DSNext ty y' ys') =
    case (compare tx ty) of
        LT -> ds_mask0_step x' tq (dstep xs' tq) uy
        GT -> ds_mask0_step_right x tq ux xs ty y' ys'
        EQ -> if (kp y') then
                if (kp x') then DSNext tx x' (ds_mask1 x' xs' ys')
                  else dstep (ds_mask1 x' xs' ys') tq
                else dstep (ds_mask0 x' xs' ys') tq
    where xs = ds_first tx x' xs'

-- refactored from ds_mask0_step.
ds_mask0_step_right :: (Ord t) => Maybe a -> t -> DStep t (Maybe a) 
    -> DSeq t (Maybe a) -> t -> Maybe b_ -> DSeq t (Maybe b_)
    -> DStep t (Maybe a)
ds_mask0_step_right x tq ux xs ty y' ys' =
    if (kp y') then 
        if (kp x) then DSNext ty x (ds_mask1 x xs ys')
          else ds_mask1_step x tq ux (dstep ys' tq)
       else ds_mask0_step x tq ux (dstep ys' tq)

-- find next element, mask exposes x
ds_mask1_step :: (Ord t) => Maybe a -> t -> DStep t (Maybe a) 
    -> DStep t (Maybe b_) -> DStep t (Maybe a)
ds_mask1_step _ _ ux DSDone = ux   -- unmasked from now on
ds_mask1_step Nothing _ DSDone _ = DSDone -- signal inactive
ds_mask1_step (Just c) tq DSDone uy = ds_const1_step c tq uy 
ds_mask1_step x _ (DSWait xs) (DSWait ys) = DSWait (ds_mask1 x xs ys)
ds_mask1_step x tq (DSNext tx x' xs') uy@(DSWait ys) =
    ds_mask1_step_left x tq tx x' xs' uy ys
ds_mask1_step x tq ux@(DSWait xs) (DSNext ty y' ys') =
    ds_mask1_step_right x tq ux xs ty y' ys'
ds_mask1_step x tq ux@(DSNext tx x' xs') uy@(DSNext ty y' ys') =
    case (compare tx ty) of
        LT -> ds_mask1_step_left x tq tx x' xs' uy ys
        GT -> ds_mask1_step_right x tq ux xs ty y' ys'
        EQ -> if (kp y') then ds_mask1_step_left x tq tx x' xs' (dstep ys' tq) ys'
                else if (kp x) then DSNext tx Nothing (ds_mask0 x' xs' ys')
                  else dstep (ds_mask0 x' xs' ys') tq
    where xs = ds_first tx x' xs'
          ys = ds_first ty y' ys'

-- refactored from ds_mask1_step.
ds_mask1_step_left :: (Ord t) => Maybe a -> t -> t 
    -> Maybe a -> DSeq t (Maybe a) 
    -> DStep t (Maybe b_) -> DSeq t (Maybe b_)
    -> DStep t (Maybe a)
ds_mask1_step_left x tq tx x' xs' uy ys =
    if (kp x || kp x') then DSNext tx x' (ds_mask1 x' xs' ys)
      else ds_mask1_step x' tq (dstep xs' tq) uy

-- refactored from ds_mask1_step.
ds_mask1_step_right :: (Ord t) => Maybe a -> t 
    -> DStep t (Maybe a) -> DSeq t (Maybe a)
    -> t -> Maybe b_ -> DSeq t (Maybe b_) 
    -> DStep t (Maybe a)
ds_mask1_step_right x tq ux xs ty y' ys' =
    if (kp y') then ds_mask1_step x tq ux (dstep ys' tq)
      else if (kp x) then DSNext ty Nothing (ds_mask0 x xs ys')
        else ds_mask0_step x tq ux (dstep ys' tq)


-- | filter duplicate 'Nothing' values. 
-- assume prior element was 'Nothing'
ds_adjn0 :: DSeq t (Maybe a) -> DSeq t (Maybe a)
ds_adjn0 ds = DSeq $ \ tq ->
    case dstep ds tq of
        DSDone -> DSDone
        DSWait ds' -> DSWait (ds_adjn0 ds')
        DSNext _ Nothing ds' -> dstep ds' tq
        DSNext tm v ds' -> DSNext tm v (ds_adjn1 ds')

-- same as ds_adjn1, but assuming prior value was Just
ds_adjn1 :: DSeq t (Maybe a) -> DSeq t (Maybe a)
ds_adjn1 ds = DSeq $ \ tq ->
    case dstep ds tq of
        DSDone -> DSDone
        DSWait ds' -> DSWait (ds_adjn1 ds')
        DSNext tm Nothing ds' -> DSNext tm Nothing (ds_adjn0 ds')
        DSNext tm v ds' -> DSNext tm v (ds_adjn1 ds')




