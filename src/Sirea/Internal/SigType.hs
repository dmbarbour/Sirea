
-- | Most developers don't need direct access to the representation
-- of signals. But for the few who do, here it is.
--
-- There are also a few operations on sequences. 
module Sirea.Internal.SigType
    ( Sig(..)
    , Seq(..)
    , seqStep, seqStop, seqDone
    , seqFirst, seqFromList, seqToList, seqTakeList
    , seqConst, seqConst0, seqConst1
    , seqMap
    , seqQuery
    , seqSigup
    , seqFilter, seqFilter0
    , seqAdjeqf
    , seqZip
    , seqMergeZip
    , seqMerge
    ) where

import Sirea.Time (T)
import Control.Exception (assert)
import Control.Applicative

-- | Sig is an abstract type for discrete-varying signals in Sirea.
-- A signal is defined for all times, but in practice the past is
-- dropped (collected) while the future is updated over time. 
data Sig a = Sig !(Maybe a) !(Seq a)

-- seq is a spine-strict, compact sequence, with time in monotonic 
data Seq a
    = Step {-# UNPACK #-} !T a !(Seq a)
    | Stop {-# UNPACK #-} !T !(Seq a)
    | Done

seqStep :: T -> a -> Seq a -> Seq a
seqStep t v s = assert (monoTime t s) $ Step t v s

seqStop :: T -> Seq a -> Seq a
seqStop t s = assert (monoTime t s) $ Stop t s

seqDone :: Seq a
seqDone = Done

-- monotonic time for assertions
monoTime :: T -> Seq a -> Bool
monoTime t (Step t' _ _) = (t < t')
monoTime t (Stop t' _) = (t < t')
monoTime _ Done = True

-- concatenate a stop or value
seqFirst :: T -> Maybe a -> Seq a -> Seq a
seqFirst t Nothing s = seqStop t s
seqFirst t (Just a) s = seqStep t a s


-- convert a list to a sequence. The list must be finite and ordered
-- in time. 
seqFromList :: [(T,Maybe a)] -> Seq a
seqFromList = foldr (uncurry seqFirst) seqDone

-- convert a sequence to a list.
seqToList :: Seq a -> [(T,Maybe a)] 
seqToList (Step t v s) = (t,Just v):seqToList s
seqToList (Stop t s) = (t,Nothing):seqToList s 
seqToList Done = []

-- partial sequence to list (elements prior to the query time)
-- will include any element exactly at the queried time.
seqTakeList :: T -> Seq a -> [(T,v)]
seqTakeList tq (Step t v s) =
    if (tq < t) then [] else
    (t,Just v):seqTakeList tq s
seqTakeList tq (Stop t s) =
    if (tq < t) then [] else
    (t,Nothing):seqTakeList tq s
seqTakeList _ Done = []

-- replace values in a sequence with a constant. 
--  seqConst0 assumes prior value was Nothing (or Stopped)
--  seqConst1 assumes prior value was the constant.
--  seqConst keeps the first value and makes no assumptions.
seqConst, seqConst0, seqConst1 :: c -> Seq a -> Seq a

seqConst0 c (Step t _ s) = Step t c (seqConst1 c s)
seqConst0 c (Stop _ s) = seqConst0 c s
seqConst0 _ Done = Done

seqConst1 c (Step _ _ s) = seqConst1 c s
seqConst1 c (Stop t s) = Stop t (seqConst0 c s)
seqConst1 _ Done = Done

seqConst c (Step t _ s) = Step t c (seqConst1 c s)
seqConst c (Stop t s) = Stop t (seqConst0 c s)
seqConst _ Done = Done

-- map a function to every value in a sequence.
seqMap :: (a -> b) -> Seq a -> Seq b
seqMap f (Step t v s) = Step t (f v) (seqMap f s)
seqMap f (Stop t s) = Stop t (seqMap f s)
seqMap _ Done = Done

instance Functor (Seq a) where
    fmap = seqMap
    (<$) = seqConst

-- query a signal to obtain its value at a particular time, and the
-- rest of the signal starting from that point.
seqQuery :: Maybe a -> T -> Seq a -> (Maybe a, Seq a)
seqQuery x tq s@(Step t v s') =
    if (tq < t) then (x,s) else
    seqQuery (Just v) tq s'
seqQuery x tq s@(Stop t s') =
    if (tq < t) then (x,s) else
    seqQuery Nothing tq s'
seqQuery x _ Done = (x,Done)


-- switch from one signal to another at a particular time.
-- note the `Maybe a` value is considered the infinite 
-- history of the second signal.
seqSigup :: Seq a -> T -> (Maybe a) -> Seq a -> Seq a
seqSigup xs tU y ys = seqSigup_i ays tU xs where
    (y',ys') = seqQuery y tU ys
    ays = seqFirst tU y' ys'

seqSigup_i :: Seq a -> T -> Seq a -> Seq a
seqSigup_i ys tU (Step tx v xs) = 
    if (tx >= tU) then ys else 
    Step tx v (seqSigup_i ys tU xs)
seqSigup_i ys tU (Stop tx xs) =
    if (tx >= tU) then ys else
    Stop tx (seqSigup_i ys tU xs)
seqSigup_i ys _ Done = ys

-- filter a sequence given a function.
--     seqFilter0 assumes prior value is Nothing.
seqFilter, seqFilter0 :: (a -> Bool) -> Seq a -> Seq a
seqFilter f (Step t v s) =
    if (f v) then Step t v (seqFilter f s)
             else Stop t (seqFilter0 f s)
seqFilter f (Stop t s) = Stop t (seqFilter0 f s)
seqFilter _ Done = Done
seqFilter0 f (Step t v s) =
    if (f v) then Step t v (seqFilter f s)
             else seqFilter0 f s
seqFilter0 f (Stop _ s) = seqFilter0 f s
seqFilter0 _ Done = Done


-- filter adjacent values, given filter function and initial value.
-- (will also filter adjacent stops or Nothing values)
seqAdjeqf :: (a -> a -> Bool) -> Maybe a -> Seq a -> Seq a
seqAdjeqf eq Nothing s = seqAdjeqf0 eq s
seqAdjeqf eq (Just v) s = seqAdjeqf1 eq v s

seqAdjeqf0 :: (a -> a -> Bool) -> Seq a -> Seq a
seqAdjeqf0 eq (Step t v s) = Step t v (seqAdjeqf1 eq v s)
seqAdjeqf0 eq (Stop _ s) = seqAdjeqf0 eq s
seqAdjeqf0 _ Done = Done

seqAdjeqf1 :: (a -> a -> Bool) -> a -> Seq a -> Seq a
seqAdjeqf1 eq v0 (Step t v s) =
    if (eq v0 v) then seqAdjeqf1 eq v0 s
                 else Step t v (seqAdjeqf1 eq v s)
seqAdjeqf1 eq _ (Stop t s) = Stop t (seqAdjeqf0 eq s)
seqAdjeqf1 _ _ Done = Done

-- combine two signals, each of which may or may not have a value.
seqZip :: (Maybe a -> Maybe b -> Maybe c) -> Maybe a -> Maybe b -> Seq a -> Seq b -> Seq c
seqZip f a _ Done bs = fmap (f a) bs
seqZip f _ b as Done = fmap (flip f b) as
seqZip f a b as bs = 
    case compareT as bs of
        LT -> seqZipL f b as bs
        EQ -> seqZipE f as bs
        GT -> seqZipR f a as bs
seqZipL :: (Maybe a -> Maybe b 



(Just a) _ Done bs = fmap (f a) bs -- constant a
seqZip f _ (Just b) as Done = fmap (flip f b) as -- constant b
seqZip f a0 b0 (Stop ta as) (Stop tb bs) =
seqZip f a0 b0 (Stop ta
seqZip f a0 b0 (Ste



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


