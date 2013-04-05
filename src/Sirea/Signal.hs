
-- | Signals represent continuous time varying state and activity.
-- Conceptually, a signal is similar to:
--
--    type Signal a = T -> Maybe a
--
-- The value is 'Nothing' while the signal is inactive; effectively
-- the signal doesn't exist for that period. An active signal will
-- hold a value (which might vary with time). The `activity cycle` 
-- of any signal is achieved by reducing values to unit, Signal (). 
--
-- For Sirea, only discrete-varying signals are supported. These
-- signals change discretely, at specific instants on a continuous 
-- timeline, then hold constant for a non-zero duration between each
-- change. A well-behaved discrete-varying behavior only changes a
-- finite count in any bounded period, avoiding Zeno's paradox. A 
-- sampling method exists to discover instants of potential change.
--
-- RDP users never work with signals directly, but foreign service
-- adapters, FFI, and similar will need to work with signals. Sirea
-- users will sometimes be writing those adapters.
--
module Sirea.Signal 
 ( Sig
 , listToSig, sigToList
 , s_sample, s_sample_d, s_trim
 , s_never, s_always
 , s_const 
 , s_fmap, s_full_map
 , s_ap
 , s_zip, s_full_zip
 --, s_weave, s_full_weave
 , s_mask
 , s_merge
 , s_switch, s_switch'
 , s_is_final, s_term
 , s_is_final2, s_term2
 , s_activeBefore
 , s_delay
 --, s_peek
 , s_adjn, s_adjeqf
 , s_tseq
 --, s_strat
 -- instances: functor, applicative, alternative
 ) where

import Sirea.Time
import Sirea.Internal.SigType
import Control.Exception (assert)
import Control.Applicative
import Data.Maybe (isNothing)

instance Functor Sig where
    fmap  = s_fmap
    (<$)  = s_const

instance Applicative Sig where
    pure  = s_always
    (<*>) = s_ap
    (<*)  = s_mask
    (*>)  = flip s_mask

instance Alternative Sig where
    empty = s_never
    (<|>) = s_merge

-- | listToSig allows developers to turn a list of signal updates
-- into a signal. The list must be ordered in strict monotonic time,
-- and must be of finite size. 
listToSig :: (Maybe a) -> [(T,Maybe a)] -> Sig a
listToSig v0 = Sig v0 . seqFromList 

-- | Sample a signal for its value at given instant. The signal
-- may be inactive at the given instant, in which case 'Nothing'
-- is returned. In addition, the trimmed signal is returned for 
-- further sampling (to avoid redundant computation)
s_sample :: Sig a -> T -> (Maybe a, Sig a)
s_sample s0 tm = let sf@(Sig hd _) = s_trim s0 tm in (hd, sf)

-- | Trim a signal to clear unnecessary history data. Collecting the
-- past while computing the future can ensure predictable space cost
-- and avoid space-time leaks.
s_trim :: Sig a -> T -> Sig a
s_trim (Sig x xs) tm = Sig x' xs' where
    (x',xs') = seqQuery x tm xs

-- | Discrete sample of a signal: rather than finding a value at a
-- given instant, returns the first instant of change between two
-- times, excluding the lower bound but including the upper. 
--
--    fst $ s_sample_d sig lower upper =
--       Just (t, v) -- potential change at instant t to value v
--         invariant: lower < t <= upper
--         note: v is sample; might be Nothing for inactive signals
--       Nothing -- no change in the given range
--
-- Note that there are no guarantees that v is actually a change in
-- value. That is, it could equal the prior v value. Use of s_filter 
-- can help eliminate duplicates and avoid redundant computations, 
-- but must be applied judiciously (or itself becomes redundant).
--
-- A trimmed signal for further sampling is also returned. The trim
-- is up to 'upper' if no sample was found, otherwise only up to 
-- the discovered sample. One can use this to efficiently acquire
-- multiple samples in a given time range (though sigToList is more
-- efficient).
s_sample_d :: Sig a -> T -> T -> (Maybe (T, Maybe a), Sig a)
s_sample_d (Sig hd tl) tLower tUpper =
    assert (tLower <= tUpper) $
    let (x,xs) = seqQuery hd tLower tl in
    case xs of
        Done -> (Nothing, Sig x Done)
        Step tx x' xs' ->
            assert (tx > tLower) $
            if (tx > tUpper) then (Nothing, Sig x xs) 
                             else (Just (tx,x'), Sig x' xs')

-- | sigToList will obtain the [(T,Maybe a)] states in a given time
-- range, similar to s_sample_d. Note that there will always be at
-- least one value for the signal's state at the lower bound time.
sigToList :: Sig a -> T -> T -> [(T, Maybe a)]
sigToList (Sig hd tl) tLower tUpper =
    assert (tLower <= tUpper) $
    let (x,xs) = seqQuery hd tLower tl in
    (tLower,x):(seqTakeList tUpper xs)

-- | a signal that is never active (`Nothing` at all times)
-- Same as `empty` from Alternative.
s_never  :: Sig a_
s_never = Sig Nothing Done

-- | a signal that is always active with a specific value c
-- Same as `pure` from Applicative.
s_always :: c -> Sig c
s_always c = Sig (Just c) Done

-- | replace all values in a signal with a constant c, such that the
-- signal varies between Just c and Nothing. This will also filter 
-- unnecessary updates from values now trivially known to be equal.
s_const :: c -> Sig a_ -> Sig c
s_const c (Sig Nothing tl) = Sig Nothing (seqConst0 c tl)
s_const c (Sig (Just _) tl) = Sig (Just c) (seqConst1 c tl) 

-- | Map applies a function across the active values of a signal.
-- Same as the Functor fmap.
s_fmap :: (a -> b) -> Sig a -> Sig b
s_fmap = s_full_map . fmap

-- | Full map applies a function across all values, including 
-- inactivity of the signal. 
s_full_map :: (Maybe a -> Maybe b) -> Sig a -> Sig b
s_full_map f (Sig x xs) = Sig (f x) (seqMap f xs)

-- | Ap applies one signal to another. 
s_ap :: Sig (a -> b) -> Sig a -> Sig b
s_ap = s_full_zip (<*>)

-- | zip two signals using a provided function. The resulting signal
-- is active only when both inputs are active. 
s_zip :: (a -> b -> c) -> Sig a -> Sig b -> Sig c
s_zip = s_full_zip . liftA2

-- | Full zip applies a function across periods of inactivity, too.
s_full_zip :: (Maybe a -> Maybe b -> Maybe c) -> Sig a -> Sig b -> Sig c
s_full_zip jf (Sig a as) (Sig b bs) = Sig (f b) (seqAp f b fs bs) where
    f = jf a
    fs = seqMap jf as

{-
-- IDEA: `weave` functions that pick one element as the contributing
-- element at any given step. This is a more specialized `zip` that
-- can avoid the interruptions due to updates in the unused element.
-- This becomes the generic basis for mask & merge, and is useful for
-- the single-element minimax demand monitors. 
--
-- Intermediate, it is also possible to have a weave-zip that can
-- identify when only one of the two inputs contributes. However,
-- this might also be achieved via a merge of a weave and a zip.
--
-- Will need to find a sensible API for these before progressing.

-- | to `weave` a value is a specialized form of `zip`; the idea is
-- that only one signal contributes to the weave at any given time,
-- so we can eliminate some updates from the signal that does not
-- contribute at that time.
-- This is tested by `a -> b -> Bool`. If True, the left side will
-- contribute. If False, the right side will contribute. 
s_weave :: (a -> a -> Bool) -> Sig a -> Sig a -> Sig a
s_weave w = s_full_weave w'
    where w' _ Nothing = True
          w' Nothing _ = False
          w' (Just a0) (Just a1) = w a0 a1

-- could add an a->b->c for a weave-zip.

-- | full weave provides some more flexibility on how to merge in
-- case of Nothing values. 
s_full_weave :: (Maybe a -> Maybe a -> Bool) 
             -> Sig a -> Sig a -> Sig a
-}

-- | Mask one signal with the activity profile of another. That is,
-- the resulting signal is only active when both input signals are
-- active, but the value is always from the signal on the left.
--    s_mask = s_zip const (but somewhat optimized)
s_mask :: Sig a -> Sig b_ -> Sig a
s_mask (Sig a as) (Sig Nothing bs) = Sig Nothing (seqMask0 a as bs)
s_mask (Sig a as) (Sig (Just _) bs) = Sig a (seqMask1 a as bs)

-- | Merge two signals by using the left signal when it is active,
-- otherwise the right signal.
--    s_merge = s_full_zip (<|>)
-- But is much more optimizable than general zips.
s_merge :: Sig a -> Sig a -> Sig a
s_merge (Sig l0 ltl) (Sig r0 rtl) = Sig (l0 <|> r0) (seqMerge l0 r0 ltl rtl)

-- | Switch from the left signal to the right signal at a given
-- instant. The left signal is used until just before the instant,
-- then the right signal is used starting at that instant.
s_switch :: Sig a -> T -> Sig a -> Sig a
s_switch (Sig hd0 tl0) t (Sig hdf tlf) = Sig hd0 (seqSigup tl0 t hdf tlf)

-- | Switch but with slightly stricter semantics - ensures that the
-- structure of the signal up to T is flat, i.e. so we don't keep
-- keep now defunct future values in memory. This can improve GC.
s_switch' :: Sig a -> T -> Sig a -> Sig a
s_switch' = s_switch -- current signal rep. is spine strict

-- | Test whether a signal is in its final state from a particular
-- instant. This is useful for garbage collection and optimizations.
-- This is a semi-decision; it may return False if the answer is
-- unknown (or would risk divergence) at the given instant.
s_is_final :: Sig a -> T -> Bool
s_is_final (Sig _ tl) tm = 
    case seqTrim tm tl of
        Done -> True
        _ -> False

-- | Same as s_is_final, but the time of the test is not the same as
-- the time for the query. s_is_final s tQuery tTest. tTest >= tQuery.
s_is_final2 :: Sig a -> T -> T -> Bool
s_is_final2 s tQ tT = assert (tT >= tQ) $ s_is_final s tQ -- current impl only needs one param

-- | Test whether a signal has terminated after a given instant.
--     s_term s t = isNothing (s_sample s t) && (s_is_final s t)
s_term :: Sig a -> T -> Bool
s_term s tQ = s_term2 s tQ tQ

-- | Test whether a signal has terminated after a given instant, but
-- time of test may be different from the time of query. This allows
-- testing further ahead to make the decision.
--     s_term s tQuery tTest. tTest >= tQuery.
s_term2 :: Sig a -> T -> T -> Bool
s_term2 (Sig hd tl) tQ tT = assert (tT >= tQ) $ 
    let (x,xs) = seqQuery hd tQ tl in
    isNothing x && termTail tT xs

termTail :: T -> Seq (Maybe a) -> Bool
termTail _ Done = True
termTail tT (Step tx x xs) = 
    if (tx > tT) then False else
    if isNothing x then termTail tT xs
                   else False
        
-- | Test whether a signal is active before a given time. This is a
-- test that helps discover lower-bounds for activity, but there is
-- no absolute lower bound (since signals can extend infinitely into
-- the past). Returns True iff there is activity strictly before the
-- queried time.
s_activeBefore :: Sig a -> T -> Bool
s_activeBefore (Sig (Just _) _) _ = True
s_activeBefore (Sig Nothing tl) tT = activeTail tT tl

activeTail :: T -> Seq (Maybe a) -> Bool
activeTail _ Done = False
activeTail tq (Step _ Nothing s) = activeTail tq s
activeTail tq (Step tx (Just _) _) = (tq < tx)

-- | s_adjn will eliminate adjacent `Nothing` values. These might 
-- exist after s_full_map to filter a signal by its values.
s_adjn :: Sig a -> Sig a
s_adjn (Sig Nothing xs) = Sig Nothing (seqAdjn0 xs)
s_adjn (Sig x@(Just _) xs) = Sig x (seqAdjn1 xs)

-- | Delay a signal - time-shifts the signal so that the same values
-- are observed at a later instant in time. Models latency. Activity
-- is also time-shifted.
--
-- Note: for RDP it is necessary to compute static delays, so this
-- function should never be used in FFI adapters. Wrap the adapter
-- with RDP 'bdelay' behaviors instead.
s_delay :: DT -> Sig a -> Sig a
s_delay dt (Sig x xs) = Sig x (seqDelay dt xs)

-- utility for s_delay
seqDelay :: DT -> Seq a -> Seq a
seqDelay _ Done = Done
seqDelay dt (Step t v s) = Step (addTime t dt) v (seqDelay dt s)

-- | Erase adjacent signal values that are equal in value. You can
-- provide the equality function that compares one value to another.
-- This will eliminate redundant updates. It is intended for 
-- performance, but must be used judiciously (the filter itself has 
-- a cost).
s_adjeqf :: (a -> a -> Bool) -> Sig a -> Sig a
s_adjeqf eq (Sig x xs) = Sig x (seqAdjeqf meq x xs) where
    meq (Just a) (Just a') = eq a a'
    meq Nothing Nothing = True
    meq _ _ = False


-- | Combination trim and sequence of a signal's values up to a
-- given time.
s_tseq :: (Maybe a -> ()) -> T -> Sig a -> Sig a
s_tseq f tm (Sig hd tl) = f hd `seq` ts hd tl where
    ts x Done = (Sig x Done)
    ts x s@(Step t v s') =
        if (tm < t) then (Sig x s) else
        f v `seq` ts v s'

-- TODO?
-- Apply a strategy to initialize parallel evaluation of a signal 
-- during sampling. I.e. if you sample at time T, may initialize 
-- parallel computation of the signal at time T+dt. 
-- s_strat :: DT -> Sig (Eval a) -> Sig a

-- IDEAS:
--  choke: should be done with intermediate state, as RDP behavior,
--    since it means we're looking at past values or tracking time.
--  improve s_adjeqf handoff? maybe some sort of `improving` value
--    model fot the update times? seems complicated.
--  

