
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
-- For Sirea, only discrete-varying signals are supported. That is,
-- active signals hold constant values for a duration, then switch
-- to different constant values. Discrete-varying signals are easy
-- to work with and don't present many semantic challenges compared
-- to continuous-varying. OTOH, they are also much less expressive.
--
-- RDP users never work with signals directly, but foreign service
-- adapters, FFI, and similar will need to work with signals.
--
module FRP.Sirea.Signal 
 ( Sig
 , listToSig, sigToList
 -- instances: functor, applicative, alternative, (monad?)
 ) where

import FRP.Sirea.Time
import FRP.Sirea.DiscreteTimedSeq
import Control.Exception (assert)
import Control.Monad (liftM2)

-- | Sig is an abstract type for discrete-varying signals in Sirea.
data Sig a = Sig 
    { s_head :: !(Maybe a) 
    , s_tail :: !(DSeq T (Maybe a)) 
    }

-- utility
mkSig :: Maybe a -> DSeq T (Maybe a) -> Sig a
mkSig v0 ds = Sig { s_head = v0, s_tail = ds }

-- | listToSig allows developers to turn a list of signal updates
-- into a signal. The list must be ordered in strict monotonic time,
-- which means no repeating times. It should not be divergent when
-- computing the next change. The value prior to the first update is
-- also provided. 
listToSig :: (Maybe a) -> [(T,Maybe a)] -> Sig a
listToSig v0 = mkSig v0 . ds_fromList 

-- | Sample a signal for its value at given instant. The signal
-- may be inactive at the given instant, in which case 'Nothing'
-- is returned. In addition, a signal is returned for further
-- sampling in non-decreasing time; it may be lossy for all times
-- strictly less than the time sampled. 
s_sample :: Sig a -> T -> (Maybe a, Sig a)
s_sample s0 tm = 
    let (x,ds) = ds_query (s_head s0) tm (s_tail s0) in
    let sf = mkSig x ds in
    (x, sf)

-- | Discrete sample of a signal: rather than finding a value at a
-- given instant, returns the next `instant of potential change`
-- before or including a given instant, if such a change exists. 
-- Otherwise will return Nothing. Either way, it also returns a 
-- signal for monotonic sampling relative to the lesser of the 
-- returned instant and the sampled time.
s_sample_d :: Sig a -> T -> (Maybe (T, Maybe a), Sig a)

-- | a signal that is never active (`Nothing` at all times)
s_empty  :: Sig a_
s_empty = mkSig Nothing ds_done

-- | a signal that is always active with a specific value c
s_pure :: c -> Sig c
s_pure c = mkSig (Just c) ds_done

-- | replace all values in a signal with a constant c, such that the
-- signal varies between Just c and Nothing. This will also filter 
-- unnecessary updates from values that are now known to be equal.
s_const :: c -> Sig a_ -> Sig c
s_const c s0 = 
    case s_head s0 of
        Nothing -> mkSig Nothing (ds_const0 c (s_tail s0))
        Just _  -> mkSig (Just c) (ds_const1 c (s_tail s0))

-- | invert the activity profile of a signal. For example, a signal
-- with value `Nothing` will have value `Just c`, and a signal with
-- an active value will have the value `Nothing`, at each instant.
s_invert_c :: c -> Sig a_ -> Sig c
s_invert_c c s0 = 
    case s_head s0 of
        Nothing -> mkSig (Just c) (ds_adjn1 ds')
        Just _  -> mkSig Nothing (ds_adjn0 ds')
    where ds' = ds_map f (s_tail s0)
          f Nothing = Just c
          f _       = Nothing

-- | Map applies a function across the active values of a signal.
-- Same as the Functor fmap.
s_fmap :: (a -> b) -> Sig a -> Sig b
s_fmap = s_full_map . fmap

-- | Full map applies a function across all values, including 
-- inactivity of the signal. 
s_full_map :: (Maybe a -> Maybe b) -> Sig a -> Sig b
s_full_map f s0 = 
    let hd' = f (s_head s0) in
    let tl' = ds_map f (s_tail s0) in
    mkSig hd' tl'

-- | zip two signals using a provided function. The resulting signal
-- is active only when both inputs are active. Works most optimally
-- if both signals are active at the same times. 
s_zip :: (a -> b -> c) -> Sig a -> Sig b -> Sig c
s_zip = s_full_zip . liftM2

-- | Full zip applies a function across periods of inactivity, too.
s_full_zip :: (Maybe a -> Maybe b -> Maybe c) -> Sig a -> Sig b -> Sig c
s_full_zip jf sa sb =
    let hda = s_head sa in
    let hdb = s_head sb in
    let tla = s_tail sa in
    let tlb = s_tail sb in
    let hdc = jf hda hdb in
    let tlc = ds_zip jf hda hdb tla tlb in 
    mkSig hdc tlc


-- | Mask one signal with the activity profile of another. That is,
-- the resulting signal is only active when both input signals are
-- active, but the value is always from the signal on the left.
--    s_mask = s_zip const (but somewhat optimized)
s_mask :: Sig a -> Sig b_ -> Sig a
s_mask sa sb = 
    case s_head sb of
        Nothing -> mkSig Nothing (tail_with ds_mask0)
        _       -> mkSig (s_head sa) (tail_with ds_mask1)
    where tail_with msk = msk (s_head sa) (s_tail sa) (s_tail sb)

-- | Merge two signals by using the left signal when it is active,
-- otherwise the right signal.
--    s_merge = s_zip_full (<|>)
-- But the implementation is a bit more optimized

    -- | compose two signals that are active at different times.
    -- Favors second input signal when it is active, otherwise 
    -- has behavior of first signal.
    --
    --   s_sample (s_merge f g) t =
    --     let (sf,f') = s_sample f t
    --         (sg,g') = s_sample g t
    --         mfg = case sg of Nothing -> sf | _ -> sg
    --     in (mfg,s_merge f' g')
    --
    s_merge  :: s a -> s a -> s a

    -- | delay a signal to occur after an offset in time. This 
    -- time-shifts the activity profile and values.
    --
    --    s_sample (s_delay dt f) t = s_sample f (t .-^ dt)
    --
    -- Undefined for negative difftime.
    s_delay  :: Diff t -> s a -> s a

    -- | switch to another signal at specified time
    --
    --   s_sample (s_future f t g) t2 =
    --      if(t2 < t)
    --        then let (sf,f') = s_sample f t in
    --             (sf, s_future f' t g)
    --        else s_sample g t
    --
    s_future :: s a -> t -> s a -> s a


    -- | test whether signal is in its `final` state; useful for GC.
    -- This is a semi-decision: it should return `False` if finality
    -- cannot be decided by the given time.
    --
    --   if (s_final f t) && (t2 > t)
    --   then s_sample f t2 = s_sample f t
    --
    s_final  :: s a -> t -> Bool
    s_final _ _ = False

-- | A signal that is always active
--
--    s_sample s_always t = (Just (), s_always)
--
s_always :: (Signal s t) => s () 
s_always = s_invert s_never

-- | Test a signal for termination from a given instant. This is a
-- simple composition of s_final with a test to see whether the 
-- final state is Nothing.
s_term :: (Signal s t) => s a -> t -> Bool
s_term s t = 
    let (x,sx) = s_sample s t in
    case x of Nothing -> s_final sx t
              _       -> False

-- | SigFun allows lifting some class of function to a signal. Type
-- `f` is typically the Haskell function type, but some signals may
-- be more restrictive about what functions may be represented (to 
-- support symbolic analysis, serialization, or embedding).
class (Signal s t) => SigFun f s t where
    -- | map a function or arrow across a signal. Signal types may
    -- constrain access to certain classes of function.
    -- 
    --    s_sample (s_fmap fn sa) t =
    --      case (s_sample sa t) of
    --        (Nothing,sa') -> (Nothing,s_fmap fn sa')
    --        (Just x,sa') -> (Just (fn x),s_fmap fn sa') 
    --
    s_fmap  :: (f a b) -> (s a) -> (s b)

-- | SigSplit divides a signal into two non-overlapping signals.
-- This effectively represents branching on runtime values.
class (Signal s t) => SigSplit s t where
    -- | split a signal such that each output is active only when 
    -- the input holds a corresponding value.
    --
    --   s_sample ((fst . s_split) f) t =
    --     let (sf,f') = s_sample f t
    --         x = case sf of (Just (Left v)) -> Just v
    --                      | _ -> Nothing
    --     in (x, (fst . s_split) f')
    -- 
    --   s_sample ((snd . s_split) f) t =
    --     let (sf,f') = s_sample f t
    --         y = case sf of (Just (Right v)) -> Just v
    --                      | _ -> Nothing
    --     in (y, (snd . s_split) f')
    --
    s_split  :: s (Either a b) -> (s a, s b)

-- | SigPeek supports anticipating the future state of a signal. Not
-- the same as s_delay with negative difftime because does not shift
-- or modify the period of activity; rather, anticipated periods of
-- inactivity are observed in-band, useful for graceful shutdown.
class (Signal s t) => SigPeek s t where
    -- | look at the future values and activity of the signal
    --
    --   s_sample ((fst . s_peek dt) f) t =
    --     let (sf,f') = s_sample f t
    --         (sf',_) = s_sample f' (t .+^ dt)
    --         x = case (sf,sf') of
    --               (Just _,Nothing) -> Just ()
    --               _ -> Nothing
    --     in (x,(fst . s_peek dt) f')
    --
    --   s_sample ((snd . s_peek dt) f) t =
    --     let (sf,f') = s_sample f t
    --         (sf',_) = s_sample f' (t .+^ dt)
    --         y = case (sf,sf') of
    --               (Just _, Just x) -> Just x
    --               _ -> Nothing
    --     in (y, (snd . s_peek dt) f')
    -- 
    -- Undefined for negative difftimes. Note that the two output
    -- signals are active at non-overlapping times, and together
    -- their activity is equal to that of the input signal.
    --
    -- Typically SigPeek must be a relatively small constant, e.g.
    -- 5 milliseconds. 
    s_peek  :: Diff t -> s a -> (s (), s a)

-- | SigSelect takes a collection of signals and generates a signal
-- of collections. The output signal is always active, though may
-- have an empty collection as its value if all inputs are inactive.
class (Signal s t) => SigSelect s t where
    -- | Produce a signal of collections.
    --
    --   s_sample (s_select sl) t = 
    --     let ss = map (flip s_sample t) sl 
    --         sl' = map snd ss
    --         xs = flip concatMap ss $ \ (mbx,_) ->
    --           case mbx of Nothing -> [] | Just x -> [x]
    --     in (Just xs, s_select sl')
    --
    s_select  :: [s a] -> s [a]
   
-- | SigDiscrete describes signals that update at a finite count of
-- instants in every continuous period, and that support developers
-- in precisely discovering those instants. This implies the signals 
-- are constant between updates. 
--
-- Warning: SigDiscrete cannot tell you that an `update` is actually
-- a `change`. For example, it is possible that a signal will report
-- a series of discrete updates of the form:
--    at time 100, update signal value to Just 3
--    at time 103, update signal value to Just 3
--    at time 107, update signal value to Just 3
-- The updates at times 103 and 107 are obviously redundant in this
-- case, but the values could have been functions or something else
-- opaque to equality tests. 
-- All signal processing must be robust to such redundant updates.
-- That is, a function observing the above series of updates must
-- have same result if updates at times 103 and 107 are deleted.
--
-- In general, this robustness is enforced in RDP by controlling
-- how signal processors are constructed, and by careful design of
-- `state` models for RDP to ensure they are insensitive to such
-- redundancies.
--
class (SigSplit s t) => SigDiscrete s t where
    -- | Find the first `update` in a bounded period.
    --
    --   > s_sample_d signal lower upper
    --
    -- This searches for the earliest update whose time t is bounded
    -- by: (lower < t) && (t <= upper). It is possible that no such
    -- update exists.
    --
    -- As with s_sample, a signal trimmed for future samples helps
    -- with GC of unnecessary history. In this case, the trimmed
    -- signal is based on either `upper` (if no samples were found)
    -- or the time of the sample (otherwise).
    s_sample_d :: s a -> t -> t -> (Maybe (t, Maybe a), s a)

-- | Transform the simplest signals to another type. Since a signal

class (Signal s t, Signal s' t) => SigShadow s s' t where
    -- | a very limited lift option, activity profiles only
    s_shadow  :: s () -> s' ()

class (SigShadow s s' t) => SigLift s s' t where
    -- | transform the signal model without losing any information.
    -- Normally one direction (e.g. discrete to continuous).
    s_lift  :: s a -> s' a

-- shadow and lift apply reflexively to all signals
instance (Signal s t) => SigShadow s s t where 
    s_shadow  = id

instance (Signal s t) => SigLift s s t where 
    s_lift    = id

-- Having too much difficulty getting 'free' instances here,
-- so I'll just provide a few useful functions for defining
-- them quickly:
--  Functor:     fmap = s_fmap -- via SigFun (->) s
--  Applicative: pure = s_pure, <*> = s_ap
--  Alternative: empty = s_empty, <|> = s_alt
s_pure  :: (Functor s, Signal s t) => c -> s c
s_ap    :: (Functor s, Signal s t) => s (a -> b) -> s a -> s b
s_empty :: (Functor s, Signal s t) => s a
s_alt   :: (Functor s, Signal s t) => s a -> s a -> s a
s_pure c   = fmap (const c) s_always
s_ap fs xs = fmap (\(f,x)->(f x)) (s_zip fs xs)
s_empty    = s_never
s_alt      = flip s_merge


--------------------------
-- PERFORMANCE CONCERNS --
--------------------------

-- | By erasing redundant updates to a signal, in carefully chosen
-- contexts, one can sometimes eliminate redundant computations and
-- stabilize the model. However, if used too much, this becomes the
-- redundant computation.
class (Signal s t) => SigAdjeqf s t where
    s_adjeqf   :: Eq a => s a -> s a




