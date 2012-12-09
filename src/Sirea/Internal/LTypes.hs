{-# LANGUAGE GADTs, TypeOperators, Rank2Types #-}

-- Types for FRP.Sirea.Link (here to avoid cyclic dependencies)
-- plus related utilities
module Sirea.Internal.LTypes 
    ( MkLnk, Lnk, LnkW(..), LnkUp(..)
    , ln_left, ln_right, ln_fst, ln_snd, ln_dead
    , ln_zero, ln_lnkup, ln_append, ln_lumap, ln_sumap
    , SigUp(..), su_signal, su_time, su_fmap, su_delay, su_apply, su_piggyback
    , SigSt(..), st_zero, st_poke, st_clear, st_sigup
    , SigM(..), sm_zero, sm_update_l, sm_sigup_l, sm_update_r, sm_sigup_r
    , sm_waiting, sm_stable, sm_emit, sm_cleanup
    , ln_forEach, ln_freeze, ln_touchAll, ln_withSigM
    ) where

import Sirea.Internal.STypes
import Sirea.Time
import Sirea.Signal
import Control.Exception (assert)
import Control.Monad (unless)
import Control.Arrow (first)
import Control.Applicative
import Data.Monoid
import Data.IORef

-- | MkLnk - construct a link in the IO monad, accepting destination 
-- capability and generating the source capability. IO is for caches
-- and any per-link resource management, but must avoid observable 
-- side-effects. (Any observable consequence must be from USING the
-- link with an active signal, not from building it.)
--
-- Dead-sink optimizations are supported as part of the construction
-- process: the `LnkDead` constructor provides some information that
-- the output is unused, which allows the MkLnk operation to control
-- accordingly how much computation to perform. 
--
-- > type MkLnk x y = Lnk y -> IO (Lnk x)
--
type MkLnk x y = Lnk y -> IO (Lnk x)

-- | A Lnk describes a complex product of LnkUp values, to 
-- support all complex signal types - S, (:&:) and (:|:). 
--
-- > data Lnk x = 
-- >     LnkDead :: Lnk x
-- >     LnkSig  :: LnkUp a -> Lnk (S p a)
-- >     LnkProd :: Lnk x -> Lnk y -> Lnk (x :&: y)
-- >     LnkSum  :: Lnk x -> Lnk y -> Lnk (x :|: y)
--
-- Lnk is a GADT type. The use of `LnkDead` can be treated as a set
-- of `ln_zero` values (`LnkUp` that does nothing), but is provided
-- to easily perform dead code elimination.
-- 
type Lnk = LnkW LnkUp

-- | LnkW is a GADT for a complex product of signals. 
data LnkW s a where
    LnkDead :: LnkW s a -- for dead code
    LnkSig  :: (s a) -> LnkW s (S p a)
    LnkProd :: (LnkW s a) -> (LnkW s b) -> LnkW s (a :&: b)
    LnkSum  :: (LnkW s a) -> (LnkW s b) -> LnkW s (a :|: b)

-- | LnkUp processes updates to a concrete signal. Complex signals
-- can be represented ultimately as a complex product of LnkUp 
-- structures. 
--
--   ln_touch - is called in initial phase of updates within a step.
--      This provides advance warning that an update is coming later
--      in the step. Affects ordering of updates and breaks cycles
--      involving shared resources. 
--
--   ln_update - updates the entire future of a signal (see SigUp).
--      In any given step, a link must be updated only once, and 
--      must be preceded by touch. Behaviors are initially activated
--      and finally deactivated by updates with appropriate signals
--      and stability.
-- 
data LnkUp a = LnkUp
    { ln_touch  :: !(IO ())
    , ln_update :: !(SigUp a -> IO ())
    }

instance Monoid (LnkUp a) where
    mempty  = ln_zero
    mappend = ln_append

-- | ln_zero is a trivial LnkUp state, like LnkDead but opaque.
ln_zero :: LnkUp a
ln_zero = LnkUp 
    { ln_touch = return ()
    , ln_update = const $ return ()
    }

-- duplicate touches and updates.
ln_append :: LnkUp a -> LnkUp a -> LnkUp a
ln_append x y =
    let touch = ln_touch x >> ln_touch y in
    let update su = ln_update x su >> ln_update y su in
    LnkUp { ln_touch = touch, ln_update = update }


-- | ln_lnkup extracts LnkUp (from LnkSig or ln_zero from LnkDead)
ln_lnkup  :: Lnk (S p a) -> (LnkUp a)
ln_lnkup (LnkSig lu) = lu
ln_lnkup _ = ln_zero

-- | extract left, right, fst, snd elements. 
ln_left   :: LnkW s (a :|: b) -> LnkW s a
ln_right  :: LnkW s (a :|: b) -> LnkW s b
ln_fst    :: LnkW s (a :&: b) -> LnkW s a
ln_snd    :: LnkW s (a :&: b) -> LnkW s b
ln_dead   :: LnkW s a -> Bool

ln_left (LnkSum a _) = a
ln_left _ = LnkDead

ln_right (LnkSum _ b) = b
ln_right _ = LnkDead

ln_fst (LnkProd a _) = a
ln_fst _ = LnkDead

ln_snd (LnkProd _ b) = b
ln_snd _ = LnkDead

ln_dead LnkDead = True
ln_dead (LnkSig _) = False
ln_dead (LnkProd a b) = ln_dead a && ln_dead b
ln_dead (LnkSum a b) = ln_dead a && ln_dead b

-- | simple link update from a signal update transformer
-- (Not all SigUp transforms are safe for RDP. Most aren't.)
ln_sumap :: (SigUp x -> SigUp y) -> LnkUp y -> LnkUp x
ln_sumap fn ln = LnkUp 
  { ln_touch = ln_touch ln -- forward touches
  , ln_update = ln_update ln . fn -- forward updates after map
  }

-- | simple transformer from LnkUp to Lnk
ln_lumap :: (LnkUp x -> LnkUp y) -> Lnk (S p x) -> Lnk (S p y)
ln_lumap _ LnkDead = LnkDead
ln_lumap fn (LnkSig l) = LnkSig (fn l)

-- | apply operation over each element.
ln_forEach :: (Applicative m, Monoid a) => (forall x . s x -> m a) -> LnkW s y -> m a
ln_forEach _ LnkDead = pure mempty
ln_forEach fn (LnkProd x y) = mappend <$> ln_forEach fn x <*> ln_forEach fn y
ln_forEach fn (LnkSum x y) = mappend <$> ln_forEach fn x <*> ln_forEach fn y
ln_forEach fn (LnkSig lu) = fn lu

{-
-- | apply a termination signal every element in a link
ln_terminate :: T -> Lnk x -> IO ()
ln_terminate t = ln_forEach (terminate t)

terminate :: T -> LnkUp x -> IO ()
terminate tm lu = ln_update lu sigTerm
    where sigTerm = SigUp { su_state = Just (s_never, tm)
                          , su_stable = Nothing }

-}

-- | freeze the current signals on a link
ln_freeze :: Lnk x -> IO ()
ln_freeze = ln_forEach freeze

freeze :: LnkUp x -> IO ()
freeze lu = ln_update lu sigFreeze
    where sigFreeze = SigUp { su_state = Nothing
                            , su_stable = Nothing }

ln_touchAll :: Lnk x -> IO ()
ln_touchAll = ln_forEach ln_touch


-- | Each signal update carries:
--    state - the new state of the signal, starting at a given time
--      which must be greater or equal to current stability. The
--      value Nothing here means that the state did not change.
--    stability - the new stability of the signal, after applying
--      the state update. A promise that all future updates happen
--      no earlier than the given instant in time, to support GC. 
--      The value Nothing here means stable forever.
-- Stability always updates. State might not update, i.e. to avoid
-- recomputing a signal when it is known it did not change.
--
-- State of the signal includes all future values, though they might
-- not be computed yet. The idea is to keep updating the future of
-- the signal slightly before it becomes the present.
data SigUp a = SigUp 
    { su_state  :: {-# UNPACK #-} !(Maybe (Sig a , T))
    , su_stable :: {-# UNPACK #-} !(Maybe T)
    }

-- | signal held by update.
su_signal :: SigUp a -> Maybe (Sig a)
su_signal = fmap fst . su_state

-- | time of actual switch (not same as stability)
su_time :: SigUp a -> Maybe T
su_time = fmap snd . su_state

-- | modify the value of a signal update
su_fmap :: (Sig a -> Sig b) -> SigUp a -> SigUp b
su_fmap fn su =
    let state' = fmap (first fn) (su_state su) in
    SigUp { su_state = state', su_stable = su_stable su }

-- | apply a signal update to a signal.
su_apply :: SigUp a -> Sig a -> Sig a
su_apply su s0 = 
    case su_state su of
      Just (sf,tu) -> s_switch s0 tu sf 
      Nothing -> s0

-- | delay all aspects of a signal update
su_delay :: DT -> SigUp a -> SigUp a
su_delay dt = if (0 == dt) then id else \ su ->
    let state' = fmap (\(s0,t) -> (s_delay dt s0, addTime t dt)) (su_state su) in
    let stable' = fmap (flip addTime dt) (su_stable su) in
    SigUp { su_state = state', su_stable = stable' }

-- | Support piggybacking. Second update overrides the first.
su_piggyback :: SigUp a -> SigUp a -> SigUp a
su_piggyback su0 su = SigUp { su_state = state', su_stable = stable' }
    where stable' = su_stable su    -- assume monotonic stability
          state' = (calcS <$> su_state su0 <*> su_state su)
                   <|> su_state su  -- su is first state update 
                   <|> su_state su0 -- su is a stability update
          calcS (s0,t0) (sf,tf) =
            if (t0 >= tf) then (sf,tf) else
            (s_switch' s0 tf sf, t0)

---------------------------------------------------------
-- SigSt represents the state of one signal.
-- This is intended for use with SigM, primarily. 
data SigSt a = SigSt
    { st_signal ::                !(Sig a)    -- signal value
    , st_stable :: {-# UNPACK #-} !(Maybe T)  -- signal stability
    , st_expect :: {-# UNPACK #-} !Bool       -- expecting an update?
    }

-- st_zero is an initial SigSt value.
st_zero :: SigSt a
st_zero = SigSt { st_signal = s_never, st_stable = Nothing, st_expect = False }

-- st_poke is called by ln_touch to indicate expected updates
st_poke :: SigSt a -> SigSt a
st_poke st = st { st_expect = True }

-- clear history up to T
st_clear :: T -> SigSt a -> SigSt a 
st_clear tt st = st { st_signal = sf }
    where sf = s_trim (st_signal st) tt

-- update the SigSt. In debug compiles, this will validate
-- that stability assumptions are respected. 
st_sigup :: SigUp a -> SigSt a -> SigSt a
st_sigup su st =
    let oldStability  = st_stable st in
    let newStability  = su_stable su in
    let tmUpdate      = fmap snd (su_state su) in
    let sigWithUpdate = su_apply su (st_signal st) in
    assert (respectsStability oldStability tmUpdate) $
    assert (monotonicStability oldStability newStability) $
    SigSt { st_signal = sigWithUpdate
          , st_stable = newStability
          , st_expect = False -- no longer expecting update
          } 

-- for some extra validation and debugging, ensure that stability
-- is non-decreasing (excepting Forever, which acts as a reset).
monotonicStability :: Maybe T -> Maybe T -> Bool
monotonicStability (Just t0) (Just tf) = (tf >= t0)
monotonicStability _ _ = True

-- validate that stability is respected by updates, i.e. that 
-- no update happens earlier than the current stability.
respectsStability  :: Maybe T -> Maybe T -> Bool
respectsStability = monotonicStability

------------------------------------------------------------------
-- SigM represents states for two signals, for zip, merge, and 
-- similar behaviors.
data SigM x y = SigM
    { sm_lsig :: {-# UNPACK #-} !(SigSt x)  -- state for left signal
    , sm_rsig :: {-# UNPACK #-} !(SigSt y)  -- state for right signal
    , sm_tmup :: {-# UNPACK #-} !(Maybe T)  -- earliest active update
    }

-- initial SigM. Note that st_expect is set True so we don't lose
-- any information (in case of multiple updates on one signal before
-- first update on other signal).
sm_zero :: SigM x y
sm_zero = SigM 
    { sm_lsig = st_poke st_zero
    , sm_rsig = st_poke st_zero
    , sm_tmup = Nothing 
    }

sm_update_l :: (SigSt x -> SigSt x) -> (SigM x y -> SigM x y)
sm_update_r :: (SigSt y -> SigSt y) -> (SigM x y -> SigM x y)
sm_update_t :: T -> SigM x y -> SigM x y

sm_update_l fn sm = sm { sm_lsig = fn (sm_lsig sm) }
sm_update_r fn sm = sm { sm_rsig = fn (sm_rsig sm) }
sm_update_t tm sm = sm { sm_tmup = tm' }
    where tm' = case sm_tmup sm of
                    Nothing -> Just tm
                    Just tx -> Just $! min tm tx

sm_sigup_t :: SigUp z -> SigM x y -> SigM x y
sm_sigup_t su =
    case su_state su of
        Nothing -> id
        Just (_,tu) -> sm_update_t tu

sm_sigup_l  :: SigUp x -> (SigM x y -> SigM x y) -- update time & left
sm_sigup_r  :: SigUp y -> (SigM x y -> SigM x y) -- update time & right
sm_sigup_l su = sm_sigup_t su . (sm_update_l . sm_sigup1) su
sm_sigup_r su = sm_sigup_t su . (sm_update_r . sm_sigup1) su

-- sm_sigup also checks that updates were appropriately touched.
sm_sigup1 :: SigUp a -> SigSt a -> SigSt a
sm_sigup1 su st = assert (st_expect st) $ st_sigup su st

-- generate a link update from a SigM.
-- This will combine two signals to generate a new signal.
-- The least stability value is used.
sm_emit :: (Sig a -> Sig b -> Sig c) -> SigM a b -> SigUp c
sm_emit fn sm = 
    let tmStable = sm_stable sm in
    case sm_tmup sm of
        Nothing -> 
            SigUp { su_state = Nothing, su_stable = tmStable }
        Just tt -> 
            let sa = st_signal $ sm_lsig sm in
            let sb = st_signal $ sm_rsig sm in
            let sc = s_trim (fn sa sb) tt in
            sc `seq` 
            SigUp { su_state = Just (sc,tt), su_stable = tmStable }

-- sm_waiting is true if either left or right has been touched 
-- (but not yet updated)
sm_waiting :: SigM x y -> Bool
sm_waiting sm = st_expect (sm_lsig sm) || st_expect (sm_rsig sm)

-- stability of SigM is lesser of two stability values.
sm_stable :: SigM x y -> Maybe T
sm_stable sm = mb lt rt
    where lt = st_stable (sm_lsig sm)
          rt = st_stable (sm_rsig sm)
          mb Nothing r = r
          mb l Nothing = l
          mb (Just l) (Just r) = Just $! min l r

-- cleanup SigM after sending a message, based on given stability.
-- in general the stability will be the same as the signal for 
-- sm_emit.
sm_cleanup :: Maybe T -> SigM x y -> SigM x y
sm_cleanup Nothing _ = sm_zero
sm_cleanup (Just tt) sm =
    let sx' = st_clear tt (sm_lsig sm) in
    let sy' = st_clear tt (sm_rsig sm) in
    sx' `seq` sy' `seq` SigM 
        { sm_lsig = sx'
        , sm_rsig = sy'
        , sm_tmup = Nothing
        }

-- | SigM is a utility type for combining two input signals. This
-- function sets up a SigM based composition and automatically 
-- handles all the propagation and cleanup requirements.
--
--     ln_withSigM onTouch onEmit
--
ln_withSigM :: IO () -> (SigM x y -> IO ()) -> IO (LnkUp x, LnkUp y)
ln_withSigM onTouch onEmit = 
    newIORef sm_zero >>= \ rfSigM ->
    return $ ln_withSigM' rfSigM onTouch onEmit

ln_withSigM' :: IORef (SigM x y) -> IO () -> (SigM x y -> IO ()) 
             -> (LnkUp x, LnkUp y)
ln_withSigM' rfSigM onTouch onEmit = (lux,luy)
    where pokeX =   readIORef rfSigM >>= \ sm ->
                    writeIORef rfSigM (sm_update_l st_poke sm) >>
                    unless (sm_waiting sm) onTouch
          pokeY =   readIORef rfSigM >>= \ sm ->
                    writeIORef rfSigM (sm_update_r st_poke sm) >>
                    unless (sm_waiting sm) onTouch
          emit  =   readIORef rfSigM >>= \ sm ->
                    unless (sm_waiting sm) $
                        let sm' = sm_cleanup (sm_stable sm) sm in
                        sm' `seq` 
                        writeIORef rfSigM sm' >>
                        onEmit sm
          updX su = modifyIORef rfSigM (sm_sigup_l su) >> emit
          updY su = modifyIORef rfSigM (sm_sigup_r su) >> emit
          lux =     LnkUp { ln_touch = pokeX, ln_update = updX }
          luy =     LnkUp { ln_touch = pokeY, ln_update = updY }




