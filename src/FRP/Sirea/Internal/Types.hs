-- {-# LANGUAGE  #-}

-- This module is a dumping ground for types that aren't intended
-- for export yet should be available to multiple modules within
-- Sirea. I might organize it eventually. 
module FRP.Sirea.Internal.Types 
    ( SigUp(..), su_signal, su_time, su_fmap, su_delay, su_apply
    , SigSt(..), st_zero, st_poke, st_clear, st_sigup
    , SigM(..), sm_zero, sm_update_l, sm_sigup_l, sm_update_r, sm_sigup_r
    , sm_waiting, sm_stable, sm_emit, sm_cleanup
    ) where

import FRP.Sirea.Time
import FRP.Sirea.Signal
import Control.Exception (assert)

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
    { su_state ::  !(Maybe (Sig a , T))
    , su_stable :: !(Maybe T)
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
    let state' = fmap (\(s0,t) -> (fn s0, t)) (su_state su) in
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

---------------------------------------------------------
-- SigSt represents the state of one signal.
-- This is intended for use with SigM, primarily. 
data SigSt a = SigSt
    { st_signal :: !(Sig a)    -- signal value
    , st_stable :: !(Maybe T)  -- signal stability
    , st_expect :: !Bool       -- recent `touch`
    }

st_stabilize :: Maybe T -> SigSt a -> SigSt a
st_stabilize tf st = 
    assert (monotonicStability (st_stable st) tf) $
    st { st_stable = tf, st_expect = False }

-- st_zero is an initial SigSt value.
st_zero :: SigSt a
st_zero = SigSt { st_signal = s_never, st_stable = Nothing, st_expect = False }

st_poke :: SigSt a -> SigSt a
st_poke st = st { st_expect = True }

-- clear history up to T
st_clear :: T -> SigSt a -> SigSt a 
st_clear tt st = st { st_signal = sf }
    where sf = s_trim (st_signal st) tt

st_sigup :: SigUp a -> SigSt a -> SigSt a
st_sigup su st =
    let tm = su_stable su in
    let sf = su_apply su (st_signal st) in
    assert (monotonicStability (st_stable st) tm) $
    SigSt { st_signal = sf, st_stable = tm, st_expect = False } 

-- for some extra validation and debugging, ensure that stability
-- is non-decreasing (excepting Forever, which acts as a reset).
monotonicStability :: Maybe T -> Maybe T -> Bool
monotonicStability (Just t0) (Just tf) = (tf >= t0)
monotonicStability _ _ = True

------------------------------------------------------------------
-- SigM represents states for two signals, for zip, merge, and 
-- similar behaviors.
data SigM x y = SigM
    { sm_lsig :: !(SigSt x)  -- state for left signal
    , sm_rsig :: !(SigSt y)  -- state for right signal
    , sm_tmup :: !(Maybe T)  -- earliest active update
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
sm_sigup_l su = sm_sigup_t su . (sm_update_l . st_sigup) su
sm_sigup_r su = sm_sigup_t su . (sm_update_r . st_sigup) su

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
-- note: not really clear on whether I should 
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


