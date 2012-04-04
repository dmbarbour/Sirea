

module FRP.Sirea.Internal.Types where

import FRP.Sirea.Time
import FRP.Sirea.Signal
import FRP.Sirea.Link(SigUp(..))

---------------------------------------------------------

-- | SigSt represents the state of one signal.
-- This is intended for use with SigM, primarily. 
data SigSt a = SigSt
    { st_signal :: !(Sig a)    -- signal value
    , st_stable :: !(Maybe T)  -- signal stability
    , st_expect :: !Bool       -- recent `touch`
    }

st_sigup :: SigUp a -> SigSt a -> SigSt a
st_sigup su s0 = 
    assert (monotonicStability t0 tf) $
    SigSt { st_signal = sf, st_stable = tf, st_expect = False }
    where t0 = st_stable s0
          tf = su_stable su
          s0 = st_signal s0
          sf = case su_state su of
                Nothing -> s0
                Just (su',tu') -> s_switch s0 tu' su'

-- st_zero is an initial SigSt value.
st_zero :: SigSt a
st_zero = SigSt { st_signal = empty, st_stable = Nothing, st_expect = False }

st_poke :: SigSt a -> SigSt a
st_poke st = st { st_expect = True }

-- clear history up to T
st_clear :: T -> SigSt a -> SigSt a 
st_clear tt st = sf `seq` st { st_signal = sf }
    where (_,sf) = s_sample (st_signal st) tt

-- for some extra validation and debugging, ensure that stability
-- is non-decreasing (excepting Forever, which acts as a reset).
monotonicStability :: Maybe T -> Maybe T -> Bool
monotonicStability (Just t0) (Just tf) = (tf >= t0)
monotonicStability _ _ = True

-- | SigM represents states for two signals, for zip, merge, and 
-- similar mechanisms.
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

sm_sigup_t  :: SigUp xy -> SigM x y -> SigM x y  -- update time
sm_sigup_l  :: SigUp x -> (SigM x y -> SigM x y) -- update time & left
sm_sigup_r  :: SigUp y -> (SigM x y -> SigM x y) -- update time & right

sm_sigup_e  :: Either (SigUp x) (SigUp y) -> (SigM x y -> SigM x y)

sm_update_l fn sm = sm { sm_lsig = fn (sm_lsig sm) }
sm_update_r fn sm = sm { sm_rsig = fn (sm_rsig sm) }
sm_update_t tm sm = sm { sm_tmup = tm' }
    where tm' = case sm_tmup sm of
                    Nothing -> Just tm
                    Just tx -> Just $! min tm tx

sm_sigup_t = maybe id sm_update_t . su_time
sm_sigup_l su = sm_sigup_t su . sm_update_l (st_sigup su)
sm_sigup_r su = sm_sigup_t su . sm_update_r (st_sigup su)

sm_sigup_e = either sm_sigup_l sm_sigup_r

-- sm_waiting is true if either left or right has been touched 
-- (but not yet updated)
sm_waiting :: SigM x y -> Bool
sm_waiting sm = st_expect (sm_lsig sm) || st_expect (sm_rsig sm)

-- stability of SigM is lesser of two stability values.
-- note: not really clear on whether I should 
sm_stable :: SigM x y -> T
sm_stable sm = mb lt rt
    where lt = st_stable (sm_lsig sm)
          rt = st_stable (sm_rsig sm)
          mb Nothing r = r
          mb l Nothing = l
          mb (Just l) (Just r) = Just $! min l r

-- generate a link update from a SigM.
-- This will combine two signals to generate a new signal.
-- The least stability value is used.
sm_emit :: (Sig a -> Sig b -> Sig c) -> SigM a b -> SigUp z
sm_emit fn sm = 
    case sm_tmup sm of
        Nothing -> 
            SigUp { su_state = Nothing, su_stable = sm_stable sm }
        Just tt -> 
            let sa = st_signal $ sm_lsig sm in
            let sb = st_signal $ sm_rsig sm in
            let sc = s_trim tt $ fn sa sb in
            sc `seq` 
            SigUp { su_state = Just (sc,tt), su_stable = sm_stable sm }

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

