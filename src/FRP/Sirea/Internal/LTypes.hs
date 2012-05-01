{-# LANGUAGE GADTs, TypeOperators #-}

-- Types for FRP.Sirea.Link (here to avoid cyclic dependencies)
-- plus related utilities
module FRP.Sirea.Internal.LTypes 
    ( MkLnk(..), Lnk, LnkW(..), LnkUp(..)
    , ln_left, ln_right, ln_fst, ln_snd, ln_dead
    , ln_zero, ln_lnkup, ln_lumap, ln_sumap
    , SigUp(..), su_signal, su_time, su_fmap, su_delay, su_apply
    , SigSt(..), st_zero, st_poke, st_clear, st_sigup
    , SigM(..), sm_zero, sm_update_l, sm_sigup_l, sm_update_r, sm_sigup_r
    , sm_waiting, sm_stable, sm_emit, sm_cleanup
    , ln_withSigM
    ) where

import FRP.Sirea.Internal.STypes
import FRP.Sirea.Time
import FRP.Sirea.Signal
import Control.Exception (assert)
import Control.Monad (unless)
import Data.IORef

-- | MkLnk - constructors and metadata for including a new behavior
-- primitive in Sirea. 
--
-- The primary operation is ln_build, which constructs a link in the
-- IO monad, accepting the response capability and generating the
-- demand capability. IO is for constructing intermediate caches and
-- any preparatory hooks to external resources, but should not have
-- observable side-effects (i.e. wait for the signal to activate).
--
-- Dead-code optimizations are handled as part of ln_build: if the
-- response target is LnkDead, one might return LnkDead for demand
-- capability. This isn't necessary, though - an effectful behavior
-- will accept a link even if it doesn't provide any meaningful
-- output.
--
-- A little extra metadata is supported for optimizations:
--
--   tsen - time sensitive: if true, prevents delay aggregation and
--     forces aggregated delay to apply prior to reaching link. 
--   peek - how much the link might peek into the future to. May
--     influence choked updates and other optimizations.
--
data MkLnk x y = MkLnk 
    { ln_build  :: !(Lnk y -> IO (Lnk x))
    , ln_tsen   :: !Bool 
    , ln_peek   :: !DT
    }

-- | A Lnk describes a complex product of LnkUp values, to 
-- support all complex signal types - S, (:&:) and (:|:). 
--   type Lnk = LnkW LnkUp
type Lnk = LnkW LnkUp

-- | LnkW is a GADT for a complex product of signals. 
data LnkW s a where
    LnkDead :: LnkW s a -- for dead code
    LnkSig  :: !(s a) -> LnkW s (S p a)
    LnkProd :: !(LnkW s a) -> !(LnkW s b) -> LnkW s (a :&: b)
    LnkSum  :: !(LnkW s a) -> !(LnkW s b) -> LnkW s (a :|: b)

-- | LnkUp processes updates to a concrete signal. Complex signals
-- can be represented ultimately as a complex product of LnkUp 
-- structures. 
--
--   ln_touch - call this if an update is guaranteed in the near
--      future but not immediately. Allows later stages in the pipe
--      to wait for the update.
--   ln_update - updates the entire future of a signal (see SigUp).
--      Note that shutdown is also modeled as an update (using the 
--      signal s_never).
-- 
-- Dead code is better represented by LnkDead than by trivial LnkUp.
data LnkUp a = LnkUp
    { ln_touch  :: !(IO ())
    , ln_update :: !(SigUp a -> IO ())
    }

-- | ln_zero is a trivial LnkUp state, similar to LnkDead but hides
-- that the input is dropped.
ln_zero :: LnkUp a
ln_zero = LnkUp 
    { ln_touch = return ()
    , ln_update = const $ return ()
    }

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




