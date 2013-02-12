{-# LANGUAGE GADTs, TypeOperators, Rank2Types, DeriveDataTypeable #-}

-- Types for FRP.Sirea.Link (here to avoid cyclic dependencies)
-- plus related utilities
module Sirea.Internal.LTypes 
    ( MkLnk, Lnk, LnkW(..), LnkUp(..)
    , StableT(..), isDoneT, fromStableT, maybeStableT
    , ln_left, ln_right, ln_fst, ln_snd, ln_dead
    , ln_zero, ln_lnkup, ln_append
    , ln_lumap, ln_sfmap
    , SigSt(..), st_zero, st_poke, st_clear, st_update, st_idle
    , monotonicStability, respectsStability
    , SigM(..), sm_zero
    , sm_update_l, sm_poke_l, sm_idle_l
    , sm_update_r, sm_poke_r, sm_idle_r 
    , sm_waiting, sm_stable, sm_emit, sm_cleanup
    , ln_forEach, ln_freeze, ln_touchAll, ln_withSigM
    , adjStableTime
    ) where

import Sirea.Internal.STypes
import Sirea.Time
import Sirea.Signal
import Sirea.Internal.Tuning (tAncient)
import Control.Exception (assert)
import Control.Monad (unless)
import Control.Applicative
import Data.Monoid
import Data.IORef
import Data.Typeable

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

-- | LnkUp processes updates to a concrete signal. Complex signal
-- processors are represented as a product of LnkUp structures via
-- the `LnkW` GADT. 
--
-- When updating a signal, ln_touch should be called in the initial
-- phase, and either ln_update or ln_idle in the second phase. Only
-- one of update or idle may be called in a step. (The concepts of
-- 'step' and 'phase' relate to partition runStepper operations.)
--
--   ln_touch: indicates an update is coming; helps with the update
--      ordering problem within a partition. Behaviors that are not
--      touched in initial phase must not be updated this step.
--
--   ln_update: update the signal's future and stability. The signal
--      is updated at a particular instant, T, time to switch to the
--      new signal. The stability value is a promise that no future
--      updates will apply earlier than the specified value (with a
--      few special exceptions for implementing DemandAggr, etc.)
--
--   ln_idle: update signal's stability only, indicating there was
--      no significant change in value. 
--
data LnkUp a = LnkUp
    { ln_touch  :: !(IO ())
    , ln_update :: !(StableT -> T -> Sig a -> IO ())
    , ln_idle   :: !(StableT -> IO ())
    --, ln_cycle  :: !((Set Int) -> IO ())
    } deriving (Typeable)

instance Monoid (LnkUp a) where
    mempty  = ln_zero
    mappend = ln_append

--  (under consideration):
--
--   ln_cycle: supports a test to determine whether a given resource
--      is part of a partition-local feedback cycle. The participant
--      adds its identifier, and if the set comes back around with
--      the participant's own identifier, the participant must break
--      the cycle, i.e. by delaying update to a future round.
--
--      IF THIS WORKS:
--    
--      While computing cycles does introduce small overhead, it can
--      improve update order, reduce rework, eliminate extra sends,
--      better compose resources, ultimately save a lot.
--
--      I think the savings will almost always be greater than the 
--      overhead. I.e. if there is no chain of operations, the cost
--      should nearly be zero. 
--
--      WILL IT WORK?
--
--      Not sure. Mostly, BDynamic seems to cause issue, since I
--      don't always know who will receive behavior updates internal
--      to new behaviors. Maybe I could make BDynamic work by delay
--      of touch/update for new BDynamic values by a step, i.e. such
--      that they always respect 'touch first phase, update second'.
--
--      Maybe try to fix BDynamic first, so I don't need to treat
--      it as a special case. (But to fix BDynamic, I'll need access
--      to the scheduling mechanisms. Does that mean BDynamic will
--      have only a BCX implementation?)
--
--      Another potential issue is the MonitorDist, i.e. that we 
--      won't always find monitors prior to the cycle tests. Maybe
--      this can be addressed by keeping a record of cycle IDs 
--      that reach a MonitorDist, then propagating these to new
--      observers when they attach.
--
--      If we're cut, we'll delay the real update to the next round
--      but maybe perform touch and stability update this round.
--
--      Notes: This could be a *conservative* estimate, e.g. using a
--      hash of an ID or even a stableName. Or per partition value?
--
-- TODO: make this (global) change after finishing the current one.



-- | StableT describes a stability value for a signal. This is a
-- concrete time, or 'DoneT' if fully finished. DoneT means the
-- signal will not update again, except in a few special cases
-- (notably, DemandAggr) where they might indicate that no more
-- updates are known to be on their way. 
--
data StableT = StableT {-# UNPACK #-} !T
             | DoneT
             deriving (Show,Eq,Typeable)

isDoneT :: StableT -> Bool
isDoneT DoneT = True
isDoneT _ = False

fromStableT :: T -> StableT -> T
fromStableT t DoneT = t
fromStableT _ (StableT t) = t

maybeStableT :: a -> (T -> a) -> StableT -> a
maybeStableT a _ DoneT = a
maybeStableT _ fn (StableT t) = fn t

-- The main reason for StableT is right here. Dealing with Maybe's
-- default Ord instance was painful.
instance Ord StableT where
    compare (StableT a) (StableT b) = compare a b
    compare DoneT DoneT = EQ
    compare DoneT _ = GT -- Done means forever
    compare _ DoneT = LT
    (<) (StableT a) (StableT b) = (a < b)
    (<) DoneT _ = False
    (<) _ _ = True
    (>=) a b = not (a < b)
    (>) = flip (<)
    (<=) = flip (>=)

-- | manipulate stability by a small increment or decrement
adjStableTime :: (T -> T) -> StableT -> StableT
adjStableTime _ DoneT = DoneT
adjStableTime fn (StableT tm) = StableT (fn tm)


-- | ln_zero is a trivial LnkUp state, like LnkDead but opaque.
ln_zero :: LnkUp a
ln_zero = LnkUp touch update idle where
    touch = return ()
    update _ _ _ = return ()
    idle _ = return ()

-- duplicate touches and updates.
ln_append :: LnkUp a -> LnkUp a -> LnkUp a
ln_append x y = LnkUp touch update idle where
    touch = ln_touch x >> ln_touch y
    idle t = ln_idle x t >> ln_idle y t
    update tS tU sig = ln_update x tS tU sig >> ln_update y tS tU sig

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
--
-- NOTE: Most Signal functions aren't safe for RDP, where 'safe'
-- means protecting all of RDP's invariants, especially duration 
-- coupling (which supports composable resource management).
--
ln_sfmap :: (Sig x -> Sig y) -> LnkUp y -> LnkUp x
ln_sfmap fn ln = LnkUp touch update idle where
    touch = ln_touch ln
    idle = ln_idle ln
    update tS tU = ln_update ln tS tU . fn

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

-- | freeze the current signals on a link
ln_freeze :: Lnk x -> IO ()
ln_freeze = ln_forEach freeze

freeze :: LnkUp x -> IO ()
freeze lu = ln_idle lu DoneT -- idle forever...

ln_touchAll :: Lnk x -> IO ()
ln_touchAll = ln_forEach ln_touch

---------------------------------------------------------
-- SigSt represents the state of one signal.
-- This is intended for use with SigM, primarily. 
data SigSt a = SigSt
    { st_signal :: !(Sig a)    -- signal value
    , st_stable :: !StableT    -- signal stability
    , st_expect :: !Bool       -- expecting an update?
    } deriving (Typeable)

-- st_zero is an initial SigSt value. 
--
-- The intial stability is set to tAncient to avoid confusing
-- lower and upper bounds. This will also prevent data-loss for
-- SigM and other elements with static SigSt inputs. 
st_zero :: SigSt a
st_zero = SigSt s_never (StableT tAncient) False

-- st_poke is called by ln_touch to indicate expected updates
st_poke :: SigSt a -> SigSt a
st_poke st = st { st_expect = True }

-- clear history based on stability.
-- asserts that we don't clear beyond stability.
st_clear :: StableT -> SigSt a -> SigSt a 
st_clear tsClr st =
    assert (st_stable st >= tsClr) $
    let s' = maybeStableT s_never (s_trim (st_signal st)) tsClr in
    st { st_signal = s' }

st_update :: StableT -> T -> Sig a -> SigSt a -> SigSt a
st_update tS tU su st =
    assert (respectsStability (st_stable st) tU) $
    assert (monotonicStability (st_stable st) tS) $
    let sf = s_switch (st_signal st) tU su in
    SigSt sf tS False

st_idle :: StableT -> SigSt a -> SigSt a
st_idle tStable st =
    assert (monotonicStability (st_stable st) tStable) $
    let sf = st_signal st in
    SigSt sf tStable False


-- is stability non-decreasing?
monotonicStability :: StableT -> StableT -> Bool
monotonicStability tOld tNew = (tNew >= tOld)

-- does update occur in stability order?
respectsStability :: StableT -> T -> Bool
respectsStability DoneT _ = False
respectsStability (StableT tS) tU = (tU >= tS)

------------------------------------------------------------------
-- SigM represents states for two signals, for zip, merge, and 
-- similar behaviors.
data SigM x y = SigM
    { sm_lsig :: {-# UNPACK #-} !(SigSt x)  -- state for left signal
    , sm_rsig :: {-# UNPACK #-} !(SigSt y)  -- state for right signal
    , sm_tmup :: !(Maybe T)  -- earliest update (if any)
    } deriving (Typeable)

-- initial SigM. 
sm_zero :: SigM x y
sm_zero = SigM st_zero st_zero Nothing


sm_update_l :: StableT -> T -> Sig x -> SigM x y -> SigM x y
sm_poke_l :: SigM x y -> SigM x y
sm_idle_l :: StableT -> SigM x y -> SigM x y
sm_expect_l :: SigM x y -> Bool

sm_update_r :: StableT -> T -> Sig y -> SigM x y -> SigM x y
sm_poke_r :: SigM x y -> SigM x y
sm_idle_r :: StableT -> SigM x y -> SigM x y
sm_expect_r :: SigM x y -> Bool

sm_expect_l = st_expect . sm_lsig
sm_update_l tS tU sig sm = sm { sm_lsig = lsig, sm_tmup = tmup } where
    lsig = assert (sm_expect_l sm) $ st_update tS tU sig (sm_lsig sm)
    tmup = Just $! maybe tU (min tU) (sm_tmup sm)
sm_poke_l sm = sm { sm_lsig = lsig } where
    lsig = st_poke (sm_lsig sm)
sm_idle_l tS sm = sm { sm_lsig = lsig } where
    lsig = assert (sm_expect_l sm) $ st_idle tS (sm_lsig sm)

sm_expect_r = st_expect . sm_rsig
sm_update_r tS tU sig sm = sm { sm_rsig = rsig, sm_tmup = tmup } where
    rsig = assert (sm_expect_r sm) $ st_update tS tU sig (sm_rsig sm)
    tmup = Just $! maybe tU (min tU) (sm_tmup sm)
sm_poke_r sm = sm { sm_rsig = rsig } where
    rsig = st_poke (sm_rsig sm)
sm_idle_r tS sm = sm { sm_rsig = rsig } where
    rsig = assert (sm_expect_r sm) $ st_idle tS (sm_rsig sm)

sm_waiting :: SigM x y -> Bool
sm_waiting sm = waitL || waitR where
    waitL = (st_expect . sm_lsig) sm
    waitR = (st_expect . sm_rsig) sm

sm_stable :: SigM x y -> StableT
sm_stable sm = min tL tR where
    tL = (st_stable . sm_lsig) sm
    tR = (st_stable . sm_rsig) sm

-- process link updates based on a SigM combiner function
sm_emit :: (Sig a -> Sig b -> Sig c) -> LnkUp c -> SigM a b -> IO ()
sm_emit fn lu sm = maybe idle update (sm_tmup sm) where
    idle = ln_idle lu (sm_stable sm)
    update tUp = 
        let sa = s_trim (st_signal (sm_lsig sm)) tUp in
        let sb = s_trim (st_signal (sm_rsig sm)) tUp in
        let sc = fn sa sb in
        ln_update lu (sm_stable sm) tUp sc

-- cleanup based on an external stability value.
sm_cleanup :: StableT -> SigM x y -> SigM x y
sm_cleanup tCut sm = SigM lsig rsig Nothing where
    lsig = st_clear tCut (sm_lsig sm)
    rsig = st_clear tCut (sm_rsig sm)

-- | SigM is a utility type for combining two input signals. (Other
-- than DemandAggr, most RDP signal processing involves one or two
-- signals.) This wraps update operations with a signal handler for
-- updates on left and right branches. 
--
--     ln_withSigM onTouch onEmit
--
ln_withSigM :: IO () -> (SigM x y -> IO ()) -> IO (LnkUp x, LnkUp y)
ln_withSigM onTouch onEmit = 
    newIORef sm_zero >>= \ rfSigM ->
    return $ ln_withSigM' rfSigM onTouch onEmit

ln_withSigM' :: IORef (SigM x y) -> IO () -> (SigM x y -> IO ()) 
             -> (LnkUp x, LnkUp y)
ln_withSigM' rf onTouch onEmit = (lux,luy)
    where pokeX = readIORef rf >>= \ sm ->
                  writeIORef rf (sm_poke_l sm) >>
                  unless (sm_waiting sm) onTouch
          idleX tS = modifyIORef rf (sm_idle_l tS) >> emit
          updX tS tU sX = modifyIORef rf (sm_update_l tS tU sX) >> emit
          pokeY = readIORef rf >>= \ sm ->
                  writeIORef rf (sm_poke_r sm) >>
                  unless (sm_waiting sm) onTouch
          idleY tS = modifyIORef rf (sm_idle_r tS) >> emit
          updY tS tU sY = modifyIORef rf (sm_update_r tS tU sY) >> emit
          emit =  readIORef rf >>= \ sm ->
                  unless (sm_waiting sm) $
                    let sm' = sm_cleanup (sm_stable sm) sm in
                    sm' `seq` writeIORef rf sm' >>
                    onEmit sm
          lux = LnkUp pokeX updX idleX
          luy = LnkUp pokeY updY idleY


