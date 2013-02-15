{-# LANGUAGE GADTs, TypeOperators, Rank2Types #-}

-- | This module provides the concrete datatypes and organization of
-- Sirea's implementation. Specific behavior logic is in B0Impl. 
module Sirea.Internal.LTypes 
    ( LnkW(..), Lnk, LnkUp, LCC, LCaps
    , StableT(..), isDoneT, fromStableT, maybeStableT
    , ln_left, ln_right, ln_fst, ln_snd, ln_dead
    , ln_zero, ln_lnkup, ln_append
    , ln_lumap, ln_sfmap
    , lc_
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
import Data.Unique (Unique)
import Data.Set (Set)

-- | LnkW is a GADT for a complex product of signals. 
-- Support complex signal types: S, V, (:&:) and (:|:).
data LnkW s a where
    LnkDead :: LnkW s a -- for dead code
    LnkSig  :: (s a) -> LnkW s (S p a)
    LnkProd :: (LnkW s a) -> (LnkW s b) -> LnkW s (a :&: b)
    LnkSum  :: (LnkW s a) -> (LnkW s b) -> LnkW s (a :|: b)

-- | A Lnk describes a complex product of LnkUp values. 
--
-- > data Lnk m x = 
-- >     LnkDead :: Lnk m x
-- >     LnkSig  :: LnkUp m a -> Lnk m (S p a)
-- >     LnkProd :: Lnk m x -> Lnk m y -> Lnk m (x :&: y)
-- >     LnkSum  :: Lnk m x -> Lnk m y -> Lnk m (x :|: y)
--
-- Lnk is a GADT type. The use of `LnkDead` can be treated as a set
-- of `ln_zero` values (`LnkUp` that does nothing), but is provided
-- to easily perform dead code elimination.
-- 
type Lnk m = LnkW (LnkUp m)

-- | LnkUp processes updates to a concrete signal. Complex signals
-- are addressed via Lnk or LnkW.
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
--   ln_cycle: recognize cycles; if one's own unique token is in the
--      set, it must have arrived by cycle. If not, add one's token,
--      then pass it on. Where cycles are detected, they can be cut
--      by delaying updates to a future step. (Developers can dampen
--      cycles by use of bfchoke.) Most links just pass this on.
--
--      Only IO resources have cycles, so they can access newUnique.
--      Only cycles within a partition, where ln_touch is used, need
--      to be broken this way.
--
data LnkUp m a = LnkUp
    { ln_touch  :: !(m ())
    , ln_update :: !(StableT -> T -> Sig a -> m ())
    , ln_idle   :: !(StableT -> m ())
    , ln_cycle  :: !((Set Unique) -> m ())
    }

instance Monoid (LnkUp a) where
    mempty  = ln_zero
    mappend = ln_append

newtype LCX m x = LCX { getLC :: LC m }
data LC m =
    { lc_cc      :: !(CC m)
    , lc_dtCurr  :: {-# UNPACK #-} !DT
    , lc_dtGoal  :: {-# UNPACK #-} !DT
    }
type LCaps m x = LnkW (LCX m) x

lc_map :: (LC m -> LC m) -> LCX m x -> LCX m y
lc_map fn = LCX . fn . getLC

lc_fwd :: LCX m x -> LCX m y
lc_fwd = LCX . getLC


-- | StableT describes a stability value for a signal. This is a
-- concrete time, or 'DoneT' if fully finished. DoneT means the
-- signal will not update again, except in a few special cases
-- (notably, DemandAggr) where they might indicate that no more
-- updates are known to be on their way. 
--
data StableT = StableT {-# UNPACK #-} !T
             | DoneT
             deriving (Show,Eq)

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
ln_append x y = LnkUp touch update idle cycle where
    touch = ln_touch x >> ln_touch y
    idle t = ln_idle x t >> ln_idle y t
    update tS tU sig = ln_update x tS tU sig >> ln_update y tS tU sig
    cycle cs = ln_cycle x cs >> ln_cycle y cs

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
ln_sfmap fn ln = LnkUp touch update idle cycle where
    touch = ln_touch ln
    idle = ln_idle ln
    update tS tU = ln_update ln tS tU . fn
    cycle = ln_cycle ln

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
    }

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
    }

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

{-

---------------------------------------------------------
-- A simple model for time-shifts. We have a current delay and a
-- goal delay. A time-shift can move either or both. Differences
-- in current delay can be turned into a final behavior - i.e. 
-- we compare ldt_curr before and after, and delay accordingly. 
--
-- TR does not cause time-shifts, but controls how timing info is
-- preserved across MkLnk behaviors. Only a few special behaviors,
-- such as bdisjoin, need other than tr_unit, id, or tr_fwd.
type TR x y = LnkD LDT x -> LnkD LDT y
type TS x = TR x x
data LDT = LDT 
    { ldt_curr :: !DT   -- actual delay from start of behavior
    , ldt_goal :: !DT   -- aggregated but unapplied logical delay
    , ldt_live :: !Bool -- is this a live branch (not binl or binr?)
    }

ldt_maxGoal, ldt_minGoal, ldt_maxCurr, ldt_minCurr :: LnkD LDT x -> DT
ldt_maxGoal = lnd_aggr max . lnd_fmap ldt_goal
ldt_minGoal = lnd_aggr min . lnd_fmap ldt_goal
ldt_maxCurr = lnd_aggr max . lnd_fmap ldt_curr
ldt_minCurr = lnd_aggr min . lnd_fmap ldt_curr

-- ldt_anyLive returns true if ANY inputs are alive.
ldt_anyLive :: LnkD LDT x -> Bool
ldt_anyLive = lnd_aggr (||) . lnd_fmap ldt_live

-- simple validation on LnkD LDT structure
-- not very efficient, but okay for assert
ldt_valid :: LnkD LDT x -> Bool
ldt_valid (LnkDUnit ldt) = (ldt_goal ldt >= ldt_curr ldt)
ldt_valid (LnkDSum x y) = ldt_valid x && ldt_valid y
ldt_valid (LnkDProd x y) = ldt_valid x && ldt_valid y && 
    (ldt_anyLive x == ldt_anyLive y)

-- tr_unit validates an assumption that all inputs have uniform
-- timing properties. It is the normal timing translator for MkLnk.
tr_unit :: TR x y
tr_unit x =
    assert (ldt_valid x) $
    assert (ldt_maxGoal x == ldt_minGoal x) $
    assert (ldt_maxCurr x == ldt_minCurr x) $
    LnkDUnit $ LDT 
        { ldt_curr = ldt_maxCurr x
        , ldt_goal = ldt_maxGoal x
        , ldt_live = ldt_anyLive x
        }

tr_fwd :: TR (S p1 x1) (S p2 x2)
tr_fwd = LnkDUnit . lnd_sig

tr_dead :: LnkD LDT x -> LnkD LDT y
tr_dead x = LnkDUnit ldtDead
    where ldtDead = LDT { ldt_curr = ldt_maxCurr x
                        , ldt_goal = ldt_maxGoal x
                        , ldt_live = False 
                        }


----------------------------------------------------------
-- LnkD is a more generic version of LnkW for metadata.
-- It allows a single value to represent a group. Usefully
-- it can be propagated even if the type is unknown.
data LnkD d x where
    LnkDUnit :: d -> LnkD d x
    LnkDProd :: (LnkD d x) -> (LnkD d y) -> LnkD d (x :&: y)
    LnkDSum  :: (LnkD d x) -> (LnkD d y) -> LnkD d (x :|: y)

lnd_fst :: LnkD d (x :&: y) -> LnkD d x
lnd_snd :: LnkD d (x :&: y) -> LnkD d y
lnd_left :: LnkD d (x :|: y) -> LnkD d x
lnd_right :: LnkD d (x :|: y) -> LnkD d y
lnd_sig  :: LnkD d (S p x) -> d

lnd_fst (LnkDUnit d) = LnkDUnit d
lnd_fst (LnkDProd x _) = x
lnd_snd (LnkDUnit d) = LnkDUnit d
lnd_snd (LnkDProd _ y) = y
lnd_left (LnkDUnit d) = LnkDUnit d
lnd_left (LnkDSum x _) = x
lnd_right (LnkDUnit d) = LnkDUnit d
lnd_right (LnkDSum _ y) = y 
lnd_sig (LnkDUnit d) = d

-- map a simple operation across all elements independently. 
lnd_fmap :: (a -> b) -> LnkD a x -> LnkD b x
lnd_fmap fn (LnkDUnit d) = LnkDUnit (fn d)
lnd_fmap fn (LnkDProd l r) = LnkDProd (lnd_fmap fn l) (lnd_fmap fn r)
lnd_fmap fn (LnkDSum l r) = LnkDSum (lnd_fmap fn l) (lnd_fmap fn r)

-- aggregate a value. Note that the aggregation function should be
-- idempotent and commutative. There aren't many applicable 
-- functions that are meaningful, except min and max.
lnd_aggr :: (b -> b -> b) -> LnkD b x -> b
lnd_aggr _ (LnkDUnit b) = b
lnd_aggr fn (LnkDProd l r) = fn (lnd_aggr fn l) (lnd_aggr fn r)
lnd_aggr fn (LnkDSum l r) = fn (lnd_aggr fn l) (lnd_aggr fn r)

-- apply a function pairwise between matching elements in structure.
lnd_zip :: (a -> b -> c) -> LnkD a x -> LnkD b x -> LnkD c x
lnd_zip fn (LnkDUnit a) (LnkDUnit b) = 
    LnkDUnit (fn a b)
lnd_zip fn (LnkDSum al ar) blr = 
    let cl = lnd_zip fn al (lnd_left blr) in
    let cr = lnd_zip fn ar (lnd_right blr) in
    LnkDSum cl cr
lnd_zip fn alr (LnkDSum bl br) =
    let cl = lnd_zip fn (lnd_left alr) bl in
    let cr = lnd_zip fn (lnd_right alr) br in
    LnkDSum cl cr
lnd_zip fn (LnkDProd a1 a2) b12 =
    let c1 = lnd_zip fn a1 (lnd_fst b12) in
    let c2 = lnd_zip fn a2 (lnd_snd b12) in
    LnkDProd c1 c2
lnd_zip fn a12 (LnkDProd b1 b2) =
    let c1 = lnd_zip fn (lnd_fst a12) b1 in
    let c2 = lnd_zip fn (lnd_snd a12) b2 in
    LnkDProd c1 c2
-}





