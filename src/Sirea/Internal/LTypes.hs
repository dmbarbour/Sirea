{-# LANGUAGE GADTs, TypeOperators, Rank2Types #-}

-- | This module provides the concrete datatypes and organization of
-- Sirea's implementation. Specific behavior logic is in B0Impl. 
module Sirea.Internal.LTypes 
    ( LnkW(..)
    , LnkUp(..), CycleSet, Lnk
    , LC(..), LCX(..), LCaps
    , StableT(..), isDoneT, fromStableT, maybeStableT
    , ln_left, ln_right, ln_fst, ln_snd, ln_toList, ln_toMaybe
    , ln_dead, ln_zero, ln_lnkup, ln_append
    , ln_lumap, ln_sfmap
    , lc_anyCap, lc_dupCaps, lc_map, lc_fwd
    , lc_minGoal, lc_maxGoal, lc_minCurr, lc_maxCurr, lc_valid
    , SigSt(..), st_zero, st_poke, st_clear, st_update, st_idle
    , monotonicStability, respectsStability
    , SigM(..), sm_zero
    , sm_update_l, sm_poke_l, sm_idle_l
    , sm_update_r, sm_poke_r, sm_idle_r 
    , sm_waiting, sm_stable, sm_emit, sm_cleanup
    , ln_forEach, ln_freeze, ln_touchAll
    , ln_withSigM
    , adjStableTime
    , module Sirea.Internal.CC
    ) where

import Prelude hiding (cycle)
import Sirea.Internal.STypes
import Sirea.Internal.CC
import Sirea.Time
import Sirea.Signal
import Sirea.Internal.Tuning (tAncient)
import Control.Exception (assert)
import Control.Monad (unless, liftM2)
import Data.Monoid
import Data.Maybe (listToMaybe)
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
    , ln_cycle  :: !(CycleSet -> m ())
    }
type CycleSet = Set Unique

instance (Monad m) => Monoid (LnkUp m a) where
    mempty  = ln_zero
    mappend = ln_append

newtype LCX m x = LCX { getLC :: LC m }
data LC m = LC
    { lc_cc      :: !(CC m)
    , lc_dtCurr  :: {-# UNPACK #-} !DT
    , lc_dtGoal  :: {-# UNPACK #-} !DT
    }
type LCaps m x = LnkW (LCX m) x

-- Thoughts: It might be worth adding some path descriptors to LC
-- for debugging purposes, i.e. to answer: "Where am I?" Could do
-- this from both sides.


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
ln_zero :: (Monad m) => LnkUp m a
ln_zero = LnkUp touch update idle cycle where
    touch = return ()
    update _ _ _ = return ()
    idle _ = return ()
    cycle _ = return ()

-- duplicate touches and updates.
ln_append :: (Monad m) => LnkUp m a -> LnkUp m a -> LnkUp m a
ln_append x y = LnkUp touch update idle cycle where
    touch = ln_touch x >> ln_touch y
    idle t = ln_idle x t >> ln_idle y t
    update tS tU sig = ln_update x tS tU sig >> ln_update y tS tU sig
    cycle cs = ln_cycle x cs >> ln_cycle y cs

-- | ln_lnkup extracts LnkUp (from LnkSig or ln_zero from LnkDead)
ln_lnkup  :: (Monad m) => Lnk m (S p a) -> (LnkUp m a)
ln_lnkup (LnkSig lu) = lu
ln_lnkup _ = ln_zero

-- | extract left, right, fst, snd elements. 
ln_left   :: LnkW s (a :|: b) -> LnkW s a
ln_right  :: LnkW s (a :|: b) -> LnkW s b
ln_fst    :: LnkW s (a :&: b) -> LnkW s a
ln_snd    :: LnkW s (a :&: b) -> LnkW s b

ln_left (LnkSum a _) = a
ln_left _ = LnkDead

ln_right (LnkSum _ b) = b
ln_right _ = LnkDead

ln_fst (LnkProd a _) = a
ln_fst _ = LnkDead

ln_snd (LnkProd _ b) = b
ln_snd _ = LnkDead

-- | extract a list of elements from a link
ln_toList :: (forall z . s z -> a) -> LnkW s x -> [a]
ln_toList fn l = ln_toList' fn l []

-- extend a list of elements from a link
ln_toList' :: (forall z . s z -> a) -> LnkW s x -> [a] -> [a]
ln_toList' _ LnkDead xs = xs
ln_toList' fn (LnkSig s) xs = (fn s):xs
ln_toList' fn (LnkProd x y) xs = ln_toList' fn x (ln_toList' fn y xs)
ln_toList' fn (LnkSum x y) xs = ln_toList' fn x (ln_toList' fn y xs)

-- | Compute whether all elements of a link are dead.
ln_dead :: LnkW s x -> Bool
ln_dead = null . ln_toList (const ()) 

-- | Generic access to a link
ln_toMaybe :: LnkW s (S p a) -> Maybe (s a)
ln_toMaybe (LnkSig sa) = Just sa
ln_toMaybe _ = Nothing

-- | simple link update from a signal update transformer
--
-- NOTE: Most Signal functions aren't safe for RDP, where 'safe'
-- means protecting all of RDP's invariants, especially duration 
-- coupling (which supports composable resource management).
--
ln_sfmap :: (Sig x -> Sig y) -> LnkUp m y -> LnkUp m x
ln_sfmap fn ln = LnkUp touch update idle cycle where
    touch = ln_touch ln
    idle = ln_idle ln
    update tS tU = ln_update ln tS tU . fn
    cycle = ln_cycle ln

-- | simple transformer from LnkUp to Lnk
ln_lumap :: (LnkUp m x -> LnkUp m y) -> Lnk m (S p x) -> Lnk m (S p y)
ln_lumap _ LnkDead = LnkDead
ln_lumap fn (LnkSig l) = LnkSig (fn l)

-- | apply operation over each element.
ln_forEach :: (Monad m, Monoid a) => (forall x . s x -> m a) -> LnkW s y -> m a
ln_forEach _ LnkDead = return mempty
ln_forEach fn (LnkProd x y) = liftM2 mappend (ln_forEach fn x) (ln_forEach fn y)
ln_forEach fn (LnkSum x y) = liftM2 mappend (ln_forEach fn x) (ln_forEach fn y)
ln_forEach fn (LnkSig lu) = fn lu

-- | freeze the current signals on a link
ln_freeze :: (Monad m) => Lnk m x -> m ()
ln_freeze = ln_forEach freeze

freeze :: LnkUp m x -> m ()
freeze lu = ln_idle lu DoneT -- idle forever...

ln_touchAll :: (Monad m) => Lnk m x -> m ()
ln_touchAll = ln_forEach ln_touch

lc_minGoal, lc_maxGoal, lc_minCurr, lc_maxCurr :: LCaps m x -> DT

lc_minGoal = foldOr min 0 . ln_toList (lc_dtGoal . getLC)
lc_maxGoal = foldOr max 0 . ln_toList (lc_dtGoal . getLC)
lc_minCurr = foldOr min 0 . ln_toList (lc_dtCurr . getLC)
lc_maxCurr = foldOr max 0 . ln_toList (lc_dtCurr . getLC)

-- fold with alternative. 
foldOr :: (a -> a -> a) -> a -> [a] -> a
foldOr fn _ (x:xs) = foldr fn x xs
foldOr _ alt [] = alt

-- test LCaps valid with regards to latency and liveness.
lc_valid :: LCaps m x -> Bool
lc_valid LnkDead = True
lc_valid (LnkSig (LCX lc)) = (lc_dtGoal lc >= lc_dtCurr lc)
lc_valid (LnkSum x y) = lc_valid x && lc_valid y
lc_valid (LnkProd x y) = lc_valid x && lc_valid y && (ln_dead x == ln_dead y)

-- | if the caps are from the same partition and synch'd, it is
-- okay to just take one. The same-partition requirement is not
-- validated here, and should be enforced at a higher layer.
-- 'Nothing' is returned if the link is dead.
lc_anyCap :: LCaps m x -> Maybe (LC m)
lc_anyCap lcx = 
    assert (lc_minGoal lcx == lc_maxGoal lcx) $
    assert (lc_minCurr lcx == lc_maxCurr lcx) $
    listToMaybe (ln_toList getLC lcx)

-- DupCaps supports typeful construction of a constant capability
data DupCaps m x = DupCaps { dupCaps :: LC m -> LCaps m x }
instance BuildMembr (DupCaps m) where
    buildSigMembr  = DupCaps $ LnkSig . LCX
    buildSumMembr dcx dcy = DupCaps $ \ lc ->
        LnkSum (dupCaps dcx lc) (dupCaps dcy lc) 
    buildProdMembr dcx dcy = DupCaps $ \ lc -> 
        LnkProd (dupCaps dcx lc) (dupCaps dcy lc)

-- | duplicate an input capability to an output capability
-- Note: this is unsafe unless caps are from a common partition
-- and are already synchronized. 
lc_dupCaps :: (SigMembr y) => LCaps m x -> LCaps m y
lc_dupCaps = maybe LnkDead (dupCaps buildMembr) . lc_anyCap


-- | Map a function to every cap.
lc_map :: (LC m -> LC m) -> LCaps m x -> LCaps m x
lc_map _ LnkDead = LnkDead
lc_map fn (LnkSig (LCX lc)) = LnkSig (LCX (fn lc))
lc_map fn (LnkProd x y) = LnkProd (lc_map fn x) (lc_map fn y)
lc_map fn (LnkSum x y) = LnkSum (lc_map fn x) (lc_map fn y)

-- | common caps update is irrelevant change in datatype.
lc_fwd :: LCaps m (S p x) -> LCaps m (S p y)
lc_fwd LnkDead = LnkDead
lc_fwd (LnkSig (LCX lc)) = LnkSig (LCX lc)

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
    let sf = s_switch' (st_signal st) tU su in
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
sm_waiting sm = sm_expect_l sm || sm_expect_r sm where

sm_stable :: SigM x y -> StableT
sm_stable sm = min tL tR where
    tL = (st_stable . sm_lsig) sm
    tR = (st_stable . sm_rsig) sm

-- process link updates based on a SigM combiner function
sm_emit :: (Monad m) => (Sig a -> Sig b -> Sig c) -> LnkUp m c -> SigM a b -> m ()
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
ln_withSigM :: (Monad m) 
            => CC m                    -- common caps
            -> m ()                    -- onTouch
            -> (SigM x y -> m ())      -- on emit
            -> (Set Unique -> m ())    -- on cycle
            -> m (LnkUp m x, LnkUp m y)
ln_withSigM cc onTouch onEmit onCycle = 
    cc_newRef cc sm_zero >>= \ rfSigM ->
    return $ ln_withSigM' rfSigM onTouch onEmit onCycle

ln_withSigM' :: (Monad m) 
             => Ref m (SigM x y) 
             -> m () 
             -> (SigM x y -> m ())
             -> (Set Unique -> m ()) 
             -> (LnkUp m x, LnkUp m y)
ln_withSigM' rf onTouch onEmit onCycle = (lux,luy) where
    touchX = doTouch sm_poke_l
    idleX = doUpdate . sm_idle_l
    updateX tS tU sX = doUpdate (sm_update_l tS tU sX)        
    touchY = doTouch sm_poke_r
    idleY = doUpdate . sm_idle_r
    updateY tS tU sY = doUpdate (sm_update_r tS tU sY)
    doTouch fn = 
        readRef rf >>= \ sm ->
        writeRef rf (fn sm) >>
        unless (sm_waiting sm) onTouch 
    doUpdate fn = readRef rf >>= maybeEmit . fn
    maybeEmit sm = 
        if (sm_waiting sm) then writeRef rf sm else
        let smCln = sm_cleanup (sm_stable sm) sm in
        writeRef' rf smCln >>
        onEmit sm 
    lux = LnkUp touchX updateX idleX onCycle
    luy = LnkUp touchY updateY idleY onCycle


