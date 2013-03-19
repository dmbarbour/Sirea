{-# LANGUAGE TypeOperators, GADTs #-}

-- | The implementation of dynamic behaviors is one of the complex
-- features for RDP and Sirea. Not only must the current dynamic
-- value be installed, so must values for the recent past (since
-- there may be straggling updates on the other input signals) and
-- the near future (for anticipation and speculative evaluation).
-- Further, when switching from one behavior to another, there is
-- a brief transition period where both are active.
--
-- One beval might be evaluating dozens of behaviors in parallel. 
--
-- Anticipation is essential for dynamic behaviors to work well in
-- RDP and Sirea, enabling behaviors to be installed and initialized
-- in advance of any absolute requirement for them. However, dynamic
-- behaviors are consequently very sensitive to instability. Their
-- role in RDP is mostly for staged programming, configurations, and
-- linking - places where stability is easy to achieve in practice.
-- 
module Sirea.Internal.B0Dynamic 
    ( evalB0
    ) where

import Prelude hiding(id,(.),drop,IO)
import Control.Category
import Control.Monad (unless, when)
import Control.Applicative
import Control.Exception (assert)
import Control.Arrow (second)
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Sirea.Internal.B0Type
import Sirea.Internal.STypes
import Sirea.Internal.LTypes
import Sirea.Internal.B0Impl 
import Sirea.Internal.B0Compile
import Sirea.Internal.Tuning (dtCompile, dtFinalize, tAncient)
import Sirea.Time
import Sirea.Signal

--import Debug.Trace

type Key = Int

-- Evaluate a behavior provided dynamically.
--
-- A coordination behavior is provided to compute latency and caps.
-- If an evaluated behavior does not fit into the latency, evalB0
-- will respond with error for that duration.
--
-- Multiple dynamic behaviors are maintained in parallel, i.e. both
-- present and future. Ability to anticipate and compile behaviors
-- slightly ahead of needing them is valuable for managing latency
-- with dynamic behaviors. `touch` is used extensively to combine
-- updates that occur at the same instant.
--
-- evalB can respect some dead code optimizations:
--   dead on input (due to binl/binr reaching x)
--   dead on output (signal is unused at y)
-- however, evalB
--   cannot predict dead input in y based on dead input in x
--   cannot predict dead sink in x based on dead sink in y
-- consequently, eval tends to hinder optimizations.
-- 
evalB0 :: (Monad m, SigInP p x, SigInP p' y) 
       => B0 m (S p ()) (S p' ()) 
       -> B0 m (S p (B0 m x y) :&: x) (y :|: S p ())
evalB0 bdt = --trace "evalB" $
    -- synchronization and preparation
    synchB0 >>> forceDelayB0 >>> execBDT >>> evalPrepB0 flc >>>
    -- after evalPrep, have (S p (Either (B0s1 m x y) ()) :&: x)
    firstB0 (splitB0 >>> mirrorB0 >>> leftB0 dupB0) >>> swapB0 >>>
    -- have (x :&: ((S p () :&: S p ()) :|: S p (B0s1 m x y)))
    disjoinB0 >>> 
    -- have (x :&: S p ()) :|: (x :&: S p (B0s1 m x y))
    leftB0 (firstB0 trivialB0 >>> s1eB0) >>> mirrorB0 >>> 
    -- now have (x :&: S p (B0s1 m x y)) :|: S p ()
    leftB0 (swapB0 >>> evalFinalB0 flc)

    where flc = bdtToFLC bdt
          execBDT = firstB0 $ voidB0 $ constB0 () >>> bdt


-- utility since I don't have access to B0 as a Behavior here.
voidB0 :: (Monad m) => B0 m x y -> B0 m x x
voidB0 b = dupB0 >>> firstB0 (b >>> trivialB0) >>> s1eB0

-- extract some useful metadata from a coordination behavior.
bdtToFLC :: (Monad m) => B0 m (S p a) (S p' b) -> (LC m -> LC m)
bdtToFLC bdt lc =
    let (_,lcy) = compileB0s1 bdt (LnkSig (LCX lc)) in
    assert ((not . ln_dead) lcy) $
    fromMaybe lc (getLC <$> ln_toMaybe lcy)

-- apply first compilation stage to the input behavior, and filter
-- behaviors that won't fit into given time `dt`.
evalPrepB0 :: (Monad m) 
           => (LC m -> LC m)
           -> B0 m (S p (B0 m x y) :&: x) (S p (Either (B0s1 m x y) ()) :&: x)
evalPrepB0 = mkLnkB0 lcPrep . mkLnEvalPrep where
    lcPrep lcbx = LnkProd ((lc_fwd . ln_fst) lcbx) (ln_snd lcbx)

mkLnEvalPrep :: (Monad m) 
             => (LC m -> LC m)
             -> LCapsM m (S p (B0 m x y) :&: x) 
             -> LnkM m (S p (Either (B0s1 m x y) ()) :&: x)
             -> m (LnkM m (S p (B0 m x y) :&: x))
mkLnEvalPrep flc lcbx@(LnkProd (LnkSig (LCX lcb)) lcx) lnb0s1x =
    assert (lc_maxGoal lcbx == lc_minCurr lcbx) $
    let lnb0s1 = ln_fst lnb0s1x in
    let lnx = ln_snd lnb0s1x in
    let dtG = lc_dtGoal (flc lcb) in
    let lnb = (ln_lumap $ ln_sfmap $ s_fmap $ fnEvalPrep dtG lcx) lnb0s1 in
    return (LnkProd lnb lnx)
mkLnEvalPrep _ lcbx _ = assert (ln_dead lcbx) $ return LnkDead

-- evalPrep will partially evaluate the inner behaviors, and decide 
-- whether they can be evaluated further (e.g. if they do not fit in
-- the alloted timeslot, they cannot be evaluated).
fnEvalPrep :: (Monad m) => DT -> LCapsM m x -> B0 m x y -> Either (B0s1 m x y) ()
fnEvalPrep dtG lcx = evalFitDelay dtG . flip compileB0s1 lcx

-- evalFitDelay is applied to each dynamic behavior. If it succeeds,
-- we can guarantee the resulting delay is equal to dtGoal on every 
-- signal in y. If the behavior is too large to fit, we'll return
-- the '()' value indicating failure.
evalFitDelay :: (Monad m) => DT -> (B0s1 m x y, LCapsM m y) -> Either (B0s1 m x y) ()
evalFitDelay dtG (b,lcy) = 
    if (lc_maxGoal lcy > dtG)
        then Right () -- latency type error
        else Left (b >>> B0s1_mkLnk (return . fitGoal))
    where fitGoal = buildTshift lcy (lc_map toGoal lcy)
          toGoal lc = lc { lc_dtCurr = dtG, lc_dtGoal = dtG }

-- final evaluation step; assumes input B is valid at this point.
-- Will also add the delays.
evalFinalB0 :: (Monad m, SigMembr x, SigMembr y) => (LC m -> LC m) 
            -> B0 m (S p (B0s1 m x y) :&: x) y
evalFinalB0 flc = mkLnkB0 lcEval lnEval where
    lcEval = lc_dupCaps . lc_map flc . ln_fst
    lnEval = buildEval flc

-- Data needed for the dynamic evaluator source
data Evaluator m x y = Evaluator 
    { ev_data     :: !(Ref m (EVD m x y)) -- primary state
    , ev_keys     :: !(Ref m Key)         -- key source (key per compile)
    , ev_sched    :: !(Sched m)
    , ev_mergeLnk :: !(Key -> LnkM m y) -- remote links
    , ev_dynX     :: !(LnkW (Dyn m) x) -- source links
    }
data EVD m x y = EVD 
    { _evd_signal  :: !(Sig (B0s1 m x y)) -- signal of dynamic behaviors
    , _evd_stable  :: {-# UNPACK #-} !StableT -- last reported stability
    , _evd_tCut    :: {-# UNPACK #-} !T   -- upper bound for compiled signal
    }
    -- note: tCut is an exclusive upper bound; an update exactly at tCut 
    -- will not have already been compiled. 

evdZero :: EVD m x y 
evdZero = EVD s_never (StableT tAncient) tAncient

-- buildEval prepares the evaluator to receive and process inputs.
buildEval :: (SigMembr x, Monad m) 
          => (LC m -> LC m)
          -> LCapsM m (S p (B0s1 m x y) :&: x)
          -> LnkM m y 
          -> m (LnkM m (S p (B0s1 m x y) :&: x))
buildEval flc lcbx@(LnkProd (LnkSig (LCX lcb)) lcx) lny =
    assert (lc_minCurr lcbx == lc_maxGoal lcbx) $
    let ccx = lc_cc lcb in
    let ccy = lc_cc (flc lcb) in
    cc_newRef ccx evdZero >>= \ rfEVD ->
    cc_newRef ccx 10000 >>= \ rfKeys ->
    cc_getSched ccx >>= \ sched ->
    mkFullDyn lcx >>= \ dynX ->
    mkMergeLnkFactory ccy lny >>= \ mergeLnkY ->
    let ev = Evaluator rfEVD rfKeys sched mergeLnkY dynX in
    let luB = lnkEvalB0 ev in
    let xLnk = fullDynToLnk (ev_dynX ev) in
    return (LnkProd (LnkSig luB) xLnk)
buildEval _ lcbx lny = assert allDead $ return LnkDead where
    allDead = ln_dead lcbx && ln_dead lny

-- Dynamic behavior updates are delayed to the next step. Touch and
-- cycle must occur in the first phase of each step, but updates to
-- dynamic behaviors are naturally received in the update step, so
-- we must wait to the next step to touch the new dynamic behaviors.
-- 
-- Any insignificant update (i.e. one that does not significantly
-- advance stability or install a new behavior) might be dropped
-- rather than forwarded.
--
-- Note: Dynamic behaviors are installed onStepEnd to simplify the
-- dynSigCycle operation. It also simplifies the regular updates;
-- update in the next step is simply idling with tmup set.
--
lnkEvalB0 :: (Monad m) => Evaluator m x y -> LnkUpM m (B0s1 m x y)
lnkEvalB0 ev = LnkUp touch update idle cyc where
    touch = return () -- dynamic behavior updates delay to next step
    cyc _ = return () -- cycles are broken by the automatic delay
    update tS tU su = 
        readRef (ev_data ev) >>= \ (EVD s0 tS0 tC) ->
        let s = s_switch' s0 tU su in
        let tLo = min tC tU in
        assert (tU >= inStableT tS0) $ -- respects stability?
        upd' tS0 tS s tLo
    idle tS =
        readRef (ev_data ev) >>= \ (EVD s0 tS0 tC) ->
        upd' tS0 tS s0 tC
    upd' tS0 tS s tLo =
        let tHi = inStableT tS `addTime` dtCompile in
        assert (tS >= tS0) $
        assert (tHi >= tLo) $
        let tLoR = tLo `subtractTime` nanosToDt 1 in
        let sGC = s_trim s (inStableT tS) in
        let range = takeWhile ((< tHi) . fst) $
                    dropWhile ((< tLo) . fst) $
                    sigToList s tLoR tHi 
        in
        let bUrgent = not (null range) || (tS /= tS0) in
        writeRef' (ev_data ev) (EVD sGC tS tHi) >>
        when bUrgent (install tS range)
    install tS [] = -- stability update only
        --trace ("Eval Stability " ++ show tS) $
        onNextStep (ev_sched ev) $
            onUpdPhase (ev_sched ev) (dynBIdle tS (ev_dynX ev)) >>
            dynBTouch (ev_dynX ev)
    install tS range = onStepEnd (ev_sched ev) $
        --trace ("dynamic install " ++ show tS ++ "  range: " ++ show (map fst range)) $
        mapM compileE range >>= \ dynLnks ->
        dynBInstall (ev_dynX ev) dynLnks >>= \ (gcTouch,gcKill) ->
        onNextStep (ev_sched ev) $
            onUpdPhase (ev_sched ev) (gcKill >> dynBIdle tS (ev_dynX ev)) >>
            gcTouch >> dynBTouch (ev_dynX ev)
    compileE (t,mb) = compile mb >>= \ lnx -> return (t,lnx)
    compile Nothing = return LnkDead
    compile (Just b) = newKey >>= compileB0s2 b . ev_mergeLnk ev
    newKey = 
        readRef (ev_keys ev) >>= \ k0 ->
        let k = succ k0 in
        writeRef' (ev_keys ev) k >>
        return k

-- build a complex dynamic behavior state that reflects the signal type
-- (including LnkDead for cases where the input signal is dead-on-input)
mkFullDyn :: (Monad m) => LCapsM m x -> m (LnkW (Dyn m) x)
mkFullDyn LnkDead = return LnkDead
mkFullDyn (LnkSig (LCX lc)) = 
    cc_newRef (lc_cc lc) dyn_zero >>= \ rf ->
    return (LnkSig (Dyn rf))
mkFullDyn (LnkProd x y) =
    mkFullDyn x >>= \ dx ->
    mkFullDyn y >>= \ dy ->
    return (LnkProd dx dy)
mkFullDyn (LnkSum x y) =
    mkFullDyn x >>= \ dx ->
    mkFullDyn y >>= \ dy ->
    return (LnkSum dx dy)

fullDynToLnk :: (Monad m) => LnkW (Dyn m) x -> LnkM m x
fullDynToLnk LnkDead = LnkDead
fullDynToLnk (LnkProd x y) = LnkProd (fullDynToLnk x) (fullDynToLnk y)
fullDynToLnk (LnkSum x y) = LnkSum (fullDynToLnk x) (fullDynToLnk y)
fullDynToLnk (LnkSig x) = LnkSig (dynToLnkUp x)

dynToLnkUp :: (Monad m) => Dyn m x -> LnkUpM m x
dynToLnkUp dyn = LnkUp touch update idle cyc where
    touch = dynSigTouch dyn
    update = dynSigUpdate dyn
    idle = dynSigIdle dyn
    cyc = dynSigCycle dyn

-- Dyn and DynSt are internal structures used for dynamic behaviors.
-- Each Dyn will join the input signal `x` with the active dynamic
-- behaviors.
--
-- TODO: consider an alternative structure for dyn_blink to simplify
-- the operations on it. 
newtype Dyn m a = Dyn (Ref m (DynSt m a))
data DynSt m a = DynSt 
    { dyn_signal :: !(SigSt a)   -- concrete input signal
    , dyn_tmup   :: !(Maybe T)   -- earliest update time (signal or behavior)
    , dyn_blink  :: ![(T,LnkUpM m a)] -- links prepared to receive signals.
    , dyn_bstable:: {-# UNPACK #-} !StableT -- stability of dynamic behavior
    , dyn_btouch :: !Bool        -- expecting behavior update?
    }

dynExpect :: DynSt m a -> Bool
dynExpect dyn = dyn_btouch dyn || st_expect (dyn_signal dyn)

dynStable :: DynSt m a -> StableT
dynStable dyn = min (dyn_bstable dyn) ((st_stable . dyn_signal) dyn)

dyn_zero :: DynSt m a
dyn_zero = DynSt { dyn_signal   = st_zero 
                 , dyn_tmup     = Nothing
                 , dyn_blink    = []
                 , dyn_bstable  = StableT tAncient
                 , dyn_btouch   = False
                 }

-- touch all signals in a complex dynamic behavior
dynBTouch :: (Monad m) => LnkW (Dyn m) a -> m ()
dynBTouch = ln_forEach $ \ (Dyn rf) ->
    readRef rf >>= \ dyn ->
    let bExpect = dynExpect dyn in
    let dyn' = dyn { dyn_btouch = True } in
    writeRef' rf dyn' >>
    unless bExpect (touchBL (dyn_blink dyn))

-- idle every link in the dynamic behaviors
-- Since actual update (install) takes place between steps, only
-- idle is ever distributed as an update.
dynBIdle :: (Monad m) => StableT -> LnkW (Dyn m) x -> m ()
dynBIdle tS = ln_forEach $ \ d@(Dyn rf) ->
    readRef rf >>= \ dyn ->
    assert (dyn_btouch dyn) $
    let bWait = st_expect (dyn_signal dyn) in
    let dyn' = dyn { dyn_btouch = False, dyn_bstable = tS } in
    writeRef' rf dyn' >>
    unless bWait (dynEmit d)

-- touch for signal updates in dynamic behavior
dynSigTouch :: (Monad m) => Dyn m a -> m ()
dynSigTouch (Dyn rf) = 
    readRef rf >>= \ dyn ->
    let bExpect = dynExpect dyn in
    let dyn' = dyn { dyn_signal = st_poke (dyn_signal dyn) } in
    writeRef' rf dyn' >>
    unless bExpect (touchBL (dyn_blink dyn))

touchBL :: (Monad m) => [(T,LnkUpM m a)] -> m ()
touchBL = mapM_ (ln_touch . snd)

-- cycles can just be forwarded to the set of active links, no need
-- to remember because updates occurs only between steps.
dynSigCycle :: (Monad m) => Dyn m a -> CycleSet -> m ()
dynSigCycle (Dyn rf) ns = readRef rf >>= mapM_ cyc . dyn_blink where
    cyc = flip ln_cycle ns . snd

-- idling updates
dynSigIdle :: (Monad m) => Dyn m a -> StableT -> m ()
dynSigIdle d@(Dyn rf) tS =
    readRef rf >>= \ dyn ->
    let st = dyn_signal dyn in
    assert (st_expect st) $
    let bWait = dyn_btouch dyn in
    let dyn' = dyn { dyn_signal = st_idle tS st } in
    writeRef' rf dyn' >>
    unless bWait (dynEmit d)

-- regular updates
dynSigUpdate :: (Monad m) => Dyn m a -> StableT -> T -> Sig a -> m ()
dynSigUpdate d@(Dyn rf) tS tU su =
    readRef rf >>= \ dyn ->
    let st = dyn_signal dyn in
    assert (st_expect st) $
    let bWait = dyn_btouch dyn in
    let st' = st_update tS tU su st in
    let tmup' = Just $! maybe tU (min tU) (dyn_tmup dyn) in
    let dyn' = dyn { dyn_signal = st', dyn_tmup = tmup' } in
    writeRef' rf dyn' >>
    unless bWait (dynEmit d)

-- Install a new set of dynamic behaviors. This is performed at the
-- end of a step, i.e. between steps, so it does not interfere with
-- cycles and regular updates. The main argument is the list of
-- compiled behaviors, with information about where to install them.
--
-- The return value is a pair of actions to perform GC of links that
-- are to be overwritten. (gcTouch,gcKill).
--
dynBInstall :: (Monad m) => LnkW (Dyn m) x -> [(T,LnkM m x)] -> m (m (), m ())
dynBInstall (LnkProd x y) bl =
    let lx = map (second ln_fst) bl in
    let ly = map (second ln_snd) bl in
    dynBInstall x lx >>= \ gcx ->
    dynBInstall y ly >>= \ gcy ->
    return (gcPair gcx gcy)
dynBInstall (LnkSum x y) bl =
    let lx = map (second ln_left) bl in
    let ly = map (second ln_right) bl in
    dynBInstall x lx >>= \ gcx ->
    dynBInstall y ly >>= \ gcy ->
    return (gcPair gcx gcy)
dynBInstall (LnkSig (Dyn rf)) blu@(b:_) =
    readRef rf >>= \ dyn ->
    let bl = map (second ln_lnkup) blu in
    let tU = fst b in
    let (blKeep,blKill) = span ((< tU) . fst) (dyn_blink dyn) in
    let bl' = blKeep ++ bl in
    assert (isNothing (dyn_tmup dyn)) $
    let dyn' = dyn { dyn_tmup = Just tU, dyn_blink = bl' } in
    let tKill = inStableT $ dynStable dyn in -- earliest legal kill
    assert (tU >= tKill) $
    let gcTouch = mapM_ (ln_touch . snd) blKill in
    let gcKill = mapM_ (terminate tKill . snd) blKill in
    writeRef' rf dyn' >>
    return (gcTouch, gcKill)
dynBInstall LnkDead bl =
    assert (all (ln_dead . snd) bl) $
    return (return (), return ())
dynBInstall _ [] =
    assert False $ -- case should be eliminated by check in lnkEvalB0
    return (return (), return ())

-- Combine two (touch,kill) pairs in the obvious way
gcPair :: (Monad mt, Monad mk) => (mt (), mk ()) -> (mt (), mk ()) -> (mt (), mk ())
gcPair (tx,kx) (ty,ky) = (tx >> ty, kx >> ky)

-- when we switch to a new dynamic behavor, we kill behaviors that
-- are no longer relevant. This recants signals starting at time tm.
terminate :: T -> LnkUpM m a -> m ()
terminate tKill lu = 
    let tS = StableT (tKill `addTime` dtFinalize) in
    ln_update lu tS tKill s_never

-- deliver the updated dynamic behavior input signals
dynEmit :: (Monad m) => Dyn m a -> m ()
dynEmit (Dyn rf) = 
    readRef rf >>= \ dyn ->
    assert ((not . dynExpect) dyn) $
    let tS   = dynStable dyn in
    let bl   = dyn_blink dyn in
    let st'  = st_clear tS (dyn_signal dyn) in
    let bl'  = bl_clear (inStableT tS) bl in
    let dyn' = dyn { dyn_signal = st'
                   , dyn_blink  = bl'
                   , dyn_tmup   = Nothing }
    in
    writeRef' rf dyn' >>
    case dyn_tmup dyn of
        Nothing ->
            deliverS tS bl
        Just tU ->
            let su = s_trim (st_signal (dyn_signal dyn)) tU in
            deliverDyn tS tU su bl

-- Eliminate old links that won't be updated further.
bl_clear :: T -> [(T,lu)] -> [(T,lu)]
bl_clear tS bl@(_:r@(hi:_)) = 
    if (tS >= fst hi) then bl_clear tS r else bl
bl_clear _ bl = bl -- always keep last element

-- Modify stability if we know it is the last update, to ensure GC
finalStability :: StableT -> T -> StableT
finalStability tS@(StableT tm) tHi = 
    if (tm >= tHi) then StableT (tm `addTime` dtFinalize) else tS

-- Deliver stability updates
deliverS :: (Monad m) => StableT -> [(T,LnkUpM m x)] -> m ()
deliverS _ [] = return ()
deliverS tS (x:[]) = ln_idle (snd x) tS
deliverS tS (lo:r@(hi:_)) = 
    ln_idle (snd lo) (finalStability tS (fst hi)) >>
    deliverS tS r

-- deliver updates 
deliverDyn :: (Monad m) => StableT -> T -> Sig x -> [(T,LnkUpM m x)] -> m ()
deliverDyn _ _ _ [] = return () -- waiting on first behavior
deliverDyn tS tU0 su (x:[]) = 
    -- full update; full speculated future
    let tU = max tU0 (fst x) in
    let lu = snd x in
    ln_update lu tS tU su
deliverDyn tS tU0 s0 (lo:r@(hi:_)) = 
    let lu = snd lo in
    let tSF = finalStability tS (fst hi) in
    case compare tU0 (fst hi) of
        GT -> -- stability update
            ln_idle lu tSF >> 
            deliverDyn tS tU0 s0 r
        EQ -> -- cutoff update
            ln_update lu tSF tU0 s_never >> 
            deliverDyn tS tU0 s0 r
        LT -> -- fragment update
            let tU = max tU0 (fst lo) in
            let sLo = s_trim s0 tU in
            let sLoCutHi = s_switch sLo (fst hi) s_never in
            ln_update lu tSF tU sLoCutHi >>
            deliverDyn tS tU0 sLo r
           
-- RESULTS LINK FACTORY.
--   Call factory for each dynamic behavior.
--   Factory should be called with different integer each time
--     (may safely reuse integers after GC eliminates old behaviors)
--   One result link for each dynamic behavior.
--   All links are merged into target `Lnk y`.
--   Dead outputs are preserved. 
--   Touches on multiple merge targets:
--     clear touch status on update
--     propagate only if no touched elements remain
--   Cycles: to avoid multiplying the number of cycle tests, cycle
--     information is recorded and only propagated if it adds new
--     names. The record lasts until update.
--   GC from collection only on propagation
--     Q: when can I remove signal from list?
--     A: when s_term holds at stability 
--
-- This design would be reasonable even in a distributed system, but
-- with a slight variation that we'd be creating URLs/IDs for remote
-- hosts (with special support for merge-links). Authorizations and
-- expirations for those links could be achieved by HMAC provided on
-- establishing the merge-link. (RDP is intended to set up without 
-- any round-trip handshaking.)
mkMergeLnkFactory :: (Monad m) => CC m -> LnkM m y -> m (Key -> LnkM m y)
mkMergeLnkFactory _ LnkDead = 
    return (const LnkDead)
mkMergeLnkFactory cc (LnkProd f s) = 
    mkMergeLnkFactory cc f >>= \ mkF ->
    mkMergeLnkFactory cc s >>= \ mkS ->
    let mkProd k = LnkProd (mkF k) (mkS k) in
    return mkProd
mkMergeLnkFactory cc (LnkSum l r) =
    mkMergeLnkFactory cc l >>= \ mkL ->
    mkMergeLnkFactory cc r >>= \ mkR ->
    let mkSum k = LnkSum (mkL k) (mkR k) in
    return mkSum
mkMergeLnkFactory cc (LnkSig lu) =
    cc_newRef cc mldZero >>= \ rf ->
    let mln = MergeLnk rf in
    let mkLnk = LnkSig . fnMergeEval mln lu in
    return mkLnk

-- MergeLnk is the state to perform merges of results from multiple
-- behaviors. It also tracks recorded cycle information, because it
-- otherwise might multiply the number of cycle tests by the number
-- of behaviors in evaluation. Finally, last reported stability is
-- recorded to ensure we report increasing stability.
newtype MergeLnk m a = MergeLnk { mln_data :: Ref m (MLD a) }
data MLD a = MLD
    { mld_touchCt  :: {-# UNPACK #-} !Int     -- current touch count
    , mld_stable   :: {-# UNPACK #-} !StableT -- reported stability
    , mld_upper    :: {-# UNPACK #-} !StableT -- max stability of inputs
    , mld_table    :: !(M.Map Key (SigSt a))
    , mld_cycle    :: !CycleSet
    , mld_tmup     :: !(Maybe T)
    }

mldZero :: MLD a
mldZero = MLD 
    { mld_touchCt = 0
    , mld_stable  = StableT tAncient
    , mld_upper   = StableT tAncient
    , mld_table   = M.empty
    , mld_tmup    = Nothing
    , mld_cycle   = S.empty
    }

mld_getSt :: MLD a -> Key -> SigSt a
mld_getSt mld k = fromMaybe st_zero $ M.lookup k (mld_table mld)

-- touch key (if not already touched)
mld_touch :: MLD a -> Key -> MLD a
mld_touch mld k =
    let st = mld_getSt mld k in
    if st_expect st then mld else 
    let st' = st_poke st in
    let tbl' = M.insert k st' (mld_table mld) in
    let tc' = succ (mld_touchCt mld) in
    mld { mld_touchCt = tc', mld_table = tbl' }

mld_idle :: MLD a -> Key -> StableT -> MLD a
mld_idle mld k tS =
    let st = mld_getSt mld k in
    assert (st_expect st) $
    let st' = st_idle tS st in
    let tbl' = M.insert k st' (mld_table mld) in
    let tc' = pred (mld_touchCt mld) in
    let tSU = max (mld_upper mld) tS in
    mld { mld_touchCt = tc', mld_table = tbl', mld_upper = tSU }

mld_update :: MLD a -> Key -> StableT -> T -> Sig a -> MLD a
mld_update mld k tS tU su =
    let st = mld_getSt mld k in
    assert (st_expect st) $
    let st' = st_update tS tU su st in
    let tbl' = M.insert k st' (mld_table mld) in
    let tc' = pred (mld_touchCt mld) in
    let tmup' = Just $! maybe tU (min tU) (mld_tmup mld) in
    let tSU = max (mld_upper mld) tS in
    mld { mld_touchCt = tc', mld_table = tbl'
        , mld_upper = tSU, mld_tmup = tmup' }

-- merge signals from present and future dynamic behaviors
-- 
-- Performed by the receiver partition. Updates at receipt are not
-- simultaneous if dynamic behavior leverages external partitions.
--
-- It is possible for two signals to temporarily overlap, when one
-- dynamic behavior is in the process of replacing another. Usually,
-- this happens "in the future" where it doesn't cause a problem.
-- However, it can lead to temporary non-determinism in the result,
-- based on the arrangement in the hash table.
--
-- The cost of merge here is proportional to the number of active
-- signals, which is proportional to update frequency for the dynamic
-- behavior.
--
fnMergeEval :: (Monad m) => MergeLnk m a -> LnkUpM m a -> Key -> LnkUpM m a
fnMergeEval mln lu k = LnkUp touch update idle cyc where
    cyc nsu = 
        readRef (mln_data mln) >>= \ mld ->
        let ns = mld_cycle mld in
        let ns' = S.union ns nsu in
        unless (S.size ns' == S.size ns) $
            let mld' = mld { mld_cycle = ns' } in
            writeRef' (mln_data mln) mld' >>
            ln_cycle lu ns'
    touch = 
        readRef (mln_data mln) >>= \ mld ->
        let bFirstTouch = (0 == mld_touchCt mld) in 
        let mld' = mld_touch mld k in
        writeRef' (mln_data mln) mld' >>
        when bFirstTouch (ln_touch lu)
    idle tS =
        readRef (mln_data mln) >>= \ mld ->
        let mld' = mld_idle mld k tS in
        let bLastUpdate = (0 == mld_touchCt mld') in
        writeRef' (mln_data mln) mld' >>
        when bLastUpdate emit
    update tS tU su = 
        readRef (mln_data mln) >>= \ mld ->
        let mld' = mld_update mld k tS tU su in
        let bLastUpdate = (0 == mld_touchCt mld') in
        writeRef' (mln_data mln) mld' >>
        when bLastUpdate emit
    emit = emitMergedSignal mln lu

-- Compute and emit the collective signal merged from consecutive
-- dynamic behaviors. The merge here favors the highest index to
-- heuristically favor the newer dynamic sources, but it wouldn't be
-- an error to merge in a random order. The table will always be non
-- empty at this point, since we've just received at least one idle
-- or update.
--
-- GC must occur on emit, to avoid accumulating signals over time.
-- GC eliminates signals that will not contribute to current or
-- future updates. It seems a bit tricky to compute this value.
-- 
emitMergedSignal :: (Monad m) => MergeLnk m a -> LnkUpM m a -> m ()
emitMergedSignal mln lu =
    readRef (mln_data mln) >>= \ mld ->
    assert (0 == mld_touchCt mld) $
    assert ((not . M.null . mld_table) mld) $
    let lst = M.toAscList (mld_table mld) in
    let tSMax = inStableT (mld_upper mld) in
    let tTest = (maybe tSMax (max tSMax) (mld_tmup mld)) `addTime` dtCompile in
    let mbtGC = leastActiveStability tTest (fmap snd lst) in
    -- trace ("Merging sigs: " ++ show (map fst lst)) $
    let tS0 = mld_stable mld in
    let tS = maybe (mld_upper mld) (max tS0) mbtGC in
    let tGC = inStableT tS in
    let tbl' = M.fromAscList $ mapMaybe (mergeGC tGC tTest) lst in 
    let mld' = MLD 
            { mld_table = tbl'
            , mld_upper = (mld_upper mld)
            , mld_stable = tS 
            , mld_touchCt = 0
            , mld_tmup = Nothing
            , mld_cycle = S.empty }
    in
    -- trace ("dynamic merge @ " ++ show tS ++ " lst=" ++ show (map fst lst)) $ 
    writeRef' (mln_data mln) mld' >>
    case mld_tmup mld of
        Nothing -> ln_idle lu tS 
        Just tU0 ->
            let tU = max tU0 (inStableT tS0) in
            --trace ("dynamic merge @ " ++ show tU ++ " lst=" ++ show (map fst lst)) $
            let lSigs = map ((`s_trim` tU) . st_signal . snd) lst in
            let sMrg = foldr (flip s_merge) s_never lSigs in
            ln_update lu tS tU sMrg
   
-- need to GC the table. An element can be removed once it no longer
-- contributes to the result
mergeGC :: T -> T -> (k,SigSt a) -> Maybe (k, SigSt a)
mergeGC tGC tT (k,st) = 
    let s' = s_trim (st_signal st) tGC in
    let bDone = s_term2 s' tGC tT in
    let st' = st { st_signal = s' } in
    if bDone then Nothing 
             else st' `seq` Just (k, st')


