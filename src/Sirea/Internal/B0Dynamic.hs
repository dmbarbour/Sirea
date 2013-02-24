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
import Data.List (foldl')
import Data.Maybe (isNothing, fromMaybe, mapMaybe)
import qualified Data.Map.Strict as M
import Sirea.Internal.B0Type
import Sirea.Internal.STypes
import Sirea.Internal.LTypes
import Sirea.Internal.B0Impl 
import Sirea.Internal.B0Compile
import Sirea.Internal.Tuning (dtCompileFuture, dtInsigStabilityUp
                             ,dtFinalize, tAncient)
import Sirea.Time
import Sirea.Signal

--import Debug.Trace

type Key = Int

-- ISSUE: I can't produce the 'y' type capabilities from the 'x'
-- type caps unless I have a constraint on partitions, which would
-- hinder dynamic routing
--
-- IDEA: Delay construction of remote link until first dynamic
-- behavior is compiled. PROBLEM: I need caps statically to 
-- construct everything.
--
-- IDEA2: Require a static `S p' ()` signal and require SigInP p' y
--
-- IDEA3: Require a B (S p ()) (S p' ()) link to indicate delay and
-- so on.


-- TODO: Shift dynamic behavior updates into a future cycle (i.e. the
-- next step). This can allow compile and touch in the opening phase.

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
    -- synchronization and preparation (including stage 1 compile)
    synchB0 >>> forceDelayB0 >>> evalPrepB0 flc >>>
    -- after evalPrep, have (S p (Either (B0s1 m x y) ()) :&: x)
    firstB0 (splitB0 >>> mirrorB0 >>> leftB0 dupB0) >>> swapB0 >>>
    -- have (x :&: ((S p () :&: S p ()) :|: S p (B0s1 m x y)))
    disjoinB0 >>> 
    -- have (x :&: S p ()) :|: (x :&: S p (B0s1 m x y))
    leftB0 (firstB0 trivialB0 >>> s1eB0) >>> mirrorB0 >>> 
    -- now have (x :&: S p (B0s1 m x y)) :|: S p ()
    leftB0 (swapB0 >>> evalFinalB0 flc)

    where flc = bdtToFLC bdt

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
             -> LCaps m (S p (B0 m x y) :&: x) 
             -> Lnk m (S p (Either (B0s1 m x y) ()) :&: x)
             -> m (Lnk m (S p (B0 m x y) :&: x))
mkLnEvalPrep flc lcbx@(LnkProd (LnkSig (LCX lcb)) lcx) lnb0s1x =
    assert (lc_maxGoal lcbx == lc_minCurr lcbx) $
    let lnb0s1 = ln_fst lnb0s1x in
    let lnx = ln_snd lnb0s1x in
    let lcTgt = flc lcb in    
    let dtf = lc_dtGoal lcTgt - lc_dtGoal lcb in
    let lnb = (ln_lumap $ ln_sfmap $ s_fmap $ fnEvalPrep dtf lcx) lnb0s1 in
    return (LnkProd lnb lnx)
mkLnEvalPrep _ lcbx _ = assert (ln_dead lcbx) $ return LnkDead

-- evalPrep will partially evaluate the inner behaviors, and decide 
-- whether they can be evaluated further (e.g. if they do not fit in
-- the alloted timeslot, they cannot be evaluated).
fnEvalPrep :: (Monad m) => DT -> LCaps m x -> B0 m x y -> Either (B0s1 m x y) ()
fnEvalPrep dtf lcx = evalFitDelay dtf . flip compileB0s1 lcx

-- evalFitDelay is applied to each dynamic behavior. If it succeeds,
-- we can guarantee the resulting delay is equal to dtGoal on every 
-- signal in y. If the behavior is too large to fit, we'll return
-- the '()' value indicating failure.
evalFitDelay :: (Monad m) => DT -> (B0s1 m x y, LCaps m y) -> Either (B0s1 m x y) ()
evalFitDelay dtf (b,lcy) = 
    if (lc_maxGoal lcy > dtf)
        then Right ()
        else Left (b >>> B0s1_mkLnk (return . fitGoal))
    where fitGoal = buildTshift lcy (lc_map toGoal lcy)
          toGoal lc = lc { lc_dtCurr = dtf, lc_dtGoal = dtf }

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
    , ev_keys     :: !(Ref m Key)         -- key source (per compile)
    , ev_sched    :: !(Sched m)
    , ev_mergeLnk :: !(Key -> Lnk m y) -- remote links
    , ev_dynX     :: !(LnkW (Dyn m) x) -- source links
    }
data EVD m x y = EVD 
    { evd_signal   :: !(SigSt (B0s1 m x y)) -- signal of dynamic behaviors
    , evd_compileT :: {-# UNPACK #-} !T -- how much of the signal has been compiled?
    }

evdZero :: EVD m x y 
evdZero = EVD st_zero tAncient 

evdModSig :: (SigSt (B0s1 m x y) -> SigSt (B0s1 m x y)) 
          -> (EVD m x y -> EVD m x y)
evdModSig fn evd = evd { evd_signal = fn (evd_signal evd) }

-- buildEval prepares the evaluator to receive and process inputs.
buildEval :: (SigMembr x, Monad m) 
          => (LC m -> LC m)
          -> LCaps m (S p (B0s1 m x y) :&: x)
          -> Lnk m y 
          -> m (Lnk m (S p (B0s1 m x y) :&: x))
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

-- The main eval link delays most operations to the next step. This
-- was actually the primary motivation for threading the 'CC' type.
-- The reason for delay is to ensure 'touches' on the new dynamic
-- behaviors are processed in the touch phase, which in turn is
-- necessary for the ln_cycle mechanisms to operate reliably.
lnkEvalB0 :: (Monad m) => Evaluator m x y -> LnkUp m (B0s1 m x y)
lnkEvalB0 ev = LnkUp touch update idle cyc where
    touch = return () -- dynamic behavior updates delay to next step
    cyc _ = return () -- cycles are broken by the automatic delay
    update tS tU su = 
        onNextStep (ev_sched ev) (updateEV ev tS tU su)
    idle tS =
        readRef (ev_data ev) >>= \ evd ->
        let tS0 = (st_stable . evd_signal) evd in
        when (urgentStability tS0 tS) $
            onNextStep (ev_sched ev) (idleEV ev tS)

-- TODO: Change the above to perform updates at the end of the step,
-- though idle can still delay to next step. Note that I can touch
-- and update GC'd behaviors independently, so they won't contribute
-- to cycles. (Or at least those GC'd through switching.)
--
-- Structure: at end of current step
--   update the behavior lists
--   compute the touch and update operations to cleanup old links
    
-- test whether an update to stability is urgent
urgentStability :: StableT -> StableT -> Bool
urgentStability DoneT _ = False
urgentStability _ DoneT = True
urgentStability (StableT t0) (StableT tf) =
    (tf > (t0 `addTime` dtInsigStabilityUp))


-- idleEV is called at the beginning of a step, after a significant
-- stability update for dynamic behaviors in the prior step.
idleEV :: (Monad m) => Evaluator m x y -> StableT -> m ()
idleEV ev tS = 
    modifyRef (ev_data ev) (evdModSig (st_idle tS)) >>    
    updateDynamicsEV ev Nothing

-- updateEV is called at the beginning of a step, after any update
-- to dynamic behaviors in a prior step. Updating at the beginning
-- of a step is essential, since that is when we'll need to compile
-- and touch the new dynamic behaviors.
updateEV :: (Monad m) => Evaluator m x y -> StableT 
         -> T -> Sig (B0s1 m x y) -> m ()
updateEV ev tS tU su =
    modifyRef (ev_data ev) (evdModSig (st_update tS tU su)) >>
    updateDynamicsEV ev (Just tU)

-- The majority of the dynamic behavior logic is handled here, on
-- updating the signal that contains dynamic behaviors, or on the
-- remote end merging multiple signals that vary in time. 
--
-- Note that, at this point, we're still in the 'touch' phase of
-- the current step. 
--
-- The `Maybe T` value here indicates whether there was a recent
-- update to the signals list. If so, we might need to replaces the
-- speculative dynamic behaviors that were already compiled and are
-- already processing their speculative signals. If not, we still
-- must provide updates to dynamic behaviors that become visible due
-- to new stability updates. 
-- 
-- This operation will also perform GC of the behaviors signal.
updateDynamicsEV :: (Monad m) => Evaluator m x y -> Maybe T -> m ()
updateDynamicsEV ev mbTU = initUpdate where
    initUpdate = 
        getBuildRangeEV ev mbTU >>= \ (tS,range) -> -- [(T,Maybe(B x y))]
        mapM compileE range >>= \ dynLnks -> -- [(T,Lnk x)]
        schedUpdate tS dynLnks >>         -- update in next phase
        dynBTouch (ev_dynX ev) >>         -- touch the old links
        mapM_ (ln_touchAll . snd) dynLnks -- touch the new links
    schedUpdate tS dynLnks = 
        onUpdPhase (ev_sched ev) $
            dynBUpdate tS dynLnks (ev_dynX ev)
    compileE (t,mb) =
        maybe (return LnkDead) compile mb >>= \ lnx ->
        return (t,lnx)
    compile b =
        newKey >>= compileB0s2 b . (ev_mergeLnk ev)
    newKey = 
        readRef (ev_keys ev) >>= \ k0 ->
        let k = succ k0 in
        writeRef' (ev_keys ev) k >>
        return k

-- TODO: there are some difficulties with the current design, for
-- how 'cycle' interacts with the new/upcoming behaviors. Perhaps I
-- should update the recorded dynamic behaviors at the *end* of the 
-- current step, 

-- this operation will compute which behaviors need to be compiled
-- and installed. This set can be adjusted due to stability or to
-- an update. Will also perform GC and report stability, to reduce
-- state mutations
--
-- TODO: consider aligning updates, or a 'cutoff' based on upcoming
-- signals. 
getBuildRangeEV :: (Monad m) => Evaluator m x y -> Maybe T 
                -> m (StableT,[(T,Maybe (B0s1 m x y))])
getBuildRangeEV ev mbTU =
    readRef (ev_data ev) >>= \ evd ->
    let st = evd_signal evd in
    let tS = st_stable st in 
    let stCln = st_clear tS st in
    let tU0 = evd_compileT evd in
    let tLo = maybe tU0 (min tU0) mbTU in
    let bRebuildFirst = maybe False (== tLo) mbTU in
    let tHi = case tS of 
            DoneT -> tLo `addTime` dtFinalize 
            StableT tm -> tm `addTime` dtCompileFuture
    in
    assert (tHi >= tLo) $
    let evd' = evd { evd_signal = stCln, evd_compileT = tHi } in
    let slR = sigToList (st_signal st) tLo tHi in
    assert ((not . null) slR) $
    let range = if bRebuildFirst then slR else tail slR in
    evd' `seq` writeRef (ev_data ev) evd' >>
    return (tS,range)

-- build a complex dynamic behavior state that reflects the signal type
-- (including LnkDead for cases where the input signal is dead-on-input)
mkFullDyn :: (Monad m) => LCaps m x -> m (LnkW (Dyn m) x)
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

fullDynToLnk :: (Monad m) => LnkW (Dyn m) x -> Lnk m x
fullDynToLnk LnkDead = LnkDead
fullDynToLnk (LnkProd x y) = LnkProd (fullDynToLnk x) (fullDynToLnk y)
fullDynToLnk (LnkSum x y) = LnkSum (fullDynToLnk x) (fullDynToLnk y)
fullDynToLnk (LnkSig x) = LnkSig (dynToLnkUp x)

dynToLnkUp :: (Monad m) => Dyn m x -> LnkUp m x
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
    , dyn_blink  :: ![(T,LnkUp m a)] -- links prepared to receive signals.
    , dyn_bstable:: !StableT     -- stability of dynamic behavior
    , dyn_btouch :: !Bool        -- still expecting update to active links?
    }

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
    unless (dyn_btouch dyn) $
        let bTouchedX = st_expect (dyn_signal dyn) in
        let dyn' = dyn { dyn_btouch = True } in
        writeRef rf dyn' >>
        unless bTouchedX (touchBL (dyn_blink dyn'))

-- touch for signal updates in dynamic behavior
dynSigTouch :: (Monad m) => Dyn m a -> m ()
dynSigTouch (Dyn rf) = 
    readRef rf >>= \ dyn ->
    let st = dyn_signal dyn in
    unless (st_expect st) $
        let st' = st_poke st in
        let bTouchedB = dyn_btouch dyn in
        let dyn' = dyn { dyn_signal = st' } in
        writeRef rf dyn' >>
        unless bTouchedB (touchBL (dyn_blink dyn'))

touchBL :: (Monad m) => [(T,LnkUp m a)] -> m ()
touchBL bl = mapM_ (ln_touch . snd) bl

dynSigCycle :: (Monad m) => Dyn m a -> CycleSet -> m ()
dynSigCycle (Dyn rf) n =
    error "TODO"

dynSigIdle :: (Monad m) => Dyn m a -> StableT -> m ()
dynSigIdle d@(Dyn rf) tS =
    readRef rf >>= \ dyn ->
    let st = dyn_signal dyn in
    assert (st_expect st) $
    let st' = st_idle tS st in
    let dyn' = dyn { dyn_signal = st' } in
    writeRef rf dyn' >>
    dynMaybeEmit d

dynSigUpdate :: (Monad m) => Dyn m a -> StableT -> T -> Sig a -> m ()
dynSigUpdate d@(Dyn rf) tS tU su =
    readRef rf >>= \ dyn ->
    let st = dyn_signal dyn in
    assert (st_expect st) $
    let st' = st_update tS tU su st in
    let tmup' = Just $! maybe tU (min tU) (dyn_tmup dyn) in
    let dyn' = dyn { dyn_signal = st', dyn_tmup = tmup' } in
    writeRef rf dyn' >>
    dynMaybeEmit d

-- update the dynamic behaviors. The links specific to each
-- concrete underlying signal are stored with that signal. 
-- The important argument is [(T,Lnk x)] - a list of compiled
-- links with the times they are supposed to receive information.
--
-- TODO: return touch&cleanup operations for dead links in next phase.
dynBUpdate :: (Monad m) => StableT -> [(T,Lnk m x)] -> LnkW (Dyn m) x -> m ()
dynBUpdate tm bl (LnkProd dynX dynY) =
    let blX = map (second ln_fst) bl in
    let blY = map (second ln_snd) bl in
    dynBUpdate tm blX dynX >>
    dynBUpdate tm blY dynY
dynBUpdate tm bl (LnkSum dynX dynY) =
    let blX = map (second ln_left) bl in
    let blY = map (second ln_right) bl in
    dynBUpdate tm blX dynX >>
    dynBUpdate tm blY dynY
dynBUpdate tm bl (LnkSig dyn) =
    let blu = map (second ln_lnkup) bl in
    dynUpdateLinks dyn tm blu >>
    dynMaybeEmit dyn
dynBUpdate _ bl LnkDead =
    assert (all (ln_dead . snd) bl) $
    return ()

-- when we update the links associated with a behavior, we may need
-- to kill prior links associated with this behavior. 
dynUpdateLinks :: (Monad m) => Dyn m a -> StableT -> [(T,LnkUp m a)] -> m ()
dynUpdateLinks (Dyn rf) tS [] =
    readRef rf >>= \ dyn ->
    assert (dyn_btouch dyn) $
    let dyn' = dyn { dyn_bstable = tS, dyn_btouch = False } in
    writeRef' rf dyn'
dynUpdateLinks (Dyn rf) tS blu@(b:_) =
    readRef rf >>= \ dyn ->
    assert (dyn_btouch dyn) $
    --traceIO ("dynUpdateLinks: " ++ show (map fst blu)) >>
    let tU = fst b in
    let tmup' = Just $! maybe tU (min tU) (dyn_tmup dyn) in
    let (blKeep,blKill) = span ((< tU) . fst) (dyn_blink dyn) in
    let blink' = blKeep ++ blu in
    let dyn' = dyn { dyn_bstable = tS, dyn_btouch = False
                   , dyn_blink = blink', dyn_tmup = tmup' } 
    in
    writeRef rf dyn' >>
    mapM_ (terminate tU . snd) blKill -- destroy replaced behaviors

-- when we switch to a new dynamic behavor, we kill behaviors that
-- are no longer relevant. This recants signals starting at time tm.
terminate :: T -> LnkUp m a -> m ()
terminate tCut lu = ln_update lu DoneT tCut s_never

-- decision point for processing updates. The actual updates are not
-- trivial, mostly due to the finalization semantics for old links.
dynMaybeEmit :: (Monad m) => Dyn m a -> m ()
dynMaybeEmit (Dyn rf) = 
    readRef rf >>= \ dyn ->
    unless (dynExpect dyn) $
        let tS    = dynStable dyn in
        let bl0   = dyn_blink dyn in
        let sig   = (st_signal . dyn_signal) dyn in
        let blUpd = case dyn_tmup dyn of
                Nothing -> loadStable tS bl0
                Just tU -> loadUpdate tS tU sig bl0
        in
        let stCln = st_clear tS (dyn_signal dyn) in
        let blCln = cleanBl blUpd in
        let dyn' = dyn { dyn_signal = stCln, dyn_blink = blCln
                       , dyn_tmup = Nothing }
        in
        writeRef' rf dyn' >>
        mapM_ bl_task blUpd

dynExpect :: DynSt m a -> Bool
dynExpect dyn = dyn_btouch dyn || st_expect (dyn_signal dyn)

dynStable :: DynSt m a -> StableT
dynStable dyn = min (dyn_bstable dyn) ((st_stable . dyn_signal) dyn)

data BLUP m x = BLUP 
    { bl_link :: !(T,LnkUp m x) 
    , bl_drop :: !Bool
    , bl_task :: (m ())
    }

-- GC the blUpd list
cleanBl :: [BLUP m x] -> [(T,LnkUp m x)]
cleanBl = map bl_link . dropWhile bl_drop

-- Stability isn't entirely trivial, mostly because some elements 
-- become "permanently" stable once stability surpasses the start
-- of the next element's term. (I.e. they become inactive forever.)
loadStable :: StableT -> [(T,LnkUp m x)] -> [BLUP m x]
loadStable DoneT = map drop 
    where drop x = BLUP x True (ln_idle (snd x) DoneT)
loadStable tS@(StableT tm) = fn
    where fn [] = []
          fn (x:[]) = (keep x):[]
          fn (lo:hi:xs) = 
            let more = fn (hi:xs) in
            if (tm >= fst hi) then (drop lo):more
                              else (keep lo):more
          keep x = BLUP x False (ln_idle (snd x) tS)
          drop x = BLUP x True (ln_idle (snd x) DoneT)


-- compute the signal updates for elements in a list. As a special
-- case, we'll send a cutoff signal if the update occurs on a change
-- in behavior. This is necessary in case of changes in B.
loadUpdate :: StableT -> T -> Sig x -> [(T,LnkUp m x)] -> [BLUP m x]
loadUpdate _ _ _ [] = []
loadUpdate tS tu sf (x:[]) = 
    let tt  = max tu (fst x) in
    let bDrop = case tS of { DoneT -> True; _ -> False } in
    let blup = BLUP x bDrop (ln_update (snd x) tS tt sf) in
    (blup:[])
loadUpdate tS tu s0 (lo:hi:xs) =
    let bDrop = case tS of
            DoneT -> True
            StableT tm -> (tm >= fst hi)
    in
    let tSK = if bDrop then DoneT else tS in
    let sf = s_trim s0 (fst lo) in
    let lu = snd lo in
    let task = case compare tu (fst hi) of
            GT -> ln_idle lu tSK -- stability update
            EQ -> ln_update lu tSK tu s_never -- cutoff update
            LT -> 
                let tuK = max tu (fst lo) in -- effective update time
                let sK = s_switch sf (fst hi) s_never in -- segment
                ln_update lu tSK tuK sK
    in
    let blup = BLUP lo bDrop task in
    let more = loadUpdate tS tu sf (hi:xs) in
    (blup:more)

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
mkMergeLnkFactory :: (Monad m) => CC m -> Lnk m y -> m (Key -> Lnk m y)
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
-- behaviors. It uses a hashtable internally, and counts touches, to
-- keep algorithmic costs down.
newtype MergeLnk m a = MergeLnk { mln_data :: Ref m (MLD a) }
data MLD a = MLD
    { mld_touchCt  :: {-# UNPACK #-} !Int
    , mld_table    :: !(M.Map Key (SigSt a))
    , mld_tmup     :: !(Maybe T)
    }


mldZero :: MLD a
mldZero = MLD 
    { mld_touchCt = 0
    , mld_table   = M.empty
    , mld_tmup    = Nothing
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
    mld { mld_touchCt = tc', mld_table = tbl' }

mld_update :: MLD a -> Key -> StableT -> T -> Sig a -> MLD a
mld_update mld k tS tU su =
    let st = mld_getSt mld k in
    assert (st_expect st) $
    let st' = st_update tS tU su st in
    let tbl' = M.insert k st' (mld_table mld) in
    let tc' = pred (mld_touchCt mld) in
    let tmup' = Just $! maybe tU (min tU) (mld_tmup mld) in
    mld { mld_touchCt = tc', mld_table = tbl', mld_tmup = tmup' }

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
fnMergeEval :: (Monad m) => MergeLnk m a -> LnkUp m a -> Key -> LnkUp m a
fnMergeEval mln lu k = LnkUp touch update idle cyc where
    cyc = ln_cycle lu
    touch = 
        readRef (mln_data mln) >>= \ mld ->
        let bFirstTouch = (0 == mld_touchCt mld) in 
        let mld' = mld_touch mld k in
        writeRef (mln_data mln) mld' >>
        when bFirstTouch (ln_touch lu)
    idle tS =
        readRef (mln_data mln) >>= \ mld ->
        let mld' = mld_idle mld k tS in
        let bLastUpdate = (0 == mld_touchCt mld') in
        writeRef (mln_data mln) mld' >>
        when bLastUpdate emit
    update tS tU su = 
        readRef (mln_data mln) >>= \ mld ->
        let mld' = mld_update mld k tS tU su in
        let bLastUpdate = (0 == mld_touchCt mld') in
        writeRef (mln_data mln) mld' >>
        when bLastUpdate emit
    emit = emitMergedSignal mln lu

-- Compute and emit the collective signal merged from consecutive
-- dynamic behaviors. The merge here favors the highest index to
-- heuristically favor the newest dynamic sources, but it wouldn't 
-- be an error to merge in a random order.
--
-- TODO: POTENTIAL BUG! Might report `DoneT` when a short-route
-- dynamic behavior is replaced by a long-route behavior across
-- partitions. No easy fix; partitions block most hacks.
emitMergedSignal :: (Monad m) => MergeLnk m a -> LnkUp m a -> m ()
emitMergedSignal mln lu =
    readRef (mln_data mln) >>= \ mld ->
    assert (0 == mld_touchCt mld) $
    let lst  = M.toAscList (mld_table mld) in
    let tS = foldl' min DoneT $ map (st_stable . snd) lst in
    let lst' = mapMaybe (mergeEvalGC tS) lst in
    -- trace ("emitMerged @(" ++ show tStable ++ ") count=" ++ show (length lst)) $ 
    let tbl' = M.fromAscList lst' in -- GC'd table
    let performUpdateAction = case (mld_tmup mld) of 
            Nothing -> ln_idle lu tS
            Just tU -> 
                let lSigs = map ((`s_trim` tU) . st_signal . snd) lst in
                let sMrg = foldr (flip s_merge) s_never lSigs in
                ln_update lu tS tU sMrg 
    in
    let mld' = MLD { mld_table = tbl', mld_touchCt = 0, mld_tmup = Nothing } in
    writeRef' (mln_data mln) mld' >>
    performUpdateAction
   
-- need to GC the table. An element can be removed once it no longer
-- contributes to the result
mergeEvalGC :: StableT -> (k,SigSt a) -> Maybe (k, SigSt a)
mergeEvalGC DoneT _ = Nothing
mergeEvalGC (StableT tm) (k,st) = 
    assert ((not . st_expect) st) $
    let (x,sf) = s_sample (st_signal st) tm in
    let bDone = isDoneT (st_stable st) &&
                isNothing x && s_is_final sf tm
    in
    let st' = st { st_signal = sf } in
    if bDone then Nothing 
             else st' `seq` Just (k, st')


