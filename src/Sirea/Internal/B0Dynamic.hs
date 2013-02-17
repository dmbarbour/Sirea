{-# LANGUAGE TypeOperators, GADTs #-}

-- | Implementation of the Dynamic behavior type for B.
-- Evaluation will logically sync the inputs, then 
-- physically sync the outputs (y). The inputs are not
-- immediately 
module Sirea.Internal.B0Dynamic 
    ( evalB0
    ) where

import Prelude hiding(id,(.),drop)
import Control.Category
import Control.Monad (unless, when)
import Control.Applicative
import Control.Exception (assert)
import Control.Arrow (second)
import Data.IORef
import Data.List (foldl')
import Data.Maybe (isNothing, fromMaybe, mapMaybe)
import Data.Unique
import qualified Data.Map.Strict as M
import Sirea.Internal.B0Type
import Sirea.Internal.STypes
import Sirea.Internal.LTypes
import Sirea.Internal.B0Impl 
import Sirea.Internal.B0Compile
import Sirea.Internal.Tuning (dtCompileFuture, dtFinalize, tAncient)
import Sirea.Time
import Sirea.Signal

--import Debug.Trace

-- TODO: Shift dynamic behavior updates into a future cycle (i.e. the
-- next step). This can allow compile and touch in the opening phase.

-- Evaluate a behavior provided dynamically.
--
-- If the evaluated behavior does not fit into dt seconds, it is
-- rejected and the error signal is returned. evalB reports a 
-- delay of dt seconds between inputs and y outputs, and a delay
-- of 0 between inputs and error.
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
evalB0 :: (SigInP p x, Monad m) => DT 
       -> B0 m (S p (B0 m x y) :&: x) (y :|: (S p () :&: x))
evalB0 dt = --trace "evalB" $
    -- synchronization and preparation (including stage 1 compile)
    synchB0 >>> forceDelayB0 >>> evalPrepB0 dt >>>
    -- after evalPrep, have (S p (Either (B0s1 m x y) ()) :&: x)
    firstB0 (splitB0 >>> mirrorB0 >>> leftB0 dupB0) >>> swapB0 >>>
    -- have (x :&: ((S p () :&: S p ()) :|: S p (B0s1 m x y)))
    disjoinB0 >>>
    -- have ((x :&: S p ()) :|: (x :&: S p (B0s1 m x y))
    leftB0 swapB0 >>> mirrorB0 >>> leftB0 swapB0 >>> 
    -- now have (S p (B0s1 m x y) :&: x) :|: (S p () :&: x); will eval on left
    leftB0 (evalFinalB0 dt)

-- apply first compilation stage to the input behavior, and filter
-- behaviors that won't fit into given time `dt`.
evalPrepB0 :: (Monad m) => DT 
           -> B0 m (S p (B0 m x y) :&: x) (S p (Either (B0s1 m x y) ()) :&: x)
evalPrepB0 dt = mkLnkB0 lcPrep mkLnEvalPrep where
    lcPrep lcbx = LnkProd ((lc_fwd . ln_fst) lcbx) (ln_snd lcbx)

mkLnEvalPrep :: (Monad m) 
             => DT -> LCaps m (S p (B0 m x y)) 
             -> Lnk m (S p (Either (B0 m x y) ()) :&: x)
             -> m (Lnk m (S p (B0 m x y) :&: x))
mkLnEvalPrep dt lcbx lnbx = return (LnkProd lnb' lnx) where
    lcb = ln_fst lcbx
    lcx = ln_snd lcbx
    lnb = ln_fst lnbx
    lnx = ln_snd lnbx
    dtFit = assert (evalSynched lcbx) $ dt + lc_maxCurr lcb
    lnb' = ln_lumap (ln_sfmap (fnEvalPrep dtFit lcx)) lnb

-- evalPrep will partially evaluate the inner behaviors, and decide 
-- whether they can be evaluated further (e.g. if they do not fit in
-- the alloted timeslot, they cannot be evaluated).
fnEvalPrep :: (Monad m) => DT -> LCaps m x -> B0 m x y -> Either (B0s1 m x y) ()
fnEvalPrep dtGoal lcx = evalFitDelay dtGoal . flip compileB0s1 lcx

-- evalFitDelay is applied to each dynamic behavior. If it succeeds,
-- we can guarantee the resulting delay is equal to dtGoal on every 
-- signal in y. If the behavior is too large to fit, we'll return
-- the '()' value indicating failure.
evalFitDelay :: (Monad m) => DT -> (B0s1 m x y, LCaps m y) -> Either (B0s1 m x y) ()
evalFitDelay dtGoal (b,lcy) = 
    if (lc_maxGoal lcy > dtGoal)
        then Right ()
        else Left (b >>> B0s1_mkLnk (return . fitGoal))
    where fitGoal = buildTshift lcy (lc_map toGoal lcy)
          toGoal lc = lc { lc_dtCurr = dtGoal, lc_dtGoal = dtGoal }

-- final evaluation step; assumes input B is valid at this point.
-- Will also add the delays.
evalFinalB0 :: (SigMembr x) => DT -> B0 m (S p (B0s1 m x y) :&: x) y
evalFinalB0 dt = mkLnkB0 lcEval buildEval where
    lcEval lcbx = 
        assert (evalSynched lcbx) $
        let dtInit = lc_maxCurr (ln_fst lcbx) in
        let dtFini = dtInit + dt in
        lc_map (setDelay dtFini) (lc_dupCaps lcbx)
    setDelay dt lc = lc { lc_dtCurr = dt, lc_dtGoal = dt }

-- are we synched for eval?
evalSynched :: LCaps m x -> Bool
evalSynched lc = (lc_valid lc) && (lc_maxGoal lc == lc_minCurr lc)

-- Data needed for the dynamic evaluator source
data Evaluator m x y = Evaluator 
    { ev_data     :: !(Ref m (EVD x y)) -- mutable state
    , ev_mergeLnk :: !(Int -> Lnk m y) -- remote links
    , ev_dynX     :: !(LnkW Dyn x) -- source links
    }
data EVD x y = EVD 
    { evd_signal   :: !(SigSt (B x y)) -- signal of dynamic behaviors
    , evd_compileT :: {-# UNPACK #-} !T -- how much of the signal has been compiled?
    , evd_nextKey  :: {-# UNPACK #-} !Int
    }

-- buildEval prepares the evaluator to receive and process inputs.
buildEval :: (SigMembr x, Monad m) => LCaps m x -> Lnk m y -> m (Lnk m (S p (B x y) :&: x))
buildEval dtx lnyFinal =
    assert (evalSynched dtx) $
    let evd0 = EVD st_zero tAncient in
    newIORef evd0 >>= \ rfEVD ->
    mkFullDyn dtx >>= \ dynX -> 
    mkMergeLnkFactory lnyFinal >>= \ mergeLnkY ->
    let ev = Evaluator rfEVD mergeLnkY dynX in
    let luB = LnkUp (touchEV ev) (updateEV ev) (idleEV ev) in
    let xLnk = fullDynToLnk (ev_dynX ev) in
    return (LnkProd (LnkSig luB) xLnk)


touchEV :: Evaluator x y -> IO ()
touchEV ev =
    readIORef (ev_data ev) >>= \ evd ->
    let st = evd_signal evd in
    unless (st_expect st) $
        let st' = st_poke st in
        let evd' = evd { evd_signal = st' } in
        writeIORef (ev_data ev) evd' >>
        dynBTouch (ev_dynX ev)

idleEV :: Evaluator x y -> StableT -> IO ()
idleEV ev tS = 
    readIORef (ev_data ev) >>= \ evd ->
    let st = evd_signal evd in
    assert (st_expect st) $
    let st' = st_idle tS st in
    let evd' = evd { evd_signal = st' } in
    writeIORef (ev_data ev) evd' >>
    updateDynamicsEV ev Nothing

updateEV :: Evaluator x y -> StableT -> T -> Sig (B x y) -> IO ()
updateEV ev tS tU su =
    readIORef (ev_data ev) >>= \ evd ->
    let st = evd_signal evd in
    assert (st_expect st) $
    let st' = st_update tS tU su st in
    let evd' = evd { evd_signal = st' } in
    writeIORef (ev_data ev) evd' >>
    updateDynamicsEV ev (Just tU)

-- The majority of the dynamic behavior logic is handled here, on
-- updating the signal that contains dynamic behaviors, or on the
-- remote end merging multiple signals that vary in time. 
--
-- The `Maybe T` value here indicates whether there was a recent
-- update to the signals list. If so, we might need to replaces the
-- speculative dynamic behaviors that were already compiled and are
-- already processing their speculative signals. If not, we still
-- must provide updates to dynamic behaviors that become visible due
-- to new stability updates. 
--
-- This operation will also perform GC of the behaviors signal.
updateDynamicsEV :: Evaluator x y -> Maybe T -> IO ()
updateDynamicsEV ev mbTU = updateBehaviors where
    updateBehaviors = 
        getBuildRangeEV ev mbTU >>= \ (tS,range) -> -- [(T,Maybe(B x y))]
        mapM compileE range >>= \ dynLnks -> -- [(T,Lnk x)]
        mapM_ (ln_touchAll . snd) dynLnks >> -- block premature merge
        dynBUpdate tS dynLnks (ev_dynX ev) -- update signal sources in x
    compileE (t,mb) =
        maybe (return LnkDead) compile mb >>= \ lnx ->
        return (t,lnx)
    compile b = newUnique >>= compileBC1 b . (ev_mergeLnk ev)

-- this operation will compute which behaviors need to be compiled
-- and installed. This set can be adjusted due to stability or to
-- an update. Will also perform GC and report stability, to reduce
-- state mutations
getBuildRangeEV :: Evaluator x y -> Maybe T -> IO (StableT,[(T,Maybe (B x y))])
getBuildRangeEV ev mbTU =
    readIORef (ev_data ev) >>= \ evd ->
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
    evd' `seq` writeIORef (ev_data ev) evd' >>
    return (tS,range)
  
-- filter x membrane for liveness of input source. (It is possible
-- that some inputs are dead, due to binl/binr upstream, though this
-- should be rare for dynamic behaviors.)
filterLS :: LnkD LDT x -> LnkW dd x -> LnkW dd x
filterLS (LnkDProd dt1 dt2) xy =
    let x' = filterLS dt1 $ ln_fst xy in
    let y' = filterLS dt2 $ ln_snd xy in
    LnkProd x' y'
filterLS (LnkDSum dtl dtr) xy =
    let x' = filterLS dtl $ ln_left xy in
    let y' = filterLS dtr $ ln_right xy in
    LnkSum x' y'
filterLS (LnkDUnit dtx) x =
    if (ldt_live dtx) then x else LnkDead

-- build a complex dynamic behavior state that reflects the signal type
-- (including LnkDead for cases where the input signal is dead-on-input)
mkFullDyn :: (SigMembr x) => LnkD LDT x -> IO (LnkW Dyn x)
mkFullDyn dtx = filterLS dtx <$> runMkDyn buildMembr

fullDynToLnk :: LnkW Dyn x -> Lnk x
fullDynToLnk LnkDead = LnkDead
fullDynToLnk (LnkProd x y) = LnkProd (fullDynToLnk x) (fullDynToLnk y)
fullDynToLnk (LnkSum x y) = LnkSum (fullDynToLnk x) (fullDynToLnk y)
fullDynToLnk (LnkSig x) = LnkSig (dynToLnkUp x)

dynToLnkUp :: Dyn x -> LnkUp x
dynToLnkUp dyn = LnkUp touch update idle where
    touch = dynSigTouch dyn
    update = dynSigUpdate dyn
    idle = dynSigIdle dyn

-- Dyn and DynSt are internal structures used for dynamic behaviors.
-- Each Dyn will join the input signal `x` with the active dynamic
-- behaviors.
--
-- TODO: consider an alternative structure for dyn_blink to simplify
-- the operations on it. 
newtype Dyn a = Dyn (IORef (DynSt a))
data DynSt a = DynSt 
    { dyn_signal :: !(SigSt a)   -- concrete input signal
    , dyn_tmup   :: !(Maybe T)   -- earliest update time (signal or behavior)
    , dyn_blink  :: ![(T,LnkUp a)] -- links prepared to receive signals.
    , dyn_bstable:: !StableT     -- stability of dynamic behavior
    , dyn_btouch :: !Bool        -- still expecting update to active links?
    }

dyn_zero :: DynSt a
dyn_zero = DynSt { dyn_signal   = st_zero 
                 , dyn_tmup     = Nothing
                 , dyn_blink    = []
                 , dyn_bstable  = StableT tAncient
                 , dyn_btouch   = False 
                 }
newDyn :: IO (Dyn a)
newDyn = Dyn <$> newIORef dyn_zero

newtype MkDyn a = MkDyn { runMkDyn :: IO (LnkW Dyn a) }
instance BuildMembr MkDyn where
    buildSigMembr = MkDyn $ LnkSig <$> newDyn
    buildSumMembr (MkDyn x) (MkDyn y) = MkDyn $ LnkSum <$> x <*> y
    buildProdMembr (MkDyn x) (MkDyn y) = MkDyn $ LnkProd <$> x <*> y

-- touch all signals in a complex dynamic behavior
dynBTouch :: LnkW Dyn a -> IO ()
dynBTouch = ln_forEach $ \ (Dyn rf) ->
    readIORef rf >>= \ dyn ->
    unless (dyn_btouch dyn) $
        let bTouchedX = st_expect (dyn_signal dyn) in
        let dyn' = dyn { dyn_btouch = True } in
        writeIORef rf dyn' >>
        unless bTouchedX (touchBL (dyn_blink dyn'))

-- touch for signal updates in dynamic behavior
dynSigTouch :: Dyn a -> IO ()
dynSigTouch (Dyn rf) = 
    readIORef rf >>= \ dyn ->
    let st = dyn_signal dyn in
    unless (st_expect st) $
        let st' = st_poke st in
        let bTouchedB = dyn_btouch dyn in
        let dyn' = dyn { dyn_signal = st' } in
        writeIORef rf dyn' >>
        unless bTouchedB (touchBL (dyn_blink dyn'))

touchBL :: [(T,LnkUp a)] -> IO ()
touchBL bl = mapM_ (ln_touch . snd) bl

dynSigIdle :: Dyn a -> StableT -> IO ()
dynSigIdle (Dyn rf) tS =
    readIORef rf >>= \ dyn ->
    let st = dyn_signal dyn in
    assert (st_expect st) $
    let st' = st_idle tS st in
    let dyn' = dyn { dyn_signal = st' } in
    writeIORef rf dyn' >>
    dynMaybeEmit rf

dynSigUpdate :: Dyn a -> StableT -> T -> Sig a -> IO ()
dynSigUpdate (Dyn rf) tS tU su =
    readIORef rf >>= \ dyn ->
    let st = dyn_signal dyn in
    assert (st_expect st) $
    let st' = st_update tS tU su st in
    let tmup' = Just $! maybe tU (min tU) (dyn_tmup dyn) in
    let dyn' = dyn { dyn_signal = st', dyn_tmup = tmup' } in
    writeIORef rf dyn' >>
    dynMaybeEmit rf

-- update the dynamic behaviors. The links specific to each
-- concrete underlying signal are stored with that signal. 
-- The important argument is [(T,Lnk x)] - a list of compiled
-- links with the times they are supposed to receive information.
dynBUpdate :: StableT -> [(T,Lnk x)] -> LnkW Dyn x -> IO ()
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
dynBUpdate tm bl (LnkSig (Dyn rf)) =
    let blu = map (second ln_lnkup) bl in
    dynUpdateLinks rf tm blu >>
    dynMaybeEmit rf
dynBUpdate _ bl LnkDead =
    -- we've touched these but won't update them; freeze them! 
    mapM_ (ln_freeze . snd) bl 

-- when we update the links associated with a behavior, we may need
-- to kill prior links associated with this behavior. 
dynUpdateLinks :: IORef (DynSt a) -> StableT -> [(T,LnkUp a)] -> IO ()
dynUpdateLinks rf tS [] =
    readIORef rf >>= \ dyn ->
    assert (dyn_btouch dyn) $
    let dyn' = dyn { dyn_bstable = tS, dyn_btouch = False } in
    writeIORef rf dyn'
dynUpdateLinks rf tS blu@(b:_) =
    readIORef rf >>= \ dyn ->
    assert (dyn_btouch dyn) $
    --traceIO ("dynUpdateLinks: " ++ show (map fst blu)) >>
    let tU = fst b in
    let tmup' = Just $! maybe tU (min tU) (dyn_tmup dyn) in
    let (blKeep,blKill) = span ((< tU) . fst) (dyn_blink dyn) in
    let blink' = blKeep ++ blu in
    let dyn' = dyn { dyn_bstable = tS, dyn_btouch = False
                   , dyn_blink = blink', dyn_tmup = tmup' } 
    in
    writeIORef rf dyn' >>
    mapM_ (terminate tU . snd) blKill -- destroy replaced behaviors

-- when we switch to a new dynamic behavor, we kill behaviors that
-- are no longer relevant. This recants signals starting at time tm.
terminate :: T -> LnkUp a -> IO ()
terminate tCut lu = ln_update lu DoneT tCut s_never

-- decision point for processing updates. The actual updates are not
-- trivial, mostly due to the finalization semantics for old links.
dynMaybeEmit :: IORef (DynSt a) -> IO ()
dynMaybeEmit rf = 
    readIORef rf >>= \ dyn ->
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
        dyn' `seq` writeIORef rf dyn' >>
        mapM_ bl_task blUpd

dynExpect :: DynSt a -> Bool
dynExpect dyn = dyn_btouch dyn || st_expect (dyn_signal dyn)

dynStable :: DynSt a -> StableT
dynStable dyn = min (dyn_bstable dyn) ((st_stable . dyn_signal) dyn)

data BLUP x = BLUP 
    { bl_link :: !(T,LnkUp x) 
    , bl_drop :: !Bool
    , bl_task :: (IO ())
    }

-- GC the blUpd list
cleanBl :: [BLUP x] -> [(T,LnkUp x)]
cleanBl = map bl_link . dropWhile bl_drop

-- Stability isn't entirely trivial, mostly because some elements 
-- become "permanently" stable once stability surpasses the start
-- of the next element's term. (I.e. they become inactive forever.)
loadStable :: StableT -> [(T,LnkUp x)] -> [BLUP x]
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
loadUpdate :: StableT -> T -> Sig x -> [(T,LnkUp x)] -> [BLUP x]
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
mkMergeLnkFactory :: Lnk y -> IO (Unique -> Lnk y)
mkMergeLnkFactory LnkDead = 
    return (const LnkDead)
mkMergeLnkFactory (LnkProd f s) = 
    mkMergeLnkFactory f >>= \ mkF ->
    mkMergeLnkFactory s >>= \ mkS ->
    let mkProd k = LnkProd (mkF k) (mkS k) in
    return mkProd
mkMergeLnkFactory (LnkSum l r) =
    mkMergeLnkFactory l >>= \ mkL ->
    mkMergeLnkFactory r >>= \ mkR ->
    let mkSum k = LnkSum (mkL k) (mkR k) in
    return mkSum
mkMergeLnkFactory (LnkSig lu) =
    mkMergeLnk >>= \ mln -> -- state to perform the merges.
    let mkLnk = LnkSig . fnMergeEval mln lu in
    return mkLnk

-- MergeLnk is the state to perform merges of results from multiple
-- behaviors. It uses a hashtable internally, and counts touches, to
-- keep algorithmic costs down.
newtype MergeLnk a = MergeLnk { mln_data :: IORef (MLD a) }
data MLD a = MLD
    { mld_touchCt  :: {-# UNPACK #-} !Int
    , mld_table    :: !(M.Map Unique (SigSt a))
    , mld_tmup     :: !(Maybe T)
    }

mkMergeLnk :: IO (MergeLnk a)
mkMergeLnk = MergeLnk <$> newIORef mldZero

mldZero :: MLD a
mldZero = MLD 
    { mld_touchCt = 0
    , mld_table   = M.empty
    , mld_tmup    = Nothing
    }

mld_getSt :: MLD a -> Unique -> SigSt a
mld_getSt mld k = fromMaybe st_zero $ M.lookup k (mld_table mld)

-- touch key (if not already touched)
mld_touch :: MLD a -> Unique -> MLD a
mld_touch mld k =
    let st = mld_getSt mld k in
    if st_expect st then mld else 
    let st' = st_poke st in
    let tbl' = M.insert k st' (mld_table mld) in
    let tc' = succ (mld_touchCt mld) in
    mld { mld_touchCt = tc', mld_table = tbl' }

mld_idle :: MLD a -> Unique -> StableT -> MLD a
mld_idle mld k tS =
    let st = mld_getSt mld k in
    assert (st_expect st) $
    let st' = st_idle tS st in
    let tbl' = M.insert k st' (mld_table mld) in
    let tc' = pred (mld_touchCt mld) in
    mld { mld_touchCt = tc', mld_table = tbl' }

mld_update :: MLD a -> Unique -> StableT -> T -> Sig a -> MLD a
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
fnMergeEval :: MergeLnk a -> LnkUp a -> Unique -> LnkUp a
fnMergeEval mln lu k = LnkUp touch update idle where
    touch = 
        readIORef (mln_data mln) >>= \ mld ->
        let bFirstTouch = (0 == mld_touchCt mld) in 
        let mld' = mld_touch mld k in
        writeIORef (mln_data mln) mld' >>
        when bFirstTouch (ln_touch lu)
    idle tS =
        readIORef (mln_data mln) >>= \ mld ->
        let mld' = mld_idle mld k tS in
        let bLastUpdate = (0 == mld_touchCt mld') in
        writeIORef (mln_data mln) mld' >>
        when bLastUpdate emit
    update tS tU su = 
        readIORef (mln_data mln) >>= \ mld ->
        let mld' = mld_update mld k tS tU su in
        let bLastUpdate = (0 == mld_touchCt mld') in
        writeIORef (mln_data mln) mld' >>
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
emitMergedSignal :: MergeLnk a -> LnkUp a -> IO ()
emitMergedSignal mln lu =
    readIORef (mln_data mln) >>= \ mld ->
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
    mld' `seq` writeIORef (mln_data mln) mld' >>
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


