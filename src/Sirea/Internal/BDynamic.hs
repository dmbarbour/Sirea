{-# LANGUAGE TypeOperators, GADTs #-}

-- | Implementation of the Dynamic behavior type for B.
-- Evaluation will logically sync the inputs, then 
-- physically sync the outputs (y). The inputs are not
-- immediately 
module Sirea.Internal.BDynamic 
    ( evalB
    ) where

import Prelude hiding(id,(.))
import Control.Category
import Control.Monad (unless, when, void)
import Control.Applicative
import Control.Exception (assert)
import Control.Arrow (second)
import Data.IORef
import Data.List (foldl')
import Data.Maybe (isNothing)
import qualified Data.HashTable as HT
import Sirea.Internal.BTypes
import Sirea.Internal.STypes
import Sirea.Internal.LTypes
import Sirea.Internal.BImpl 
import Sirea.Internal.BCompile
import Sirea.Time
import Sirea.Signal
--import Sirea.Internal.BImpl

-- how far ahead should we establish behaviors?
-- should make this dynamic, based on observed stability.
dt_compile_future :: DT
dt_compile_future = 6 -- seconds into future 

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
evalB :: (SigInP p x) => DT -> B w (S p (B w x y) :&: x) (y :|: (S p () :&: x))
evalB dt = 
    -- synchronization and preparation (& initial compile step)
    synchB >>> forceDelayB >>> evalPrepB dt >>>
    -- after evalPrep, have (S p (Either (B w x y) ()) :&: x)
    B_first (splitB >>> mirrorB >>> B_left dupB) >>> swapB >>>
    -- have (x :&: ((S p () :&: S p ()) :|: S p (B w x y)))
    disjoinB >>>
    -- have ((x :&: S p ()) :|: (x :&: S p (B w x y))
    B_left swapB >>> mirrorB >>> B_left swapB >>> 
    -- now have (S p (B w x y) :&: x) :|: (S p () :&: x); will eval on left
    B_left (evalFinalB dt)

-- apply first compilation stage to the input behavior, and filter
-- behaviors that won't fit into given time `dt`.
evalPrepB :: DT -> B w (S p (B w x y) :&: x) ((S p (Either (B w x y) ())) :&: x)
evalPrepB dt = B_latent $ \ tbx ->
    assert (evalSynched tbx) $
    let tx = lnd_snd tbx in
    B_first (fmapB (evalPrep dt tx))

-- final evaluation step; assumes input B is valid at this point.
evalFinalB :: (SigMembr x) => DT -> B w (S p (B w x y) :&: x) y
evalFinalB dt = B_latent $ \ tbx ->
    let tx = lnd_snd tbx in
    B_mkLnk (trEval dt) (mkLnkEval tx)

-- trEval reports the delay incurred by the eval process
-- i.e. so that operations using `y` are timed properly.
-- (actually causing this delay is left to evalFinalB.
-- Here, the delay is only reported.)
trEval :: DT -> LnkD LDT x -> LnkD LDT y
trEval dt t0 = 
    assert (evalSynched t0) $
    let dt0 = ldt_maxCurr t0 in
    let dtf = dt0 + dt in
    trDTF dtf t0

-- evalPrep will partially evaluate the inner behaviors, and decide 
-- whether they can be evaluated further (e.g. if they do not fit in
-- the alloted timeslot, they cannot be evaluated).
--
-- The output `B` is not quite the same as the input `B` due to the
-- first compilation phase eliminating B_latent expressions, and
-- applying any fixed delays and dead-code-on-input optimizations.
-- Basically, the `B` after this point is highly context dependent.
evalPrep :: DT -> LnkD LDT x -> B w x y -> Either (B w x y) ()
evalPrep dt ldtx =
    assert (evalSynched ldtx) $
    evalFitDelay dtf . precompile
    where dt0 = ldt_maxCurr ldtx
          dtf = dt0 + dt 
          precompile = flip compileBC0 ldtx

-- evalFitDelay is applied to each dynamic behavior. If it succeeds,
-- we can guarantee the resulting delay is equal to dtf on every value
-- entering `y`. It may fail if the behavior is too large for dtf.
evalFitDelay :: DT -> (B w x y, LnkD LDT y) -> Either (B w x y) ()
evalFitDelay dtf (b,t0) =
    assert (ldt_valid t0) $
    if (ldt_maxGoal t0 > dtf) 
        then Right () -- cannot fit to delay
        else Left (b >>> delayToFit) 
    where tfn = trDTF dtf
          delayToFit = B_mkLnk tfn lnkDTF
          lnkDTF = MkLnk { ln_build = return . buildTshift t0 (tfn t0)
                         , ln_tsen = False, ln_peek = 0 }

-- "delay to fit" a particular time
trDTF :: DT -> LnkD LDT x -> LnkD LDT y
trDTF dtf t0 = 
    assert (ldt_valid t0) $
    assert (dtf >= ldt_maxGoal t0) $
    LnkDUnit $ LDT { ldt_curr = dtf
                   , ldt_goal = dtf
                   , ldt_live = ldt_anyLive t0 }

-- a sanity test for assertions; eval has some stringent
-- synchronization requirements, but should achieve them
-- internally. 
evalSynched :: LnkD LDT x -> Bool
evalSynched ldt =
    (ldt_valid ldt) &&
    (ldt_maxGoal ldt == ldt_minGoal ldt) &&
    (ldt_maxCurr ldt == ldt_minCurr ldt) &&
    (ldt_maxGoal ldt == ldt_minCurr ldt)

mkLnkEval :: (SigMembr x) => LnkD LDT x -> MkLnk w (S p (B w x y) :&: x) y
mkLnkEval dtx = MkLnk { ln_build = buildEval dtx
                      , ln_tsen = True, ln_peek = 0 }

-- buildEval prepares the evaluator to receive and process inputs.
buildEval :: (SigMembr x) => LnkD LDT x -> Lnk y -> IO (Lnk (S p (B w x y) :&: x))
buildEval dtx lnyFinal = 
    assert (evalSynched dtx) $
    -- need to store the input signals
    newIORef st_zero >>= \ rfBSig -> -- present and future dynamic behaviors
    newIORef Nothing >>= \ rfTt -> -- until when has this been compiled?
    mkFullDyn dtx >>= \ dynX -> -- store and process the `x` signals
    let xLnk = fullDynToLnk dynX in -- Lnk x
    -- use indexed `Lnk y` to merge responses from dynamic behaviors
    let arbitraryStartingIndex = 10000 in
    newIORef arbitraryStartingIndex >>= \ rfIdx -> 
    mkMergeLnkFactory lnyFinal >>= \ mkLny ->
    let compile b = takeIdx rfIdx >>= compileBC1 b . mkLny in
    let compileE (t,mb) =
            maybe (return LnkDead) compile mb >>= \ lx ->
            return (t,lx)
    in  
    -- install updates to the dynamic behavior
    let touch_bsig = 
            readIORef rfBSig >>= \ st ->
            unless (st_expect st) $ -- prevent double touch
                let st' = st_poke st in
                writeIORef rfBSig st' >>
                dynBTouch dynX
    in
    let update_bsig su = 
            touch_bsig >> -- touch (in case not already touched)
            stSigupRef rfBSig su >>= \ st' -> -- update signal & clear touch
            let tu = fmap snd (su_state su) in
            buildUpdateRange rfTt tu st' >>= \ range -> -- compute compilation target
            mapM compileE range >>= \ dynLnks -> -- [(T,Lnk x)]
            dynBUpdate (su_stable su) dynLnks dynX 
    in
    let lub = LnkUp { ln_touch = touch_bsig, ln_update = update_bsig } in
    return (LnkProd (LnkSig lub) xLnk)

-- update & GC the signal held by reference; return intermediate
stSigupRef :: IORef (SigSt a) -> SigUp a -> IO (SigSt a)
stSigupRef rf su = 
    readIORef rf >>= \ st ->
    let st' = st_sigup su st in -- st' is updated; stc is GC'd
    let stc = maybe st_zero (`st_clear` stc) (st_stable st') in
    st' `seq` stc `seq` writeIORef rf stc >>
    return st'

-- compute which elements to compile based on time and update time
buildUpdateRange :: IORef (Maybe T) -> (Maybe T) -> SigSt a -> IO [(T,Maybe a)]
buildUpdateRange rfTt tu st' =
    readIORef rfTt >>= \ tUpperLast ->
    let tStable = st_stable st' in
    let tLower = leastTime tUpperLast tu in
    let tUpper = fmap (`addTime` dt_compile_future) (tStable <|> tu) in
    writeIORef rfTt tUpper >>
    let times = (,) <$> tLower <*> tUpper in
    let range = case times of
            Just (tLo,tHi) ->
                if (tLo > tHi) then [] else
                let sl = sigToList (st_signal st') tLo tHi in
                if (tu == tLower) then sl else tail sl
            Nothing -> []
    in 
    return range

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
dynToLnkUp dyn = LnkUp { ln_touch = dynSigTouch dyn 
                       , ln_update = dynSigUpdate dyn }

-- Dyn and DynSt are internal structures used for dynamic behaviors.
-- Each Dyn will join the input signal `x` with the active dynamic
-- behaviors.
--
-- TODO: consider an alternative structure for dyn_blink to simplify
-- the operations on it. 
newtype Dyn a = Dyn (IORef (DynSt a))
data DynSt a = DynSt 
    { dyn_sigst  :: !(SigSt a)     -- concrete input signal
    , dyn_tmupd  :: !(Maybe T)     -- earliest update time (between blink & sigst)
    , dyn_blink  :: ![(T,LnkUp a)] -- active links to receive signals
    , dyn_bstable:: !(Maybe T)     -- current blink stability
    , dyn_btouch :: !Bool          -- expecting update to active links?
    }

dyn_zero :: DynSt a
dyn_zero = DynSt { dyn_sigst = st_poke st_zero
                 , dyn_tmupd = Nothing
                 , dyn_blink = []
                 , dyn_bstable = Nothing
                 , dyn_btouch = True 
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
        let dyn' = dyn { dyn_btouch = True } in
        writeIORef rf dyn' >>
        unless ((st_expect . dyn_sigst) dyn') 
            (mapM_ (ln_touch . snd) (dyn_blink dyn'))

-- touch for signal updates in dynamic behavior
dynSigTouch :: Dyn a -> IO ()
dynSigTouch (Dyn rf) = 
    readIORef rf >>= \ dyn ->
    unless ((st_expect . dyn_sigst) dyn) $
        let sigst' = (st_poke . dyn_sigst) dyn in
        let dyn' = dyn { dyn_sigst = sigst' } in
        writeIORef rf dyn' >>
        unless (dyn_btouch dyn')
            (mapM_ (ln_touch . snd) (dyn_blink dyn'))

dynSigUpdate :: Dyn a -> SigUp a -> IO ()
dynSigUpdate (Dyn rf) su =
    readIORef rf >>= \ dyn ->
    let sigst' = (st_sigup su . dyn_sigst) dyn in
    let tmupd' = leastTime (dyn_tmupd dyn) ((fmap snd . su_state) su) in
    let dyn' = dyn { dyn_tmupd = tmupd', dyn_sigst = sigst' } in
    writeIORef rf dyn' >>
    dynMaybeEmit rf

-- update the dynamic behaviors globally, given a new stability value
-- and a (finite, possibly empty) list of present and future behaviors. 
dynBUpdate :: (Maybe T) -> [(T,Lnk x)] -> LnkW Dyn x -> IO ()
dynBUpdate _ bl LnkDead = mapM_ (ln_freeze . snd) bl 
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
    dynBUpdateLinks tm blu rf >>
    dynMaybeEmit rf

-- update the [(T,LnkUp a)] sequence for a particular input signal.
dynBUpdateLinks :: (Maybe T) -> [(T,LnkUp a)] -> IORef (DynSt a) -> IO ()
dynBUpdateLinks tStable [] rf =
    -- no changes to links or update time
    readIORef rf >>= \ dyn ->
    let dyn' = dyn { dyn_bstable = tStable
                   , dyn_btouch = False }  in
    writeIORef rf dyn'
dynBUpdateLinks tStable bl@((tu,_):_) rf =
    readIORef rf >>= \ dyn ->
    let tmupd' = leastTime (Just tu) (dyn_tmupd dyn) in
    let blink' = updateList (dyn_blink dyn) tu bl in
    let oldTail = filter ((>= tu) . fst) (dyn_blink dyn) in
    let dyn' = dyn { dyn_bstable = tStable
                   , dyn_btouch = False
                   , dyn_blink = blink'
                   , dyn_tmupd = tmupd'   } in
    writeIORef rf dyn' >>
    mapM_ (terminate tu . snd) oldTail

terminate :: T -> LnkUp x -> IO ()
terminate tm lu = ln_update lu sigTerminate
    where sigTerminate = SigUp { su_state = Just (s_never,tm)
                               , su_stable = Nothing }

updateList :: (Ord t) => [(t,a)] -> t -> [(t,a)] -> [(t,a)]
updateList [] _ nt = nt
updateList (hd:tl) tm nt = 
    if (fst hd < tm) then hd:(updateList tl tm nt)
                     else assert (tm == (fst . head) nt) $ nt


-- need to process signal updates, then garbage-collect any data
-- that is not necessary for future updates.
dynMaybeEmit :: IORef (DynSt a) -> IO ()
dynMaybeEmit rf = 
    readIORef rf >>= \ dyn ->
    let bExpectSig = (st_expect . dyn_sigst) dyn in
    let bExpectB = dyn_btouch dyn in
    unless (bExpectSig || bExpectB) $
        let st = dyn_sigst dyn in
        let bl = dyn_blink dyn in
        let tUpdate = dyn_tmupd dyn in
        let tStable = leastTime (dyn_bstable dyn) (st_stable st) in
        -- do some list processing to match a SigUp to each LnkUp!
        let blTimes  = map fst bl in 
        let trimSignals tu = bl_signal tu (st_signal st) blTimes in
        let blSignal = maybe (repeat Nothing) trimSignals tUpdate in
        let blStable = maybe (repeat Nothing) (`bl_stable` blTimes) tStable in
        let blSigUp  = zipWith SigUp blSignal blStable in
        let blUpdates = zip bl blSigUp in -- list of ((T,LnkUp),SigUp) pairs.
        -- can GC any elements that are now 100% stable. 
        let bl' = (map fst . dropWhile (isNothing . su_stable . snd)) blUpdates in
        let st' = maybe st_zero (`st_clear` st) tStable in
        let dyn' = dyn { dyn_tmupd = Nothing, dyn_sigst = st', dyn_blink = bl' } in
        dyn' `seq` writeIORef rf dyn' >> -- save GC'd version of dyn
        mapM_ emitUpdate blUpdates

-- final send operation.
emitUpdate :: ((t,LnkUp a),SigUp a) -> IO ()
emitUpdate ((_,lu),su) = ln_update lu su

-- extract a series of update signals.
bl_signal :: T -> Sig a -> [T] -> [Maybe (Sig a,T)]
bl_signal _ _ [] = []
bl_signal tu sf (tf:[]) = -- final signal
    let tt  = max tu tf in
    let sfTrim = s_trim sf tt in
    (Just (sfTrim,tt)):[]
bl_signal tu sk (tLo:tHi:ts) = -- internal signal
    if (tu >= tHi) 
        then Nothing : (bl_signal tu sk (tHi:ts)) 
        else let tt = max tu tLo in
             let skTrim = s_trim sk tt in
             let skCutHi = s_switch skTrim tHi s_never in
             (Just (skCutHi,tt)) : (bl_signal tu skTrim (tHi:ts))

-- extract stability values for the next round of updates.
-- all signals have the same stability, independent of update.
bl_stable :: (Ord t) => t -> [t] -> [Maybe t]
bl_stable _ [] = []
bl_stable tStable (tf:[]) = (Just tStable):[]
bl_stable tStable (tLo:tHi:ts) =
    let more = bl_stable tStable (tHi:ts) in
    if (tStable >= tHi) then Nothing:more
                        else (Just tStable):more


-- extract a series of stability values
    

{-
    let (cancelOldLinks, blink') = replaceLinks tm bl 
    let (cancelOldLinks, blink') = prepCancel st blu in
    bluSwap tm (dyn_blink st) blu >>= \ blink' ->
    let st' = st { dyn_blink = blu'
                 , dyn_tmupd = tmupd'
                 , dyn_bstable = tm
                 , dyn_btouch = False } in
    writeIORef rf st' >>
    mapM_ (c tb . snd)
    dynEmit rf
-}


-- when should links be canceled?
--   (a) it is possible that `tb` will reduce further, later
--   (b) the change is only "stable forever"
--   (b) the first element of the cancellation ca



-- filter x membrane for liveness of input source.
-- (It is possible that some inputs are dead, due to
-- binl/binr, though this should be rare for dynamic
-- behaviors.)
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

-- a trivial function for sequential indexes
takeIdx :: IORef Int -> IO Int
takeIdx rf =
    readIORef rf >>= \ n0 ->
    let n = succ n0 in
    n `seq` 
    writeIORef rf n >>
    return n


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
mkMergeLnkFactory :: Lnk y -> IO (Int -> Lnk y)
mkMergeLnkFactory LnkDead = 
    return (const LnkDead)
mkMergeLnkFactory (LnkProd f s) = 
    mkMergeLnkFactory f >>= \ mkF ->
    mkMergeLnkFactory s >>= \ mkS ->
    let mkProd n = 
            let f' = mkF n in
            let s' = mkS n in
            (LnkProd f' s')
    in return mkProd
mkMergeLnkFactory (LnkSum l r) =
    mkMergeLnkFactory l >>= \ mkL ->
    mkMergeLnkFactory r >>= \ mkR ->
    let mkSum n = 
            let l' = mkL n in
            let r' = mkR n in
            (LnkSum l' r')
    in return mkSum
mkMergeLnkFactory (LnkSig lu) =
    mkMergeLnk >>= \ mln -> -- state to perform the merges.
    let mkLnk = LnkSig . fnMergeEval mln lu in
    return mkLnk

-- MergeLnk is the state to perform merges of results from multiple
-- behaviors. It uses a hashtable internally, and counts touches, to
-- keep algorithmic costs down.
data MergeLnk a = MergeLnk 
    { mln_touch_ct :: IORef Int -- count of touches
    , mln_table :: HT.HashTable Int (SigSt a)
    , mln_tmupd :: IORef (Maybe T)
    }
mkMergeLnk :: IO (MergeLnk a)
mkMergeLnk = MergeLnk <$> newIORef 0 
                      <*> HT.new (==) fromIntegral
                      <*> newIORef Nothing

mln_get :: MergeLnk a -> Int -> IO (SigSt a)
mln_get mln n = maybe st_zero id `fmap` HT.lookup (mln_table mln) n

mln_put :: MergeLnk a -> Int -> SigSt a -> IO ()
mln_put mln n st = void $ HT.update (mln_table mln) n st

-- touch, and return whether this is the first touch
mln_touch :: MergeLnk a -> Int -> IO Bool
mln_touch mln n =
    mln_get mln n >>= \ st ->
    if (st_expect st) then return False else
    let st' = st_poke st in
    mln_put mln n st' >>
    readIORef (mln_touch_ct mln) >>= \ ct ->
    let ct' = succ ct in
    writeIORef (mln_touch_ct mln) ct' >>
    (return $! (1 == ct'))

-- update, and return whether this is the last expected update
mln_update :: MergeLnk a -> Int -> SigUp a -> IO Bool
mln_update mln n su =
    -- update the associated signal
    mln_get mln n >>= \ st ->
    let st' = st_sigup su st in
    mln_put mln n st' >>
    -- update the associated time
    readIORef (mln_tmupd mln) >>= \ tm ->
    let tm' = leastTime tm (fmap snd (su_state su)) in
    tm' `seq` writeIORef (mln_tmupd mln) tm' >>
    -- test touch count, potentially update if was touched.
    readIORef (mln_touch_ct mln) >>= \ ct ->
    let ct' = if (st_expect st) then (pred ct) else ct in
    writeIORef (mln_touch_ct mln) ct' >>
    (return $! (0 == ct'))

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
fnMergeEval :: MergeLnk a -> LnkUp a -> Int -> LnkUp a
fnMergeEval mln lu idx = LnkUp { ln_touch = touch, ln_update = update }
    where touch = 
            mln_touch mln idx >>= \ bFirstTouch ->
            when bFirstTouch (ln_touch lu)
          update su = 
            mln_update mln idx su >>= \ bLastExpectedUpdate ->
            when bLastExpectedUpdate emitMergedSignal
          emitMergedSignal =
            -- operate on elements as collection.
            HT.toList (mln_table mln) >>= \ lSt -> -- original state of hashtable
            let tmStable = foldl' leastTime Nothing $ map (st_stable . snd) lSt in
            mapM_ (mergeEvalGC mln tmStable) lSt >> -- manual garbage collection of history
            readIORef (mln_tmupd mln) >>= \ tmUpd -> -- time of update
            writeIORef (mln_tmupd mln) Nothing >>
            case tmUpd of
                Nothing -> -- stability update only
                    let su = SigUp { su_state = Nothing
                                   , su_stable = tmStable } in
                    ln_update lu su
                Just tu -> -- full signal update
                    let sigMerged = foldr s_merge s_never $ map (st_signal . snd) lSt in
                    let sigTrimmed = s_trim sigMerged tu in
                    let su = SigUp { su_state = Just (sigTrimmed, tu)
                                   , su_stable = tmStable } in
                    ln_update lu su

-- need to GC the hashtable based on stability.
mergeEvalGC :: MergeLnk a -> Maybe T -> (Int,SigSt a) -> IO ()
mergeEvalGC mln Nothing (idx,_) = 
    HT.delete (mln_table mln) idx
mergeEvalGC mln (Just tm) (idx,st) = 
    let (x,s') = s_sample (st_signal st) tm in
    if (isNothing x && s_is_final s' tm)
        then HT.delete (mln_table mln) idx
        else let st' = st { st_signal = s' } in
             st' `seq` 
             void (HT.update (mln_table mln) idx st')


-- leastTime where `Nothing` is forever (upper bound)
leastTime :: Maybe T -> Maybe T -> Maybe T
leastTime l r = (min <$> l <*> r) <|> l <|> r



