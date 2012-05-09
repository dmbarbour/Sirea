{-# LANGUAGE TypeOperators, GADTs #-} 

-- | BImpl has the implementation details for most of Sirea's
-- concrete behaviors, mostly because I did not want them cluttering
-- FRP.Sirea.Behavior (which is about documenting typeclasses).
--
-- This includes both the symbolic and concrete implementations for
-- basic behaviors except for `first` and `left`, which need special 
-- treatment at compilation. 
--
-- Exposed behaviors for users will be re-exported elsewhere.
module FRP.Sirea.Internal.BImpl
    ( fwdB
    , fstB, firstB, swapB, assoclpB, dupB, zapB -- BProd 
    , inlB, leftB, mirrorB, assoclsB, mergeB, splitB -- BSum
    , disjoinB
    , fmapB, constB, touchB, stratB, adjeqfB -- BFmap
    , delayB, synchB, peekB, tshiftB, forceDelayB -- temporal

    -- miscellaneous
    , unsafeAddStabilityB 
    , unsafeEqShiftB
    , unsafeFullMapB
    , undeadB, sparkOfLifeB
    ) where

import Prelude hiding (id,(.))
import FRP.Sirea.Internal.STypes
import FRP.Sirea.Internal.LTypes
import FRP.Sirea.Internal.BTypes
import FRP.Sirea.Time
import FRP.Sirea.Signal

import Control.Category
import Control.Applicative
import Control.Parallel (pseq)
import Control.Parallel.Strategies (Eval, runEval, evalList, rseq)
import Control.Exception (assert)
import Data.Function (on)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)

import qualified Data.List as L

instance Category B where
  id  = fwdB
  (.) = flip B_pipe


firstB :: B x x' -> B (x :&: y) (x' :&: y)
firstB = B_first

leftB :: B x x' -> B (x :|: y) (x' :|: y)
leftB = B_left

mkLnkPure :: (Lnk y -> Lnk x) -> MkLnk x y
mkLnkPure = mkLnkSimp . (return .)

mkLnkSimp :: (Lnk y -> IO (Lnk x)) -> MkLnk x y
mkLnkSimp build = MkLnk { ln_tsen = False 
                        , ln_peek = 0
                        , ln_build = build
                        }

mkLnkB :: TR x y -> MkLnk x y -> B x y
mkLnkB = B_mkLnk

-- fwdB is the simplest behavior...
fwdB :: B x x 
fwdB = mkLnkB id $ mkLnkPure id

-- in fstB, snd output is dead. 
fstB :: B (x :&: y) x
fstB = mkLnkB lnd_fst $ mkLnkPure lnFst
    where lnFst ln = LnkProd ln LnkDead 

-- simple swap on Lnk sinks
swapB :: B (x :&: y) (y :&: x)
swapB = mkLnkB trSwap $ mkLnkPure lnSwap
    where trSwap tr = LnkDProd (lnd_snd tr) (lnd_fst tr)
          lnSwap ln = LnkProd (ln_snd ln) (ln_fst ln)

-- simple rearrangement like swap            
assoclpB :: B (x :&: (y :&: z)) ((x :&: y) :&: z)
assoclpB = mkLnkB trRotl $ mkLnkPure lnAso
    where trRotl tr = 
            let tx = lnd_fst tr in
            let ty = (lnd_snd >>> lnd_fst) tr in
            let tz = (lnd_snd >>> lnd_snd) tr in
            LnkDProd (LnkDProd tx ty) tz
          lnAso ln =
            let x = (ln_fst >>> ln_fst) ln in
            let y = (ln_fst >>> ln_snd) ln in
            let z = ln_snd ln in
            LnkProd x (LnkProd y z)


-- deep-duplicate signals (at least where two sinks are available)
dupB :: B x (x :&: x)
dupB = mkLnkB trDup $ mkLnkPure lnDup
    where trDup tr = LnkDProd tr tr -- duplicate timing properties
          lnDup ln = lnDeepDup (ln_fst ln) (ln_snd ln)

-- deep duplicate signal updates, except for dead output.
lnDeepDup :: Lnk x -> Lnk x -> Lnk x
lnDeepDup x LnkDead = x
lnDeepDup LnkDead y = y
lnDeepDup (LnkProd x1 y1) (LnkProd x2 y2) =
    let x = lnDeepDup x1 x2 in
    let y = lnDeepDup y1 y2 in
    LnkProd x y
lnDeepDup (LnkSum x1 y1) (LnkSum x2 y2) =
    let x = lnDeepDup x1 x2 in
    let y = lnDeepDup y1 y2 in
    LnkSum x y
lnDeepDup (LnkSig x) (LnkSig y) =
    let touch = ln_touch x >> ln_touch y in
    let update su = ln_update x su >> ln_update y su in
    LnkSig $ LnkUp { ln_touch = touch, ln_update = update } 
           
-- if inl, can ignore the right bucket
inlB :: B x (x :|: y)
inlB = mkLnkB trinl $ mkLnkPure ln_left
    where trinl tr = LnkDSum tr (tr_dead tr)

-- the `time` of the RHS signal is dead when we're `binl`
tr_dead :: LnkD LDT x -> LnkD LDT y
tr_dead x = LnkDUnit ldtDead
    where ldtDead = LDT { ldt_curr = ldt_maxCurr x
                        , ldt_goal = ldt_maxGoal x
                        , ldt_live = False 
                        }

-- simple rearrangement
mirrorB :: B (x :|: y) (y :|: x)
mirrorB = mkLnkB trMirr $ mkLnkPure lnMirr 
    where trMirr tr = LnkDSum (lnd_right tr) (lnd_left tr)
          lnMirr ln = LnkSum (ln_right ln) (ln_left ln)

-- simple rearrangement
assoclsB :: B (x :|: (y :|: z)) ((x :|: y) :|: z)
assoclsB = mkLnkB trRotl $ mkLnkPure lnAso
    where trRotl tr =
            let tx = lnd_left tr in
            let ty = (lnd_right >>> lnd_left) tr in
            let tz = (lnd_right >>> lnd_right) tr in
            LnkDSum (LnkDSum tx ty) tz
          lnAso ln = -- here `ln` is output
            let x = (ln_left >>> ln_left) ln in
            let y = (ln_left >>> ln_right) ln in
            let z = ln_right ln in
            LnkSum x (LnkSum y z)

-- merge is among the more challenging behaviors due to complex
-- synchronization and optimization requirements.
mergeB :: B (x :|: x) x
mergeB = mergeSynchB >>> latentOnTime mergeSigsB

-- pre-synch for merge; minimal, does not synch with dead branches
mergeSynchB :: B (x :|: x) (x :|: x)
mergeSynchB = B_tshift synchTs
    where synchTs xx = 
            let synchLR = lnd_zip synchLDT (lnd_left xx) (lnd_right xx) in
            let lx = lnd_fmap fst synchLR in
            let rx = lnd_fmap snd synchLR in
            LnkDSum lx rx 
          synchLDT lhs rhs =
            let inl = not $ ldt_live rhs in 
            let inr = not $ ldt_live lhs in 
            if (inl || inr) then (lhs,rhs) else
            let u = shallowSynch lhs rhs in
            (u,u)

-- simple synch of live signals for merge.
shallowSynch :: LDT -> LDT -> LDT
shallowSynch lhs rhs = 
    assert (ldt_live lhs && ldt_live rhs) $
    LDT { ldt_goal = (max `on` ldt_goal) lhs rhs
        , ldt_curr = (max `on` ldt_curr) lhs rhs
        , ldt_live = True
        }

mergeSigsB :: LnkD LDT (x :|: x) -> B (x :|: x) x
mergeSigsB tr = mkLnkB trMerge (mkLnkSimp $ buildMerge tr) 
    where trMerge xx = lnd_zip ldtMerge (lnd_left xx) (lnd_right xx)
          ldtMerge lhs rhs =
            let inl = not (ldt_live lhs) in
            let inr = not (ldt_live rhs) in
            if inl then lhs else
            if inr then rhs else
            assert (((==) `on` ldt_goal) lhs rhs) $
            assert (((==) `on` ldt_curr) lhs rhs) $
            lhs

-- now to build the link behaviors for merge.
buildMerge :: LnkD LDT (x :|: x) -> Lnk x -> IO (Lnk (x :|: x))
buildMerge tr dst =
    buildMerge_i (lnd_left tr) (lnd_right tr) dst >>= 
    return . uncurry LnkSum

-- this variation is a bit more structure agnostic to support
-- recursive deep-merge of sum and product types.
buildMerge_i :: LnkD LDT x -> LnkD LDT x -> Lnk x -> IO (Lnk x, Lnk x)
buildMerge_i _ _ LnkDead = return (LnkDead, LnkDead)
buildMerge_i tl tr (LnkProd x y) =
    buildMerge_i (lnd_fst tl) (lnd_fst tr) x >>= \ (xl,xr) ->
    buildMerge_i (lnd_snd tl) (lnd_snd tr) y >>= \ (yl,yr) ->
    return (LnkProd xl yl, LnkProd xr yr)
buildMerge_i tl tr (LnkSum x y) =
    buildMerge_i (lnd_left  tl) (lnd_left  tr) x >>= \ (xl,xr) ->
    buildMerge_i (lnd_right tl) (lnd_right tr) y >>= \ (yl,yr) ->
    return (LnkSum xl yl, LnkSum xr yr)
buildMerge_i tl tr (LnkSig lu) =
    let lLiv = (ldt_live . lnd_sig) tl in
    let rLiv = (ldt_live . lnd_sig) tr in
    case (lLiv, rLiv) of
        (False,False) -> return (LnkDead, LnkDead)  -- merge of dead branches
        (False,True) -> return (LnkDead, LnkSig lu) -- always in rhs path
        (True,False) -> return (LnkSig lu, LnkDead) -- always in lhs path
        (True,True) -> -- perform an actual merge of live data!
            let onEmit = ln_update lu . sm_emit (<|>) in
            let onTouch = ln_touch lu in
            ln_withSigM onTouch onEmit >>= \ (ul,ur) ->
            return (LnkSig ul, LnkSig ur)



-- | disjoin will distribute a decision. This will synchronize the
-- choice signal with the external signal (a special case for which
-- B_tshift was heavily revised) then apply the split.
disjoinB :: B (S p a :&: ((S p () :&: x) :|: y) )
              ( (S p a :&: x) :|: (S p a :&: y) )
disjoinB = disjSynchB >>> latentOnTime disjSigsB

-- pre-synch for disjoin operation
disjSynchB :: B (S p a :&: ((S p () :&: x) :|: y) ) 
                 (S p a :&: ((S p () :&: x) :|: y) )
disjSynchB = B_tshift disjSynchTs
    where disjSynchTs auxy = 
            let ux  = (lnd_left . lnd_snd) auxy in
            let a   = (lnd_fst) auxy in
            let u   = (lnd_fst) ux in
            let x   = (lnd_snd) ux in
            let y   = (lnd_right . lnd_snd) auxy in
            let inl = not (ldt_anyLive y) in
            let inr = not (ldt_anyLive ux) in
            if (inl || inr) then auxy else
            let dta = (lnd_sig) a in
            let dtu = (lnd_sig) u in
            let dts = shallowSynch dta dtu in
            let a' = LnkDUnit dts in
            let u' = LnkDUnit dts in
            (a' `LnkDProd` ((u' `LnkDProd` x) `LnkDSum` y))

-- primary disjoin behavior, includes latent optimization for dead
-- code on input (i.e. binl, binr)
disjSigsB :: LnkD LDT (S p a :&: ((S p () :&: x) :|: y))
          -> B (S p a :&: ((S p () :&: x) :|: y) )
               ( (S p a :&: x) :|: (S p a :&: y) )
disjSigsB tr = mkLnkB disjTr (mkLnkSimp $ buildDisj tr)
    where disjTr auxy = 
            -- restructure data; maintain liveness of x,y. 
            let ux   = (lnd_left . lnd_snd) auxy in
            let x    = (lnd_snd) ux in
            let y    = (lnd_right . lnd_snd) auxy in
            let dta  = (lnd_sig . lnd_fst) auxy in
            let aLiv = ldt_live dta in
            let lLiv = aLiv && ldt_anyLive ux in
            let rLiv = aLiv && ldt_anyLive y in
            let l = LnkDUnit $ dta { ldt_live = lLiv } in
            let r = LnkDUnit $ dta { ldt_live = rLiv } in
            ((l `LnkDProd` x) `LnkDSum` (r `LnkDProd` y))

-- build disjoin behavior, using some input to decide left/right path.
buildDisj :: LnkD LDT (S p a :&: ((S p () :&: x) :|: y))
          -> Lnk ((S p a :&: x) :|: (S p a :&: y)) 
          -> IO (Lnk (S p a :&:  ((S p () :&: x) :|: y)))
buildDisj tr = 
    let ux   = (lnd_left . lnd_snd) tr in
    let y    = (lnd_right . lnd_snd) tr in
    let inl  = not (ldt_anyLive y) in
    let inr  = not (ldt_anyLive ux) in
    if inl then buildDisjInl else
    if inr then buildDisjInr else
    buildDisjFull

-- specializations of buildDisj (based on dead inputs; outputs not
-- accounted for yet).
buildDisjInl, buildDisjInr, buildDisjFull :: 
    Lnk ((S p a :&: x) :|: (S p a :&: y)) ->
    IO (Lnk (S p a :&:  ((S p () :&: x) :|: y)))

buildDisjInl lxry = 
    -- pipe `S p a` directly to l; don't use `S p ()`
    let l = (ln_fst . ln_left) lxry in
    let x = (ln_snd . ln_left) lxry in
    let y = (ln_snd . ln_right) lxry in
    return (l `LnkProd` ((LnkDead `LnkProd` x) `LnkSum` y))

buildDisjInr lxry = 
    -- pipe `S p a` directly to r; cannot use `S p ()`
    let r = (ln_fst . ln_right) lxry in
    let x = (ln_snd . ln_left) lxry in
    let y = (ln_snd . ln_right) lxry in
    return (r `LnkProd` ((LnkDead `LnkProd` x) `LnkSum` y))

buildDisjFull lxry = 
    let l = (ln_fst . ln_left) lxry in
    let x = (ln_snd . ln_left) lxry in
    let r = (ln_fst . ln_right) lxry in
    let y = (ln_snd . ln_right) lxry in
    let bLnkDead = ln_dead l && ln_dead r in -- don't need `S p a`
    let lnkIfDead = LnkDead `LnkProd` ((LnkDead `LnkProd` x) `LnkSum` y) in
    if bLnkDead then return lnkIfDead else
    -- else need to perform masking for left, right, or both
    let lul = ln_lnkup l in
    let lur = ln_lnkup r in
    let onTouch = ln_touch lul >> ln_touch lur in
    let onEmit sm =
            let sul = sm_emit disjMaskLeft sm in
            let sur = sm_emit disjMaskRight sm in
            ln_update lul sul >> ln_update lur sur
    in
    ln_withSigM onTouch onEmit >>= \ (a,u) ->
    return (LnkSig a `LnkProd` ((LnkSig u `LnkProd` x) `LnkSum` y))

-- maskLeft and maskRight must take the two original signals and
-- generate the split signals for the disjoin function. Assume 
-- that the signal has already been synchronized...
disjMaskLeft, disjMaskRight :: Sig a -> Sig () -> Sig a
disjMaskLeft = s_mask
disjMaskRight sa su = s_mask sa su'
    where su' = s_full_map inv su
          inv Nothing = Just ()
          inv _       = Nothing


-- | apply pure functions in one signal to values in another
zapB :: B (S p (a -> b) :&: S p a) (S p b)
zapB = synchB >>> mkLnkB tr_unit (mkLnkSimp buildZap)

-- intermediate state is needed to perform zip, zap, etc.
-- since updates on fst and snd might occur at different times.
buildZap :: Lnk (S p b) -> IO (Lnk (S p (a -> b) :&: S p a))
buildZap LnkDead = return LnkDead
buildZap (LnkSig lu) = 
    let onTouch = ln_touch lu in
    let onEmit = ln_update lu . sm_emit (<*>) in
    ln_withSigM onTouch onEmit >>= \ (sf,sa) ->
    return (LnkProd (LnkSig sf) (LnkSig sa))

-- | split a signal based on its data. Main source of (:|:) signals.
splitB :: B (S p (Either x y)) (S p x :|: S p y)
splitB = mkLnkB trSplit $ mkLnkPure lnkSplit 
    where trSplit = LnkDUnit . lnd_sig -- unit of a different type
          lnkSplit lr =
            let l = ln_left lr in
            let r = ln_right lr in
            let bDead = ln_dead l && ln_dead r in
            if bDead then LnkDead else
            let lul = ln_lnkup l in
            let lur = ln_lnkup r in
            let touch = ln_touch lul >> ln_touch lur in
            let update su = 
                    let sul = (su_fmap (s_adjn . s_full_map takeLeft)) su in
                    let sur = (su_fmap (s_adjn . s_full_map takeRight)) su in
                    ln_update lul sul >>
                    ln_update lur sur
            in
            let lu = LnkUp { ln_touch = touch, ln_update = update } in
            LnkSig lu

-- helper functions for split
takeLeft :: Maybe (Either x y) -> Maybe x
takeLeft (Just (Left x)) = Just x
takeLeft _ = Nothing

takeRight :: Maybe (Either x y) -> Maybe y
takeRight (Just (Right x)) = Just x
takeRight _ = Nothing


-- | map an arbitrary Haskell function across an input signal.
fmapB :: (a -> b) -> B (S p a) (S p b)
fmapB = mkLnkB tr_fwd . mkLnkPure . lnFmap
    where lnFmap = ln_lumap . ln_sumap . su_fmap . fmap

-- | map haskell function across an input signal 
-- (unsafe! could damage duration coupling.) 
unsafeFullMapB :: (Maybe a -> Maybe b) -> B (S p a) (S p b)
unsafeFullMapB = mkLnkB tr_fwd . mkLnkPure . lnFullMap
    where lnFullMap = ln_lumap . ln_sumap . su_fmap . s_full_map

-- | map a constant to a signal. 
constB :: DT -> c -> B (S p a) (S p c)
constB dt c = mkLnkB tr_fwd constLnk >>> unsafeEqShiftB dt alwaysEq
    where constLnk = mkLnkPure $ lnConst c
          alwaysEq = (const . const) True
          lnConst  = ln_lumap . ln_sumap . su_fmap . (<$)

-- | add stability to the signal (used by forceB).
unsafeAddStabilityB :: DT -> B (S p x) (S p x)
unsafeAddStabilityB dt = 
    if (0 == dt) then fwdB else 
    mkLnkB id (mkLnkPure lnAddStability)
    where lnAddStability = ln_lumap $ ln_sumap suAddStability
          suAddStability su =
            let tStable = fmap (flip addTime dt) (su_stable su) in
            su { su_stable = tStable }

-- | force evaluation of signal relative to stability, up to `Just`. 
-- This will track the signal over its lifetime in order to ensure
-- every step is touched. I.e. after an update in stability, all
-- prior elements will be touched. If stability is infinite, then 
-- nothing more is touched. 
touchB :: DT -> B (S p x) (S p x)
touchB dt = 
    unsafeAddStabilityB dt >>> 
    mkLnkB id (mkLnkSimp buildTouchB) >>> 
    unsafeAddStabilityB (negate dt)

-- touch up to stability
buildTouchB :: Lnk (S p x) -> IO (Lnk (S p x))
buildTouchB (LnkSig lu) = 
    newIORef (s_never, Nothing) >>= \ rf ->
    let lu' = buildTouchB' rf lu in
    return (LnkSig lu')

-- touch signal up to stability. 
-- stability may have been adjusted a bit just for this op, so it
-- does not make stability assumptions (and doesn't use SigSt). 
buildTouchB' :: IORef (Sig x, Maybe T) -> LnkUp x -> LnkUp x
buildTouchB' rf lu = 
    LnkUp { ln_touch = (ln_touch lu), ln_update = onUpdate }
    where onUpdate su = 
            -- update state and cleanup
            readIORef rf >>= \ (s0,t0) ->
            let sf = su_apply su s0 in
            let tf = su_stable su in
            let scln = maybe s_never (s_trim sf) tf in
            scln `seq`
            writeIORef rf (scln,tf) >>

            -- obtain bounds for computing signal sf.
            let mtUpd   = fmap snd (su_state su) in
            let mtLower = (pure min <*> t0 <*> mtUpd) <|> t0 <|> mtUpd in
            let mtUpper = tf in
            let mtPair  = pure (,) <*> mtLower <*> mtUpper in
            let compute = case mtPair of
                    Nothing -> ()
                    Just (tLower,tUpper) -> 
                        if(tLower < tUpper) 
                        then forceSig tLower tUpper sf
                        else ()
            in
            -- force thunks then forward the update.
            compute `pseq`
            ln_update lu su

-- force signal up to Just|Nothing (simpl rseq for Maybe).
forceSig :: T -> T -> Sig x -> ()
forceSig tLower tUpper sig =
    assert (tLower < tUpper) $
    let updates = sigToList sig tLower tUpper in
    let values = fmap snd updates in
    runEval (evalList rseq values >> return ())


-- | stratB currently evaluates based on stability, not sampling. It
-- ensures that evaluation is initialized before the `Just y` signal
-- value is observed. This should achieve a decent level of parallelism.
stratB :: B (S p (Eval x)) (S p x)
stratB = unsafeFullMapB unwrapStrat

-- apply a 
unwrapStrat :: Maybe (Eval x) -> Maybe x
unwrapStrat Nothing = Nothing
unwrapStrat (Just x) = runEval (Just <$> x)

-- | filter adjacent equal values from a signal (performance), with
-- some scan-ahead to combine equal values. Useful after fmapB if 
-- it results in far fewer values. 
adjeqfB :: (Eq x) => DT -> B (S p x) (S p x)
adjeqfB dt = adjeqfSig >>> unsafeEqShiftB dt (==)
    where adjeqfSig = mkLnkB id $ mkLnkPure lnAdjeqf
          lnAdjeqf = ln_lumap $ ln_sumap $ su_fmap $ s_adjeqf (==)

-- | delay a signal (logically)
delayB :: DT -> B x x
delayB = B_tshift . lnd_fmap . addDelay
    where addDelay dt ldt = ldt { ldt_goal = (dt + (ldt_goal ldt)) }

-- | synchronize signals (logically)
-- note: ignores dead branches due to binl or binr.
-- (This is more consistent with synch on merge and disjoin.)
synchB :: B x x
synchB = B_tshift doSynch
    where doSynch x = 
            assert (ldt_valid x) $
            let dtGoal = lnd_aggr max $ lnd_fmap (liveGoalOr 0) x in
            lnd_fmap (applyGoal dtGoal) x
          liveGoalOr def ldt = 
            if (ldt_live ldt) then (ldt_goal ldt) else def
          applyGoal dtg ldt = 
            if (ldt_live ldt) then ldt { ldt_goal = dtg } else ldt

-- | look ahead in a signal slightly.
peekB :: DT -> B (S p x) (S p (Either x ()))
peekB dt = mkLnkB tr_fwd peekLnk
    where lnPeek = ln_lumap $ ln_sumap $ su_fmap $ s_peek dt
          peekLnk = MkLnk { ln_tsen  = False
                          , ln_peek  = dt -- to track anticipation.
                          , ln_build = return . lnPeek
                          }

-- | force aggregated lazy delays to apply at this location.
-- (unnecessary in most cases)
forceDelayB :: B x x
forceDelayB = B_tshift doBar
    where doBar = lnd_fmap $ \ ldt -> ldt { ldt_curr = (ldt_goal ldt) }

-- | tshiftB turns a difference of `tshift` values into a MkLnk behavior.
-- This is used by the compiler to apply delays. 
tshiftB :: LnkD LDT x -> LnkD LDT x -> B x x
tshiftB t0 tf = mkLnkB id (mkLnkPure $ buildTshift t0 tf)

-- buildTshift will apply delays based on before/after LDT values
buildTshift :: LnkD LDT x -> LnkD LDT x -> Lnk x -> Lnk x
buildTshift _ _ LnkDead = LnkDead
buildTshift t0 tf (LnkProd x y) =
    let opx = buildTshift (lnd_fst t0) (lnd_fst tf) x in
    let opy = buildTshift (lnd_snd t0) (lnd_snd tf) y in
    LnkProd opx opy
buildTshift t0 tf (LnkSum x y) =
    let opx = buildTshift (lnd_left  t0) (lnd_left  tf) x in
    let opy = buildTshift (lnd_right t0) (lnd_right tf) y in
    LnkSum opx opy
buildTshift t0 tf (LnkSig lu) = 
    let dt0 = lnd_sig t0 in
    let dtf = lnd_sig tf in
    let dtDiff = (ldt_curr dtf) - (ldt_curr dt0) in
    assert (dtDiff >= 0) $
    if (0 == dtDiff) then LnkSig lu else
    LnkSig (ln_sumap (su_delay dtDiff) lu)
    

-- | eqshiftB tries to push updates a bit into the future if they
-- would otherwise be redundant, with a given limit for how far into
-- the future we should search for the first change. The given eq
-- function might be a semi-decision on equality, in general.
unsafeEqShiftB :: DT -> (a -> a -> Bool) -> B (S p a) (S p a)
unsafeEqShiftB dt eq = mkLnkB id (mkLnkSimp (buildEqShift dt eq))

buildEqShift :: DT -> (a -> a -> Bool) -> Lnk (S p a) -> IO (Lnk (S p a))
buildEqShift _ _ LnkDead = return LnkDead
buildEqShift dt eq (LnkSig lu) = 
    newIORef st_zero >>= \ rfSt ->
    let lu' = lnEqShift dt eq rfSt lu in
    return (LnkSig lu')

lnEqShift :: DT -> (a -> a -> Bool) -> IORef (SigSt a) -> LnkUp a -> LnkUp a
lnEqShift dt eq rf lu = LnkUp { ln_touch = onTouch, ln_update = onUpdate }
    where onTouch = ln_touch lu
          onUpdate su = 
            readIORef rf >>= \ st ->
            let s0 = st_signal st in
            let st' = st_sigup su st in
            let stCln = maybe st_zero (flip st_clear st') (st_stable st') in
            stCln `seq` 
            writeIORef rf stCln >>
            case su_state su of
                Nothing -> ln_update lu su
                Just (sf,tLower) ->
                    -- search for change between update time and stability + dt
                    let tUpper = maybe tLower (flip addTime dt) (su_stable su) in
                    let shifted = eqShift eq s0 sf tLower tUpper in
                    let su' = su { su_state = Just shifted } in
                    ln_update lu su'

-- find first difference between signals in a given range.
eqShift :: (a -> b -> Bool) -> Sig a -> Sig b -> T -> T -> (Sig b, T)
eqShift eq as bs tLower tUpper =
    if (tLower >= tUpper) then (bs,tLower) else
    let seq = s_full_zip activeWhileEq as bs in -- compare signals
    let seqList = sigToList seq tLower tUpper in 
    let cutL = L.dropWhile sampleActive seqList in
    let tChanged = if (L.null cutL) then tUpper else (fst . L.head) cutL in
    let bs' = s_trim bs tChanged in
    (bs', tChanged)
    where activeWhileEq (Just x) (Just y) = 
                if (eq x y) then Just () 
                            else Nothing
          activeWhileEq Nothing Nothing = Just ()
          activeWhileEq _ _ = Nothing
          sampleActive = (/= Nothing) . snd


-- | undeadB is a behavior that emulates an effectful consumer of 
-- data. It passes the data forward unaltered, but keeps a behavior
-- `alive` with respect to dead code optimization caused by bfst or
-- bsnd later on in the behavior. Could be useful for debugging or 
-- performance analysis, but probably a bad idead.
--
-- Like undead in movies, undeadB infects everything it consumes, 
-- and without a lot of sunlight (eyes on code) it might get left
-- even in your release application.
--
-- I suggest use of `sparkOfLifeB` instead.
undeadB :: B (S p x) (S p x)
undeadB = mkLnkB id $ mkLnkPure (LnkSig . ln_lnkup)

-- | sparkOfLifeB will keep the first element alive so long as other
-- parts of the signal are alive. This should be more robust for
-- composition and dead-code optimization than `bundead`, but 
-- provide similar benefits for debugging.
--
-- Again, this isn't something to leave in most applications. It may
-- seem necessary if you're using some sort of unsafePerformIO hacks
-- to cause effects. But don't do that!
sparkOfLifeB :: B (S p x :&: y) (S p x :&: y)
sparkOfLifeB = mkLnkB id $ mkLnkPure lnkMatchLiveness
    where lnkMatchLiveness xy =
            if (ln_dead xy) then LnkDead else   
            let x = (LnkSig . ln_lnkup . ln_fst) xy in
            let y = ln_snd xy in
            LnkProd x y


