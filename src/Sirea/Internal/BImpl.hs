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
module Sirea.Internal.BImpl
    ( fwdB
    , s1iB, s1eB, trivialB, firstB, swapB, assoclpB, dupB, zapB -- BProd 
    , s0iB, s0eB, vacuousB, leftB, mirrorB, assoclsB, mergeB, splitB -- BSum
    , disjoinB
    , fmapB, constB, seqB, stratB, adjeqfB -- BFmap
    , tshiftB, tshiftB', delayB, synchB, forceDelayB, peekB -- temporal

    -- miscellaneous
    , unsafeSigZipB
    , unsafeChangeScopeB 
    , unsafeEqShiftB, lnEqShift, wrapEqFilter, firstDiffT

    , unsafeFullMapB
    , phaseUpdateB
    , undeadB
    , keepAliveB
    , buildTshift -- for BDynamic
    ) where

import Prelude hiding (id,(.))
import Sirea.Internal.STypes
import Sirea.Internal.LTypes
import Sirea.Internal.BTypes
import Sirea.Time
import Sirea.Signal

import Control.Category
import Control.Applicative
import Control.Parallel.Strategies (Eval, runEval)
import Control.Exception (assert)
import Data.Function (on)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
--import Data.Maybe (isNothing)

import qualified Data.List as L

instance Category B where
  id  = fwdB
  (.) = flip B_pipe


firstB :: B x x' -> B (x :&: y) (x' :&: y)
firstB = B_first

leftB :: B x x' -> B (x :|: y) (x' :|: y)
leftB = B_left

mkLnkPure :: (Lnk y -> Lnk x) -> MkLnk x y
mkLnkPure = (return .)

mkLnkB :: TR x y -> MkLnk x y -> B x y
mkLnkB = B_mkLnk

-- fwdB is the simplest behavior...
fwdB :: B x x 
fwdB = mkLnkB id $ mkLnkPure id

-- introduce S1. This creates an imaginary signal out of nothing.
s1iB :: B x (S1 :&: x)
s1iB = mkLnkB trTriv $ mkLnkPure lnTriv
    where trTriv tx =
            let t1 = LnkDUnit $ LDT { ldt_curr = ldt_maxCurr tx
                                    , ldt_goal = ldt_maxGoal tx
                                    , ldt_live = ldt_anyLive tx }
            in LnkDProd t1 tx
          lnTriv ln =
            let ln1 = ln_fst ln in
            let lnx = ln_snd ln in
            assert (ln_dead ln1) lnx
            
-- eliminate S1
s1eB :: B (S1 :&: x) x
s1eB = mkLnkB trTriv $ mkLnkPure lnTriv
    where trTriv = lnd_snd
          lnTriv x = LnkProd LnkDead x

-- trivialB - S1 is a final state; anything else should be dead
-- code, so go ahead and validate this property.
trivialB :: B x S1
trivialB = mkLnkB trTriv $ mkLnkPure lnTriv
    where trTriv x = LnkDUnit $ LDT { ldt_curr = ldt_maxCurr x
                                    , ldt_goal = ldt_maxGoal x
                                    , ldt_live = ldt_anyLive x }
          lnTriv ln1 = assert (ln_dead ln1) LnkDead 

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
lnDeepDup x LnkDead = x -- no dup
lnDeepDup LnkDead y = y -- no dup
lnDeepDup (LnkProd x1 y1) xy2 =
    let x2 = ln_fst xy2 in
    let y2 = ln_snd xy2 in
    let x = lnDeepDup x1 x2 in
    let y = lnDeepDup y1 y2 in
    LnkProd x y
lnDeepDup (LnkSum x1 y1) xy2 =
    let x2 = ln_left xy2 in
    let y2 = ln_right xy2 in
    let x = lnDeepDup x1 x2 in
    let y = lnDeepDup y1 y2 in
    LnkSum x y
lnDeepDup (LnkSig x) rhs = 
    LnkSig (ln_append x (ln_lnkup rhs))
           
{-- if inl, can ignore the right bucket
inlB :: B x (x :|: y)
inlB = mkLnkB trinl $ mkLnkPure ln_left
    where trinl tr = LnkDSum tr (tr_dead tr) -}

-- introduce a vacuous signal (old role of inlB)
s0iB :: B x (S0 :|: x)
s0iB = mkLnkB trVac $ mkLnkPure lnVac
    where trVac tx = LnkDSum (tr_dead tx) tx
          lnVac = ln_right

-- eliminate a vacuous signal (implicitly performed by mergeB)
s0eB :: B (S0 :|: x) x
s0eB = mkLnkB trVac $ mkLnkPure lnVac
    where trVac t0x =
            let t0 = lnd_left t0x in
            let tx = lnd_right t0x in
            assert ((not . ldt_live . lnd_sig) t0) tx
          lnVac lx = LnkSum LnkDead lx

-- prove anything from nothing
vacuousB :: B S0 x
vacuousB = mkLnkB trVac $ mkLnkPure lnVac
    where trVac t0 = 
            let ldt = lnd_sig t0 in
            assert ((not . ldt_live) ldt) $
            LnkDUnit ldt
          lnVac _ = LnkDead

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
mergeSynchB = tshiftB synchTs
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
mergeSigsB tr = mkLnkB trMerge (buildMerge tr) 
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
    if (not lLiv) then return (LnkDead, LnkSig lu) else
    if (not rLiv) then return (LnkSig lu, LnkDead) else
    let onEmit = sm_emit (<|>) lu in
    let onTouch = ln_touch lu in
    ln_withSigM onTouch onEmit >>= \ (ul,ur) ->
    return (LnkSig ul, LnkSig ur)



-- | disjoin will distribute a decision. This will synchronize the
-- choice signal with the external signal (a special case for which
-- tshiftB was heavily revised) then apply the split.
disjoinB :: (SigInP p x)
         => B (x :&: ((S p () :&: y) :|: z))
                ((x :&: y) :|: (x :&: z))
disjoinB = disjSynchB >>> latentOnTime disjSigsB

-- pre-synch for disjoin operation
disjSynchB :: B (x :&: ((S p () :&: y) :|: z) ) 
                  (x :&: ((S p () :&: y) :|: z) )
disjSynchB = tshiftB disjSynchTs
    where disjSynchTs xuyz =
            let x   = (lnd_fst) xuyz in
            let uyz = (lnd_snd) xuyz in
            let uy  = (lnd_left) uyz in
            let u   = (lnd_fst) uy in
            let y   = (lnd_snd) uy in
            let z   = (lnd_right) uyz in
            let inl = not (ldt_anyLive z) in
            let inr = not (ldt_anyLive uy) in
            if (inl || inr) then xuyz else
            -- simplistic synchronize with x and u
            let xu' = fullSynch (LnkDProd x u) in
            let x'  = lnd_fst xu' in
            let u'  = lnd_snd xu' in
            (x' `LnkDProd` ((u' `LnkDProd` y) `LnkDSum` z))

fullSynch :: LnkD LDT x -> LnkD LDT x
fullSynch = synchCurr . synchGoal
    where synchGoal x =
            let dt = ldt_maxGoal x in
            flip lnd_fmap x (\ldt -> ldt {ldt_goal = dt })
          synchCurr x =
            if (ldt_minCurr x == ldt_maxCurr x) then x else
            let dt = ldt_maxGoal x in
            flip lnd_fmap x (\ldt -> ldt { ldt_curr = dt })

-- primary disjoin behavior, includes latent optimization for dead
-- code on input (i.e. binl, binr)
disjSigsB :: LnkD LDT (x :&: ((S p () :&: y) :|: z))
          -> B (x :&: ((S p () :&: y) :|: z))
                        ((x :&: y) :|: (x :&: z))
disjSigsB tr = mkLnkB disjTr (buildDisj tr)
    where disjTr xuyz = 
            -- restructure data; maintain liveness of x,y. 
            assert (ldt_valid xuyz) $
            let x    = (lnd_fst) xuyz in
            let uy   = (lnd_left . lnd_snd) xuyz in
            let y    = (lnd_snd) uy in
            let z    = (lnd_right . lnd_snd) xuyz in
            let l    = flip lnd_fmap x (andLive (ldt_anyLive uy)) in
            let r    = flip lnd_fmap x (andLive (ldt_anyLive  z)) in
            ((l `LnkDProd` y) `LnkDSum` (r `LnkDProd` z))
          andLive b x = x { ldt_live = (b && (ldt_live x)) }

-- build disjoin behavior, using some input to decide left/right path.
buildDisj :: LnkD LDT (x :&: ((S p () :&: y) :|: z))
          -> Lnk ((x :&: y) :|: (x :&: z)) 
          -> IO (Lnk (x :&: ((S p () :&: y) :|: z)))
buildDisj tr =
    assert (ldt_valid tr) $
    let uy   = (lnd_left  . lnd_snd) tr in
    let z    = (lnd_right . lnd_snd) tr in
    let inl  = not (ldt_anyLive z) in
    let inr  = not (ldt_anyLive uy) in
    if inl then buildDisjInl else
    if inr then buildDisjInr else
    let ux   = LnkDProd (lnd_fst uy) (lnd_fst tr) in
    assert (ldt_maxCurr ux == ldt_minCurr ux) $
    assert (ldt_maxGoal ux == ldt_minGoal ux) $
    buildDisjFull

-- specializations of buildDisj (based on dead inputs; outputs not
-- accounted for yet).
buildDisjInl, buildDisjInr, buildDisjFull 
    ::     Lnk ((x :&: y) :|: (x :&: z)) 
    -> IO (Lnk (x :&: ((S p () :&: y) :|: z)))

buildDisjInl lyrz = 
    -- pipe x directly to l; don't use `S p ()`
    let l = (ln_fst . ln_left)  lyrz in
    let y = (ln_snd . ln_left)  lyrz in
    let z = (ln_snd . ln_right) lyrz in
    return (l `LnkProd` ((LnkDead `LnkProd` y) `LnkSum` z))

buildDisjInr lyrz = 
    -- pipe x directly to r; cannot use `S p ()`
    let y = (ln_snd . ln_left)  lyrz in
    let r = (ln_fst . ln_right) lyrz in
    let z = (ln_snd . ln_right) lyrz in
    return (r `LnkProd` ((LnkDead `LnkProd` y) `LnkSum` z))

buildDisjFull lyrz = 
    let l = (ln_fst . ln_left)  lyrz in
    let y = (ln_snd . ln_left)  lyrz in
    let r = (ln_fst . ln_right) lyrz in
    let z = (ln_snd . ln_right) lyrz in
    let bLnkDead = ln_dead l && ln_dead r in
    let lnkIfDead = LnkDead `LnkProd` ((LnkDead `LnkProd` y) `LnkSum` z) in
    if bLnkDead then return lnkIfDead else
    buildDisjFull_i l r >>= \ (x,u) -> 
    return (x `LnkProd` ((LnkSig u `LnkProd` y) `LnkSum` z))

buildDisjFull_i :: Lnk x -> Lnk x -> IO (Lnk x, LnkUp ())
buildDisjFull_i LnkDead LnkDead   = return (LnkDead, ln_zero)
buildDisjFull_i l@(LnkSig _) r    = buildDisjFull_sig l r
buildDisjFull_i l r@(LnkSig _)    = buildDisjFull_sig l r
buildDisjFull_i l@(LnkProd _ _) r = buildDisjFull_prod l r
buildDisjFull_i l r@(LnkProd _ _) = buildDisjFull_prod l r
buildDisjFull_i l@(LnkSum _ _) r  = buildDisjFull_sum l r
buildDisjFull_i l r@(LnkSum _ _)  = buildDisjFull_sum l r

buildDisjFull_prod :: Lnk (x :&: y) -> Lnk (x :&: y) -> IO (Lnk (x :&: y), LnkUp ())
buildDisjFull_prod l r =
    buildDisjFull_i (ln_fst l) (ln_fst r) >>= \ (x,ux) ->
    buildDisjFull_i (ln_snd l) (ln_snd r) >>= \ (y,uy) ->
    return (LnkProd x y, ln_append ux uy)

buildDisjFull_sum :: Lnk (x :|: y) -> Lnk (x :|: y) -> IO (Lnk (x :|: y), LnkUp ())
buildDisjFull_sum l r =
    buildDisjFull_i (ln_left  l) (ln_left  r) >>= \ (x,ux) ->
    buildDisjFull_i (ln_right l) (ln_right r) >>= \ (y,uy) ->
    return (LnkSum x y, ln_append ux uy)

buildDisjFull_sig :: Lnk (S p a) -> Lnk (S p a) -> IO (Lnk (S p a), LnkUp ())
buildDisjFull_sig l r =
    let lul = ln_lnkup l in
    let lur = ln_lnkup r in
    let onTouch = ln_touch lul >> ln_touch lur in
    let onEmit sm = 
            sm_emit disjMaskLeft lul sm >>
            sm_emit disjMaskRight lur sm
    in
    ln_withSigM onTouch onEmit >>= \ (a,u) ->
    return (LnkSig a, u)

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
zapB = synchB >>> unsafeSigZipB (<*>)

-- | combine arbitrary signals. Be careful, this can easily break 
-- invariants unless synchronization constraints are enforced and 
-- the signal function is carefully constrained.
unsafeSigZipB :: (Sig a -> Sig b -> Sig c) -> B (S p a :&: S p b) (S p c)
unsafeSigZipB = mkLnkB tr_unit . buildZip

-- intermediate state is needed to perform zip, zap, etc.
-- since updates on fst and snd might occur at different times.
buildZip :: (Sig a -> Sig b -> Sig c) -> Lnk (S p c) -> IO (Lnk (S p a :&: S p b))
buildZip _ LnkDead = return LnkDead
buildZip fnZip (LnkSig lu) = 
    let onTouch = ln_touch lu in
    let onEmit = sm_emit fnZip lu in
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
            let idle tS = ln_idle lul tS >> ln_idle lur tS in
            let update tS tU sig =
                    let sigL = (s_adjn . s_full_map takeLeft) sig in
                    let sigR = (s_adjn . s_full_map takeRight) sig in
                    ln_update lul tS tU sigL >>
                    ln_update lur tS tU sigR
            in
            let lu = LnkUp touch update idle in
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
    where lnFmap = ln_lumap . ln_sfmap . fmap

-- | map haskell function across an input signal 
-- (unsafe! could damage duration coupling.) 
unsafeFullMapB :: (Maybe a -> Maybe b) -> B (S p a) (S p b)
unsafeFullMapB = mkLnkB tr_fwd . mkLnkPure . lnFullMap
    where lnFullMap = ln_lumap . ln_sfmap . s_full_map

-- | map a constant to a signal. 
constB :: c -> B (S p a) (S p c)
constB c = mkLnkB tr_fwd constLnk
    where constLnk = mkLnkPure $ lnConst c
          lnConst  = ln_lumap . ln_sfmap . s_const

-- | force evaluation of signal relative to stability, up to `Just`. 
-- This will track the signal over its lifetime in order to ensure
-- every step is touched. I.e. after an update in stability, all
-- prior elements will be touched. If stability is infinite, then 
-- nothing more is touched. 
seqB :: DT -> B (S p x) (S p x)
seqB dt = mkLnkB id (buildSeqB dt)

-- sequence data in signal up to stability
buildSeqB :: DT -> Lnk (S p x) -> IO (Lnk (S p x))
buildSeqB _ LnkDead = return LnkDead
buildSeqB dt (LnkSig lu) =
    newIORef s_never >>= \ rf -> 
    let lu' = buildSeqB' dt rf lu in
    return (LnkSig lu')

-- sequence signal spine up to stability (modified by dt). This will
-- work for all updates except the last (where stability is DoneT).
buildSeqB' :: DT -> IORef (Sig x) -> LnkUp x -> LnkUp x
buildSeqB' dt rf lu = LnkUp touch update idle where
    touch = ln_touch lu
    idle tS = process tS >> ln_idle lu tS
    update tS tU su =
        readIORef rf >>= \ s0 ->
        let sf = s_switch s0 tU su in
        writeIORef rf sf >>
        process tS >>
        ln_update lu tS tU su
    process DoneT = writeIORef rf s_never
    process (StableT tStable) =
        let tTgt = tStable `addTime` dt in
        readIORef rf >>= \ s0 ->
        let seqSig = s_tseq (`seq` ()) tTgt s0 in
        let sf = s_trim s0 tTgt in -- trim to reduce rework
        seqSig `seq` sf `seq`
        writeIORef rf sf

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
adjeqfB :: (Eq x) => B (S p x) (S p x)
adjeqfB = mkLnkB id $ mkLnkPure lnAdjeqf
    where lnAdjeqf = (ln_lumap . ln_sfmap) (s_adjeqf (==))


-- | tshiftB achieves a declarative delay, via B_latent and B_mkLnk.
-- The given function not only computes the delay, it applies the
-- delay if there is a change in ldt_curr values.
tshiftB :: TS x -> B x x
tshiftB = latentOnTime . tshiftB' 

tshiftB' :: TS x -> LnkD LDT x -> B x x
tshiftB' fn t0 = B_mkLnk fn lnk
    where tf  = fn t0
          lnk = mkLnkPure $ buildTshift t0 tf

-- buildTshift will apply delays based on before/after LDT values
--  asserts latency is non-decreasing
--  no post-compile cost for links that aren't delayed
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
    LnkSig (luApplyDelay dtDiff lu)

-- apply actual delay to signal
luApplyDelay :: DT -> LnkUp x -> LnkUp x
luApplyDelay dt lu = LnkUp touch update idle where
    adjtS = adjStableTime (`addTime` dt)
    touch = ln_touch lu
    idle = ln_idle lu . adjtS
    update tS tU su = 
        let tS' = adjtS tS in
        let tU' = tU `addTime` dt in
        let su' = s_delay dt su in
        tS' `seq` tU' `seq`
        ln_update lu tS' tU' su'

-- | delay a signal (logically; delay isn't applied to signals yet)
delayB :: DT -> B x x
delayB = tshiftB . lnd_fmap . addDelay
    where addDelay dt ldt = ldt { ldt_goal = (dt + (ldt_goal ldt)) }

-- | synchronize signals (logically) by updating the logical time.
-- This doesn't perform any actual synchronization.
-- Actual synchronization happens at a tshiftB.
-- note: ignores dead branches due to binl or binr.
-- (This is more consistent with synch on merge and disjoin.)
synchB :: B x x
synchB = tshiftB doSynch
    where doSynch x = 
            assert (ldt_valid x) $
            let dtGoal = lnd_aggr max $ lnd_fmap (liveGoalOr 0) x in
            lnd_fmap (applyGoal dtGoal) x
          liveGoalOr def ldt = 
            if (ldt_live ldt) then (ldt_goal ldt) else def
          applyGoal dtg ldt = 
            if (ldt_live ldt) then ldt { ldt_goal = dtg } else ldt

-- | force aggregated lazy delays to apply at this location.
-- This ensures the signal values are consistent with the delays.
forceDelayB :: B x x
forceDelayB = tshiftB (lnd_fmap toGoal)
    where toGoal ldt = ldt { ldt_curr = (ldt_goal ldt) }


-- | look ahead in a signal slightly. Reduces stability of signal,
-- i.e. updates at time T can affect peek signal at time T-dt.
peekB :: DT -> B (S p x) (S p (Either x ()))
peekB = mkLnkB tr_fwd . mkLnPeek 

-- | Each update to mkLnPeek will correct any prior view of the
-- future from a given update time. Peek reduces stability of the 
-- signal by dt. 
mkLnPeek :: DT -> Lnk (S p (Either x ())) -> IO (Lnk (S p x))
mkLnPeek _ LnkDead = return LnkDead
mkLnPeek dt (LnkSig lu) = 
    newIORef s_never >>= \ rf ->
    let lu' = lnPeek dt rf lu in
    return (LnkSig lu')

-- If signal changes at tUpdate, then my anticipated value at 
-- (tU - dt) must change to match the updated signal. Stability is
-- therefore reduced by dt. While the signal value in the range of
-- (tU-dt) to tU is not relevant, s_peek needs the signal activity.
--
-- TODO: Consider adjusting lnPeek to keep only the activity info.
-- This could improve GC and memory costs, albeit potentially with
-- a CPU overhead. 
lnPeek :: DT -> IORef (Sig x) -> LnkUp (Either x ()) -> LnkUp x
lnPeek dt rf lu = LnkUp touch update idle where
    touch = ln_touch lu
    adjTS = adjStableTime (`subtractTime` dt)
    idle tS0 =
        let tS = adjTS tS0 in
        readIORef rf >>= \ s0 ->
        let sCln = gcSig tS s0 in
        sCln `seq` writeIORef rf sCln >>
        ln_idle lu tS
    update tS0 tU0 su =
        let tS = adjTS tS0 in
        readIORef rf >>= \ s0 ->
        let s' = s_switch s0 tU0 su in
        let sCln = gcSig tS s' in
        sCln `seq` writeIORef rf sCln >>
        let tU = tU0 `subtractTime` dt in
        let sPk = s_peek dt (s_trim s' tU) in
        tU `seq` sPk `seq`
        ln_update lu tS tU sPk
        
-- GC a signal based on stability
gcSig :: StableT -> Sig x -> Sig x
gcSig DoneT _ = s_never
gcSig (StableT tS) s = s_trim s tS

-- | scopes are trivial variations on id
unsafeChangeScopeB :: B (S p1 x) (S p2 x)
unsafeChangeScopeB = mkLnkB tr_fwd $ mkLnkPure lnkFwd

lnkFwd :: Lnk (S p1 x) -> Lnk (S p2 x)
lnkFwd LnkDead = LnkDead
lnkFwd (LnkSig lu) = LnkSig lu

-- | eqshiftB tries to push updates a bit into the future if they
-- would otherwise be redundant, with a given limit for how far into
-- the future we should search for the first change. The given eq
-- function might be a semi-decision on equality, in general.
--
-- TODO: consider filtering in incremental steps up to a distance in
-- the future, or how to make this more dynamic to the actual update
-- style. 
unsafeEqShiftB :: DT -> (a -> a -> Bool) -> B (S p a) (S p a)
unsafeEqShiftB dt eq = mkLnkB id (buildEqShift dt eq)

buildEqShift :: DT -> (a -> a -> Bool) -> Lnk (S p a) -> IO (Lnk (S p a))
buildEqShift _ _ LnkDead = return LnkDead
buildEqShift dt eq (LnkSig lu) = LnkSig <$> wrapEqFilter dt eq lu

lnEqShift :: DT -> (a -> a -> Bool) -> IORef (Sig a) -> LnkUp a -> LnkUp a
lnEqShift dt eq rf lu = LnkUp touch update idle where
    touch = ln_touch lu
    idle tS = 
        readIORef rf >>= \ s0 ->
        let sCln = gcSig tS s0 in
        sCln `seq` writeIORef rf sCln >>
        ln_idle lu tS
    update tS tU su =
        -- need to have an upper bound for comparison, tSeek
        let tSeek = case tS of
                DoneT -> tU `addTime` dt
                StableT tm -> tm `addTime` dt
        in
        readIORef rf >>= \ s0 -> -- old signal for comparison
        let tU' = firstDiffT eq s0 su tU tSeek in
        let su' = s_trim su tU' in
        tU' `seq` su' `seq` -- shifted su and tU based on equal values
        let s' = s_switch s0 tU' su' in 
        let sCln = gcSig tS s' in
        sCln `seq` writeIORef rf sCln >> -- record updated, GC'd signal
        ln_update lu tS tU' su' -- send eqShift'd update downstream

-- find time of first difference between two signals. 
firstDiffT :: (a -> b -> Bool) -> Sig a -> Sig b -> T -> T -> T
firstDiffT eq as bs tLower tUpper =
    if (tLower >= tUpper) then tLower else
    let sigEq = s_full_zip activeWhileEq as bs in -- compare signals
    let sigEqList = sigToList sigEq tLower tUpper in 
    let cutL = L.dropWhile sampleActive sigEqList in
    case cutL of
        [] -> tUpper
        (x:_) -> fst x
    where activeWhileEq (Just x) (Just y) = 
                if (eq x y) then Just () 
                            else Nothing
          activeWhileEq Nothing Nothing = Just ()
          activeWhileEq _ _ = Nothing
          sampleActive = (/= Nothing) . snd

-- | Wrap a LnkUp in an equality filter. This involves keeping an 
-- intermediate cache. (Implementation is same used by badjeqf.)
wrapEqFilter :: DT -> (z -> z -> Bool) -> LnkUp z -> IO (LnkUp z)
wrapEqFilter dteqf zeq zlu =
    newIORef s_never >>= \ rfz ->
    return $ lnEqShift dteqf zeq rfz zlu


-- | phaseUpdateB 
--
-- phaseUpdateB is the receiver-side for `bcross`. Multiple updates
-- may be received on runStepper, i.e. through many bcross entries
-- and possibly through multiple in-flight messages to a single 
-- bcross (in case of fast producer, slow consumer). 
--
-- To handle the latter case, phaseUpdateB will "piggyback" multiple 
-- updates on a single connection, composing one larger update. The
-- former case is handled by the ln_touch mechanism and by delaying
-- the actual update a phase (thus the name).
--
-- phaseUpdateB does not have direct knowledge of how to delay one
-- phase. Instead, that is provided through the `mkPQ` argument.
phaseUpdateB :: IO PhaseQ -> B (S p x) (S p x)
phaseUpdateB mkPQ = mkLnkB id lnPhase
    where lnPhase LnkDead = return LnkDead
          lnPhase (LnkSig lu) =
            mkPQ >>= \ pq ->
            newIORef NoUpdate >>= \ rfSu ->
            let lu' = makePhaseLU rfSu pq lu in 
            return (LnkSig lu')

data UpdateRecord a 
    = NoUpdate 
    | IdleUpdate !StableT 
    | FullUpdate !StableT !T !(Sig a)

type PhaseQ = IO () -> IO () -- receive a phase operation.

makePhaseLU :: IORef (UpdateRecord a) -> PhaseQ -> LnkUp a -> LnkUp a
makePhaseLU rf pq lu = LnkUp touch update idle where
    touch = return ()
    maybeSchedule NoUpdate = pq deliver >> ln_touch lu
    maybeSchedule _ = return ()
    deliver =
        readIORef rf >>= \ rec ->
        writeIORef rf NoUpdate >>
        case rec of
            NoUpdate -> error "unexpected update record state!"
            IdleUpdate tS -> ln_idle lu tS
            FullUpdate tS tU su -> ln_update lu tS tU su
    idle tS =
        readIORef rf >>= \ rec ->
        maybeSchedule rec >>
        let rec' = case rec of
                FullUpdate _ tU su -> FullUpdate tS tU su
                _ -> IdleUpdate tS
        in
        writeIORef rf rec'
    update tS tU su =
        readIORef rf >>= \ rec ->
        maybeSchedule rec >>
        let rec' = case rec of
                FullUpdate _ tU0 su0 ->
                    if (tU0 >= tU) 
                        then FullUpdate tS tU su 
                        else FullUpdate tS tU0 (s_switch su0 tU su)
                _ -> FullUpdate tS tU su
        in
        writeIORef rf rec'

-- | keepAliveB will keep the first element alive so long as other
-- parts of the signal are alive. (used by unsafeOnUpdateBLN)
keepAliveB :: B (S p x :&: y) (S p x :&: y)
keepAliveB = mkLnkB id $ mkLnkPure lnkMatchLiveness
    where lnkMatchLiveness xy =
            if (ln_dead xy) then LnkDead else   
            let x = (LnkSig . ln_lnkup . ln_fst) xy in
            let y = ln_snd xy in
            LnkProd x y

-- | undeadB will keep a signal alive on output. Like undead in
-- any horror film, undeadB will infect everything it consumes...
undeadB :: B (S p x) (S p x)
undeadB = mkLnkB id $ mkLnkPure (LnkSig . ln_lnkup)



