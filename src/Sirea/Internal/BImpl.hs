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
    , fmapB, constB, touchB, stratB, adjeqfB -- BFmap
    , tshiftB, tshiftB', delayB, synchB, forceDelayB, peekB -- temporal

    -- miscellaneous
    , unsafeSigZipB
    , unsafeChangeScopeB 
    , unsafeAddStabilityB 
    , unsafeEqShiftB
    , unsafeFullMapB
    , phaseUpdateB
    , undeadB
    , keepAliveB
    , unsafeAutoSubscribeB, OnSubscribe, UnSubscribe
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
import Control.Monad (unless)
import Control.Parallel.Strategies (Eval, runEval, evalList, rseq)
import Control.Exception (assert)
import Data.Function (on)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
--import Data.Maybe (isNothing)

import qualified Data.List as L

instance Category (B w) where
  id  = fwdB
  (.) = flip B_pipe


firstB :: B w x x' -> B w (x :&: y) (x' :&: y)
firstB = B_first

leftB :: B w x x' -> B w (x :|: y) (x' :|: y)
leftB = B_left

mkLnkPure :: (Lnk y -> Lnk x) -> MkLnk w x y
mkLnkPure = mkLnkSimp . (return .)

mkLnkSimp :: (Lnk y -> IO (Lnk x)) -> MkLnk w x y
mkLnkSimp build = MkLnk { ln_tsen = False 
                        , ln_peek = 0
                        , ln_build = build
                        }

mkLnkB :: TR x y -> MkLnk w x y -> B w x y
mkLnkB = B_mkLnk

-- fwdB is the simplest behavior...
fwdB :: B w x x 
fwdB = mkLnkB id $ mkLnkPure id

-- introduce S1. This creates an imaginary signal out of nothing.
s1iB :: B w x (S1 :&: x)
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
s1eB :: B w (S1 :&: x) x
s1eB = mkLnkB trTriv $ mkLnkPure lnTriv
    where trTriv = lnd_snd
          lnTriv x = LnkProd LnkDead x

-- trivialB - S1 is a final state; anything else should be dead
-- code, so go ahead and validate this property.
trivialB :: B w x S1
trivialB = mkLnkB trTriv $ mkLnkPure lnTriv
    where trTriv x = LnkDUnit $ LDT { ldt_curr = ldt_maxCurr x
                                    , ldt_goal = ldt_maxGoal x
                                    , ldt_live = ldt_anyLive x }
          lnTriv ln1 = assert (ln_dead ln1) LnkDead 

-- simple swap on Lnk sinks
swapB :: B w (x :&: y) (y :&: x)
swapB = mkLnkB trSwap $ mkLnkPure lnSwap
    where trSwap tr = LnkDProd (lnd_snd tr) (lnd_fst tr)
          lnSwap ln = LnkProd (ln_snd ln) (ln_fst ln)

-- simple rearrangement like swap            
assoclpB :: B w (x :&: (y :&: z)) ((x :&: y) :&: z)
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
dupB :: B w x (x :&: x)
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
inlB :: B w x (x :|: y)
inlB = mkLnkB trinl $ mkLnkPure ln_left
    where trinl tr = LnkDSum tr (tr_dead tr) -}

-- introduce a vacuous signal (old role of inlB)
s0iB :: B w x (S0 :|: x)
s0iB = mkLnkB trVac $ mkLnkPure lnVac
    where trVac tx = LnkDSum (tr_dead tx) tx
          lnVac ln =
            let l0 = ln_left ln in
            let lx = ln_right ln in
            assert (ln_dead l0) lx

-- eliminate a vacuous signal (implicitly performed by mergeB)
s0eB :: B w (S0 :|: x) x
s0eB = mkLnkB trVac $ mkLnkPure lnVac
    where trVac t0x =
            let t0 = lnd_left t0x in
            let tx = lnd_right t0x in
            assert ((not . ldt_live . lnd_sig) t0) tx
          lnVac lx = LnkSum LnkDead lx

-- prove anything from nothing
vacuousB :: B w S0 x
vacuousB = mkLnkB trVac $ mkLnkPure lnVac
    where trVac t0 = 
            let ldt = lnd_sig t0 in
            assert ((not . ldt_live) ldt) $
            LnkDUnit ldt
          lnVac lx = assert (ln_dead lx) LnkDead

-- simple rearrangement
mirrorB :: B w (x :|: y) (y :|: x)
mirrorB = mkLnkB trMirr $ mkLnkPure lnMirr 
    where trMirr tr = LnkDSum (lnd_right tr) (lnd_left tr)
          lnMirr ln = LnkSum (ln_right ln) (ln_left ln)

-- simple rearrangement
assoclsB :: B w (x :|: (y :|: z)) ((x :|: y) :|: z)
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
mergeB :: B w (x :|: x) x
mergeB = mergeSynchB >>> latentOnTime mergeSigsB

-- pre-synch for merge; minimal, does not synch with dead branches
mergeSynchB :: B w (x :|: x) (x :|: x)
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

mergeSigsB :: LnkD LDT (x :|: x) -> B w (x :|: x) x
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
    if (not lLiv) then return (LnkDead, LnkSig lu) else
    if (not rLiv) then return (LnkSig lu, LnkDead) else
    let onEmit = ln_update lu . sm_emit (<|>) in
    let onTouch = ln_touch lu in
    ln_withSigM onTouch onEmit >>= \ (ul,ur) ->
    return (LnkSig ul, LnkSig ur)



-- | disjoin will distribute a decision. This will synchronize the
-- choice signal with the external signal (a special case for which
-- tshiftB was heavily revised) then apply the split.
disjoinB :: (SigInP p x)
         => B w (x :&: ((S p () :&: y) :|: z))
                ((x :&: y) :|: (x :&: z))
disjoinB = disjSynchB >>> latentOnTime disjSigsB

-- pre-synch for disjoin operation
disjSynchB :: B w (x :&: ((S p () :&: y) :|: z) ) 
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
          -> B w (x :&: ((S p () :&: y) :|: z))
                        ((x :&: y) :|: (x :&: z))
disjSigsB tr = mkLnkB disjTr (mkLnkSimp $ buildDisj tr)
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
            let sul = sm_emit disjMaskLeft sm in
            let sur = sm_emit disjMaskRight sm in
            ln_touch lur >>
            ln_update lul sul >> ln_update lur sur
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
zapB :: B w (S p (a -> b) :&: S p a) (S p b)
zapB = synchB >>> unsafeSigZipB (<*>)

-- | combine arbitrary signals. Be careful, this can easily break 
-- invariants unless synchronization constraints are enforced and 
-- the signal function is carefully constrained.
unsafeSigZipB :: (Sig a -> Sig b -> Sig c) -> B w (S p a :&: S p b) (S p c)
unsafeSigZipB = mkLnkB tr_unit . mkLnkSimp . buildZip

-- intermediate state is needed to perform zip, zap, etc.
-- since updates on fst and snd might occur at different times.
buildZip :: (Sig a -> Sig b -> Sig c) -> Lnk (S p c) -> IO (Lnk (S p a :&: S p b))
buildZip _ LnkDead = return LnkDead
buildZip fnZip (LnkSig lu) = 
    let onTouch = ln_touch lu in
    let onEmit = ln_update lu . sm_emit fnZip in
    ln_withSigM onTouch onEmit >>= \ (sf,sa) ->
    return (LnkProd (LnkSig sf) (LnkSig sa))

-- | split a signal based on its data. Main source of (:|:) signals.
splitB :: B w (S p (Either x y)) (S p x :|: S p y)
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
                    ln_touch lur >>
                    ln_update lul sul >> ln_update lur sur
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
fmapB :: (a -> b) -> B w (S p a) (S p b)
fmapB = mkLnkB tr_fwd . mkLnkPure . lnFmap
    where lnFmap = ln_lumap . ln_sumap . su_fmap . fmap

-- | map haskell function across an input signal 
-- (unsafe! could damage duration coupling.) 
unsafeFullMapB :: (Maybe a -> Maybe b) -> B w (S p a) (S p b)
unsafeFullMapB = mkLnkB tr_fwd . mkLnkPure . lnFullMap
    where lnFullMap = ln_lumap . ln_sumap . su_fmap . s_full_map

-- | map a constant to a signal. 
constB :: c -> B w (S p a) (S p c)
constB c = mkLnkB tr_fwd constLnk
    where constLnk = mkLnkPure $ lnConst c
          lnConst  = ln_lumap . ln_sumap . su_fmap . (<$)

-- | add stability to the signal (used by forceB).
unsafeAddStabilityB :: DT -> B w (S p x) (S p x)
unsafeAddStabilityB dt = 
    if (0 == dt) then fwdB else 
    mkLnkB id (mkLnkPure lnAddStability)
    where lnAddStability = ln_lumap $ ln_sumap suAddStability
          suAddStability su =
            let tStable = fmap (`addTime` dt) (su_stable su) in
            su { su_stable = tStable }

-- | force evaluation of signal relative to stability, up to `Just`. 
-- This will track the signal over its lifetime in order to ensure
-- every step is touched. I.e. after an update in stability, all
-- prior elements will be touched. If stability is infinite, then 
-- nothing more is touched. 
touchB :: DT -> B w (S p x) (S p x)
touchB dt = 
    unsafeAddStabilityB dt >>> 
    mkLnkB id (mkLnkSimp buildTouchB) >>> 
    unsafeAddStabilityB (negate dt)

-- touch up to stability
buildTouchB :: Lnk (S p x) -> IO (Lnk (S p x))
buildTouchB LnkDead = return LnkDead
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
            compute `seq`
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
stratB :: B w (S p (Eval x)) (S p x)
stratB = unsafeFullMapB unwrapStrat

-- apply a 
unwrapStrat :: Maybe (Eval x) -> Maybe x
unwrapStrat Nothing = Nothing
unwrapStrat (Just x) = runEval (Just <$> x)

-- | filter adjacent equal values from a signal (performance), with
-- some scan-ahead to combine equal values. Useful after fmapB if 
-- it results in far fewer values. 
adjeqfB :: (Eq x) => B w (S p x) (S p x)
adjeqfB = mkLnkB id $ mkLnkPure lnAdjeqf
    where lnAdjeqf = ln_lumap $ ln_sumap $ su_fmap $ s_adjeqf (==)


-- | tshiftB achieves a declarative delay, via B_latent and B_mkLnk.
-- The given function not only computes the delay, it applies the
-- delay if there is a change in ldt_curr values.
tshiftB :: TS x -> B w x x
tshiftB = latentOnTime . tshiftB' 

tshiftB' :: TS x -> LnkD LDT x -> B w x x
tshiftB' fn t0 = B_mkLnk fn lnk
    where tf  = fn t0
          lnk = MkLnk { ln_build = return . buildTshift t0 tf
                      , ln_tsen = False, ln_peek = 0 }

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
    LnkSig (ln_sumap (su_delay dtDiff) lu)



-- | delay a signal (logically)
delayB :: DT -> B w x x
delayB = tshiftB . lnd_fmap . addDelay
    where addDelay dt ldt = ldt { ldt_goal = (dt + (ldt_goal ldt)) }

-- | synchronize signals (logically) by updating the logical time.
-- This doesn't perform any actual synchronization.
-- Actual synchronization happens at a tshiftB.
-- note: ignores dead branches due to binl or binr.
-- (This is more consistent with synch on merge and disjoin.)
synchB :: B w x x
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
forceDelayB :: B w x x
forceDelayB = tshiftB (lnd_fmap toGoal)
    where toGoal ldt = ldt { ldt_curr = (ldt_goal ldt) }


-- | look ahead in a signal slightly. Reduces stability of signal,
-- i.e. updates at time T can affect peek signal at time T-dt.
peekB :: DT -> B w (S p x) (S p (Either x ()))
peekB dt = mkLnkB tr_fwd peekLnk
    where peekLnk = MkLnk { ln_tsen  = False
                          , ln_peek  = dt -- to track anticipation.
                          , ln_build = mkLnPeek dt
                          }
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
-- (tUpdate - dt) must always change to match the new signal. The
-- stability is thus also reduced by dt.
lnPeek :: DT -> IORef (Sig x) -> LnkUp (Either x ()) -> LnkUp x
lnPeek dt rf lu = LnkUp { ln_touch = onTouch, ln_update = onUpdate }
    where onTouch = ln_touch lu 
          onUpdate su =
            let tStable = (`subtractTime` dt) <$> su_stable su in
            readIORef rf >>= \ s0 ->
            let s' = su_apply su s0 in
            let sCln = maybe s_never (s_trim s') tStable in
            sCln `seq`
            writeIORef rf sCln >>
            case su_state su of
                Nothing ->
                    let su' = SigUp { su_state = Nothing, su_stable = tStable } in
                    ln_update lu su'
                Just (_,tUp) ->
                    let tUp' = tUp `subtractTime` dt in
                    let sigPk  = s_peek dt (s_trim s' tUp') in
                    let st' = Just (sigPk, tUp') in
                    let su' = SigUp { su_state = st', su_stable = tStable } in
                    tUp `seq` sigPk `seq`
                    ln_update lu su'
    

-- | scopes are trivial variations on id
unsafeChangeScopeB :: B w (S p1 x) (S p2 x)
unsafeChangeScopeB = mkLnkB tr_fwd $ mkLnkPure lnkFwd

lnkFwd :: Lnk (S p1 x) -> Lnk (S p2 x)
lnkFwd LnkDead = LnkDead
lnkFwd (LnkSig lu) = LnkSig lu

-- | eqshiftB tries to push updates a bit into the future if they
-- would otherwise be redundant, with a given limit for how far into
-- the future we should search for the first change. The given eq
-- function might be a semi-decision on equality, in general.
unsafeEqShiftB :: DT -> (a -> a -> Bool) -> B w (S p a) (S p a)
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
                    let tUpper = maybe tLower (`addTime` dt) (su_stable su) in
                    let shifted = eqShift eq s0 sf tLower tUpper in
                    let su' = su { su_state = Just shifted } in
                    ln_update lu su'

-- find first difference between signals in a given range.
eqShift :: (a -> b -> Bool) -> Sig a -> Sig b -> T -> T -> (Sig b, T)
eqShift eq as bs tLower tUpper =
    if (tLower >= tUpper) then (bs,tLower) else
    let sigEq = s_full_zip activeWhileEq as bs in -- compare signals
    let sigEqList = sigToList sigEq tLower tUpper in 
    let cutL = L.dropWhile sampleActive sigEqList in
    let tChanged = if (L.null cutL) then tUpper else (fst . L.head) cutL in
    let bs' = s_trim bs tChanged in
    (bs', tChanged)
    where activeWhileEq (Just x) (Just y) = 
                if (eq x y) then Just () 
                            else Nothing
          activeWhileEq Nothing Nothing = Just ()
          activeWhileEq _ _ = Nothing
          sampleActive = (/= Nothing) . snd

-- | phaseUpdateB 
--
-- when a partition is receiving batch updates, it receives multiple
-- updates from multiple in-flight batches. phaseUpdateB combines
-- these into one big update. Further, most often many signals will
-- be updated at once. Systematic use of phaseUpdateB can make this
-- efficient by using the ln_touch system for every updated signal
-- before processing any of them. 
--
-- The (IO () -> IO ()) operation is to enqueue the phase task. It
-- should be specific to the partition. It is assumed that updates
-- and the phase queue are handled in the same thread. It is created
-- when the behavior is built, if necessary.
phaseUpdateB :: IO PhaseQ -> B w (S p x) (S p x)
phaseUpdateB mkPQ = mkLnkB id $ mkLnkSimp lnPhase
    where lnPhase LnkDead = return LnkDead
          lnPhase (LnkSig lu) =
            mkPQ >>= \ pq ->
            newIORef (suZero,False) >>= \ rfSu ->
            let lu' = makePhaseLU rfSu pq lu in 
            return (LnkSig lu')

type PhaseQ = IO () -> IO () -- receive a phase operation.

makePhaseLU :: IORef (SigUp a,Bool) -> PhaseQ -> LnkUp a -> LnkUp a
makePhaseLU rf pq lu = LnkUp { ln_touch = touch, ln_update = update }
    where touch = return ()
          update su =
            readIORef rf >>= \ (su0,bPrepared) ->
            writeIORef rf (appendSigUp su0 su, True) >>
            unless bPrepared doPrepare
          doPrepare = 
            ln_touch lu >> 
            pq deliver
          deliver = -- called later, by PhaseQ
            readIORef rf >>= \ (su,bPrepared) ->
            assert bPrepared $
            writeIORef rf (suZero,False) >>
            ln_update lu su

suZero :: SigUp a
suZero = SigUp { su_state = Nothing, su_stable = Nothing }

appendSigUp :: SigUp a -> SigUp a -> SigUp a
appendSigUp su0 su = SigUp { su_state = state', su_stable = stable' }
    where stable' = su_stable su  -- assume monotonic stability
          state' = (calcS <$> su_state su0 <*> su_state su)
                   <|> su_state su  -- su is first state update 
                   <|> su_state su0 -- su is a stability update
          calcS (s0,t0) (sf,tf) =
            if (t0 >= tf) then (sf,tf) else
            (s_switch' s0 tf sf, t0)

-- | keepAliveB will keep the first element alive so long as other
-- parts of the signal are alive. (used by unsafeOnUpdateBLN)
keepAliveB :: B w (S p x :&: y) (S p x :&: y)
keepAliveB = mkLnkB id $ mkLnkPure lnkMatchLiveness
    where lnkMatchLiveness xy =
            if (ln_dead xy) then LnkDead else   
            let x = (LnkSig . ln_lnkup . ln_fst) xy in
            let y = ln_snd xy in
            LnkProd x y

-- | undeadB will keep a signal alive on output. Like undead in
-- any horror film, undeadB will infect everything it consumes...
undeadB :: B w (S p x) (S p x)
undeadB = mkLnkB id $ mkLnkPure (LnkSig . ln_lnkup)


-- | unsafeAutoSubscribeB is a simplified subscription model for a
-- behavior: subscribe whenever there is demand now or in future, 
-- and unsubscribe when signal seems to be unnecessary. Subscription
-- has a callback based on Lnk x, and is insensitive to demand. 
--
-- Note that unsafeAutoSubscribeB does not provide any signals to
-- the output link directly. It is up to the OnSubscribe x to hook
-- the Lnk x to something that provides a signal.
--
-- Note that you should probably use a barrier, forceDelayB, on the
-- input signal to unsafeAutoSubscribeB. And probably a phaseDelay
-- or delayToNextStepB on the output.
--
-- This is `unsafe` because it does not respect duration coupling;
-- indeed, you'll need to hack around it a bit to regain duration
-- coupling, perform merges or masking, ensure thread safety.
unsafeAutoSubscribeB :: OnSubscribe x -> B w (S p ()) x
unsafeAutoSubscribeB = mkLnkB tr_unit . mkLnkSimp . buildSubscriber 

type OnSubscribe x = Lnk x -> IO (UnSubscribe)
type UnSubscribe = IO ()

buildSubscriber :: OnSubscribe x -> Lnk x -> IO (Lnk (S p ()))
buildSubscriber onSub lnx = 
    if (ln_dead lnx) then return LnkDead else
    newIORef st_zero >>= \ rfSt ->
    newIORef Nothing >>= \ rfUnsub ->
    let touch = return () in
    let update = updateSubscriber onSub lnx rfSt rfUnsub in
    return $ LnkSig $ LnkUp { ln_touch = touch, ln_update = update }

updateSubscriber :: OnSubscribe x -> Lnk x 
                 -> IORef (SigSt ()) -> IORef (Maybe UnSubscribe)
                 -> SigUp () -> IO ()
updateSubscriber onSub lnx rfSt rfUnsub su =
    readIORef rfSt >>= \ st -> 
    let st' = st_sigup su st in
    case st_stable st' of
        Nothing -> writeIORef rfSt st'
        Just t -> 
            let stc = st_clear t st' in
            let bTerm = s_term (st_signal st) t in
            writeIORef rfSt stc >>
            if bTerm then unsubscribe 
                     else subscribe
    where subscribe =
            readIORef rfUnsub >>= \ ux ->
            case ux of
                Just _  -> return ()
                Nothing -> onSub lnx >>=
                           writeIORef rfUnsub . Just
          unsubscribe =
            readIORef rfUnsub >>= \ ux ->
            writeIORef rfUnsub Nothing >>
            maybe (return ()) id ux
            
    -- don't subscribe yet; subscribe on demand update.
    -- will need some state to track demand signal and
    -- to track UnSubscribe command if available. 

    


