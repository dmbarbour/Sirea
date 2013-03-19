{-# LANGUAGE TypeOperators, GADTs #-} 

-- | B0Impl is the bulk of Sirea's concrete behavior implementations
-- for type B0. The main exceptions are beval and bcross, plus the
-- resource adapters. Developers don't work directly with these 
-- behaviors, instead operate through Sirea.Behavior classes.
module Sirea.Internal.B0Impl
    ( fwdB0
    , s1iB0, s1eB0, trivialB0, firstB0, swapB0, assoclpB0, dupB0, zapB0 -- BProd 
    , s0iB0, s0eB0, vacuousB0, leftB0, mirrorB0, assoclsB0, mergeB0, splitB0 -- BSum
    , disjoinB0
    , fmapB0, constB0, touchB0, stratB0, adjeqfB0 -- BFmap
    , tshiftB0, delayB0, synchB0, forceDelayB0 -- temporal

    -- miscellaneous
    , mkLnkPure, mkLnkB0
    , luApplyDelay
    , unsafeSigZipB0
    , unsafeEqShiftB0
    , wrapLnEqShift
    , unsafeFullMapB0
    , undeadB0
    , keepAliveB0
    , buildTshift -- for BDynamic
    ) where

import Prelude hiding (id,(.),cycle)
import Sirea.Internal.STypes
import Sirea.Internal.LTypes
import Sirea.Internal.B0Type
import Sirea.Internal.Tuning (dtTouch, dtEqShift, dtAlign)
import Sirea.Time
import Sirea.Signal

import Control.Applicative ((<$>))
import Control.Category
import Control.Parallel.Strategies (Eval, runEval)
import Control.Exception (assert)
import Data.Function (on)
--import Data.Maybe (fromMaybe)
import qualified Data.List as L

-- import Debug.Trace

instance (Monad m) => Category (B0 m) where
  id  = B0_mkLnk id (const return)
  (.) = flip B0_pipe

firstB0 :: B0 m x x' -> B0 m (x :&: y) (x' :&: y)
firstB0 = B0_first

leftB0 :: B0 m x x' -> B0 m (x :|: y) (x' :|: y)
leftB0 = B0_left

-- Many behaviors are pure and atemporal, no need for LCapsM or monad
mkLnkPure :: (Monad m) 
          => (LCapsM m x -> LCapsM m y) 
          -> (LnkM m y -> LnkM m x) 
          -> B0 m x y
mkLnkPure lcf lnf = B0_mkLnk lcf (const (return . lnf))

-- Some behaviors make use of LCapsM.
mkLnkB0 :: (Monad m)
        => (LCapsM m x -> LCapsM m y) 
        -> (LCapsM m x -> LnkM m y -> m (LnkM m x)) 
        -> B0 m x y
mkLnkB0 = B0_mkLnk

-- fwdB is the simplest behavior...
fwdB0 :: (Monad m) => B0 m x x 
fwdB0 = id

-- introduce S1. This creates an inaccessible signal out of nothing.
s1iB0 :: (Monad m) => B0 m x (S1 :&: x)
s1iB0 = mkLnkPure lcTriv lnTriv where
    lcTriv lcx = 
        if (ln_dead lcx) then LnkDead else
        LnkProd lcS1 lcx 
    lnTriv ln =
        let lnS1 = ln_fst ln in
        let lnx = ln_snd ln in
        assert (ln_dead lnS1) lnx

lcS1 :: (Monad m) => LCapsM m S1
lcS1 = LnkSig (LCX lc) where
    lc = LC { lc_dtCurr = 0, lc_dtGoal = 0, lc_cc = cc }
    cc = CC { cc_newRef = noMem, cc_getSched = noSched }
    noMem _ = fail "cannot create references in Void partition"
    noSched = fail "no scheduler available in Void partition"
        
-- eliminate an S1
s1eB0 :: (Monad m) => B0 m (S1 :&: x) x
s1eB0 = mkLnkPure ln_snd (LnkProd LnkDead)

-- trivialB - S1 is a final state; anything else should be dead
-- code, so will assert that property.
trivialB0 :: (Monad m) => B0 m x S1
trivialB0 = mkLnkPure lcTriv lnTriv where
    lcTriv lcx = if (ln_dead lcx) then LnkDead else lcS1
    lnTriv lnS1 = assert (ln_dead lnS1) LnkDead 

-- simple swap on LnkM sinks
swapB0 :: (Monad m) => B0 m (x :&: y) (y :&: x)
swapB0 = mkLnkPure lcSwap lnSwap where
    lcSwap xy = LnkProd (ln_snd xy) (ln_fst xy)
    lnSwap yx = LnkProd (ln_snd yx) (ln_fst yx)

-- simple rearrangement like swap            
assoclpB0 :: (Monad m) => B0 m (x :&: (y :&: z)) ((x :&: y) :&: z)
assoclpB0 = mkLnkPure lcAssoc lnAssoc where
    lcAssoc x_yz = 
        let lcx = ln_fst x_yz in
        let lcy = (ln_snd >>> ln_fst) x_yz in
        let lcz = (ln_snd >>> ln_snd) x_yz in
        LnkProd (LnkProd lcx lcy) lcz
    lnAssoc xy_z =
        let lnx = (ln_fst >>> ln_fst) xy_z in
        let lny = (ln_fst >>> ln_snd) xy_z in
        let lnz = ln_snd xy_z in
        LnkProd lnx (LnkProd lny lnz)

-- deep-duplicate signals (at least where two sinks are available)
dupB0 :: (Monad m) => B0 m x (x :&: x)
dupB0 = mkLnkPure lcDup lnDup where
    lcDup lcx = LnkProd lcx lcx
    lnDup lnxx = lnDeepDup (ln_fst lnxx) (ln_snd lnxx)

-- deep duplicate signal updates, except for dead output.
-- (The idea here is to only duplicate the signals that are used.)
lnDeepDup :: (Monad m) => LnkM m x -> LnkM m x -> LnkM m x
lnDeepDup x LnkDead = x -- no dup
lnDeepDup LnkDead y = y -- no dup
lnDeepDup (LnkProd x1 y1) xy2 =
    let x = lnDeepDup x1 (ln_fst xy2) in
    let y = lnDeepDup y1 (ln_snd xy2) in
    LnkProd x y
lnDeepDup (LnkSum x1 y1) xy2 =
    let x = lnDeepDup x1 (ln_left xy2) in
    let y = lnDeepDup y1 (ln_right xy2) in
    LnkSum x y
lnDeepDup (LnkSig x) rhs = 
    LnkSig (ln_append x (ln_lnkup rhs))
           
-- introduce a vacuous signal (old role of inlB)
s0iB0 :: (Monad m) => B0 m x (S0 :|: x)
s0iB0 = mkLnkPure lcVac lnVac where
    lcVac = LnkSum LnkDead
    lnVac ln = assert ((ln_dead . ln_left) ln) (ln_right ln)

-- eliminate a vacuous signal (implicitly performed by mergeB)
s0eB0 :: (Monad m) => B0 m (S0 :|: x) x
s0eB0 = mkLnkPure lcVac lnVac where
    lcVac lc = assert ((ln_dead . ln_left) lc) (ln_right lc)
    lnVac = LnkSum LnkDead

-- we can pretend a vacuous signal is anything
vacuousB0 :: (Monad m) => B0 m S0 x
vacuousB0 = mkLnkPure lcVac lnVac where
    lcVac lc0 = assert (ln_dead lc0) LnkDead
    lnVac lnx = assert (ln_dead lnx) LnkDead

-- simple rearrangement
mirrorB0 :: (Monad m) => B0 m (x :|: y) (y :|: x)
mirrorB0 = mkLnkPure lcMirr lnMirr where
    lcMirr xy = LnkSum (ln_right xy) (ln_left xy)
    lnMirr yx = LnkSum (ln_right yx) (ln_left yx)

-- simple rearrangement
assoclsB0 :: (Monad m) => B0 m (x :|: (y :|: z)) ((x :|: y) :|: z)
assoclsB0 = mkLnkPure lcAssoc lnAssoc where
    lcAssoc x_yz =
        let lcx = ln_left x_yz in
        let lcy = (ln_right >>> ln_left) x_yz in
        let lcz = (ln_right >>> ln_right) x_yz in
        LnkSum (LnkSum lcx lcy) lcz
    lnAssoc xy_z =
        let lnx = (ln_left >>> ln_left) xy_z in
        let lny = (ln_left >>> ln_right) xy_z in
        let lnz = ln_right xy_z in
        LnkSum lnx (LnkSum lny lnz)

-- merge is among the more challenging behaviors due to complex
-- synchronization and optimization requirements.
mergeB0 :: (Monad m) => B0 m (x :|: x) x
mergeB0 = synchForMergeB0 >>> mergeSigsB0

-- pre-synch for merge; minimal, does not synch with dead branches
synchForMergeB0 :: (Monad m) => B0 m (x :|: x) (x :|: x)
synchForMergeB0 = tshiftB0 synchDTs where
    synchDTs xx =
        let (xl,xr) = synchWith (ln_left xx) (ln_right xx) in
        LnkSum xl xr

-- synchWith will synch elements pairwise (needed for merge).
-- The other requirement here is to respect liveness, which
-- might differ due to inner (:|:) types. 
synchWith :: LCapsM m x -> LCapsM m x -> (LCapsM m x, LCapsM m x)
synchWith l LnkDead = (l,LnkDead)
synchWith LnkDead r = (LnkDead,r)
synchWith (LnkSig (LCX lc)) (LnkSig (LCX rc)) =
    let dtCurr = (max `on` lc_dtCurr) lc rc in
    let dtGoal = (max `on` lc_dtGoal) lc rc in
    let lc' = lc { lc_dtCurr = dtCurr, lc_dtGoal = dtGoal } in
    let rc' = rc { lc_dtCurr = dtCurr, lc_dtGoal = dtGoal } in
    (LnkSig (LCX lc'), LnkSig (LCX rc'))
synchWith (LnkProd lx ly) r = 
    let (xl,xr) = synchWith lx (ln_fst r) in
    let (yl,yr) = synchWith ly (ln_snd r) in
    (LnkProd xl yl, LnkProd xr yr)
synchWith (LnkSum lx ly) r =
    let (xl,xr) = synchWith lx (ln_left r) in
    let (yl,yr) = synchWith ly (ln_right r) in
    (LnkSum xl yl, LnkSum xr yr)
synchWith l r = assert False $ (l,r)
    
mergeSigsB0 :: (Monad m) => B0 m (x :|: x) x
mergeSigsB0 = mkLnkB0 lcMerge lnMerge where
    lcMerge xx = mergeCaps (ln_left xx) (ln_right xx)
    lnMerge lcxx lnx = 
        mergeLnks (ln_left lcxx) (ln_right lcxx) lnx >>= \ (lnl,lnr) ->
        return (LnkSum lnl lnr)

-- merge caps is pretty trivial; it favors the caps from the LHS.
mergeCaps :: LCapsM m x -> LCapsM m x -> LCapsM m x
mergeCaps LnkDead r = r
mergeCaps l LnkDead = l
mergeCaps l@(LnkSig _) _ = l
mergeCaps (LnkProd lx ly) r =
    let x = mergeCaps lx (ln_fst r) in
    let y = mergeCaps ly (ln_snd r) in
    LnkProd x y
mergeCaps (LnkSum lx ly) r =
    let x = mergeCaps lx (ln_left r) in
    let y = mergeCaps ly (ln_right r) in
    LnkSum x y

-- structured merge. Matching on first cap, after quick liveness test.
mergeLnks :: (Monad m) => LCapsM m x -> LCapsM m x -> LnkM m x -> m (LnkM m x, LnkM m x)
mergeLnks _ _ LnkDead = return (LnkDead, LnkDead)
mergeLnks _ LnkDead x = return (x, LnkDead)
mergeLnks LnkDead _ x = return (LnkDead, x)
mergeLnks (LnkProd lc1 lc2) rc ln =
    mergeLnks lc1 (ln_fst rc) (ln_fst ln) >>= \ (x1,x2) ->
    mergeLnks lc2 (ln_snd rc) (ln_snd ln) >>= \ (y1,y2) ->
    assert (ln_dead x1 == ln_dead y1) $
    assert (ln_dead x2 == ln_dead y2) $
    return (LnkProd x1 y1, LnkProd x2 y2)
mergeLnks (LnkSum lcl lcr) rc ln =
    mergeLnks lcl (ln_left rc) (ln_left ln) >>= \ (xl,xr) ->
    mergeLnks lcr (ln_right rc) (ln_right ln) >>= \ (yl,yr) ->
    return (LnkSum xl yl, LnkSum xr yr)
mergeLnks (LnkSig (LCX lc)) rc ln =
    assert ((not . ln_dead) rc && (not . ln_dead) ln) $
    let lu = ln_lnkup ln in
    let cc = lc_cc lc in
    let onTouch = ln_touch lu in
    let onEmit = sm_emit s_merge lu in
    let onCycle = ln_cycle lu in
    ln_withSigM cc onTouch onEmit onCycle >>= \ (ul,ur) ->
    return (LnkSig ul, LnkSig ur)

-- | disjoin will distribute a decision. This will synchronize the
-- choice signal with the external signal (a special case for which
-- tshiftB was heavily revised) then apply the split.
disjoinB0 :: (Monad m, SigInP p x)
          => B0 m (x :&: ((S p () :&: y) :|: z))
                  ((x :&: y) :|: (x :&: z))
disjoinB0 = synchForDisjoinB0 >>> disjSigsB0

-- pre-synch for disjoin operation. This must synch everything in x
-- with the given (S p ()) signal. 
synchForDisjoinB0 :: (Monad m) 
                  => B0 m (x :&: ((S p () :&: y) :|: z) ) 
                          (x :&: ((S p () :&: y) :|: z) )
synchForDisjoinB0 = tshiftB0 disjSynch where
    disjSynch xuyz =
        let x = ln_fst xuyz in
        let u = (ln_snd >>> ln_left >>> ln_fst) xuyz in
        let y = (ln_snd >>> ln_left >>> ln_snd) xuyz in
        let z = (ln_snd >>> ln_right) xuyz in
        -- don't need to synch if statically in left or right
        if (ln_dead u || ln_dead z) then xuyz else 
        let xu' = fullSynch (LnkProd x u) in
        let x' = ln_fst xu' in
        let u' = ln_snd xu' in
        LnkProd x' (LnkSum (LnkProd u' y) z) 

-- fullSynch will ensure that all caps are synched logically, and
-- will also set them to be synched physically if their ldt_curr
-- values are not uniform.
fullSynch :: LCapsM m x -> LCapsM m x
fullSynch lcx = lc_map update lcx where
    dt = lc_maxGoal lcx
    bCurrSynch = lc_minCurr lcx == lc_maxCurr lcx
    updateGoal lc = lc { lc_dtGoal = dt }
    updateGoalAndCurr lc = lc { lc_dtGoal = dt, lc_dtCurr = dt }
    update = if bCurrSynch then updateGoal else updateGoalAndCurr

-- primary disjoin behavior, includes latent optimization for dead
-- code on input (i.e. binl, binr). This is one of the more complex
-- primitive behaviors
disjSigsB0 :: (Monad m, SigInP p x) 
           => B0 m (x :&: ((S p () :&: y) :|: z))
                   ((x :&: y) :|: (x :&: z))
disjSigsB0 = mkLnkB0 disjCaps buildDisj


disjCaps :: LCapsM m (x :&: ((S p () :&: y) :|: z))
         -> LCapsM m ((x :&: y) :|: (x :&: z))
disjCaps xuyz =
    let x = ln_fst xuyz in
    let y = (ln_snd >>> ln_left >>> ln_snd) xuyz in
    let z = (ln_snd >>> ln_right) xuyz in
    let xy = if ln_dead y then LnkDead else LnkProd x y in
    let xz = if ln_dead z then LnkDead else LnkProd x z in
    LnkSum xy xz
    
-- build disjoin behavior, using some input to decide left/right path.
buildDisj :: (Monad m)
          => LCapsM m (x :&: ((S p () :&: y) :|: z))
          -> LnkM m ((x :&: y) :|: (x :&: z))
          -> m (LnkM m (x :&: ((S p () :&: y) :|: z)))
buildDisj lc ln = disj where
    lcu  = (ln_snd >>> ln_left >>> ln_fst) lc
    lcz  = (ln_snd >>> ln_right) lc
    lnxy = ln_left ln
    lnxz = ln_right ln
    lnxL = ln_fst lnxy
    lnxR = ln_fst lnxz
    lny  = ln_snd lnxy
    lnz  = ln_snd lnxz
    inr  = ln_dead lcu
    inl  = ln_dead lcz
    lnkInR = assert (ln_dead lnxy) $ 
        LnkProd lnxR (LnkSum LnkDead lnz)
    lnkInL = assert (ln_dead lnxz) $ 
        LnkProd lnxL (LnkSum (LnkProd LnkDead lny) LnkDead)
    disj = 
        if inr then return lnkInR else
        if inl then return lnkInL else
        assert isProperlySynchronized $
        fullDisj lcu
    fullDisj LnkDead = -- shouldn't happen
        assert (ln_dead ln) $ return LnkDead
    fullDisj (LnkSig (LCX lcuInner)) =
        let cc = lc_cc lcuInner in
        mkDisjFull cc lnxL lnxR >>= \ (x,u) ->
        return (LnkProd x (LnkSum (LnkProd u lny) lnz))
    isProperlySynchronized =
        let ux = LnkProd lcu (ln_fst lc) in
        (lc_minCurr ux == lc_maxCurr ux) &&
        (lc_minGoal ux == lc_maxGoal ux)
        
mkDisjFull :: (Monad m) => CC m -> LnkM m x -> LnkM m x -> m (LnkM m x, LnkM m (S p ()))
mkDisjFull _ LnkDead LnkDead = return (LnkDead, LnkDead)
mkDisjFull cc l@(LnkSig _) r = mkDisjSigs cc l r
mkDisjFull cc l r@(LnkSig _) = mkDisjSigs cc l r
mkDisjFull cc l@(LnkProd _ _) r = mkDisjProd cc l r
mkDisjFull cc l r@(LnkProd _ _) = mkDisjProd cc l r
mkDisjFull cc l@(LnkSum _ _) r = mkDisjSum cc l r
mkDisjFull cc l r@(LnkSum _ _) = mkDisjSum cc l r

-- LnkDead may occur because we don't need the signal upstream. But
-- the remaining signal must still be asked properly. 
mkDisjSigs :: (Monad m) => CC m -> LnkM m (S p' x) -> LnkM m (S p' x) 
           -> m (LnkM m (S p' x), LnkM m (S p ()))
mkDisjSigs cc l r =
    let lu = ln_lnkup l in
    let ru = ln_lnkup r in
    let onTouch = ln_touch lu >> ln_touch ru in
    let onEmit sm =
            sm_emit disjMaskLeft lu sm >>
            sm_emit disjMaskRight ru sm
    in
    let onCycle s = ln_cycle lu s >> ln_cycle ru s in
    ln_withSigM cc onTouch onEmit onCycle >>= \ (x,u) ->
    return (LnkSig x, LnkSig u)            
    
mkDisjProd :: (Monad m) => CC m -> LnkM m (x :&: y) -> LnkM m (x :&: y) 
           -> m (LnkM m (x :&: y), LnkM m (S p ()))
mkDisjProd cc l r =
    mkDisjFull cc (ln_fst l) (ln_fst r) >>= \ (x,ux) ->
    mkDisjFull cc (ln_snd l) (ln_snd r) >>= \ (y,uy) ->
    return (LnkProd x y, lnDeepDup ux uy)

mkDisjSum :: (Monad m) => CC m -> LnkM m (x :|: y) -> LnkM m (x :|: y) 
          -> m (LnkM m (x :|: y), LnkM m (S p ()))
mkDisjSum cc l r =
    mkDisjFull cc (ln_left l) (ln_left r) >>= \ (x,ux) ->
    mkDisjFull cc (ln_right l) (ln_right r) >>= \ (y,uy) ->
    return (LnkSum x y, lnDeepDup ux uy)

-- maskLeft and maskRight must take the two original signals and
-- generate the split signals for the disjoin function. Assumes
-- that the signals have already been synchronized.
disjMaskLeft, disjMaskRight :: Sig a -> Sig () -> Sig a
disjMaskLeft = s_mask
disjMaskRight sa su = s_mask sa su'
    where su' = s_full_map inv su
          inv Nothing = Just ()
          inv _       = Nothing

-- | apply pure functions in one signal to values in another
zapB0 :: (Monad m) => B0 m (S p (a -> b) :&: S p a) (S p b)
zapB0 = synchB0 >>> unsafeSigZipB0 s_ap

-- | combine arbitrary signals. Be careful, this can easily break 
-- invariants unless synchronization constraints are enforced and 
-- the signal function is carefully constrained.
unsafeSigZipB0 :: (Monad m) => (Sig a -> Sig b -> Sig c) 
               -> B0 m (S p a :&: S p b) (S p c)
unsafeSigZipB0 fn = mkLnkB0 (lc_fwd . ln_fst) mkZip where
    mkZip lcab lnc =
        case lc_anyCap lcab of -- is Just lc after dead-code elim
            Nothing -> assert (ln_dead lnc) $ return LnkDead
            Just lc -> buildZip fn (lc_cc lc) lnc 

-- intermediate state is needed to perform zip, zap, etc.
-- since updates on fst and snd might occur at different times.
buildZip :: (Monad m) => (Sig a -> Sig b -> Sig c) 
         -> CC m -> LnkM m (S p c) -> m (LnkM m (S p a :&: S p b))
buildZip _ _ LnkDead = return LnkDead
buildZip fnZip cc (LnkSig lu) = 
    let onTouch = ln_touch lu in
    let onEmit = sm_emit fnZip lu in
    let onCycle = ln_cycle lu in
    ln_withSigM cc onTouch onEmit onCycle >>= \ (sa,sb) ->
    return (LnkProd (LnkSig sa) (LnkSig sb))

-- | split a signal based on its data. Main source of (:|:) signals.
splitB0 :: (Monad m) => B0 m (S p (Either x y)) (S p x :|: S p y)
splitB0 = mkLnkPure lcSplit lnSplit where
    lcSplit lc = LnkSum (lc_fwd lc) (lc_fwd lc)
    lnSplit xy =
        let x = ln_left xy in
        let y = ln_right xy in
        if ln_dead y then ln_lumap (ln_sfmap s_lefts) x else
        if ln_dead x then ln_lumap (ln_sfmap s_rights) y else
        let lu = ln_lnkup x in
        let ru = ln_lnkup y in
        let touch = ln_touch lu >> ln_touch ru in
        let idle tS = ln_idle lu tS >> ln_idle ru tS in
        let cycle n = ln_cycle lu n >> ln_cycle ru n in
        let update tS tU su =
                ln_update lu tS tU (s_lefts su) >>
                ln_update ru tS tU (s_rights su) 
        in
        LnkSig (LnkUp touch update idle cycle)

s_lefts :: Sig (Either x y) -> Sig x
s_lefts  = s_adjn . s_full_map takeLeft

s_rights :: Sig (Either x y) -> Sig y
s_rights = s_adjn . s_full_map takeRight

-- helper functions for split
takeLeft :: Maybe (Either x y) -> Maybe x
takeLeft (Just (Left x)) = Just x
takeLeft _ = Nothing

takeRight :: Maybe (Either x y) -> Maybe y
takeRight (Just (Right x)) = Just x
takeRight _ = Nothing

-- | map an arbitrary Haskell function across an input signal.
fmapB0 :: (Monad m) => (a -> b) -> B0 m (S p a) (S p b)
fmapB0 = mkLnkPure lc_fwd . lnFmap where
    lnFmap = ln_lumap . ln_sfmap . fmap

-- | map haskell function across an input signal 
-- (unsafe! could damage duration coupling.) 
unsafeFullMapB0 :: (Monad m) => (Maybe a -> Maybe b) -> B0 m (S p a) (S p b)
unsafeFullMapB0 = mkLnkPure lc_fwd . lnFullMap where
    lnFullMap = ln_lumap . ln_sfmap . s_full_map

-- | map a constant to a signal; equality filter applied separately
constB0 :: (Monad m) => c -> B0 m (S p a) (S p c)
constB0 = mkLnkPure lc_fwd . lnConst where
    lnConst = ln_lumap . ln_sfmap . s_const

-- | force evaluation of signal relative to stability, up to `Just`. 
-- This will track the signal over its lifetime in order to ensure
-- every step is touched. I.e. after an update in stability, all
-- prior elements will be touched. If stability is infinite, then 
-- nothing more is touched. 
--
-- TODO: make the timing properties more adaptive, i.e. based on an
-- expected stability computed based on past updates.
touchB0 :: (Monad m) => B0 m (S p x) (S p x)
touchB0 = mkLnkB0 id mkLnTouch 

mkLnTouch :: (Monad m) => LCapsM m (S p x) -> LnkM m (S p x) -> m (LnkM m (S p x))
mkLnTouch (LnkSig (LCX lc)) (LnkSig lu) =
    cc_newRef (lc_cc lc) s_never >>= \ rf ->
    return (LnkSig (luTouch rf lu))
mkLnTouch _ _ = return LnkDead

-- sequence signal spine up to stability (modified by dt). This will
-- work for all updates except the last.
luTouch :: (Monad m) => Ref m (Sig x) -> LnkUpM m x -> LnkUpM m x
luTouch rf lu = LnkUp touch update idle cycle where
    touch = ln_touch lu
    cycle = ln_cycle lu
    idle tS = process tS >> ln_idle lu tS
    update tS tU su =
        readRef rf >>= \ s0 ->
        let sf = s_switch' s0 tU su in
        writeRef rf sf >>
        process tS >>
        ln_update lu tS tU su
    process (StableT tStable) =
        let tTgt = tStable `addTime` dtTouch in
        readRef rf >>= \ s0 ->
        let seqSig = s_tseq (`seq` ()) tTgt s0 in
        let sf = s_trim s0 tTgt in -- trim to reduce rework
        seqSig `seq`
        writeRef' rf sf

-- | stratB currently evaluates based on stability, not sampling. It
-- ensures that evaluation is initialized before the `Just y` signal
-- value is observed. This should achieve a decent level of parallelism.
stratB0 :: (Monad m) => B0 m (S p (Eval x)) (S p x)
stratB0 = unsafeFullMapB0 unwrapStrat

-- apply a strategy just before evaluating Just.
unwrapStrat :: Maybe (Eval x) -> Maybe x
unwrapStrat Nothing = Nothing
unwrapStrat (Just x) = runEval (Just <$> x)

-- | filter adjacent equal values from a signal (performance), with
-- some scan-ahead to combine equal values. Useful after fmapB if 
-- it results in far fewer values. This applies just to the signal;
-- further choke is a separate behavior.
adjeqfB0 :: (Eq x, Monad m) => B0 m (S p x) (S p x)
adjeqfB0 = mkLnkPure id lnAdjeqf where
    lnAdjeqf = (ln_lumap . ln_sfmap . s_adjeqf) (==) 

-- | tshiftB is useful for declarative delays. Changes in lc_dtCurr
-- result in actual signal delays to match the new specified latency.
tshiftB0 :: (Monad m) => (LCapsM m x -> LCapsM m x) -> B0 m x x
tshiftB0 fn = mkLnkB0 fn lnTshift where
    lnTshift lc0 lnx = return (buildTshift lc0 (fn lc0) lnx)

-- buildTshift will apply delays based on before/after LCapsM values
buildTshift :: (Monad m) => LCapsM m x -> LCapsM m x -> LnkM m x -> LnkM m x
buildTshift _ _ LnkDead = LnkDead
buildTshift t0 tf (LnkProd x y) =
    let opx = buildTshift (ln_fst t0) (ln_fst tf) x in
    let opy = buildTshift (ln_snd t0) (ln_snd tf) y in
    LnkProd opx opy
buildTshift t0 tf (LnkSum x y) =
    let opx = buildTshift (ln_left  t0) (ln_left  tf) x in
    let opy = buildTshift (ln_right t0) (ln_right tf) y in
    LnkSum opx opy
buildTshift t0 tf (LnkSig lu) =
    assert ((not . ln_dead) t0 && (not . ln_dead) tf) $
    let dtDiff = getCurr tf - getCurr t0 in
    if (0 == dtDiff) then LnkSig lu else
    assert (dtDiff > 0) $
    LnkSig (luApplyDelay dtDiff lu)

getCurr :: LCapsM m (S p x) -> DT
getCurr (LnkSig (LCX lc)) = lc_dtCurr lc
getCurr LnkDead = 0

-- apply actual delay to signal
luApplyDelay :: DT -> LnkUpM m x -> LnkUpM m x
luApplyDelay dt lu = LnkUp touch update idle cycle where
    adjtS = adjStableT (`addTime` dt)
    touch = ln_touch lu
    cycle = ln_cycle lu
    idle = ln_idle lu . adjtS
    update tS tU su = 
        let tS' = adjtS tS in
        let tU' = tU `addTime` dt in
        let su' = s_delay dt su in
        ln_update lu tS' tU' su'

-- | delay a signal logically; delay is not applied to signals at
-- this point, allowing multiple delays to aggregate across pure,
-- time-insensitive operations.
delayB0 :: (Monad m) => DT -> B0 m x x
delayB0 dt = mkLnkPure (lc_map addDelay) id where
    addDelay lc = lc { lc_dtGoal = (dt + lc_dtGoal lc) }

-- | synchronize signals (logically) by matching delay goals. Does
-- not apply any actual synchronization, which will occur later if 
-- forced (the goal being to accumulate delays that don't affect 
-- time-insensitive intermediate operations).
synchB0 :: (Monad m) => B0 m x x
synchB0 = mkLnkPure synchGoals id where
    synchGoals lc = 
        let dt = lc_maxGoal lc in
        lc_map (appGoal dt) lc
    appGoal dt lc = lc { lc_dtGoal = dt }


-- | force aggregated lazy delays to apply at this location.
-- This ensures the signal values are consistent with the delays.
forceDelayB0 :: (Monad m) => B0 m x x
forceDelayB0 = tshiftB0 (lc_map toGoal)
    where toGoal lc = lc { lc_dtCurr = (lc_dtGoal lc) }

-- | keepAliveB will keep the first element alive so long as other
-- parts of the signal are alive. (used by unsafeOnUpdateBLN)
keepAliveB0 :: (Monad m) => B0 m (S p x :&: y) (S p x :&: y)
keepAliveB0 = mkLnkPure id lnMatchLiveness where
    lnMatchLiveness xy =
        if (ln_dead xy) then LnkDead else
        let x = (LnkSig . ln_lnkup . ln_fst) xy in
        let y = ln_snd xy in
        LnkProd x y

-- | undeadB will keep a signal alive on output. Like the undead in
-- any horror film, undeadB will infect everything it consumes...
undeadB0 :: (Monad m) => B0 m (S p x) (S p x)
undeadB0 = mkLnkPure id (LnkSig . ln_lnkup)


-- | eqshiftB0 tries to push updates a bit into the future if they
-- would otherwise be redundant, with a given limit for how far into
-- the future we should search for the first change. The given eq
-- function might be a semi-decision on equality (may return False
-- if not known).
--
unsafeEqShiftB0 :: (Monad m) => (a -> a -> Bool) -> B0 m (S p a) (S p a)
unsafeEqShiftB0 eq = mkLnkB0 id mkln where
    mkln (LnkSig (LCX lc)) (LnkSig lu) =
        cc_newRef (lc_cc lc) s_never >>= \ rf ->
        return (LnkSig (luEqShift eq rf lu))
    mkln _ _ = return LnkDead

-- | wrapLnEqShift enables IO resources to access the same logic as
-- the eqShiftB0 behavior.
wrapLnEqShift :: (a -> a -> Bool) -> LnkUp a -> IO (LnkUp a)
wrapLnEqShift eq lu =
    newRefIO s_never >>= \ rf ->
    return (luEqShift eq rf lu)

luEqShift :: (Monad m) => (a -> a -> Bool) -> Ref m (Sig a) 
         -> LnkUpM m a -> LnkUpM m a
luEqShift eq rf lu = LnkUp touch update idle cycle where
    touch = ln_touch lu
    cycle = ln_cycle lu
    idle tS = 
        modifyRef' rf (`s_trim` inStableT tS) >>
        ln_idle lu tS
    update tS tU su =
        readRef rf >>= \ s0 -> -- old signal for comparison
        let s' = s_switch s0 tU su in
        let sCln = s_trim s' (inStableT tS) in
        writeRef' rf sCln >> -- recorded signal
        -- if this is an obvious termination update, don't delay it;
        -- don't want to increase GC recognition burdens downstream
        if (s_is_final sCln (inStableT tS)) 
           then ln_update lu tS tU su 
           else deliverEqf s0 tS tU su
    deliverEqf s0 tS tU su =
        let tSeek = inStableT tS `addTime` dtEqShift in
        let mbDiffT = firstDiffT eq s0 su tU tSeek in
        case mbDiffT of
            Nothing  -> ln_idle lu tS -- both signals are constant
            Just tU' -> ln_update lu tS tU' su

-- find time of first difference between two signals in a region. OR
-- if we don't find a difference, seek any existing point of change 
-- in the signals to get an 'aligned' update. An aligned update will
-- avoid increasing frequency of updates within limits of a search.
-- 
-- If there is no aligned update, this will test for a final state.
-- If the signals are equal up to the final state (not just queried
-- domain) then this will return Nothing; otherwise, it returns the
-- alignment time up to which the signals are known to be equal.
firstDiffT :: (a -> b -> Bool) -> Sig a -> Sig b -> T -> T -> Maybe T
firstDiffT eq as bs tLower tUpper =
    if (tLower >= tUpper) then Just tLower else -- aligned on tLower
    let sigEq = s_full_zip activeWhileEq as bs in -- compare signals
    let sigEqList = sigToList sigEq tLower tUpper in 
    let cutL = L.dropWhile sampleActive sigEqList in
    case cutL of
        (x:_) -> Just $! fst x -- found a difference
        [] -> let tAlign = tUpper `addTime` dtAlign in
              let (x,xs) = s_sample_d sigEq tUpper tAlign in
              case x of
                Just (tU,_) -> Just tU -- align with an update
                Nothing ->
                    -- final test for whether signal is constant
                    if s_is_final xs tAlign 
                        then Nothing 
                        else Just tAlign 
    where activeWhileEq (Just x) (Just y) = 
                if (eq x y) then Just () 
                            else Nothing
          activeWhileEq Nothing Nothing = Just ()
          activeWhileEq _ _ = Nothing
          sampleActive = (/= Nothing) . snd

-- TODO: consider an additional optimization to recognize when we've
-- reached the end of the current signal (s_is_final) and avoid the
-- update entirely. This would only need to apply if the tAlign
-- test fails.

