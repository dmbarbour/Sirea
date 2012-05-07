

-- | BImpl has the implementation details for most of Sirea's
-- concrete behaviors, mostly because I did not want them cluttering
-- FRP.Sirea.Behavior (which is about documenting typeclasses).
--
-- This includes both the symbolic and concrete implementations for
-- basic behaviors except for `first` and `left`, which need special 
-- treatment at compilation. 
module FRP.Sirea.Internal.BImpl
    ( fwdB
    , fstB, firstB, swapB, assoclpB, dupB -- BProd 
    , inlB, leftB, mirrorB, assocrsB, mergeB -- BSum
    , zapB, splitB
    , disjoinB
    , fmapB, constB, forceB, stratB, adjeqfB -- BFmap
    , addStabilityB, delayB, synchB, peekB, forceDelayB, tshiftB

    ) where

import Prelude hiding (id,(.))
import FRP.Sirea.Internal.STypes
import FRP.Sirea.Internal.LTypes
import FRP.Sirea.Internal.BTypes
import FRP.Sirea.Time
import FRP.Sirea.Signal

import Control.Category
import Control.Applicative
import Control.Parallel.Strategies (Eval, runEval)
import Control.Exception (assert)

instance Category B where
  id  = fwdB
  (.) = flip B_pipe


firstB :: B x x' -> B (x :&: y) (x' :&: y)
firstB = B_first

leftB :: B x x' -> B (x :|: y) (x' :|: y)
leftB = B_left

mkLnkPure :: (Lnk y -> Lnk x) -> MkLnk x y
mkLnkPure bfn = MkLnk { ln_tsen = False 
                      , ln_peek = 0
                      , ln_build = return . bfn  
                      }

mkLnkB :: TR x y -> MkLnk x y -> B x y
mkLnkB = B_mkLnk

-- fwdB is the simplest behavior...
fwdB :: B x x 
fwdB = mkLnkB id $ mkLnkPure id

-- in fstB, snd output is dead. 
fstB :: B (x :&: y) x
fstB = mkLnkB lnd_fst $ mkLnkPure lnFst
    where lnFst ln = LnkProd ln LnKDead 

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
            let x = ln_fst ln in
            let y = (ln_snd >>> ln_fst) ln in
            let z = (ln_snd >>> ln_snd) ln in
            LnkProd (LnkProd x y) z


-- deep-duplicate signals (at least where two sinks are available)
dupB :: B x (x :&: x)
dupB = mkLnkB trDup $ mkLnkPure lnDup
    where trDup tr = LnkDProd tr tr
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
tr_dead :: TR x -> TR y
tr_dead x = LnkDUnit ldtDead
    where ldtDead = LDT { ldt_curr = ldt_minCurr x
                        , ldt_goal = ldt_minGoal x
                        , ldt_live = False 
                        }

-- simple rearrangement
mirrorB :: B (x :|: y) (y :|: x)
mirrorB = mkLnkB trMirr $ mkLnkPure lnMirr 
    where trMirr tr = LnkDSum (lnd_right tr) (lnd_left tr)
          lnMirr ln = LnkSum (ln_right ln) (ln_left ln)

-- simple rearrangement
asscorsB :: B (x :|: (y :|: z)) ((x :|: y) :|: z)
assoclsB = mkLnkB trRotl $ mkLnkPure lnAso
    where trRotl tr =
            let tx = lnd_left tr in
            let ty = (lnd_right >>> lnd_left) tr in
            let tz = (lnd_right >>> lnd_right) tr in
            LnkDSum (LnkDSum tx ty) tz
          lnAso ln =
            let x = ln_left ln in
            let y = (ln_right >>> ln_left) ln in
            let z = (ln_right >>> ln_left) ln in
            LnkSum (LnkSum x y) z

-- merge is among the more challenging behaviors due to complex
-- synchronization and optimization requirements.
mergeB :: B (x :|: x) x
mergeB = mergeSynchB >>> latentBC0Time mergeSigsB

mergeSynchB :: B (x :|: x) (x :|: x)
mergeSynchB = B_tshift synchTs
    where synchTs xx = uncurry LnkDSum $ 
                deepSynchMergeTs (lnd_left xx) (lnd_right xx)

-- deepSynchMergeTs will synch matching signals only, and will add 
-- delay to at most one of them. Liveness and other properties are 
-- preserved. Output has same order as input.
deepSynchMergeTs :: LnkD LDT x -> LnkD LDT x -> (LnkD LDT x, LnkD LDT x)
deepSynchMergeTs (LnkDSum xl yl) (LnkDSum xr yr) =
    let (xl',xr') = deepSynchMergeTs xl xr in
    let (yl',yr') = deepSynchMergeTs yl yr in
    let lhs = LnkDSum xl' yl' in
    let rhs = LnkDSum xr' yr' in
    (lhs,rhs)
deepSynchMergeTs (LnkDProd xl yl) (LnkDProd xr yr) =
    let (xl',xr') = deepSynchMergeTs xl xr in
    let (yl',yr') = deepSynchMergeTs yl yr in
    let lhs = LnkDProd xl' yl' in
    let rhs = LnkDProd xr' yr' in
    (lhs,rhs)
deepSynchMergeTs l@(LnkDUnit lhs) r@(LnkDUnit rhs) = 
    -- perform synch for merge, but only if both inputs live.
    let liveMerge = ldt_live lhs && ldt_live rhs in
    if (not liveMerge) then (l,r) else
    let ldtSynch = shallowSynch lhs rhs in
    (LnkDUnit ldtSynch, LnkDUnit ldtSynch) 

-- shallow synch will synchronize two signals.
shallowSynch :: LDT -> LDT -> LDT
shallowSynch lhs rhs =
    let liveness = max (ldt_live lhs) (ldt_live rhs) in
    let maxGoal = max (ldt_goal lhs) (ldt_goal rhs) in
    let maxCurr = max (ldt_curr lhs) (ldt_curr rhs) in
    LDT { ldt_goal = maxGoal
        , ldt_curr = maxCurr
        , ldt_live = liveness 
        }

mergeSigsB :: LnkD LDT (x :|: x) -> B (x :|: x) x
mergeSigsB tr = mkLnkB trMerge lnkMerge
    where trMerge xx = trDeepMerge (lnd_left xx) (lnd_right xx)
          lnkMerge = MkLnk { ln_tsen = False
                           , ln_peek = 0
                           , ln_build = buildMergeB tr
                           }

-- trDeepMerge computes liveness for each signal after merge.
-- (liveness is lost due to binl, binr).
trDeepMerge :: LnkD LDT x -> LnkD LDT x -> LnkD LDT x
trDeepMerge (LnkDSum xl yl) (LnkDSum xr yr) =
    let x = trDeepMerge xl xr in
    let y = trDeepMerge yl yr in
    LnkDSum x y
trDeepMerge (LnkDProd xl yl) (LnkDProd xr yr) =
    let x = trDeepMerge xl xr in
    let y = trDeepMerge yl yr in
    LnkDProd x y
trDeepMerge (LnkDUnit lhs) (LnkDUnit rhs) =
    assert (ldt_goal lhs == ldt_goal rhs) $
    assert (ldt_curr lhs == ldt_curr rhs) $
    let liveness = ldt_live lhs || ldt_live rhs in
    LnkDUnit $ LDT 
        { ldt_curr = (ldt_curr lhs)
        , ldt_goal = (ldt_goal lhs)
        , ldt_live = liveness
        }

-- now to actually build the merge behaviors, including dead code
-- optimizations. Most relevant is the optimization where if only
-- one branch is dead, the destination is passed onto the other 
-- branch without any zip action. 
buildMergeB :: LnkD LDT (x :|: x) -> Lnk x -> IO (Lnk (x :|: x))
buildMergeB tr dst =
    buildMergeB_i (lnd_left tr) (lnd_right tr) dst >>= 
    return . uncurry LnkDSum

buildMergeB_i :: LnkD LDT x -> LnkD LDT x -> Lnk x -> IO (Lnk x, Lnk x)
buildMergeB_i _ _ LnkDead = return (LnkDead, LnkDead)
buildMergeB_i tl tr (LnkProd x y) =
    buildMergeB_i (lnd_fst tl) (lnd_fst tr) x >>= \ (xl,xr) ->
    buildMergeB_i (lnd_snd tl) (lnd_snd tr) y >>= \ (yl,yr) ->
    return (LnkProd xl yl, LnkProd xr yr)
buildMergeB_i tl tr (LnkSum x y) =
    buildMergeB_i (lnd_left tl) (lnd_left tr) x >>= \ (xl,xr) ->
    buildMergeB_i (lnd_right tl) (lnd_right tr) y >>= \ (yl,yr) ->
    return (LnkSum xl yl, LnkSum xr yr)
buildMergeB_i tl tr (LnkSig lu) =
    let lAlive = (ldt_live . lnd_sig) tl in
    let rAlive = (ldt_live . lnd_sig) tr in
    case (lAlive, rAlive) of
        (False,False) -> return (LnkDead, LnkDead)  -- never invoked anyway
        (False,True) -> return (LnkDead, LnkSig lu) -- always in rhs path
        (True,False) -> return (LnkSig lu, LnkDead) -- always in lhs path
        (True,True) -> -- perform an actual merge of live data!
            let onEmit = ln_update lu . sm_emit (<|>) in
            let onTouch = ln_touch lu in
            ln_withSigM onTouch onEmit >>= \ (ul,ur) ->
            return (LnkSig ul, LnkSig ur)


-- | map an arbitrary Haskell function across an input signal.
fmapB :: (a -> b) -> B (S p a) (S p b)
fmapB = mkLnkB tr_fwd . mkLnkPure . lnFmap

lnFmap :: (a -> b) -> (Lnk (S p b) -> Lnk (S p a))
lnFmap = ln_lumap . ln_sumap . su_fmap . fmap

-- | map a constant to a signal. 
constB :: DT -> c -> B (S p a) (S p c)
constB dt c = mkLnkB tr_fwd constLnk >>> eqshiftB dt alwaysEq
    where constLnk = mkLnkPure $ lnConst c
          alwaysEq = (const . const) True

lnConst :: c -> (Lnk (S p c) -> LnkUp (S p b))
lnConst = ln_lumap . ln_sumap . su_fmap . (<$)


-- | add stability to the signal (used by forceB).
addStabilityB :: DT -> B (S p x) (S p x)
addStabilityB dt = if (0 == dt) then fwdB else 
    mkLnkB id (mkLnkPure lnAddStability)
    where lnAddStability = ln_lumap . ln_sumap . suAddStability
          suAddStability su =
            let tStable = fmap (flip addTime dt) (su_stable su) in
            su { su_stable = tStable }

-- | force evaluation of signal relative to stability. 
-- This will track the signal over its lifetime in order to ensure
-- every step is evaluated. I.e. after an update in stability, all
-- prior elements will be evaluated with rnf even if there was no
-- other update.
--
-- If stability is infinite (Nothing), then nothing is forced.
forceB :: DT -> (x -> ()) -> B (S p x) (S p x)
forceB dt rnf = wrapStability dt $ mkLnkB id forceLnk
    where wrapStability dt b = addStabilityB dt >>> b >>> addStabilityB (negate dt)
          forceLnk = MkLnk { ln_tsen = False
                           , ln_peek = 0
                           , ln_build = buildForceB rnf
                           }

-- force up to stability
buildForceB :: (x -> ()) -> Lnk (S p x) -> IO (Lnk (S p x))
buildForceB rnf (LnkSig lu) = 
    newIORef (s_never, Nothing) >>= \ rfSt ->
    let lu' = buildForceB' dt rnf rfSt lu in
    return (LnkSig lu')

-- force signal up to stability.
-- stability may have been adjusted a bit just for this op, so it
-- does not make stability assumptions (and doesn't use SigSt). 
buildForceB' :: (x -> ()) -> IORef (SigSt x) -> LinkUp x -> LinkUp x
buildForceB' rnf rf lu = 
    LinkUp { ln_touch = (ln_touch lu), ln_update = onUpdate }
    where onUpdate su = 
            -- update state and cleanup
            readIORef rf >>= \ (s0,t0) ->
            let sf = su_apply su s0 in
            let tf = su_stable su in
            let scln = maybe s_never (s_trim sf) tf in 
            scln `seq` 
            writeIORef rf (scln,tf) >>

            -- obtain bounds for computing signal sf.
            let mtOld   = st_stable st0 in
            let mtUpd   = fmap snd (su_state su) in
            let mtLower = (pure min <*> mtOld <*> mtUpd) <|> mtOld <|> mtUpd
            let mtUpper = st_stable stUp <|> mtUpd in
            let mtPair  = pure (,) <*> mtLower <*> mtUpper
            let compute = case mtPair of
                    Nothing -> ()
                    Just (tLower,tUpper) -> 
                        if(tLower < tUpper) 
                        then forceSig rnf tLower tUpper (st_signal st)
                        else ()
            in
            -- force thunks then forward the update.
            compute `seq`
            ln_update lu su

-- s_sample_d :: Sig a -> T -> T -> (Maybe (T, Maybe a), Sig a)
forceSig :: (x -> ()) -> T -> T -> Sig x -> ()
forceSig rnf tLower tUpper sig =
    let (sample,sig') = s_sample_d sig tLower tUpper in
    case sample of
        Nothing -> ()
        Just (_,e) -> 
            maybe () rnf e `seq` 
            forceSig rnf tLower tUpper sig'
        

-- TODO: install an evaluator into a signal. 
-- At the moment stratB is just a dummy implementation. Eventually, 
-- however, it should set up a signal to initiate evaluation of its
-- own future whenever the current value is sampled. This will be a
-- tricky bit of code. It might be best if I get a bit sloppy about
-- the update handoff, in which case stratB could be a pure signal
-- operation. Trying to be clever about this is giving me headaches.
stratB :: DT -> (x -> Eval y) -> B (S p x) (S p y)
stratB _ = fmapB . (runEval .)

-- | delay a signal (logically)
delayB :: DT -> B x x
delayB = B_tshift . lnd_fmap . addDelay
    where addDelay dt ldt = 
            let dtGoal = dt + (ldt_goal lt) in
            ldt { ldt_goal = dtGoal }

-- | synchronize signals (logically)
synchB :: B x x
synchB = B_tshift doSynch
    where doSynch x =
            let dtGoal = ldt_maxGoal x in
            lnd_fmap $ \ ldt -> ldt { ldt_goal = dtGoal }
            -- setting all elements to max delay goal among them

-- | look ahead in a signal slightly.
peekB :: DT -> B (S p x) (S p (Either x ())
peekB dt = mkLnkB tr_fwd peekLnk
    where peekLnk = MkLnk { ln_tsen  = False
                          , ln_peek  = dt -- track total anticipation.
                          , ln_build = return . ln_peek dt
                          }

ln_peek :: DT -> LnkUp (Either a ()) -> LnkUp a
ln_peek = ln_lumap . ln_sumap . su_fmap . s_peek

-- | force aggregated lazy delays to apply at this location.
-- (unnecessary in most cases)
forceDelayB :: B x x
forceDelayB = B_tshift doBar
    where doBar = lnd_fmap $ \ ldt -> ldt { ldt_curr = (ldt_goal ldt) }

-- | tshiftB turns a difference of `tshift` values into a MkLnk behavior.
-- This is used by the compiler to apply delays. 
tshiftB :: LnkD LDT x -> LnkD LDT x -> B x x
tshiftB t0 tf = mkLnkB id tsLnk
    where tsLnk = MkLnk { ln_tsen = False
                         , ln_peek = 0
                         , ln_build = return . buildTshift t0 tf
                         }

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
    if (0 == dtDiff) then LnkSig lu else
    LnkSig ((ln_sumap . su_delay dtDiff) lu)
    

-- | disjoin will distribute a decision. This will synchronize the
-- choice signal with the external signal (a special case for which
-- B_tshift was heavily revised) then apply the split.
disjoinB :: B (S p a :&: ((S p () :&: x) :|: y) )
              ( (S p a :&: x) :|: (S p a :&: y) )
disjoinB = disjSynchB >>> latentBC0Time disjSigsB

-- apply synchronization between bonf and bonslf
disjSynchB :: B (S p a :&: ((S p () :&: x) :|: y) ) 
                 (S p a :&: ((S p () :&: x) :|: y) )
disjSynchB = B_tshift disjSynchTS

-- This is a specialized `synch` that targets just the two elements.
-- will delay at most one of the two input signals. Will not synch
-- if one of right or left is dead.
disjSynchTS :: TS (S p a :&: ((S p () :&: x) :|: y))
disjSynchTS auxy =
    let a   = lnd_fst auxy in
    let u   = (lnd_fst . lnd_left . lnd_snd) auxy in
    let x   = (lnd_snd . lnd_left . lnd_snd) auxy in
    let y   = (lnd_right . lnd_snd) auxy in
    let aLiv = (ldt_live . lnd_sig) a in
    let uLiv = (ldt_live . lnd_sig) u in
    let yLiv = ldt_anyLive y in
    let xLiv = ldt_anyLive x in
    assert (uLiv == xLiv) $
    assert (aLiv == (xLiv || yLiv)) $
    -- don't synch dead branches; we'll just pipe `S p a` to the
    -- right destination in these cases.
    if (not xLiv || not yLiv) then auxy else
    -- otherwise we actually need synch
    let sdt = shallowSynch (lnd_sig a) (lnd_sig u) in
    let a' = LnkDUnit $ sdt { ldt_live = aLiv } in
    let u' = LnkDUnit $ sdt { ldt_live = uLiv } in
    (a' `LnkDProd` ((u' `LnkDProd` x) `LnkDSum` y))

-- primary disjoin behavior, includes latent optimization for dead
-- code on input (i.e. binl, binr)
disjSigsB :: LnkD LDT (S p a :&: ((S p () :&: x) :|: y))
          -> B (S p a :&: ((S p () :&: x) :|: y) )
               ( (S p a :&: x) :|: (S p a :&: y) )
disjSigsB tr = mkLnkB disjTR lnkDisj
    where lnkDisj = MkLnk { ln_tsen = False
                          , ln_peek = 0
                          , ln_build = buildDisj tr
                          }

-- translate time from before and after disjoin. This also computes
-- dead-code before and after, since one or both branches might be 
-- dead on input (due to binl or binr)
disjTR :: TR (S p a :&: ((S p () :&: x) :|: y) )
             ( (S p a :&: x) :|: (S p a :&: y) )
disjTR auxy = 
    let a = lnd_fst auxy in
    let u = (lnd_fst . lnd_left . lnd_snd) auxy in
    let x = (lnd_snd . lnd_left . lnd_snd) auxy in
    let y = (lnd_right . lnd_snd) auxy in
    let adt = lnd_sig a in
    let udt = lnd_sig u in
    let aLiv = ldt_live adt in
    let uLiv = ldt_live udt in
    let yLiv = ldt_anyLive y in
    let xLiv = ldt_anyLive x in
    assert (uLiv == xLiv) $
    assert (aLiv == (xLiv || yLiv)) $
    let l = LnkDUnit $ adt { ldt_live = (aLiv && xLiv) } in
    let r = LnkDUnit $ adt { ldt_live = (aLiv && yLiv) } in
    (l `LnkDProd` x) `LnkDSum` (r `LnkDProd` y)


-- build the disjoin behavior. This takes a little information about the
-- inputs so it can optimize for if only the u path or y path is live.
buildDisj :: LnkD LDT (S p a :&: ((S p () :&: x) :|: y))
          -> Lnk ((S p a :&: x) :|: (S p a :&: y)) 
          -> IO (Lnk (S p a :&:  ((S p () :&: x) :|: y)))
buildDisj tr =
    -- account for dead code on input, i.e. due to binl, binr. 
    let aLiv = (ldt_live . lnd_sig . lnd_fst) tr in
    let uLiv = (ldt_live . lnd_sig . lnd_fst . lnd_left . lnd_snd) tr in
    let xLiv = (ldt_anyLive . lnd_snd . lnd_left . lnd_snd) tr in
    let yLiv = (ldt_anyLive . lnd_right . lnd_snd) tr in
    assert (uLiv == xLiv) $ -- RDP invariant on signals
    assert (aLiv == (xLiv || yLiv)) $ -- RDP invariant on signals 
    if (not aLiv) then buildDisjXY else 
    if (not yLiv) then buildDisjX else -- special case, only X lives
    if (not xLiv) then buildDisjY else -- special case, only Y lives
    buildDisjXY

-- specializations of buildDisj based on partial dead inputs due to
-- binl, binr. If one path is dead, then `S p ()` is unnecessary to 
-- perform the disjoin (can simply push signal into correct path). 
--
-- Dead outputs are accounted for by these specializations.
buildDisjX, buildDisjY, buildDisjXY :: 
    Lnk ((S p a :&: x) :|: (S p a :&: y)) ->
    IO (Lnk (S p a :&:  ((S p () :&: x) :|: y)))

buildDisjX lxry = 
    -- pipe `S p a` directly to l
    let l = (ln_fst . ln_left) lxry in
    let x = (ln_snd . ln_left) lxry in
    let y = (ln_snd . ln_right) lxry in
    return (l `LnkProd` ((LnkDead `LnkProd` x) `LnkSum` y))

buildDisjY lxry =
    -- pipe `S p a` directly to r
    let x = (ln_snd . ln_left) lxry in
    let r = (ln_fst . ln_right) lxry in
    let y = (ln_snd . ln_right) lxry in
    return (r `LnkProd` ((LnkDead `LnkProd` x) `LnkSum` y))

buildDisjXY lxry = 
    let l = (ln_fst . ln_left) lxry in
    let x = (ln_snd . ln_left) lxry in
    let r = (ln_fst . ln_right) lxry in
    let y = (ln_snd . ln_right) lxry in
    let bLnkDead = ln_null l && ln_null r in -- don't need `S p a`.
    let lnkIfDead = LnkDead `LnkProd` ((LnkDead `LnkProx` x) `LnkSum` y) in
    if bLnkDead then return lnkIfDead else
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

-- | as bzipWith, but not constrained to valid partition class
bUnsafeZipWith :: (a -> b -> c) -> B (S p a :&: S p b) (S p c)
bUnsafeZipWith = bUnsafeSigZip . s_zip

-- | generic combiner for two signals. This requires some intermediate
-- state to build the signals and recombine them. That state is GC'd as
-- it updates.
bUnsafeSigZip :: (Sig a -> Sig b -> Sig c) -> B (S p a :&: S p b) (S p c)
bUnsafeSigZip fn = bUnsafeLnk $ MkLnk 
    { ln_tsen = False
    , ln_build = mkln_zip fn
    }

mkln_zip :: (Sig a -> Sig b -> Sig c) -> Lnk (S p c) -> IO (Lnk (S p a :&: S p b))
mkln_zip fn lnc = 
    if (ln_null lnc) then return LnkDead else
     
--ln_mksigzip :: (Sig x -> Sig y -> Sig z) -> LnkUp z -> IO (LnkUp x, LnkUp y)

-- | lift choice of data to choice of behaviors
bsplit :: B (S p (Either a b)) (S p a :|: S p b)
bsplit = bUnsafeSplit

-- | as bsplit, but not constrained to partition class
bUnsafeSplit :: B (S p (Either a b)) (S p a :|: S p b)
bUnsafeSplit = bUnsafeLnk $ MkLnk
    { ln_tsen = False
    , ln_build = return . ln_split
    }



-- todo:
--   Context - via Control.Make.
--     will use an applicative behavior transformer.
--     add tuning for partitions, bconst, badjeqf, bforce, bstrat
--
--   Sirea Context - might not be worth it
--     tuning: max outstanding ops between threads
--     initial partitions (if any) can be added
--     behaviors in each thread
--     can be used for multiple initial behaviors.
--
--   Should I attempt to support `periodic` actions?
--      could simplify inclusion of OpenGL and similar within Sirea.
--      But I could instead get OpenGL support via a dedicated thread,
--        without compromising computation of Sirea behaviors.
--      Alternatively, this could be provided by the 
--
--   Maybe some sort of partition-local observable variables? (PVar?)
--  
--   Observable variables: (OVar y)
--     some sort of externally managed observable variable
--
--     not very useful for RDP, but could be useful at the boundary
--       between RDP and an IO model. 
--     maybe set `in future` as with DT -> a -> IO (), to avoid any
--       unnecessary recomputations and avoid feedback cycles.
--     something to think about, I suppose.
--
--     should not be set from inside a Sirea thread.
--     can set the variable to a Signal? (in which case we need to
--       distinguish a response of set vs. unset).
--     or can set variable to a series of [(T,a)] values.
--     or can set one future at a time. set :: DT -> a -> IO ().
--       (which would allow setting the value for the future).
--
--   Behavior transformer for static context? e.g. env. vars
--
--   bcross: change partitions. 
--     partitions are named by types (Data.Typeable)
--     each partition is `created` once per Sirea Context
--        might pre-exist; that's okay, too.
--     once created, stored in a map for the Sirea behavior.
--        map lost only on shutdown. no thread GC during operation.
--
--     within the destination thread:
--       reduce multiple updates to one update
--       touch the destination & schedule send
--       allows huge batch updates to be processed efficiently
--     within source thread:
--       updates to a remote partition are batched by thread ID
--       batches delivered all at once at end of round
--       multiple rounds might occur.
--     controlling progress of each partition? 
--       flow-based programming, finite number of buffers between
--       each partition; will wait-on-send whenever a thread is too
--       far ahead of its partners.
--
--     thoughts:
--       * Use a typeclass to obtain.
--       * Provide an argument to the typeclass:
--            the mapping mechanism?
--         Or...
--         Assert that the typeclass IO operation will be called once
--           per unique identifier (String) for partitions
--           (this seems viable; Sirea keeps its own map, no global)
--
--     if I use MkLnk, I probably cannot pass a useful argument for
--     the partitioning map. So I probably need to handle this as a
--     dedicated B operation
--        - accumulation for batch updates
--        - can I benefit from batching touches?
--             maybe if I reify the batches to avoid double-batching
--             from a single partition. Basically I'd need to mark 
--             each batch with its source, take up to one batch per
--             source, touch them, then apply updates. Doable? Maybe.
--        - take one frame from each input source. apply all touch.
--             apply all updates in batch. Repeat until no input.
--             Generate all output? hmmm...
--        - I need to somehow combine redundant updates, otherwise I
--             will never reduce computation. A good place to do so
--             is upon bcross. bcross behavior itself can store the
--             duplicate updates, perform `touch` locally, then do a
--             big `emit` phase. (*** seems very promising. ***)
--
--   bcross with dynamic behaviors...
--      this is a bit more difficult; does a dynamic behavior get new
--      threads or use the existing threads?
--          * For OpenGL and such, using existing threads is better.
--          * 
--
--   Will I need a dedicated B operation for bcross?
--      doing so would avoid need for `global` state
--      it is also `simple` in that only need one type?
--          (S p a) to (S p' a)
--      unless I need some special features (like dedicated types)
--
--   I think a dedicated `Partition context` would be appropriate.
--
--     but would passing as argument to bcross also work?
--     or even as argument to running a behavior? (i.e. so developer
--        can prepare some threads in advance)?
--       
-- Specific to signal and partition types.
--     dedicated thread per partition, or at least for certain partitions
--     maybe use a dedicated variation of bcross? Or alternatively, create
--     a typeclass for obtaining the necessary data for entering each
--     partition (and starting up any associated threads)
--   bpeek (anticipate)

--   
-- weaker disjoin?
-- initial stateful and pseudo-state ops
--- reactive term rewriting, reactive state transition, 
--- reactive constraint-logic


-------------------------------------------
------------------ UTILITY ----------------
-------------------------------------------

-- beqshift is a helper behavior. It seeks the first visible change
-- in a signal, relative to its prior value and stability, and will
-- shift the signal update to occur at that time. Note that beqshift
-- only changes the `su_time` value of a signal.
beqshift :: (a -> a -> Bool) -> B (S p a) (S p a)
beqshift eq = B_mkLnk tr_fwd shiftLnk
    where shiftLnk = MkLnk { ln_tsen = False, ln_build = bdshift }
          bdshift LnkDead = LnkDead
          bdshift (LnkSig x) = 
                newIORef st_zero >>= \ rfSt ->
                return $! LnkSig $! ln_eqshift rfSt eq x

ln_eqshift :: IORef (SigSt a) -> (a -> a -> Bool) -> LnkUp a -> LnkUp a
ln_eqshift rfSt eq ln = LnkUp { ln_touch = pokefwd, ln_update = upd }
    where pokefwd = ln_touch ln
          upd su = 
            readIORef rfSt >>= \ st ->
            let s0  = st_state st in
            let st' = st_sigup su st in
            let stClr = maybe (flip st_clear st') 
                          st_zero (st_stable st') in
            stClr `seq` 
            writeIORef rfSt stClr >> -- updated!
            case su_state su of
                Nothing -> ln_update ln su
                Just (sf,tLower) ->
                    let tUpper = maybe (flip addTime dt_eqf_peek) 
                                    tLower (su_stable st) in
                    if (tLower >= tUpper) then ln_update ln su else
                    let (sf',tu') = eqshift eq s0 sf tLower tUpper in
                    let su' = su { su_state = Just (sf',tu') } in
                    tu' `seq` sf' `seq` 
                    ln_update ln su'


-- eqshift initiates a pairwise comparison
eqshift :: (a -> b -> Bool) -> Sig a -> Sig b -> T -> T -> (Sig b, T)
eqshift eq as bs tLower tUpper =
    assert (tLower < tUpper) $
    let (a0,as') = s_sample as tLower in
    let (b0,bs') = s_sample bs tLower in
    let eqStart = maybeEq eq a0 b0 in
    if eqStart 
        then eqshift_i eq tLower tUpper a0 as' b0 bs' 
        else (sf', tLower)

-- find first difference between signals?
-- at this point we know both s0 and sf are equal to 
-- sample at their heads. There might be more to
-- see, of course. 
eqshift_i :: (a -> b -> Bool) -> T -> T 
          -> Maybe a -> Sig a -> Maybe b -> Sig b 
          -> (Sig b, T)
eqshift_i eq tL tU a0 as b0 bs =
    let (ma,as') = s_sample_d as tL tU in
    let (mb,bs') = s_sample_d bs tL tU in
    case (ma0,maf) of
        (Nothing, Nothing) -> (sf', tU) -- all done!
        (Nothing, Just (tf,af)) ->
            let eqSamp = maybeEq eq sample af in
            if eqSamp 
                then eqshift_i eq sample s0' sf' tL tU
                else (sf',tf)
        (Just (t0,a0), Nothing) -> 
            let eqSamp = maybeEq eq sample a0 in
            if eqSamp 
                then eqshift_i eq 

            
maybeEq :: (a -> b -> Bool) -> (Maybe a -> Maybe b -> Bool)
maybeEq _ Nothing Nothing = True
maybeEq eq (Just x) (Just y) = eq x y
maybeEq _ _ _ = False


