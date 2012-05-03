

-- | BImpl has the implementation details for most of Sirea's
-- concrete behaviors, mostly because I did not want them cluttering
-- FRP.Sirea.Behavior (which is about documenting typeclasses).
--
-- This includes both the symbolic and concrete implementations for
-- basic behaviors except for `first` and `left`, which need special 
-- treatment at compilation. 
module FRP.Sirea.Internal.BImpl
    ( fwdB
    , fmapB, constB, forceB, stratB, adjeqfB -- BFmap
    , delayB, synchB, peekB, forceDelayB
    , disjoinB, zapB, splitB
    , fstB, firstB, swapB, dupB, assoclpB -- BProd (first pass)
    , fstBLnk, swapBLnk, dupBLnk, assoclpBLnk -- impls
    , inlB, leftB, mirrorB, mergeB, assocrsB -- BSum
    , inlBLnk, mirrorBLnk, assocrsBLnk -- impls
    , mergeBLnkOpt
    ) where

import Prelude hiding (id,(.))
import FRP.Sirea.Internal.STypes
import FRP.Sirea.Internal.LTypes
import FRP.Sirea.Internal.BTypes
import FRP.Sirea.Signal
import Control.Category
import Control.Exception (assert)

instance Category B where
  id  = fwdB
  (.) = flip B_pipe

-- miscellaneous. Note that:
--   first, left, and merge
-- need some special support from the compiler. First and left
-- need it for traversals, and merge for optimizations.
fwdB     :: B x x 
firstB   :: B x x' -> B (x :&: y) (x' :&: y)
fstB     :: B (x :&: y) x
swapB    :: B (x :&: y) (y :&: x)
dupB     :: B x (x :&: x)
assoclpB :: B (x :&: (y :&: z)) ((x :&: y) :&: z)
leftB    :: B x x' -> B (x :|: y) (x' :|: y)
inlB     :: B x (x :|: y)
mirrorB  :: B (x :|: y) (y :|: x)
mergeB   :: B (x :|: x) x
asscorsB :: B (x :|: (y :|: z)) ((x :|: y) :|: z)

firstB   = B_first
leftB    = B_left

mkLnkPure :: (Lnk y -> Lnk x) -> MkLnk x y
mkLnkPure bfn = MkLnk { ln_tsen = False 
                      , ln_peek = 0
                      , ln_build = return . bfn  
                      }

mkLnkB :: TR x y -> MkLnk x y -> B x y
mkLnkB = B_mkLnk

-- fwdB is the simplest behavior...
fwdB = mkLnkB id $ mkLnkPure id

-- in fstB, snd output is dead. 
fstB = mkLnkB lnd_fst $ mkLnkPure lnFst
    where lnFst ln = LnkProd ln LnKDead 

-- simple swap on Lnk sinks
swapB = mkLnkB trSwap $ mkLnkPure lnSwap
    where trSwap tr = LnkDProd (lnd_snd tr) (lnd_fst tr)
          lnSwap ln = LnkProd (ln_snd ln) (ln_fst ln)

-- deep-duplicate signals (at least where two sinks are available)
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

-- simple rearrangement like swap            
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
           
-- if inl, can ignore the right bucket
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
mirrorB = mkLnkB trMirr $ mkLnkPure lnMirr 
    where trMirr tr = LnkDSum (lnd_right tr) (lnd_left tr)
          lnMirr ln = LnkSum (ln_right ln) (ln_left ln)

-- simple rearrangement
assoclsB = mkLnkB trRotl $ mkLnkPure lnAso
    where trRotl tr =
            let tx = lnd_fst tr in
            let ty = (lnd_snd >>> lnd_fst) tr in
            let tz = (lnd_snd >>> lnd_snd) tr in
            LnkDSum (LnkDSum tx ty) tz
          lnAso ln =
            let x = ln_fst ln in
            let y = (ln_snd >>> ln_fst) ln in
            let z = (ln_snd >>> ln_snd) ln in
            LnkSum (LnkSum x y) z

-- merge is among the most challenging behaviors, in part because it
-- must 
mergeB :: LnkD LDT (x :|: x) -> B (x :|: x) x


mergeSynchB :: B (x :|: x) (x :|: x)
mergeSynchB



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

-- | disjoin will distribute a decision. This will synchronize the
-- choice signal with the external signal (a special case for which
-- B_tshift was heavily revised) then apply the split.
disjoinB :: B (S p a :&: ((S p () :&: x) :|: y) )
               ( (S p a :&: x) :|: (S p a :&: y) )
disjoinB = B_tshift disjSynch >>> B_mkLnk disjTime disjLnk
    where disjLnk = MkLnk { ln_tsen = False
                          , ln_peek = 0
                          , ln_build = disjBuild 
                          }

-- This is a specialized `synch` that targets just the two elements.
-- will perform minimal synch based on actual delays.
disjSynch :: TS (S p a :&: ((S p () :&: x) :|: y))
disjSynch auxy =
    let a   = lnd_fst auxy in
    let u   = (lnd_fst . lnd_left . lnd_snd) auxy in
    let x   = (lnd_snd . lnd_left . lnd_snd) auxy in
    let y   = (lnd_right . lnd_snd) auxy in
    let dta = lnd_sig a in
    let dtu = lnd_sig u in
    let dtSynch = (max `on` ldt_goal) dta dtu in
    let dtCurr = (max `on` ldt_curr) dta dtu in
    let dt' = LDT { ldt_curr = dtCurr, ldt_goal = dtSynch } in
    let a' = LnkDProd dt' in
    let u' = LnkDProd dt' in
    a' `LnkDProd` ((u' `LnkDProd` x) `LnkDSum` y)

-- translate time from before and after disjoin
-- unlike most MkLnk time translations, disjoin preserves timing
-- of the x and y elements.
disjTime :: TR (S p a :&: ((S p () :&: x) :|: y) )
               ( (S p a :&: x) :|: (S p a :&: y) )
disjTime auxy = axay 
    where a = lnd_fst auxy
          x = (lnd_snd . lnd_left . lnd_snd) auxy
          y = (lnd_right . lnd_snd) auxy 
          axay = (a `LnkDProd` x) `LnkDSum` (a `LnkDProd` y)

-- disjBuild decides whether this is dead code (LnkDead) and otherwise
-- builds the necessary state and passes the buck.
disjBuild :: Lnk ( (S p a :&: x) :|: (S p a :&: y) )
      -> IO (Lnk (S p a :&: ((S p () :&: x) :|: y) ) )
disjBuild lxry = 
    let lnl = (ln_fst . ln_left) lxry in
    let lnr = (ln_fst . ln_right) lxry in
    let x   = (ln_snd . ln_left) lxry in
    let y   = (ln_snd . ln_right) lxry in
    let bNull = ln_null lnl && ln_null lnr in
    let LnkDead = LnkDead `LnkProd` ((LnkDead `LnkProd` x) `LnkSum` y) in
    if bNull then return LnkDead else 
    let l   = ln_lnkup lnl in
    let r   = ln_lnkup lnr in
    let onTouch = ln_touch l >> ln_touch r in
    let onEmit sm = 
            let sul = sm_emit disjMaskLeft sm in
            let sur = sm_emit disjMaskRight sm in
            ln_update l sul >> ln_update r sur
    in
    ln_withSigM onTouch onEmit >>= \ (a,u) ->
    return (LnkSig a `LnkProd` ((LnkSig u `LnkProd` x) `LnkSum` y))

-- maskLeft and maskRight must take the two original signals and
-- generate the split signals for the disjoin function. Assume 
-- that the signal has already been synchronized...
disjMaskLeft, disjMaskRight :: Sig a -> Sig () -> Sig a
disjMaskLeft = s_mask
disjMaskRight sa su = s_mask sa su'
    where su' = s_full_zip inv su
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


