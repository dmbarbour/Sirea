
{-# LANGUAGE TypeOperators, EmptyDataDecls, GADTs, TypeFamilies #-}

module FRP.Sirea.Behavior  
    ( (:&:), (:|:), B, S -- from FRP.Sirea.Internal.BTypes & STypes
    , (>>>) -- from Control.Category
    , bfwd, bfmap, bconst, bvoid
    , bfirst, bsecond, (***), bswap, bdup, bfst, bsnd, bassoclp, bassocrp
    , bleft, bright, (+++), bmirror, bmerge, binl, binr, bassocls, bassocrs
    , bconjoinl, bconjoinr
    , bdisjoin0
    , bzip, bzipWith
    , bsplit
    , bdelay, bsynch, bdelbar
    -- , bcross
    , bUnsafeLnk
    
    ) where

import Prelude hiding (id,(.))
import Control.Category

import FRP.Sirea.Internal.STypes
import FRP.Sirea.Internal.BTypes
import FRP.Sirea.Internal.Types

import FRP.Sirea.Signal
import FRP.Sirea.Time
import FRP.Sirea.Link 

import Data.IORef
import Data.Function (on)
import Control.Monad (unless)


infixr 3 ***
infixr 2 +++

-- I really don't trust the RULES pragma, but I haven't gotten
-- around to applying my own optimizations. Here are a few simple
-- cases that are likely to happen often.
{-# RULES
"bfmap.bfmap" forall f g .
                (bfmap f) . (bfmap g) = bfmap (f . g)
"bfmap.bconst" forall f c . 
                (bfmap f) . (bconst c) = bconst (f c)
"bconst.bfmap" forall c f .
                (bconst c) . (bfmap f) = bconst c
"bconst.bconst" forall c d .
                (bconst c) . (bconst d) = bconst c

"bswap.bswap"  bswap . bswap = id
"bmirror.bmirror" bmirror . bmirror = id

"bfirst.bfirst" forall f g .
                (bfirst f) . (bfirst g) = bfirst (f . g)
"bleft.bleft"   forall f g .
                (bleft f) . (bleft g) = bleft (f . g)
"bright.bleft"  forall f g .
                (bright f) . (bleft g) = (bleft g) . (bright f)
"bsecond.bfirst" forall f g .
                (bsecond f) . (bfirst g) = (bfirst g) . (bsecond f)
 #-}

-- TUNING
-- dt_eqf_peek: 
--   (for bconst, badjeqf via beqshift)
--   The first update in a signal might not represent a real change.
--   Delivering it as a change, however, might cause redundant eval
--   later in the pipeline (e.g. when zipping values). Ideally find
--   the first real change in signal and deliver that as the time of 
--   signal state update. But discovering such a time potentially 
--   needs infinite search. As compromise, I search bounded distance
--   into future for change, relative to stability. Idea is that the
--   computation doesn't run very far ahead of stability anyway, so
--   pushing update ahead of stability is almost free. 
dt_eqf_peek :: DT
dt_eqf_peek = 3.0 -- seconds ahead of stability


instance Category B where
  id  = B_fwd
  (.) = flip B_pipe

-- | RDP behaviors are arrows, but incompatible with Control.Arrow
-- due to `arr` being more powerful than RDP allows. A number of
-- behaviors support arrow composition, and several serve as basic
-- data plumbing that `arr` would perform in Control.Arrow. These
-- are also incompatible with Megacz's Generalized Arrows because no 
-- generic unit signal exists (cannot just create signals).
--
-- In Sirea, most data plumbing is essentially free at runtime. But
-- there are exceptions; bzip, bmerge, bconjoin, bsplit have runtime 
-- overheads, for example.
--
-- Listed here are several `generic` behaviors, excluding resource
-- specific behaviors (e.g. state, clock, mouse, display, network,
-- filesystem, etc.) which are provided by dedicated modules.
--
--  CATEGORY
--   bfwd - identity behavior
--   (>>>) - forward composition (from Control.Category)
--
--  PRODUCTS
--   bfirst b - apply b on first element in product
--   bsecond b - apply b on second element in product
--   (b1 *** b2) = bfirst b1 >>> bsecond b2
--   bswap - flip first and second signals
--   bdup - duplicate signal
--   bfst - keep first signal, drop second
--   bsnd - keep second signal, drop first
--   bassoc(l|r)p - associate left or right on product
--   bzipWith, bzip - combine two concrete signals
-- 
--  SUM (CHOICE)
--   bleft b - apply b on left option in sum
--   bright b - apply b on right option in sum
--   (bl +++ br) - bleft bl >>> bright br
--   bmirror - flip left and right signals
--   bmerge - combine two choices into one signal (implicit synch)
--   binl - static choice of left option (~ if true) 
--   binr - static choice or right option (~ if false)
--   bassoc(l|r)s - associate left or right on sum
--   bsplit - lift a decision in a signal to asynchronous layer
--
--  ARROW MISC
--   bfmap - apply function to a concrete signal (when active)
--   bconst - map a constant value to a concrete signal (when active) 
--   bvoid - execute one behavior but drop the result
--   bconjoin - partial merge on a product of sums
--   bdisjoin - distribute a sum into a product
--
--  SPATIAL-TEMPORAL
--   bdelay - delay a signal by a fixed amount
--   bsynch - synch an asynchronous signal (to slowest)
--   bDelayBarrier - force application of aggregated delay
--   bcross - communicate a signal between partitions
--            see FRP.Sirea.Partition
--
--  EXTENSIONS AND ADAPTERS
--   bUnsafeLnk - hook FFI, legacy services, external resources
--
--  PERFORMANCE (maybe another module)
--   bforce  - apply sequential strategy relative to stability
--   bstrat  - apply parallel strategy relative to sampling
--   badjeqf - eliminate adjacent equal updates
--   bUnsafeChoke - skip minor frames when updates too fast
--
bfwd     :: B x x

bfirst   :: B x x' -> B (x :&: y) (x' :&: y)
bsecond  :: B y y' -> B (x :&: y) (x :&: y')
(***)    :: B x x' -> B y y' -> B (x :&: y) (x' :&: y')
bswap    :: B (x :&: y) (y :&: x)
bdup     :: B x (x :&: x)
bfst     :: B (x :&: y) x
bsnd     :: B (x :&: y) y
bassoclp :: B (x :&: (y :&: z)) ((x :&: y) :&: z)
bassocrp :: B ((x :&: y) :&: z) (x :&: (y :&: z))

bleft    :: B x x' -> B (x :|: y) (x' :|: y)
bright   :: B y y' -> B (x :|: y) (x :|: y')
(+++)    :: B x x' -> B y y' -> B (x :|: y) (x' :|: y')
bmirror  :: B (x :|: y) (y :|: x)
bmerge   :: B (x :|: x) x
binl     :: B x (x :|: y)
binr     :: B y (x :|: y)
bassocls :: B (x :|: (y :|: z)) ((x :|: y) :|: z)
bassocrs :: B ((x :|: y) :|: z) (x :|: (y :|: z))

bfwd = id

bfirst = B_on_fst
bsecond f = bswap >>> bfirst f >>> bswap 
(***) f g = bfirst f >>> bsecond g
bswap = B_swap
bdup = B_dup
bfst = B_fst
bsnd = bswap >>> bfst
bassoclp = B_asso_p
bassocrp = bswap3 >>> bassoclp >>> bswap3

bleft = B_on_lft
bright f = bmirror >>> bleft f >>> bmirror
(+++) f g = bleft f >>> bright g
bmirror = B_mirror
bmerge = B_merge
binl = B_in_lft
binr = binl >>> bmirror
bassocls = B_asso_s
bassocrs = bmirr3 >>> bassocls >>> bmirr3

-- bswap3 is utility for bassocrp
bswap3 :: B ((x :&: y) :&: z) (z :&: (y :&: x))
bswap3 = bfirst bswap >>> bswap

-- bmirror3 is utility for bassocrs
bmirr3 :: B ((x :|: y) :|: z) (z :|: (y :|: x))
bmirr3 = bleft bmirror >>> bmirror

-- | Map an arbitrary Haskell function across an input signal.
bfmap :: (a -> b) -> B (S p a) (S p b)
bfmap fn = B_mkLnk tr_fwd fmapLnk
    where fmapLnk = MkLnk { ln_tsen = False, ln_build = bdfmap }
          bdfmap = return . (ln_lumap . ln_sumap . su_fmap . s_fmap) fn

-- | Map a constant to a signal. A constant signal can still vary 
-- between active and inactive over time. Same as bfmap (const c),
-- but potentially much more efficient and able to move or shift
-- most redundant updates.  
bconst :: c -> B (S p a) (S p c)
bconst c = B_mkLnk tr_fwd constLnk >>> beqshift alwaysEq
    where fmapLnk = MkLnk { ln_tsen = False, ln_build = bdconst }
          bdconst = return . (ln_lumap . ln_sumap . su_fmap . s_const) c
          alwaysEq = (const . const) True

-- | `bvoid b` will activate behavior b but ignore its result.
-- The input is duplicated and passed onwards. 
bvoid :: B x y -> B x x
bvoid b = bdup >>> bfirst b >>> bsnd 

-- | conjoin is a partial merge. 
bconjoinl :: B ((x :&: y) :|: (x :&: z)) (x :&: (y :|: z))
bconjoinr :: B ((x :&: z) :|: (y :&: z)) ((x :|: y) :&: z)
bconjoinl = bdup >>> (isolateX *** isolateYZ) 
   where isolateX = (bfst +++ bfst) >>> bmerge
         isolateYZ = (bsnd +++ bsnd)
bconjoinr = (bswap +++ bswap) >>> bconjoinl >>> bswap

-- | Disjoin will distribute a decision. To achieve disjoin involves 
-- combining a signal representing a split with an external signal.
-- Disjoin is necessary for effective use of `choice` in RDP, i.e.
-- most design patterns using bsplit will use bdisjoin.
--
-- Unfortunately, disjoin is neither generic nor dual to conjoin.
-- This seems unavoidable due to potential for partitioning and the
-- need to communicate between partitions.
--
-- It may be feasible to achieve a more generic disjoin by use of
-- templates or type-based meta programming. 
--
bdisjoin0 :: B (S p a :&: ((S p () :&: x) :|: y) )
               ( (S p a :&: x) :|: (S p a :&: y) )
bdisjoin0 = B_tshift disjSynch >>> B_mkLnk disjTime disjMkLnk
    where disjMkLnk = MkLnk { ln_tsen = False, ln_build = disjBuild }


-- This is a specialized `synch` that targets just the two elements
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

-- disjBuild decides whether this is dead code (LnkNull) and otherwise
-- builds the necessary state and passes the buck.
disjBuild :: Lnk ( (S p a :&: x) :|: (S p a :&: y) )
      -> IO (Lnk (S p a :&: ((S p () :&: x) :|: y) ) )
disjBuild lxry = 
    let lnl = (ln_fst . ln_left) lxry in
    let lnr = (ln_fst . ln_right) lxry in
    let x   = (ln_snd . ln_left) lxry in
    let y   = (ln_snd . ln_right) lxry in
    let bNull = ln_null lnl && ln_null lnr in
    let lnkNull = LnkNull `LnkProd` ((LnkNull `LnkProd` x) `LnkSum` y) in
    if bNull then return lnkNull else 
    newIORef sm_zero >>= \ rfSigM ->
    let l   = ln_lnkup lnl in
    let r   = ln_lnkup lnr in
    let (a,u) = disjBuild' rfSigM l r in
    return (LnkSig a `LnkProd` ((LnkSig u `LnkProd` x) `LnkSum` y))
    
-- poke on a or u will pokeLR
-- emit will always emit to both left and right
-- mask is used to split the data (SigUp a on input) signal. 
disjBuild' :: IORef (SigM a ()) -> LnkUp a -> LnkUp a -> (LnkUp a, LnkUp u)
disjBuild' rfSigM l r = (a,u) 
    where pokeA = 
            readIORef rfSigM >>= \ sm ->
            writeIORef rfSigM (sm_update_l st_poke sm) >>
            unless (sm_waiting sm) pokeLR
          pokeU =
            readIORef rfSigM >>= \ sm ->
            writeIORef rfSigM (sm_update_r st_poke sm) >>
            unless (sm_waiting sm) pokeLR
          pokeLR = ln_touch l >> ln_touch r
          emit = 
            readIORef rfSigM >>= \ sm ->
            unless (sm_waiting sm) $
            let su_left = sm_emit disjMaskLeft sm in
            let su_right = sm_emit disjMaskRight sm in
            (writeIORef rfSigM $! sm_cleanup (su_stable su_left) sm) >>
            ln_update l su_left >>
            ln_update r su_right
          updA su = modifyIORef rfSigM (sm_sigup_l su) >> emit
          updU su = modifyIORef rfSigM (sm_sigup_r su) >> emit
          a = LnkUp { ln_touch = pokeA, ln_update = updA }
          u = LnkUp { ln_touch = pokeU, ln_update = updU }

-- maskLeft and maskRight must take the two original signals and
-- generate the split signals for the disjoin function. Assume 
-- that the signal has already been synchronized...
disjMaskLeft, disjMaskRight :: Sig a -> Sig () -> Sig a
disjMaskLeft = s_mask
disjMaskRight sa su = s_mask sa su'
    where su' = s_full_zip inv su
          inv Nothing = Just ()
          inv _       = Nothing

{-    --     
ln_mksigzip' :: IORef (SigM x y) -> (Sig x -> Sig y -> Sig z) 
           -> LnkUp z -> (LnkUp x, LnkUp y)
ln_mksigzip' rfSigM jf luz = (lux,luy)
    where pokeX = 
            readIORef rfSigM >>= \ sm ->
            writeIORef rfSigM (sm_update_l st_poke sm) >>
            unless (sm_waiting sm) (ln_touch luz)
          pokeY = 
            readIORef rfSigM >>= \ sm ->
            writeIORef rfSigM (sm_update_r st_poke sm) >>
            unless (sm_waiting sm) (ln_touch luz)
          emit  = 
            readIORef rfSigM >>= \ sm ->
            unless (sm_waiting sm) $
              let su = sm_emit jf sm in
              (writeIORef rfSigM $! sm_cleanup (su_stable su) sm) >>
              ln_update luz su 
          updX su = modifyIORef rfSigM (sm_sigup_l su) >> emit
          updY su = modifyIORef rfSigM (sm_sigup_r su) >> emit
          lux = LnkUp { ln_touch = pokeX, ln_update = updX }
          luy = LnkUp { ln_touch = pokeY, ln_update = updY }
-}


    --    the x and y elements are trivial, unchanged.
    --    the signal element is split by use of a mask and invert-mask.
    --      maybe a dedicated masking operator would be appropriate?
    --    dead code only if both outputs are dead code.
    --    update both outputs on every update.
    --    

{- transformative behaviors 
-- berrseq - composition with error options.
-- todo: move to a arrow transformer...
berrseq :: B x (err :|: y) -> B y (err :|: z) -> B x (err :|: z)
berrseq bx by = bx >>> bright by >>> bassocls >>> bleft bmerge

-- benvseq - composition with environment (~reader)
-- todo: move to a arrow transfomer
benvseq :: B (env :&: x) y -> B (env :&: y) z -> B (env :&: x) z
benvseq bx by = bdup >>> (bfst *** bx) >>> by
 -}

-- | Represent latency of calculation or communication by delaying
-- a signal a small, logical difftime. Appropriate use of delay can
-- greatly improve system consistency and efficiency. In case of a
-- complex signal, every signal receives the same delay.
--
-- Note that delay does not cause any actual delay in computation.
-- Instead, it applies a delay to signals (i.e. s_delay) logically,
-- though it can still affect actual timing of real-world behavior.
bdelay :: DT -> B x x
bdelay = B_tshift . lnd_fmap . addDelay
    where addDelay dt ldt = 
            let dtGoal = dt + (ldt_goal lt) in
            ldt { ldt_goal = dtGoal }

-- | Synch automatically delays all signals to match the slowest in
-- a composite. Immediately after synchronization, you can be sure 
-- (x :&: y) products precisely overlap, and (x :|: y) sums handoff
-- smoothly without gap or overlap. For non-composte signals, bsynch
-- has no effect. bsynch twice has no extra effect. Synchronization
-- is logical in RDP, and the implementation is wait-free.
--
-- Signals even in different partitions may be synchronized. 
bsynch :: B x x
bsynch = B_tshift doSynch
    where doSynch x =
            let dtGoal = ldt_maxGoal x in
            lnd_fmap $ \ ldt -> ldt { ldt_goal = dtGoal }
            -- setting all elements to max delay goal among them

-- | Normally bdelay and bsynch do not cause immediate processing of
-- the signal. Instead, multiple small delays are accumulated then 
-- applied once when necessary (i.e. before an effectful operation),
-- called the delay aggregation optimization. bdelbar serves as a
-- barrier against this optimization for cases where a developer
-- needs tight control over performance properties.
bDelayBarrier :: B x x
bDelayBarrier = B_tshift doBar
    where doBar = lnd_fmap $ \ ldt -> ldt { ldt_curr = (ldt_goal ldt) }



-- bcross:
--   do I make it for a specific partition type?
--   or do I make a typeclass for entering partitions with IO?
--   I think I'll skip this for now and get back to it... in 
--   another module.
-- not a typeclass for bcross, but per-partition could be okay.


-- | combine a product of signals into a signal of products
bzip :: B (S p a :&: S p b) (S p (a,b))
bzip = bzipWith (,)

-- | combine signals with a given function
bzipWith :: (a -> b -> c) -> B (S p a :&: S p b) (S p c)
bzipWith = bUnsafeZipWith

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
    if (ln_null lnc) then return LnkNull else
     
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

-- | bUnsafeLnk extends Sirea with primitive behaviors, FFI, foreign
-- services, legacy adapters, access to state and IO. Most primitive
-- behaviors that touch signals are implemented atop the same MkLnk
-- mechanism. bUnsafeLnk can be used safely, but it takes caution to
-- avoid violating RDP's declarative properties:
--
--   spatial commutativity - order of link creation or attach does
--     not affect program behavior.
--   spatial idempotence - if two links have equivalent demands at a
--     given time, they have equivalent response. Duplicate demands
--     do not cause any additional effect.
--   duration coupling - the activity of response is tightly coupled
--     to activity of demand. Signals cannot be created or destroyed
--     by the link, only transformed.
--   locally stateless - caches are allowed, but there should be no
--     `history` of a signal kept in the link itself; i.e. if the
--     link is destroyed and created fresh, it will recover the same
--     state it had before. State can be modeled as external to the
--     link (e.g. in a filesystem or database)
--   eventless - a signal with zero duration is never observed. If 
--     the link is updated multiple times at a given instant, only
--     the last update should have a lasting effect on system state.
--   
-- Further the developer must ensure that the created links properly
-- detach when the signal is in a final state (s_fini) so that the
-- behavior can be garbage collected. This is especially important
-- when using dynamic behaviors!
--
-- While delay is a normal part of RDP behavior, it is also critical
-- that it be accessible at compile time, so bUnsafeLnk must add no
-- delay to signals. Use bdelay instead. 
--
-- Complex signals entering bUnsafeLnk are implicitly synchronized.
--
-- Each instance of bUnsafeLnk results in construction of one link 
-- (via ln_build operation) when the Sirea behavior is started. Dead 
-- code from binl or binr would be an exception. The IO operation in
-- MkLnk is for intermediate caches and other preparation, not for
-- observable side-effects. The link only becomes active when the
-- signal is updated.
--
-- Hopefully a few useful libraries of behaviors can be built atop 
-- this to cover most common requirements safely.
bUnsafeLnk :: MkLnk x y -> B x y
bUnsafeLnk mklnk = bsynch >>> B_tshift xBarrier >>> B_mkLnk tr_unit mklnk
    where xBarrier dts = 
            let bNeedBarrier = ldt_minCurr dts /= ldt_maxCurr dts in
            if bNeedSynch || ln_tsen mkLnk 
                then flip lnd_fmap dts $ \ x -> x { ldt_curr = (ldt_goal x) }
                else dts -- no change; all or nothing (for now)






-- todo:
--   bcross: change partitions. 
--     partitions are named by types (Data.Typeable)
--     each partition is `created` once per Sirea behavior
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
          bdshift LnkNull = LnkNull
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

eqshift :: (a -> a -> Bool) -> Sig a -> Sig a -> T -> T -> (Sig a, T)
eqshift eq s0 sf tLower tUpper =
    assert (tLower < tUpper) $
    let s0' = s_trim 
            



