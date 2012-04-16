
{-# LANGUAGE TypeOperators #-}

-- | This module describes the basic RDP behaviors in Sirea. It also
-- exports the concrete behavior type `B` for clients.
--
-- Several behaviors are provided in dedicated modules:
--   see FRP.Sirea.Link for bUnsafeLnk
--   see FRP.Sirea.Partition for bcross, bscope
module FRP.Sirea.Behavior  
    ( (:&:), (:|:), S, B -- from FRP.Sirea.Internal.STypes
    , (>>>) -- from Control.Category
    , bfwd, bfmap, bconst, bvoid
    , bfirst, bsecond, (***), bswap, bdup, bfst, bsnd, bassoclp, bassocrp, (&&&)
    , bleft, bright, (+++), bmirror, bmerge, binl, binr, bassocls, bassocrs, (|||)
    , bconjoinl, bconjoinr
    , bdisjoin0
    , bzip, bzipWith
    , bsplit
    , bdelay, bsynch, bdelbar
    -- , bcross is in 
    -- , bUnsafeLnk
    
    ) where

import Prelude hiding (id,(.))
import Control.Category

import FRP.Sirea.Internal.STypes
import FRP.Sirea.Internal.BTypes (B)
import FRP.Sirea.Time (DT)

infixr 3 ***
infixr 2 +++



This module describes abstract behaviors for RDP, and one type
-- of concrete behaviors, B, for Sirea. Not all relevant behaviors 
-- are defined in this file, just the generic ones. 
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
--   blocal - logical, thread-local sub-partitions.
--   bcross - communicate a signal between threaded partitions
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
--
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

-- | bfwd is just another name for Control.Category.id.
bfwd :: (Category b) => b x x
bfwd = id


class (Category b) => BFmap b where
    bfmap :: (a -> b) -> b (S p a) (S p b)
    bconst :: c -> b (S p a) (S p c)
    bconst = bfmap . const


-- | DYNAMIC BEHAVIORS
--
-- RDP behaviors may be first-class, which enables Object Oriented
-- styles of programming, and object capability model approaches to
-- security. This is represented by having signals carry behaviors
-- (which looks in Haskell's type system like 'S p (B x y)'). Small
-- behaviors can be composed into complex applications then invoked
-- with beval or bexec.
--
-- Unlike OO styles, or even FRP switches, first-class behaviors in
-- RDP cannot be stored. They are volatile, implicitly revoked after
-- you stop sharing them. This is a valuable property for security,
-- safety, resource management (including GC), and live programming.
-- 

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
bdisjoin :: B (S p a :&: ((S p () :&: x) :|: y) )
               ( (S p a :&: x) :|: (S p a :&: y) )
bdisjoin = B_tshift disjSynch >>> B_mkLnk disjTime disjMkLnk
    where disjMkLnk = MkLnk { ln_tsen = False, ln_build = disjBuild }

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


{- transformative behaviors. Need a dedicated `Trans` model, which 
   in turn needs a 'class' for behaviors.
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
-- It applies a delay to signals (i.e. s_delay) logically, which can
-- affect the timing of real-world effects controlled by the signal.
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
-- applied once when necessary (e.g. for time-sensitive operations).
-- This will force immediate application of delays. In rare cases, 
-- forcing the delay might avoid a lot of minor delays down the line,
-- but the benefit is likely marginal even then. 
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
--   Sirea Context - might not be worth it (painful for user)
--     tuning: max outstanding ops between threads
--     initial partitions (if any) can be added
--     can be used for multiple initial behaviors.
--
--   Should I attempt to support `periodic` actions?
--      could simplify inclusion of OpenGL and similar within Sirea.
--      But I could instead get OpenGL support via a dedicated thread,
--        without compromising computation of Sirea behaviors.
--      Alternatively, this could be provided by the 
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
    case (ma,mb) of
        (Nothing, Nothing) -> (bs', tU) -- all done!
        (Nothing, Just (tb,bf)) ->
            let eqSamp = maybeEq eq a0 bf in
            if eqSamp 
                then eqshift_i eq tL tU a0 as' bf bs'
                else (bs',tb)
        (Just (ta,af), Nothing) -> 
            let eqSamp = maybeEq eq af b0 in
            if eqSamp 
                then eqshift_i eq tL tU af as' b0 bs'
                else (bs',ta)
        (Just (ta,af), Just (tb,bf)) ->
            case compare ta tb of
                
            
maybeEq :: (a -> b -> Bool) -> (Maybe a -> Maybe b -> Bool)
maybeEq _ Nothing Nothing = True
maybeEq eq (Just x) (Just y) = eq x y
maybeEq _ _ _ = False



