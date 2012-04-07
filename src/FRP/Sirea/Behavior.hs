
{-# LANGUAGE TypeOperators, EmptyDataDecls, GADTs, TypeFamilies #-}

module FRP.Sirea.Behavior  
    ( (:&:), (:|:), B, S -- from FRP.Sirea.Internal.BTypes
    , (>>>) -- from Control.Category
    , bfwd, bfmap, bconst, bvoid
    , bfirst, bsecond, (***), bswap, bdup, bfst, bsnd, bassoclp, bassocrp
    , bleft, bright, (+++), bmirror, bmerge, binl, binr, bassocls, bassocrs
    , bzip, bzipWith, bsplit
    , bdelay, bsynch
    , bconjoinl, bconjoinr,
    -- , bdisjoin0
    , bUnsafeLnk
    ) where

import Prelude hiding (id,(.))
import Control.Category

import FRP.Sirea.Signal
import FRP.Sirea.Time
import FRP.Sirea.Internal.Types
import FRP.Sirea.Internal.BTypes
import Data.IORef
import Data.Function (on)


infixr 3 ***
infixr 2 +++

-- I really don't trust the RULES pragma, but I haven't gotten
-- around to applying my own optimizations. Here are a few simple
-- cases that are likely to happen often.
{-# RULES
"bfmap>>>bfmap" forall f g .
                (bfmap f) . (bfmap g) = bfmap (f . g)
"bconst>>>bfmap" forall f g . 
                (bfmap f) . (bconst c) = bconst (f c)
 #-}

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
--   bdisjoin - distribute a decision into a product
--
--  SPATIAL-TEMPORAL
--   bdelay - delay signals (multi-part signals delayed equally)
--   bsynch - synch an asynchronous signal (to slowest)
--   bcross - communicate a signal between partitions
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
bfmap fn = B_mkLnk $ MkLnk { ln_tsen = False, ln_build = bdFmap }
    where bdFmap = ln_sumap (su_fmap (s_fmap fn))

-- | Map a constant to a signal. A constant signal can still vary 
-- between active and inactive over time. Same as bfmap (const c),
-- but potentially more efficient. 
bconst :: c -> B (S p a) (S p c)
bconst c = B_mkLnk $ MkLnk { ln_tsen = False, ln_build = bdConst }
    where bdConst = ln_sumap (su_fmap (s_const c))

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
-- implemented as full merge with drops. This works out okay due to
-- dead-code elimination.

-- | Disjoin will distribute a decision. Alternatively understood as
-- loading an environment into a choice. This pattern is valuable,
-- for modeling lexical environments and using choice. To achieve
-- disjoin requires combining a signal representing the split with a
-- signal representing the external data, which unfortunatly makes
-- disjoin non-generic and not-quite dual to conjoin.
--
-- I hope I might be able to figure out some typeclasses or template
-- programs to make disjoin more generic. So bdisjoin0 is for this
-- non-generic version of disjoin. 
--
bdisjoin0 :: B (S p a :&: ((S p () :&: x) :|: y))
               ((S p a :&: x) :|: (S p a :&: y))
bdisjoin0 = B_tshift disjSynch >>> B_mkLnk disjMkLnk
    where disjMkLnk = MkLnk { ln_tsen = False, ln_build = disjBuild }
          disjSynch auxy =
            -- Synchronize just the two signal elements.
            let a   = lnd_first auxy in
            let uxy = lnd_second auxy in
            let ux  = lnd_left uxy in
            let u   = lnd_first ux in
            let x   = lnd_second ux in
            let y   = lnd_right uxy in
            let dta = lnd_sig a in
            let dtu = lnd_sig u in
            let dtSynch = (max `on` ldt_goal) dta dtu in
            let dtCurr  = (max `on` ldt_curr) dta dtu in
            let dt' = LDT { ldt_curr = dtCurr, ldt_goal = dtSynch } in
            let a' = LnkDProd dt' in
            let u' = LnkDProd dt' in
            a' `LnkDProd` ((u' `LnkDProd` x) `LnkDSum` y)
          disjBuild lxry = undefined 
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
    where doSynch ldt =
            let dtGoal = ldt_maxGoal ldt in
            lnd_fmap $ \ ldt -> ldt { ldt_goal = dtGoal }
            -- setting all elements to max delay goal among them

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
    { ln_time_sensitive = False
    , ln_effectful = False
    , ln_build = mkln_zip fn
    }

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
--     by MkLnk, only transformed. MkLnk may also not add delay. The
--     response is modeled as instantaneous with the demand. (Delay
--     may be modeled by wrapping the bUnsafeLnk op with bdelay.) 
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
bUnsafeLnk mklnk = bsynch >>> B_tshift xSynch >>> B_mkLnk mklnk
    where xSynch ldt = 
            let bNeedSynch = ldt_minCurr ldt /= ldt_maxCurr ldt in
            let bFullSynch = ln_tsen mkLnk in
            if bNeedSynch || bFullSynch 
                then flip lnd_fmap ldt $ \ x -> x { ldt_curr = (ldt_goal x) }
                else ldt -- no change; all or nothing for now






-- todo:
--   bcross: change partitions. Specific to signal and partition types.
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


---------------------
 -- UTILITY TYPES --
---------------------




