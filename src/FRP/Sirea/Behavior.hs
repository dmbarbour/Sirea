
{-# LANGUAGE TypeOperators, EmptyDataDecls, GADTs, TypeFamilies #-}

module FRP.Sirea.Behavior  
  ( (:&:), (:|:), B, S
  , (>>>) -- from Control.Category
  , bfwd, bdelay, bsynch
  , bfirst, bsecond, (***), bswap, bcopy, bfst, bsnd, bassoclp, bassocrp
  , bleft, bright, (+++), bmirror, bmerge, binl, binr, bassocls, bassocrs
  , bconjoinl, bconjoinr
  , bvoid
  , bfmap, bzip, bzipWith, bsplit
  , bUnsafeLnk
  , MkLnk(..), Lnk(..), SigUp(..)
  , ln_fmap, su_fmap, su_delay
  ) where

import Prelude hiding (id,(.))
import Control.Category

import Data.IORef
import FRP.Sirea.Signal
import FRP.Sirea.Time
import FRP.Sirea.MiscTypes


infixr 3 ***
--infixr 3 &&&
infixr 2 +++
--infixr 2 |||

-- | (x :&: y). Product of asynchronous or partitioned signals, but
-- x and y will have equal and tightly coupled active periods. For
-- example, if x is active for 300ms, inactive 100ms, then active
-- 600ms, then y will have the same profile. However, asynchronous
-- delays enable a small divergence of exactly when these periods
-- occur. (They'll be synchronized before recombining signals.)
data (:&:) x y

-- | (x :|: y). Union or Sum of asynchronous or partitioned signals.
-- Signals are active for different durations, i.e. if x is active
-- 100 ms, inactive 400 ms, then active 100 ms: then y is inactive
-- 100 ms, active up to 400 ms, then inactive 100 ms. (There may be
-- durations where both are inactive.) Due to asynchronous delays 
-- the active periods might overlap for statically known periods.
data (:|:) x y

-- | (S p a) is a Sig in a blanket - Sig a in partition p. 
--
-- See FRP.Sirea.Signal for a description of signals. RDP developers
-- do not work directly with signals, but rather with behaviors that
-- transform signals. However, a Sirea developer might interact with
-- signals by the `bUnsafeLnk` behavior for FFI and legacy adapters.
--
-- Partitions represent the spatial distribution of signals, across
-- threads, processes, or heterogeneous systems. Developers can keep
-- certain functionality to certain partitions (or classes thereof).
-- Communication between partitions requires explicit behavior, such
-- as `bcross`.
--
-- Partitions should be Data.Typeable to support analysis of types
-- as values. Some types may have special meaning, indicating that
-- extra threads should be constructed when behavior is initiated.
data S p a

-- | PtHask is a simple partition class that indicates the partition
-- is local to the Haskell process and supports the full gamut of 
-- Haskell types and functions.
class PtHask p
instance PtHask ()


-- | (B x y) describes an RDP behavior - a signal transformer with
-- potential for declarative `demand effects`. Signal x is called
-- the demand, and y the response. Behaviors may be composed, so the
-- response from one behavior easily becomes demand on another.
--
-- A common effect on demand is to acquire resources, e.g. to power
-- up a sensor only while there is code interested in observing it.
-- But demands can also influence state, and thereby interact with
-- many other behaviors via shared state or stateful services.
--
-- RDP demand effects are constrained: 
--   * spatial idempotence - in any given instant, the same demand
--     twice, or a thousand times, has no additional effect. 
--   * spatial commutativity - the origin of a demand signal does
--     not affect its meaning; demands at any given instant must be
--     processed as an unordered set.
--   * duration coupling - the active periods of response y are
--     tightly coupled to the active periods of demand x. If x is
--     active 100 ms, inactive 300 ms, active 200 ms then so will
--     be y (albeit, possibly delayed a little).
--   * continuous & eventless - no instantaneous states or values, 
--     and conceptually infinite instants between times. Rather than
--     a button-press event, for example, developers will see the
--     button down state a few milliseconds then back in up-state.
--
-- These constraints make RDP very declarative. But developers must
-- learn new patterns, idioms, and state models.
--
-- Behaviors compose much like arrows (from Control.Arrow), but are
-- more constrained due to partitioning, asynchrony, and duration
-- coupling. 
--
data B x y where
  B_lnk     :: !(MkLnk x y) -> B x y

  B_fwd     :: B x x
  B_seq     :: !(B x y) -> !(B y z) -> B x z
  B_delay   :: !DT -> B x x
  B_synch   :: B x x

  B_fst     :: B (x :&: y) x
  B_on_fst  :: !(B x x') -> B (x :&: y) (x' :&: y)
  B_swap    :: B (x :&: y) (y :&: x)
  B_copy    :: B x (x :&: x)
  B_asso_p  :: B (x :&: (y :&: z)) ((x :&: y) :&: z)
  
  B_lft     :: B x (x :|: y)
  B_on_lft  :: !(B x x') -> B (x :|: y) (x' :|: y)
  B_mirror  :: B (x :|: y) (y :|: x)
  B_merge   :: B (x :|: x) x
  B_asso_s  :: B (x :|: (y :|: z)) ((x :|: y) :|: z) 
-- how am I to distribute commands and hooks?
--   Either GADT or Type Family + Typeclasses


instance Category B where
  id  = B_fwd
  (.) = flip B_seq

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
--   bcopy - duplicate signal
--   bfst - keep first signal, drop second
--   bsnd - keep second signal, drop first
--   bassoclp - associate left on product
--   bassocrp - associate right on product
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
--   bassocls - associate left on sum
--   bassocrp - associate right on sum
--   bsplit - lift a decision in a signal to asynchronous layer
--
--  ARROW MISC
--   bfmap - apply function to a concrete signal
--   bvoid - execute a behavior for side-effects
--   bconjoin - partial merge on a product of sums
--
--  SPATIAL-TEMPORAL
--   bdelay - delay signals (multi-part signals delayed equally)
--   bsynch - synch an asynchronous signal (to slowest)
--   bcross - communicate a signal between partitions
--
--  EXTENSIONS AND ADAPTERS
--   bUnsafeLnk - hook FFI, legacy services, external resources
--
--  PERFORMANCE
--   bforce - evaluate next several samples before progress. 
--   bspark - evaluate next several samples in a spark 
--   bstrat - sequence strategy [a] -> () on several samples
--   bpstrat - periodic application of strategy in dedicated thread
--   badjeqf - eliminate adjacent equal updates  
--   bUnsafeChoke - limit rate of updates, skip intermediate values
--   bUnsafeChokeEq - choke small changes; pass big changes
--
bfwd     :: B x x

bfirst   :: B x x' -> B (x :&: y) (x' :&: y)
bsecond  :: B y y' -> B (x :&: y) (x :&: y')
(***)    :: B x x' -> B y y' -> B (x :&: y) (x' :&: y')
bswap    :: B (x :&: y) (y :&: x)
bcopy    :: B x (x :&: x)
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

bfwd = B_fwd

bfirst = B_on_fst
bsecond f = bswap >>> bfirst f >>> bswap 
(***) f g = bfirst f >>> bsecond g
bswap = B_swap
bcopy = B_copy
bfst  = B_fst
bsnd  = bswap >>> bfst
bassoclp = B_asso_p
bassocrp = bswap3 >>> bassoclp >>> bswap3

bleft = B_on_lft
bright f = bmirror >>> bleft f >>> bmirror
(+++) f g = bleft f >>> bright g
bmirror = B_mirror
bmerge = B_merge
binl = B_lft
binr = binl >>> bmirror
bassocls = B_asso_s
bassocrs = bmirr3 >>> bassocls >>> bmirr3

-- utility

bswap3 :: B ((x :&: y) :&: z) (z :&: (y :&: x))
bswap3 = bfirst bswap >>> bswap

bmirr3 :: B ((x :|: y) :|: z) (z :|: (y :|: x))
bmirr3 = bleft bmirror >>> bmirror

-- | bvoid executes a behavior for side-effects only,
-- and simply propagates its input. This is a common
-- pattern.
bvoid :: B x y -> B x x
bvoid b = bcopy >>> bfirst b >>> bsnd 

-- | Conjoin is a partial merge.
bconjoinl :: B ((x :&: y) :|: (x :&: z)) (x :&: (y :|: z))
bconjoinr :: B ((x :&: z) :|: (y :&: z)) ((x :|: y) :&: z)
bconjoinl = bcopy >>> (isolateX *** isolateYZ) 
   where isolateX = (bfst +++ bfst) >>> bmerge
         isolateYZ = (bsnd +++ bsnd)
bconjoinr = (bswap +++ bswap) >>> bconjoinl >>> bswap

-- berrseq - composition with error options.
-- todo: move to a arrow transformer...
berrseq :: B x (err :|: y) -> B y (err :|: z) -> B x (err :|: z)
berrseq bx by = bx >>> bright by >>> bassocls >>> bleft bmerge

-- benvseq - composition with environment (~reader)
-- todo: move to a arrow transfomer
benvseq :: B (env :&: x) y -> B (env :&: y) z -> B (env :&: x) z
benvseq bx by = bcopy >>> (bfst *** bx) >>> by

{- Disjoin would be a powerful behavior:

     bdisjoinl :: B (x :&: (y :|: z)) ((x :&: y) :|: (x :&: z))

   This essentially expresses a decision for remote processes on x
   based on a decision in another partition (y or z). However, it
   seems impossible to express while respecting a certain rule, that
   communication between partitions be explicit.
  
   So there is no disjoin in Sirea, not in the general case anyway.
   A weaker version of disjoin on particular signals will be viable.
     
   What would I need here...
  
   At moment, developers need to zip data before splitting. Kinda painful.


-}

-- | Represent latency of calculation or communication by delaying
-- a signal a small, logical difftime. Appropriate use of delay can
-- greatly improve system consistency and efficiency. In case of a
-- complex signal, every signal receives the same delay.
bdelay :: DT -> B x x
bdelay = B_delay

-- | Synch automatically delays all signals to match the slowest in
-- a composite. Immediately after synchronization, you can be sure 
-- (x :&: y) products precisely overlap, and (x :|: y) sums handoff
-- smoothly without gap or overlap. For non-composte signals, bsynch
-- has no effect. bsynch twice has no extra effect. Synchronization
-- is logical in RDP, and the implementation is wait-free.
--
-- Signals even in different partitions may be synchronized. 
bsynch :: B x x
bsynch = B_synch

-- | map an arbitrary Haskell function across an input signal.
bfmap :: (PtHask p) => (a -> b) -> B (S p a) (S p b)
bfmap = bUnsafeFmap

-- | as bfmap, but not constrained to partition class
bUnsafeFmap :: (a -> b) -> B (S p a) (S p b)
bUnsafeFmap = bUnsafeSigUpMap . su_fmap . s_fmap

-- | simple map from update to update
bUnsafeSigUpMap :: (SigUp a -> SigUp b) -> B (S p a) (S p b)
bUnsafeSigUpMap fn = bUnsafeLnk $ MkLnk 
    { ln_time_sensitive = False
    , ln_effectful = False
    , ln_build  = return . ln_fmap fn
    }

-- | combine a product of signals into a signal of products
bzip :: (PtHask p) => B (S p a :&: S p b) (S p (a,b))
bzip = bzipWith (,)

-- | combine signals with a given function
bzipWith :: (PtHask p) => (a -> b -> c) -> B (S p a :&: S p b) (S p c)
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
bsplit :: (PtHask p) => B (S p (Either a b)) (S p a :|: S p b)
bsplit = bUnsafeSplit

-- | as bsplit, but not constrained to partition class
bUnsafeSplit :: B (S p (Either a b)) (S p a :|: S p b)
bUnsafeSplit = bUnsafeLnk $ MkLnk
    { ln_time_sensitive = False
    , ln_effectful = False
    , ln_build = return . ln_split
    }

-- | bUnsafeLnk extends Sirea with primitive behaviors, FFI, foreign
-- services, legacy adapters, access to state and IO. Some primitive
-- behaviors (bfmap, bzip, bsplit) are also implemented atop this. 
--
-- Each instance of this behavior (excluding dead code) results in
-- construction of one link (the ln_build operation) when the Sirea
-- behavior is initiated. This construction allows for intermediate
-- state, intended for caches and other safe uses. The link will
-- also have access to Haskell IO on each update. The updates may
-- specify future times for when they become active or expire.
--
-- As indicated in the name, bUnsafeLnk is unsafe - it may easily
-- violate the RDP abstraction. It can be used safely, but caution
-- is warranted. Developers must avoid violating the tenets of RDP:
-- duration coupling, spatial commutativity, spatial idempotence, 
-- locally stateless (except for regenerable-from-scratch state like
-- caches or memoization), eventless (ignore zero-duration signals),
-- and so on. Further, developers must avoid structure that hinders 
-- GC of a behavior after shutdown, and sometimes explicitly remove 
-- dead links (by testing s_fini).
--
-- Safe uses of bUnsafeLnk should be hidden behind a library API.
-- Unsafe uses should still have the word 'Unsafe' in their name.
--
-- Note: Developers must not introduce delay by use of bUnsafeLnk.
-- Delay must be visible for analysis at bsynch, so use bdelay. Use
-- of bUnsafeLnk will force a synchronization for complex signals,
-- which should (with rare exceptions) all be in the same partition.
bUnsafeLnk :: MkLnk x y -> B x y
bUnsafeLnk mkLnk = bsynch >>> B_lnk mkLnk


-- combine two parallel signals. 
-- This works by constructing the two input signals (from updates)
mkln_zip :: (Sig a -> Sig b -> Sig c) -> Lnk (S p1 c) -> IO (Lnk (S p2 a :&: S p3 b))
mkln_zip = 
    newIORef emptyZipper >>= \ rz ->
    undefined
    -- create a signal accumulator for a
    -- create a signal accumulator for b
    -- touch forwards if both a,b are untouched

-- split a signal.
ln_split :: Lnk ((S p3 a) :|: (S p2 b)) -> Lnk (S p1 (Either a b))
ln_split = undefined

-- | modify the primary function of a signal update
--
-- Note that this function on signals should not delay the signal or
-- otherwise change its activity profile.
su_fmap :: (Sig a -> Sig b) -> SigUp a -> SigUp b
su_fmap fn su =
    let state' = fmap (\(s0,t) -> (fn s0, t)) (su_state su) in
    SigUp { su_state = state', su_stable = su_stable su }

-- | delay all aspects of a signal update
su_delay :: DT -> SigUp a -> SigUp a
su_delay dt = if (0 == dt) then id else \ su ->
    let state' = fmap (\(s0,t) -> (s_delay dt s0, addTime t dt)) (su_state su) in
    let stable' = fmap (flip addTime dt) (su_stable su) in
    SigUp { su_state = state', su_stable = stable' }


-- todo:
--   bcross: change partitions. Specific to signal and partition types.
--     Likely a typeclass!
--   bdrop
--   bpeek (anticipate)

--   bspark (spark a calculation)
--   bforce
--   
-- weaker disjoin?
-- initial stateful and pseudo-state ops
--- reactive term rewriting, reactive state transition, 
--- reactive constraint-logic


---------------------
 -- UTILITY TYPES --
---------------------




