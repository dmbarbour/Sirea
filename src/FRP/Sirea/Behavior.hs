
{-# LANGUAGE TypeOperators, EmptyDataDecls, GADTs, TypeFamilies #-}

module FRP.Sirea.Behavior  
  ( (:&:), (:|:), B, S
  , (>>>) -- from Control.Category
  , bfwd, bdelay, bsynch
  , bfirst, bsecond, (***), bswap, bcopy, bfst, bsnd, bassoclp, bassocrp
  , bleft, bright, (+++), bmirror, bmerge, binl, binr, bassocls, bassocrs
  , bconjoinl, bconjoinr
  , bvoid
  , bfmap, bzip, bsplit
  , bUnsafeFmap, bUnsafeZipWith, bUnsafeSplitWith
  , bUnsafeExt
  , MkLnk(..), Lnk(..), LnkProd(..), LnkSum(..), SigUp(..)
  , ln_fmap, su_fmap, su_delay
  ) where

import Prelude hiding (id,(.))
import Control.Category

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
-- signals by the `bUnsafeExt` behavior for FFI and legacy adapters.
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
newtype S p a

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
--     twice has no additional effect
--   * spatial commutativity - the origin of a demand signal does
--     not affect its meaning; demands are unordered
--   * duration coupling - the active periods of response y are
--     tightly coupled to the active periods of demand x. 
--   * continuous & eventless - no instantaneous states or values, 
--     and conceptually infinite instants between times.
--
-- These constraints make RDP very declarative. But developers must
-- learn new patterns, idioms, and state models.
--
-- Behaviors compose much like arrows (from Control.Arrow), but are
-- more constrained due to partitioning, asynchrony, and duration
-- coupling. 
--
data B x y where
  B_ext     :: !(MkLnk x y) -> B x y

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
-- there are a few exceptions - bzip, bmerge, bconjoin, bsplit have
-- runtime overheads.
--
--  TRIVIAL
--   bfwd - identity behavior 
--   (>>>) - forward composition (Control.Category)
--
--  PRODUCTS
--   bfirst b - apply b on first element in product
--   bsecond b - apply b on second element in product
--   (***) b1 b2 = bfirst b1 >>> bsecond b2
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
--   (+++) bl br - bleft bl >>> bright br
--   bmirror - flip left and right signals
--   bmerge - combine two choices into one signal (implicit synch)
--   binl - static choice of left option (~ if true) 
--   binr - static choice or right option (~ if false)
--   bassocls - associate left on sum
--   bassocrp - associate right on sum
--   bsplit - lift a decision in a signal to asynchronous layer
--
--  MISCELLANEOUS
--   bconjoin - partial merge on a product of sums
--   bvoid - execute a behavior for side-effects
--
--  SPATIAL or TEMPORAL
--   bdelay - delay signals (multi-part signals delayed equally)
--   bsynch - synch an asynchronous signal (to slowest)
--   bcross - communicate a signal between partitions
--
-- DATA OPERATIONS
--   bfmap - apply an arbitrary Haskell function to a signal 
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
bassocrp = swap3 >>> bassoclp >>> swap3
  where swap3 = bfirst bswap >>> bswap

bleft = B_on_lft
bright f = bmirror >>> bleft f >>> bmirror
(+++) f g = bleft f >>> bright g
bmirror = B_mirror
bmerge = B_merge
binl = B_lft
binr = binl >>> bmirror
bassocls = B_asso_s
bassocrs = mirr3 >>> bassocls >>> mirr3
  where mirr3 = bleft bmirror >>> bmirror

-- | bvoid executes a behavior for side-effects only.
bvoid :: B x y -> B x x
bvoid b = bcopy >>> bfirst b >>> bsnd 

-- | Conjoin is a partial merge.
bconjoinl :: B ((x :&: y) :|: (x :&: z)) (x :&: (y :|: z))
bconjoinr :: B ((x :&: z) :|: (y :&: z)) ((x :|: y) :&: z)
bconjoinl = bcopy >>> (isolateX *** isolateYZ) 
   where isolateX = (bfst +++ bfst) >>> bmerge
         isolateYZ = (bsnd +++ bsnd)
bconjoinr = (bswap +++ bswap) >>> bconjoinl >>> bswap

{- Disjoin would be a powerful behavior:

     bdisjoinl :: B (x :&: (y :|: z)) ((x :&: y) :|: (x :&: z))

   This essentially expresses a decision for remote processes on x
   based on a decision in another partition (y or z). However, it
   seems impossible to express while respecting a certain rule, that
   communication be explicit.
  
   So there is no disjoin in Sirea, not in the general case anyway.
   A weaker version of disjoin on particular signals will be viable.
     
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

-- | as bfmap, but not constrained to valid partition class
bUnsafeFmap :: (a -> b) -> B (S p1 a) (S p2 b)
bUnsafeFmap fn = bUnsafeExt $ MkLnk 
        { ln_time_sensitive = False
        , ln_effectful = False
        , ln_build  = return . ln_fmap (su_fmap $ s_fmap fn)
        }

-- | combine signals from parallel pipelines with a common function.
bzip :: (PtHask p) => B (S p a :&: S p b) (S p (a,b))
bzip = bUnsafeZipWith (,)

-- | as bzip, but not constrained to valid partition class
bUnsafeZipWith :: (a -> b -> c) -> B (S p1 a :&: S p2 b) (S p3 c)
bUnsafeZipWith fn = bUnsafeExt $ MkLnk 
        { ln_time_sensitive = False
        , ln_effectful = False
        , ln_build = mkln_zip (s_zip fn)
        }

-- | lift choice of data to choice of behaviors
bsplit :: (PtHask p) => B (S p (Either a b)) (S p a :|: S p b)
bsplit = bUnsafeSplitWith id

-- | as bsplit, but not constrained to valid partition class
bUnsafeSplitWith :: (c -> Either a b) -> B (S p1 c) (S p2 a :|: S p3 b)
bUnsafeSplitWith fn = bUnsafeExt $ MkLnk
        { ln_time_sensitive = False
        , ln_effectful = False
        , ln_build = return . ln_split fn
        }

-- | bUnsafeExt supports extend Sirea with primitive behaviors, FFI,
-- foreign services, legacy adapters. It's also used to implement
-- many primitive behavior types in Sirea, such as bfmap, bsplit,
-- and bzip. 
--
-- As indicated in the name, bUnsafeExt is unsafe - it can easily
-- violate the RDP abstraction. Developers must be cautious, use it
-- safely, hide it behind safe behaviors in libraries. Concerns
-- include duration coupling, spatial commutativity and idempotence,
-- and eventual GC of old Lnk objects.
--
-- Note: Developers must not introduce delay by use of bUnsafeExt.
-- Delay needs to be visible for analysis to support bsynch, which
-- means using bdelay as the only cause for delay. In order to keep
-- analysis simple, bUnsafeExt will implicitly synchronize complex
-- signal types. 
bUnsafeExt :: MkLnk x y -> B x y
bUnsafeExt mkLnk = bsynch >>> B_ext mkLnk

-- | MkLnk - constructors and metadata for including a new behavior
-- primitive in Sirea. There are currently two metadata values:
--
--   time sensitive - if false, may shift delays before or after
--     (affecting delay-aggregation optimizations)
--   effectful - if false, assume behavior only needed for output
--     (affecting dead-code optimizations)
--   
-- The primary operation is ln_build, which receives a link to put
-- output and must return a link to receive input. Construction in
-- IO allows allocating intermediate state, but should not have any
-- observable effects. The link will later be activated by a signal
-- update. (Assume signal empty until first update.)
data MkLnk x y = MkLnk 
    { ln_time_sensitive :: Bool 
    , ln_effectful :: Bool  
    , ln_build  :: Lnk y -> IO (Lnk x)
    }

-- | A Lnk will process updates to a signal - potentially a complex,
-- asynchronous, partitioned signal using (:&:) or (:|:), but most
-- often mapping concrete signal (S p a) to another.
-- 
--   ln_touch - may be called to indicate an update is coming soon.
--     This can be used to avoid redundant computation, i.e. by
--     delaying processing of updates on one input when it is known
--     that updates will arrive on another input.
--   ln_update - called to deliver an update on the link.
--
-- Lnk is used indirectly by bUnsafeExt to adapt foreign services.
-- Developers creating a link must be careful to maintain RDP's set
-- of properties - commutativity, idempotence, continuity, duration
-- coupling, locally stateless (not counting regenerable state such
-- as caches or memoization), etc. 
data family Lnk a 
data instance Lnk (S p a) = LnkSig 
    { ln_touch  :: !(IO ())
    , ln_update :: !(SigUp a -> IO ())
    }
data instance Lnk (x :&: y) = LnkProd !(Lnk x) !(Lnk y)
data instance Lnk (x :|: y) = LnkSum !(Lnk x) !(Lnk y)
-- Try to generalize on this for folds? 
--  No! Keep it simple and sufficient.

-- | Each signal update carries:
--    stability - a promise that the signal is stable up to a given
--      time. The value Nothing indicates that a signal is stable
--      forever. Stability is useful for garbage collecting state.
--    state - the new state of the signal, starting at a given time.
--      The value Nothing here means that the state did not change.
-- Stability always updates. State might not update, i.e. to avoid
-- recomputing a signal when it is known it did not change.
data SigUp a = SigUp 
    { su_state :: !(Maybe (Sig a , T))
    , su_stable :: !(Maybe T)
    }

-- utility operations 

-- | simple link update from a signal update transformer
ln_fmap :: (SigUp a -> SigUp b) -> Lnk (S p a) -> Lnk (S p b)
ln_fmap fn ln = LnkSig 
  { ln_touch = ln_touch ln
  , ln_update = ln_update ln . fn 
  }

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
su_delay dt = if (0 == dt) then id else suDelay
    where suDelay su = 
        let state' = fmap (\(s0,t) -> (s_delay dt s0, addTime t dt)) (su_state su) in
        let stable' = fmap (flip addTime dt) (su_stable su) in
        SigUp { su_state = state', su_stable = stable' }


-- todo:
--   bcross: change partitions. Specific to signal and partition types.
--     Likely a typeclass!
--   bfmap
--   bdrop
--   bdelay
--   bpeek (anticipate)
--   
-- weaker disjoin?





