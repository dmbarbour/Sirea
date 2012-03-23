
{-# LANGUAGE TypeOperators, EmptyDataDecls, GADTs, TypeFamilies #-}

module FRP.Sirea.Behavior  
  ( (:&:), (:|:), B, S
  , (>>>) -- from Control.Category
  , bfwd, bdelay, bsynch
  , bfirst, bsecond, (***), bswap, bcopy, bfst, bsnd, bassoclp, bassocrp
  , bleft, bright, (+++), bmirror, bmerge, binl, binr, bassocls, bassocrs
  , bconjoinl, bconjoinr
  --, bUnsafeExt
  --, MkLnk(..), Lnk(..), LnkSig(..), LnkProd(..), LnkSum(..)
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
-- signals might not overlap for short, statically known, periods.
data (:&:) x y

-- | (x :|: y). Union or Sum of asynchronous or partitioned signals.
-- x and y split a duration, i.e. over the course of one second,
-- if x is active for 300ms then y cannot be active for more than
-- 700ms. However, asynchronous signals might overlap for short,
-- statically known, periods.
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
-- Partitions are Data.Typeable. A small subset of partition types
-- have special meaning, telling Sirea to fork threads to run the
-- behavior in parallel.
newtype S p a


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
  B_ext     :: MkLnk x y -> B x y

  B_fwd     :: B x x
  B_seq     :: B x y -> B y z -> B x z
  B_delay   :: DT -> B x x
  B_synch   :: B x x

  B_fst     :: B (x :&: y) x
  B_on_fst  :: B x x' -> B (x :&: y) (x' :&: y)
  B_swap    :: B (x :&: y) (y :&: x)
  B_copy    :: B x (x :&: x)
  B_asso_p  :: B (x :&: (y :&: z)) ((x :&: y) :&: z)
  
  B_lft     :: B x (x :|: y)
  B_on_lft  :: B x x' -> B (x :|: y) (x' :|: y)
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
-- data plumbing that `arr` would perform in Control.Arrow.
--
-- In Sirea, most data plumbing is essentially free at runtime. But
-- there are a few exceptions - bzip, bmerge, bconjoin, bsplit have
-- runtime overheads.
--
-- DATA PLUMBING BEHAVIORS (~ARROWS)
--  TRIVIAL
--   bfwd - identity behavior
--   (>>>) - forward composition
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
--   bzip - combine two concrete signals 
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
--  OTHER (see below for details)
--   bconjoin - partial merge on a product of sums
--   bdelay - delay signals (multi-part signals delayed equally)
--   bsynch - synch an asynchronous signal (to slowest)
--   bfmap - apply an arbitrary Haskell function to a signal 
--   bcross - communicate a signal between partitions
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

-- | Conjoin is a partial merge.
bconjoinl :: B ((x :&: y) :|: (x :&: z)) (x :&: (y :|: z))
bconjoinr :: B ((x :&: z) :|: (y :&: z)) ((x :|: y) :&: z)
bconjoinl = bcopy >>> (isolateX *** isolateYZ) 
   where isolateX = (bfst +++ bfst) >>> bmerge
         isolateYZ = (bsnd +++ bsnd)
bconjoinr = (bswap +++ bswap) >>> bconjoinl >>> bswap


-- | Partition classes are a useful approach to constrain behavior
-- to certain partitions. A few default classes are provided with
-- Sirea, but developers can create more using bUnsafeExt for new
-- primitives. 
--
-- The first example of these classes supports applying Haskell
-- functions to signals. This is restricted because some partitions
-- might represent remote or heterogeneous elements for which an
-- opaque Haskell function cannot readily be serialized.




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
--   ln_tsense - time sensitive; if false, may shift delays across
--   ln_effect - effectful behavior; if false, may treat as dead
--     code if the response is dropped (e.g. via bfst). 
--   
-- The primary operation is ln_build, which receives a link to put
-- output and must return a link to receive input. Construction in
-- IO allows creating intermediate state, but should not have any
-- observable effects. The link will later be activated by a signal
-- update. (The assumed signal before first update is inactive.)
--
-- An additional string, ln_desc, is provided for debugging or
-- display purposes. 
--
data MkLnk x y = MkLnk 
    { ln_tsense :: Bool 
    , ln_effect :: Bool  
    , ln_build  :: Lnk y -> IO (Lnk x)
    , ln_desc   :: String
    }


-- | A Lnk will process updates to a signal - potentially a complex,
-- asynchronous, partitioned signal using (:&:) or (:|:). 
--
-- The main update operation is ln_update, which receives a signal
-- update for a concrete, discrete-varying signal type. The 
--
-- See `bext` for how Lnk is used.
--
--
-
-- Lnk-based behaviors are built back to front. That is, they are
-- provided an output Lnk and must generate an input Lnk. This
-- occurs when the behavior is initially run, and should have no
-- observable side-effects. 
--
--    bforeign :: ((Lnk p y) -> IO (Lnk p x)) -> B x y
--
-- If x and y are composite signal types, there will be a set of
-- update capabilities for each individual signal (using LnkProd
-- or LnkSum), and the behavior is implicitly synchronized
-- 
--
-- The basic Lnk is on just a plain signal.
data family Lnk a 
data instance Lnk (S p a) = 
    LnkSig {
      ln_touch  :: IO (),
      ln_update :: SigUp a -> IO (),
      ln_drop   :: Bool
    }
data instance Lnk (x :&: y) = LnkProd (Lnk x) (Lnk y)
data instance Lnk (x :|: y) = LnkSum (Lnk x) (Lnk y)
-- Try to generalize on this for folds? 
--  No! Keep it simple and sufficient.


-- | Partition classes are a useful way to constrain behaviors. This
-- HaskPart class s


-- todo:
--   bcross: change partitions. Specific to signal and partition types.
--     Likely a typeclass!
--   bfmap
--   bdrop
--   bdelay
--   bpeek (anticipate)
--   
-- weaker disjoin?





