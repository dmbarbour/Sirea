
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

-- | (S p a) describes a signal of type `a` in partition `p`.
--
-- See FRP.Sirea.Signal for a description of signals. RDP developers
-- do not work directly with signals, but rather with behaviors that
-- transform signals. However, a Sirea developer might interact with
-- signals by the `bUnsafeExt` behavior for FFI and legacy adapters.
--
-- Partitions represent the spatial distribution of signals, across
-- threads, processes, or heterogeneous systems. Developers can keep
-- certain functionality to certain partitions (or classes thereof).
-- Communication between partitions happens only by explicit bcross
-- behaviors.
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

-- | Identity for behaviors. (Same as Control.Category.id)
bfwd :: B x x
bfwd = B_fwd

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

-- | RDP behaviors are arrows, but incompatible with Control.Arrow
-- due to `arr` being more powerful than RDP allows. A number of
-- behaviors support arrow composition, and several serve as basic
-- data plumbing that `arr` would perform in Control.Arrow.
--
-- In Sirea, all this data plumbing is essentially free at runtime.
-- You'll pay for it once when initializing the behavior, but it is
-- easy to eliminate. A major exception is `merge`, which requires
-- synchronization and intermediate state. 
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

{- Disjoin is a conceptually difficult behavior. It seems to describe
   a split at a distance, i.e. we split `x` based on the division of
   y or z. This would be 
  
   This would be a useful behavior. For example, if I want to model
   a lexical scope across a decision, and how my decision affects 
   further use of my lexical scope, I could use a disjoin. To model
   something like ArrowReader requires disjoin, too. 

   Yet it seems infeasible in RDP due to the partitioning. x, y, z
   may be in different processes. Communication between partitions 
   should be obvious and well constrained in RDP code, but disjoin
   would require much implicit distribution of data.
   Also, if y = (v :|: w) or similar, then it isn't even clear when
   y is active without extra merges.
  
   I might need a much weaker version of disjoin, i.e. that keeps it
   in the partition.

-- Disjoin is a distributed split?
bdisjoinl :: B (x :&: (y :|: z)) ((x :&: y) :|: (x :&: z))
bdisjoinr :: B ((x :|: y) :&: z) ((x :&: z) :|: (y :&: z))
bdisjoinl = (???)
bdisjoinr = bswap >>> bdisjoinl >>> (bswap +++ bswap)

-}

-- todo:
--   bcross: change partitions.
--     Note: this might be a typeclass.
--       Ability to constrain signal-type for the partition.
--       Ability to constrain 
--   bfmap
--   bdrop
--   bdelay
--   bpeek (anticipate)
--   
-- weaker disjoin?

-- | bUnsafeExt supports extend Sirea with primitive behaviors, FFI,
-- foreign services, legacy adapters. It's also used to implement
-- many primitive behavior types in Sirea, such as bfmap, bsplit,
-- and bzip.
--
-- As indicated in the name, bUnsafeExt is unsafe - it can easily
-- violate the RDP abstraction. Developers must be cautious, use it
-- safely, hide them behind safe behaviors in libraries. Concerns
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
data MkLnk x y = MkLnk 
    { ln_tsense :: Bool 
    , ln_effect :: Bool  
    , ln_build  :: Lnk y -> IO (Lnk x)
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
--  No. Keep it simple and sufficient.







