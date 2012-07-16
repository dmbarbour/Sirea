
{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, Rank2Types #-}

-- | This module describes RDP behaviors classes in Sirea. Behaviors 
-- are a restricted class of Arrows that transform and orchestrate 
-- (but neither create nor destroy) signals. 
--
-- This module contains behaviors only for data plumbing, pure
-- functional computation, and simple performance annotations. In
-- general, RDP behaviors may be effectful. 
--
-- For concrete behavior types, see Sirea.B or Sirea.BCX. 
-- For partition management behaviors, see Sirea.Partition.
module Sirea.Behavior  
    ( (:&:), (:|:), S, SigInP
    , (>>>) -- from Control.Category
    , bfwd
    , BFmap(..), bforce, bspark, bstratf
    , BProd(..), bsecond, bsnd, bassocrp, (***), (&&&), bvoid
    , BSum(..), bright, binr, bassocrs, (+++), (|||), bskip 
    , bconjoinl, bconjoinr
    , BDisjoin(..), bdisjoinl, bdisjoinlz, bdisjoinlk, bdisjoinlkz
    ,               bdisjoinr, bdisjoinrz, bdisjoinrk, bdisjoinrkz
    , BZip(..), bzipWith, bunzip
    , BSplit(..), bsplitWith, bsplitOn, bunsplit
    , BTemporal(..), BPeek(..)
    , BDynamic(..), bevalOrElse
    , Behavior
    ) where

import Prelude hiding (id,(.))
import Control.Category
import Control.Parallel (pseq, par)
import Control.Parallel.Strategies (Eval, runEval)

import Sirea.Internal.STypes ((:&:),(:|:),S,SigInP)
import Sirea.Time (DT)

infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||

-- | Behavior is a grouping of all basic behavior classes.
class ( Category b
      , BFmap b
      , BProd b, BZip b
      , BSum b, BSplit b
      , BDisjoin b
      , BTemporal b, BPeek b
      ) => Behavior b

-- | bfwd is just another name for Control.Category.id.
bfwd :: (Category b) => b x x
bfwd = id

-- | BFmap - pure operations on concrete signals. Includes common
-- performance annotations. BFmap supports arbitrary Haskell 
-- functions. 
class (Category b) => BFmap b where
    -- | bfmap applies a function to a concrete signal. This allows
    -- arbitrary Haskell functions to integrate with RDP. Lazy.
    bfmap :: (x -> y) -> b (S p x) (S p y)

    -- | bconst maps a constant to the signal. The resulting signal
    -- is still non-trivial, varying between active and inactive as
    -- did the input signal. 
    --   bconst = bfmap . const
    -- It may be specialized easily for performance, e.g. eliminate
    -- most redundant updates like badjeqf.
    bconst :: y -> b (S p x) (S p y)
    bconst = bfmap . const

    -- | bstrat provides developers great control of when and where
    -- computations occur. Control.Parallel.Strategies can specify
    -- both data parallelism (via sparks) and sequential strategies.
    -- Other evaluator models (Monad.Par, for example) can be lifted
    -- across bstrat by use of bstratf.
    --
    -- The idea of bstrat is to ensure the Eval completes before the
    -- `Just x` constructor is observed when sampling the signal, 
    -- but prior to observing x. This can be combined with btouch to
    -- kickstart the computation, and bfmap will provide the `Eval` 
    -- structure in the first place. 
    bstrat :: b (S p (Eval x)) (S p x)
    bstrat = bfmap runEval

    -- | btouch annotates that a signal should be computed as it 
    -- updates, generally based on stability of the signal's value
    -- (see FRP.Sirea.Link for more about stability). The signal is
    -- computed up to `Just x | Nothing`; x is not observed. 
    --
    -- This is meant for use in tandem with bstrat to lift desired
    -- computations to occur prior observing the `Just` constructor.
    btouch :: b (S p x) (S p x)
    btouch = bfwd

    -- | Types that can be tested for equality can be filtered to
    -- eliminate redundant updates. Redundant updates are common if
    -- mapping lossy functions (like `x->Bool`) to signals. badjeqf,
    -- for "behavior adjacent equality filter", annotates the RDP
    -- computation to perform such filtering.
    --
    -- The reason to eliminate redundant updates is to eliminate
    -- redundant computations further down the line, eg. at `bzip`.
    -- This is a valuable, safe performance optimization, though it
    -- must be used judiciously (or badjeqf could itself become the
    -- redundant computation).
    --
    -- Not all redundant updates are eliminated. In particular, you
    -- may still see redundant updates for each actual signal update
    -- because it isn't feasible to scan indefinitely into the prior
    -- signal's future to find the first difference. (But the update
    -- can be shifted a bit into the future to avoid much rework.)
    badjeqf :: (Eq x) => b (S p x) (S p x)
    badjeqf = bfwd -- semantic effect is identity

-- | bforce will sequence evaluation when the signal update occurs,
-- according to a provided sequential strategy. Useful idiom:
--   > import Control.DeepSeq (rnf)
--   > bforce rnf
-- This would reduce a signal to normal form before further progress 
-- in the partition's thread. This can improve data parallelism by
-- making more efficient use of partition threads, can help control
-- memory overheads, and can achieve more predictable performance.
bforce :: (BFmap b) => (x -> ()) -> b (S p x) (S p x)
bforce f = bfmap seqf >>> bstrat >>> btouch
    where seqf x = (f x) `pseq` return x

-- | `bspark` is the similar to `bforce` except that it sparks each
-- computation rather than running it in the partition thread, and 
-- does not wait for the computation to complete. 
--
-- bspark is predictable, but not very composable. For example, in
--   > bspark foo >>> bspark bar
-- The bar reduction will occur in a spark that immediately waits
-- for the foo reduction to complete. This ensures the bar reduction
-- doesn't compete with the foo reduction, but limits parallelism.
-- Consequently, bspark is best used to just perform full reduction.
--
-- A lazy variation of bspark would be easy to implement, but would
-- be problematic due to ad-hoc competition and GC interaction. If
-- developers desire precise control over parallelism, they should
-- directly use bstrat or bstratf, and btouch.
--
bspark :: (BFmap b) => (x -> ()) -> b (S p x) (S p x)
bspark f = bfmap sparkf >>> bstrat >>> btouch
    where sparkf x = 
            let d = f x in 
            d `par` return (d `pseq` x)

-- | bstratf - a convenience operation to lift identity functors in
-- the same form as bstrat. This was motivated mostly for Monad.Par:
--
--     import Monad.Par
--     brunPar :: (BFmap b) => b (S p (Par x)) (S p x)
--     brunPar = bstratf runParAsync
--
-- Here brunPar will initiate computation if `Just x` constructor is
-- observed when sampling the signal, i.e. when signal is touched,
-- begin computing `x` value in parallel. (NOTE: it would be unsafe 
-- to leak IVars from a Par computation.)
--
bstratf :: (BFmap b, Functor f) => (forall e . (f e -> e)) 
        -> b (S p (f x)) (S p x)
bstratf runF = bfmap (runF . fmap return) >>> bstrat

-- | BProd - data plumbing for asynchronous products. Asynchronous
-- product (x :&: y) means that both signals are active for the same
-- durations and approximately for the same times (modulo variations
-- in delay). This can be understood as modeling parallel pipelines. 
--
--     bfirst - operate on the first signal
--     bdup - duplicate any signal, new parallel pipeline
--     bfst - keep the first signal, drop the second.
--     bswap - products are commutative
--     bassoclp - products are associative (move parens left)
--
-- The above operations should be free at runtime. 
--
-- A few operations are defined from the primitives: 
--
--     bsecond - operate on the second signal
--     bsnd - keep second signal, drop first
--     bassocrp - products are associative (move parens right)
--     (***) - operate on first and second in parallel
--     (&&&) - create and define multiple pipelines at once
--     bvoid - branch behavior just for side-effects, drop result
--
class (Category b) => BProd b where
    bfirst   :: b x x' -> b (x :&: y) (x' :&: y)
    bdup     :: b x (x :&: x)
    bfst     :: b (x :&: y) x
    bswap    :: b (x :&: y) (y :&: x)
    bassoclp :: b (x :&: (y :&: z)) ((x :&: y) :&: z)

bsecond  :: (BProd b) => b y y' -> b (x :&: y) (x :&: y')
bsnd     :: (BProd b) => b (x :&: y) y
bassocrp :: (BProd b) => b ((x :&: y) :&: z) (x :&: (y :&: z))
(***)    :: (BProd b) => b x x' -> b y y' -> b (x :&: y) (x' :&: y')
(&&&)    :: (BProd b) => b x y  -> b x z  -> b x (y :&: z)
bswap3   :: (BProd b) => b ((x :&: y) :&: z) (z :&: (y :&: x))
bvoid    :: (BProd b) => b x y  -> b x x

bsecond f = bswap >>> bfirst f >>> bswap
bsnd = bswap >>> bfst
bassocrp = bswap3 >>> bassoclp >>> bswap3
bswap3 = bfirst bswap >>> bswap
(***) f g = bfirst f >>> bsecond g
(&&&) f g = bdup >>> (f *** g)
bvoid f = bdup >>> bfirst f >>> bsnd

-- staticSelect :: (BProd b) => Bool -> b (x :&: x) x
-- staticSelect choice = if choice then bfst else bsnd

-- | BSum - data plumbing for asynchronous sums. Asynchronous sums
-- (x :|: y) means that x and y are active for different durations
-- and times, but may overlap slightly due to variation in delay. 
-- Sums model conditional expressions in RDP, and can work well at
-- smaller scales. If there are a large number of rare choices, or
-- an unbounded number of choices, BDynamic should be favored.
--
--     bleft - apply behavior only to left path.
--     bmerge - combine elements of a sum (implicit synch)
--     binl - constant choose left; i.e. if true
--     bmirror - sums are commutative; flip left and right
--     bassocls - sums are associative (shift parens left)
--
-- Excepting bmerge, the above operations should be free at runtime.
-- bmerge has an overhead similar to bzip (in some cases it might be
-- better for performance to simply apply the same operation to left
-- and right and forego merging). 
--
-- A few more utility operations are defined from the primitives:
--
--     bright - apply behavior only to the right path
--     binr - constant choose right; i.e. if false
--     bassocrs - sums are associative (shift parens right)
--     (+++) - apply operations to both paths 
--     (|||) - apply operations to both paths then merge them
--     bskip - behavior never performed, for symmetry with bvoid.
-- 
class (Category b) => BSum b where
    bleft    :: b x x' -> b (x :|: y) (x' :|: y)
    bmirror  :: b (x :|: y) (y :|: x)
    bmerge   :: b (x :|: x) x
    binl     :: b x (x :|: y)
    bassocls :: b (x :|: (y :|: z)) ((x :|: y) :|: z)

bright   :: (BSum b) => b y y' -> b (x :|: y) (x :|: y')
binr     :: (BSum b) => b y (x :|: y)
bassocrs :: (BSum b) => b ((x :|: y) :|: z) (x :|: (y :|: z))
(+++)    :: (BSum b) => b x x' -> b y y' -> b (x :|: y) (x' :|: y')
(|||)    :: (BSum b) => b x z  -> b y z  -> b (x :|: y) z
bmirror3 :: (BSum b) => b ((x :|: y) :|: z) (z :|: (y :|: x))
bskip    :: (BSum b) => b y x -> b x x

bright f = bmirror >>> bleft f >>> bmirror
binr = binl >>> bmirror
bassocrs = bmirror3 >>> bassocls >>> bmirror3
(+++) f g = bleft f >>> bright g
(|||) f g = (f +++ g) >>> bmerge
bmirror3 = bleft bmirror >>> bmirror
bskip f = binr >>> bleft f >>> bmerge

-- staticSwitch :: (BSum b) => Bool -> b x (x :|: x)
-- staticSwitch choice = if choice then binl else binr

-- | bconjoin is a partial merge, extracting from a sum. 
bconjoinl :: (BSum b, BProd b) => b ((x :&: y) :|: (x :&: z)) (x :&: (y :|: z))
bconjoinr :: (BSum b, BProd b) => b ((x :&: z) :|: (y :&: z)) ((x :|: y) :&: z)
bconjoinl = bdup >>> (isolateX *** isolateYZ)
    where isolateX = (bfst +++ bfst) >>> bmerge
          isolateYZ = (bsnd +++ bsnd)
bconjoinr = (bswap +++ bswap) >>> bconjoinl >>> bswap

-- | Disjoin will propagate a split on one signal to other signals 
-- in the asynchronous product. This doesn't happen magically; there
-- must be a signal representing the split (only one branch needed)
-- in the same partition as the signal being split. To distribute a 
-- decision across multiple partitions requires explicit propagation
-- of the signal representing the split condition, ensuring explicit
-- communication.
--
-- Disjoin is necessary for effective use of choice in RDP. Patterns
-- using bsplit will typically need bdisjoin to access signals that
-- are available in context. Without disjoin, one can split signals 
-- via intermediate state (split would happen on query), but disjoin
-- is a more primitive and pure mechanism.
--
-- Disjoin is dual to conjoin, though this property is obfuscated by
-- the partition types and the explicit decision for which signal is
-- to represent the split. Utility disjoins (bdisjoin(l|r)(k?)(z?))
-- cover some duals to conjoin, albeit for more specific types.
--
--     bdisjoin - primitive disjoin; painful to use directly
--     bdisjoin :: b (x :&: ((S p () :&: y) :|: z))
--                   ((x :&: y) :|: (x :&: z))
--
--        S p () - unit signal representing split
--        x - signal being split, may be complex
--        y - preserved signal on left
--        z - preserved signal on right
--
--   Common case splitting on the "y" signal (S p y :|: z)
--
--     bdisjoinl  - disjoin left on one signal
--     bdisjoinr  - disjoin right on one signal
--     bdisjoinlk - disjoin left on first signal of many
--     bdisjoinrk - disjoin right on first signal of many
--
--   Common case splitting on the "z" signal (y :|: S p z)
--
--     bdisjoinlz  - disjoin left on one signal
--     bdisjoinrz  - disjoin right on one signal
--     bdisjoinlkz - disjoin left on first signal of many
--     bdisjoinrkz - disjoin right on first signal of many
--
class (BSum b, BProd b) => BDisjoin b where
    bdisjoin :: (SigInP p x) => b (x :&: ((S p () :&: y) :|: z)) ((x :&: y) :|: (x :&: z))

bdisjoinl   :: (BDisjoin b, BFmap b, SigInP p x) => b (x :&: (S p y :|: z))  ((x :&: S p y) :|: (x :&: z))
bdisjoinlk  :: (BDisjoin b, BFmap b, SigInP p x) => b (x :&: ((S p y :&: y') :|: z)) ((x :&: (S p y :&: y')) :|: (x :&: z))
bdisjoinlz  :: (BDisjoin b, BFmap b, SigInP p x) => b (x :&: (y :|: S p z)) ((x :&: y) :|: (x :&: S p z))
bdisjoinlkz :: (BDisjoin b, BFmap b, SigInP p x) => b (x :&: (y :|: (S p z :&: z'))) ((x :&: y) :|: (x :&: (S p z :&: z')))

bdisjoinr   :: (BDisjoin b, BFmap b, SigInP p x) => b ((S p y :|: z) :&: x) ((S p y :&: x) :|: (z :&: x))
bdisjoinrk  :: (BDisjoin b, BFmap b, SigInP p x) => b (((S p y :&: y') :|: z) :&: x) (((S p y :&: y') :&: x) :|: (z :&: x))
bdisjoinrz  :: (BDisjoin b, BFmap b, SigInP p x) => b ((y :|: S p z) :&: x) ((y :&: x) :|: (S p z :&: x))
bdisjoinrkz :: (BDisjoin b, BFmap b, SigInP p x) => b ((y :|: (S p z :&: z')) :&: x) ((y :&: x) :|: ((S p z :&: z') :&: x))

bdisjoinl   = prep >>> bdisjoin
    where prep = (bsecond . bleft) $ bdup >>> (bfirst $ bconst ())
bdisjoinlk  = prep >>> bdisjoin
    where prep = (bsecond . bleft) $ bfirst bdup >>> bassocrp >>> (bfirst $ bconst ())
bdisjoinlz  = (bsecond bmirror) >>> bdisjoinl  >>> bmirror
bdisjoinlkz = (bsecond bmirror) >>> bdisjoinlk >>> bmirror

bdisjoinr   = bswap >>> bdisjoinl   >>> (bswap +++ bswap)
bdisjoinrk  = bswap >>> bdisjoinlk  >>> (bswap +++ bswap)
bdisjoinrz  = bswap >>> bdisjoinlz  >>> (bswap +++ bswap)
bdisjoinrkz = bswap >>> bdisjoinlkz >>> (bswap +++ bswap)

-- | BZip is a behavior for combining elements of an asynchronous 
-- product. The main purpose is to combine them to apply a Haskell
-- function. The arguments must already be in the same partition to
-- zip them. The signals are implicitly synchronized. 
--
-- At least one of bzip or bzap must be defined.
--
class (BProd b, BFmap b) => BZip b where
    -- | bzip is a traditional zip, albeit between signals. Values
    -- of the same times are combined.
    bzip :: b (S p x :&: S p y) (S p (x,y))
    bzip = bzipWith (,)
    
    -- | bzap describes an applicative structure. It applies a
    -- function while zipping the two signals. Usefully, this can
    -- support some partial reuse optimizations if the left element
    -- changes slower than the right element.
    bzap :: b (S p (x -> y) :&: S p x) (S p y)
    bzap = bzip >>> bfmap (uncurry ($))

-- | A common pattern - zip with a particular function.
bzipWith :: (BZip b) => (x -> y -> z) -> b (S p x :&: S p y) (S p z)
bzipWith fn = bfirst (bfmap fn) >>> bzap

-- | unzip is included for completeness. 
bunzip :: (BProd b, BFmap b) => b (S p (x,y)) (S p x :&: S p y)
bunzip = (bfmap fst &&& bfmap snd)

-- | BSplit is how we lift decisions from data to control. It is the
-- RDP equivalent to `if then else` expressions, except bdisjoin is
-- necessary to apply the split to other values in lexical scope. 
class (BSum b, BFmap b) => BSplit b where
    bsplit :: b (S p (Either x y)) (S p x :|: S p y)

-- | bsplitWith is included to dual zip, and might be useful.
bsplitWith :: (BSplit b) => (x -> Either y z) 
           -> b (S p x) (S p y :|: S p z)
bsplitWith fn = bfmap fn >>> bsplit

-- | bsplitOn is a convenience operation, filtering True values to
-- the left and False values to the right.
bsplitOn :: (BSplit b) => (x -> Bool) 
         -> b (S p x) (S p x :|: S p x)
bsplitOn f = bsplitWith f'
    where f' x = if f x then Left x else Right x

-- | unsplit is included for completeness.
bunsplit :: (BSum b, BFmap b) => b (S p x :|: S p y) (S p (Either x y))
bunsplit = (bfmap Left ||| bfmap Right)

-- | BTemporal - operations for orchestrating signals in time.
-- (For spatial orchestration, see FRP.Sirea.Partition.)
--
-- For arrow laws, it would be ideal to model timing properties of
-- signals in the type system. But doing so in Haskell is awkward.
-- For Sirea, many operations implicitly synchronize signals: zip,
-- merge, disjoin, etc.. 
class (Category b) => BTemporal b where
    -- | Delay a signal. For asynchronous products or sums, branches
    -- that pass through `delay` are delayed by the same amount. The
    -- delays in different branches may diverge: bdelay may apply to
    -- only the left or first branch. 
    --
    -- Delay represents communication or calculation time. Without
    -- delay, updates straggle and cause glitches at larger scales.
    -- Delay also dampens feedback patterns with shared state.
    --
    -- This is logical delay. It does not cause an actual wait in 
    -- the implementation. It only modifies the signal value. Many
    -- small delays might aggregate and be applied to a signal at
    -- once, as a simple optimization.
    bdelay :: DT -> b x x
    
    -- | Synchronize signals. Affects asynchronous products or sums.
    -- Adds delay to the lower-latency signals to ensure every input
    -- has equal latency - i.e. logical synchronization. Results in
    -- logically seamless transitions between choices, or logically
    -- simultaneous actions with products. Idempotent.
    bsynch :: b x x


-- | BPeek - anticipate a signal by studying its projected future.
-- RDP does not provide any support for prediction, but any future
-- for a signal will propagate through an RDP system. Modules can
-- benefit from predictions by components they don't know about.
-- This makes it easy to chain prediction systems together, or feed
-- plans right back into the predictions. 
--
-- BPeek can also serve as a state alternative if you need diffs or
-- a small history window. With peek you compare future vs. present
-- instead of present vs. past. And for buffered history, use delay
-- with peek to build a small buffer of valid state.
--
-- Peek places strain on a behavior's stability and efficiency. Use
-- it for small lookaheads only. For far predictions, use a proper
-- prediction model.
--
-- Due to peek, signals are observably distinct if they differ in
-- the future. Developers get abstraction and refactoring benefits
-- from idempotent expression, but network optimizations (multicast
-- and proxy cache) are hindered unless we have knowledge of how far
-- a service uses `bpeek` into signal futures.
--
class (BTemporal b) => BPeek b where
    -- | bpeek - anticipate a signal. The Left side is the future
    -- signal value, while the Right side indicates the signal is
    -- inactive in the given future. The activity of the signal 
    -- does not change; bpeek does not cause delay.
    --
    -- Use of Either here (instead of Maybe) enables use of bsplit.
    bpeek :: DT -> b (S p a) (S p (Either a ()))


-- | Dynamic behaviors are behaviors constructed or discovered at
-- runtime. They are useful for modeling resources, extensions,
-- service brokering, live programming, and staged computation 
-- (compilation, linking), and capability security patterns. 
--
-- Dynamic behaviors provide alternative to large (:|:) structures.
-- This is analogous to using objects instead of case dispatch. Best
-- practices will eventually exist for selecting between dynamic and
-- choice behaviors.
--
-- RDP is internally stateless, and dynamic behaviors are not stored
-- anywhere. Logically, RDP continuously expires and revokes dynamic
-- behaviors. This simplifies security (no grandfather capabilities)
-- and garbage collection, especially in open, distributed systems.
-- It also leads to clearer disruption semantics.
-- 
-- All arguments for dynamic behaviors are implicitly synchronized.
class (Behavior b) => BDynamic b where
    -- | evaluate a dynamic behavior and obtain the response. The DT
    -- argument indicates the maximum latency for dynamic behaviors,
    -- and the latency for beval as a whole. 
    --
    -- If there are any problems with the dynamic behavior, e.g. if
    -- too large for DT, the error path is selected. (If I could 
    -- statically enforce valid beval, I'd favor that option.)
    -- 
    beval :: (SigInP p x) => DT -> b (S p (b x y) :&: x) (y :|: S p ())

    -- | bexec will eval dropping the result. The success signal is 
    -- a simple reduction of the behavior signal. 
    --
    -- The default implementation is in terms of beval; override for
    -- performance if necessary.
    bexec :: (SigInP p x) => b (S p (b x y) :&: x) (S p () :|: S p ())
    bexec = prep >>> beval 0
        where prep = bfirst (bfmap f &&& bconst ()) >>> bassocrp
              f b' = bsecond b' >>> bfst -- modifies b'

-- | bevalOrElse use the `x` signal for a fallback behavior.
-- This performs the necessary dup and disjoin operations to
-- make `x` available during failure cases.
bevalOrElse :: (SigInP p x, BDynamic b) => DT -> b (S p (b x y) :&: x) (y :|: x)
bevalOrElse dt = bsecond bdup >>> bassoclp >>> bfirst (beval dt)
             -- now have (y :|: S p ()) :&: x 
             >>> bfirst (bmirror >>> bleft bdup) >>> bswap 
             -- now have x :&: ((S p () :&: S p ()) :|: y)
             >>> bdisjoin
             -- now have ((x :&: S p ()) :|: (x :&: y))
             >>> bleft bfst >>> bmirror >>> bleft bsnd
             -- now have (y :|: x)

-- WISHLIST: a behavior-level map operation.
--
--  I'd love to have a notion of performing a behavior on every
--  element in a collection, something like:
--
--    bforeach :: B (S p x) (S p y) -> B (S p [x]) (S p [y]) 
--  
--  Currently this can be achieved with beval, but would not be very 
--  efficient since it may need to rebuilt whenever an element in a
--  collection is modified.  
--
--  I'm not sure HOW to do much better, except maybe to create types
--  for collections of behaviors. If SL is a set of complex signals
--  of a common type:
--     map       :: B x y -> B (SL x) (SL y)
--     singleton :: B x (SL x)
--     cons      :: B (x :&: SL x) (SL x)
--     foldl     :: B (y :&: x) y -> B (y :&: SL x) y
--  But I don't want to complicate Sirea with a new signal type, and
--  it isn't clear that this would help. Might be better to stick
--  with type-level operators like:  (x :&: (x :&: (x :&: (x ...
--


-- TODO: convenience operators?
--  I've added Bdeep - eqvs. of bcadadr and setf bcadadr from Lisp
--  Need some stack-like operators
--      on (x :&: (y :&: (z :& ...
--      kswap, krotl(3,4,5,6,7), krotr(3,4,5,6,7), kdup, kover, 
--      kdisjoin would be feasible for some number of arguments.
--      ktake,kput
--  Maybe some support for data-driven dynamic patterns.
--      folds, recursion
--
-- support for bcar, bcdr, bcadr, bcddr, bcdar, bcaar, etc. from Lisp
--    plus variations for first, second (application to elements)
-- rotations, deep copies of elements, etc.
--  


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


-- Continuous Signals: some quirkiness when signal carries information
-- relative to time. In particular, functions of time must be time
-- shifted to retain their shape when viewed at a later time. Perhaps 
-- instead what I need to model is the shape itself, with its own
-- relative time. 
--
-- Alternatively, I could constrain `bdelay` to operate on values of
-- specific types, i.e. not every value can be delayed.
--
-- A promising option is a behavior transform, with its own notion of
-- delay. I.e. b => ContinuousB.



