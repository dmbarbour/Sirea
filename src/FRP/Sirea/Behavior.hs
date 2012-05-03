
{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}

-- | This module describes the basic RDP behaviors in Sirea. It also
-- exports the concrete behavior type `B` for clients.
--
-- Several behaviors are provided in dedicated modules:
--   see FRP.Sirea.Link for bUnsafeLnk
--   see FRP.Sirea.Partition for bcross, bscope
module FRP.Sirea.Behavior  
    ( (:&:), (:|:), S, B 
    , (>>>) -- from Control.Category
    , bfwd
    , BFmap(..)
    , BProd(..), bsecond, bsnd, bassocrp, (***), (&&&), bvoid
    , BSum(..), bright, binr, bassocrs, (+++), (|||), bskip 
    , bconjoinl, bconjoinr
    , BDisjoin(..), bdisjoin'
    , BZip(..), bzipWith, bunzip
    , BSplit(..), bsplitWith, bunsplit
    , BTemporal(..), BPeek(..)
    -- , bcross, bscope - see FRP.Sirea.Partition
    -- , bUnsafeLnk - see FRP.Sirea.Link
    ) where

import Prelude hiding (id,(.))
import Control.Category
import Control.Parallel (pseq)
import Control.Parallel.Strategies (Eval, runEval, rpar)

import FRP.Sirea.Internal.STypes ((:&:),(:|:),S)
import FRP.Sirea.Internal.BTypes (B)
import FRP.Sirea.Time (DT)

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

    -- | bforce can force computation of accummulated thunks from
    -- fmap and other operations. This can control space costs and
    -- improve parallelism (by forcing in multiple partitions). This
    -- is applied per update, based on stability and perhaps a bit
    -- beyond the stable value (based on type b, configurable or
    -- adaptive). Since updates tend to monotonically increase
    -- stability, this results in incrementally forcing evaluations.
    --
    -- How much is forced is configurable via a sequential strategy,
    -- (x -> ()), which allows arbitrarily deep sequencing. See
    -- Control.Seq for more information.
    bforce :: (x -> ()) -> b (S p x) (S p x)
    bforce _ = bfwd -- semantically

    -- | bstrat applies a parallel strategy to a signal based on
    -- sampling. The essential idea is to initialize evaluation of
    -- the next sample when the current one is accessed. This simple
    -- technique can achieve respectable levels of data parallelism. 
    -- Since logical delay corresponds to physical delays, this also
    -- controls space overhead and avoids wasted sparks.
    --
    -- Ignoring performance concerns, bstrat is a simple variation
    -- of bfmap. A parallel strategy is typically (x -> Eval x), but
    -- this is generalized to (x -> Eval y) to enforce semantics.
    --
    -- See Control.Parallel.Strategies for more information. 
    bstrat :: (x -> Eval y) -> b (S p x) (S p y)
    bstrat = bfmap . (runEval .) -- semantically

    -- | Types that can be tested for equality can be filtered to
    -- eliminate redundant updates. Redundant updates are common if
    -- mapping lossy functions (like `x->Bool`)to signals. badjeqf,
    -- for adjacent equality filter, performs this operation.
    --
    -- The reason to eliminate redundant updates is to eliminate
    -- redundant computations further down the line, eg. at `bzip`.
    -- This is a valuable performance optimization.
    --
    -- Not all redundant updates are eliminated. When a signal is
    -- updated, scanning for the first actual change could diverge
    -- if there is no change. Type `b` may allow configuration of
    -- how far to scan.
    badjeqf :: (Eq x) => b (S p x) (S p x)
    badjeqf = bfwd -- semantically

-- | bspark will initiate computation of a signal's future value in 
-- parallel when the current value is sampled. A useful technique is
--
-- >  import Control.DeepSeq
-- >  bspark rnf -- reduce values to normal form.
--
-- This leverages both data parallelism and dataflow parallelism.
-- Lazy thunks are computed in parallel, and the future values of a
-- signal are computed in parallel with observing present values.
--
bspark :: (BFmap b) => (x -> ()) -> b (S p x) (S p x)
bspark f = bstrat $ rpar . withSeq f
    where withSeq f x = f x `pseq` x

-- | BProd - data plumbing for asynchronous products. Asynchronous
-- product (x :&: y) means that both signals are active for the same
-- durations and approximately for the same times (modulo variations
-- in delay). This can be understood as modeling parallel pipelines. 
--
--     bfirst - operate on just one signal (the first one)
--     bdup - duplicate any signal and process both pipelines.
--     bfst - keep the first signal, drop the second.
--     bswap - products are commutative
--     bassoclp - products are associative (move parens left)
--
-- The above operations should be free at runtime. A few operations 
-- are defined based on the above. 
--
--     bsecond - operate on second signal
--     bsnd - keep second signal, drop first
--     bassocrp - products are associative (move parens right)
--     (***) - operate on first and second in parallel
--     (&&&) - create and define multiple pipelines at once
--     bvoid - branch behavior just for side-effects, drop result
--
-- Delays diverge due to performing operations and adding latency to
-- only one of two signals. Use `bsynch` to synchronize the signals
-- when synchronous behavior is required.
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
-- bmerge does have some overhead, but is rarely a useful operation.
-- A few more operations are defined using bmirror.
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

-- | Disjoin will distribute a decision. To achieve disjoin involves 
-- combining a signal representing a split with an external signal.
-- Disjoin is necessary for effective use of `choice` in RDP, i.e.
-- most design patterns using bsplit will use bdisjoin.
--
-- Unfortunately, disjoin is neither generic nor dual to conjoin.
-- This seems unavoidable due to potential for partitioning and the
-- need to communicate between partitions. 
--
-- Due to lack of generic disjoin, a structural discipline can be
-- used to rapidly disjoin multiple elements. See 
--
-- In some cases it may be more convenient to use intermediate state
-- rather than disjoin. Disjoin is necessary to model lexical scope
-- without shared state.
--
class (BSum b, BProd b) => BDisjoin b where
    -- | bdisjoinl combines a signal representing the current choice
    -- (S p ()) and a signal representing available data (S p x) in
    -- the same partition `p`. This splits the external data.
    bdisjoin  :: b (S p x :&: ((S p () :&: y) :|: z) )
                   ( (S p x :&: y) :|: (S p x :&: z) )

-- | bdisjoin' preserves the choice signal to support further disjoin.
bdisjoin' :: (BDisjoin b) => b (S p x :&: ((S p () :&: y) :|: z))
                               ((S p () :&: S p x :&: y) :|: (S p x :&: z))
bdisjoin' = dupChoiceSig >>> bdisjoin >>> rotChoiceSig
    where dupChoiceSig = (bsecond . bleft) $ bfirst bdup >>> bassocrp
          rotChoiceSig = bleft $ bassoclp >>> bfirst bswap >>> bassocrp

-- now what I need is to systematically split a stack.
-- and maybe a way to split a split?  some distribution mechanic would be nice:
--    ((a :|: b) :&: (c :|: d)) -> 
--       ((a :&: c) :|: (a :&: d) :|: (b :&: c) :|: (b :&: d)) 
--    probably would need conjoin to make that work.
-- I think a language for RDP dataflow between partitions 
-- will need some auto-wiring mechanisms...



-- | BZip is a behavior for combining elements of an asynchronous 
-- product. The main purpose is to combine them to apply a Haskell
-- function. The arguments must already be in the same partition to
-- zip them. The signals are implicitly synchronized.
--
-- At least one of bzip or bzap must be defined.
--
class (BProd b, BFmap b) => BZip b where
    -- | bzip is a traditional zip, albeit between signals.
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
-- necessary to apply the split to any parameters. 
class (BSum b, BFmap b) => BSplit b where
    bsplit :: b (S p (Either x y)) (S p x :|: S p y)

-- | bsplitWith is included to dual zip, and might be useful.
bsplitWith :: (BSplit b) => (x -> Either y z) 
           -> b (S p x) (S p y :|: S p z)
bsplitWith fn = bfmap fn >>> bsplit

-- | unsplit is included for completeness.
bunsplit :: (BSum b, BFmap b) => b (S p x :|: S p y) (S p (Either x y))
bunsplit = (bfmap Left ||| bfmap Right)

-- | BTemporal - operations for orchestrating signals in time.
-- (For spatial orchestration, see FRP.Sirea.Partition.)
class (Category b) => BTemporal b where
    -- | Delay a signal. For products or sums, every branch delays
    -- equally. Delay models communication or calculation time. It
    -- is important for consistency; without delay, updates straggle
    -- and cause glitches at larger scales. Delay can also dampen
    -- some feedback patterns with shared state resources.
    --
    -- Delays for elements of asynchronous products and sums diverge
    -- due to bfirst, bleft.
    bdelay :: DT -> b x x
    
    -- | Synchronize signals. Affects asynchronous products or sums.
    -- Ensures that delay on every branch is equal, suitable for zip
    -- or merge. Note that the signal components may be in different
    -- partitions, which makes bsynch useful for synchronous action
    -- at a distance.
    --
    -- Synch is achieved by adding delay to the faster branches. It
    -- has no effect if applied to an already synchronous signal or
    -- a lone concrete signal.
    --
    -- Synchronization is implicit for many operations like zip or
    -- merge. RDP forbids combining signals of different latencies
    -- (except indirectly, e.g. via shared state resources).
    bsynch :: b x x


-- | BPeek - anticipate a signal by studying its projected future.
-- RDP does not provide any support for prediction, but does ensure
-- that predictions are propagated through behavior and potentially
-- even through stateful resources.
--
-- Use of BPeek isn't suitable for long-term predictions. For that,
-- you need a proper predictions model. But it can help compose many 
-- prediction models. It can support detecting changes or `events`
-- (i.e. gesture detection, redraw dirty windows). It can simplify 
-- computing derivatives for smooth interpolation or animation. It 
-- can enable advance preparation or loading of resources. It also
-- separates the concerns of making a prediction and consuming it.
--
-- Anticipation reduces need for many traditionally stateful idioms.
--
-- Using bpeek can damage stability of the behavior, i.e. if we look
-- four seconds into the future, then an update within four seconds 
-- can upset our predictions. Retroactive correction has limits, so
-- instability can ultimately cause glitches and indeterminism. This
-- can be countered by use of bdelay so we're actually looking at a
-- buffered future (an idiom for short-term memory). Developers must
-- balance the benefits of bpeek against the stability costs. Domain
-- can affect the tradeoff. Low latency can be more important than
-- accuracy (e.g. for dodging in combat), or naturally stable models
-- could allow us to look further without glitches.
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
-- (compilation, linking). They can represent runtime authority 
-- and object capability patterns. Dynamic behaviors continuously
-- expire, so to stop sharing a behavior also revokes it.
--
-- Unfortunately, dynamic behaviors are not first class for reasons
-- similar to bdisjoin. Every dynamic behavior must start and end
-- in a single partition. (It is possible to use `bcross` within the
-- dynamic behavior, though.)
--
-- Dynamic behaviors may perform better than use of sum types (:|:)
-- in some cases, especially when there are a lot of rarely selected
-- choices. However, they aren't as composable, may hinder bpeek for
-- anticipation, and certainly hinder dead code elimination. 
-- 
-- All arguments for dynamic behaviors are implicitly synchronized.
class (BEmbed b b', Behavior b, Behavior b') => BDynamic b b' where
    -- | evaluate a dynamic behavior and obtain the response.
    -- If the dynamic behavior would take more than DT time, or has
    -- other static problems, failure is returned immediately via
    -- the left output signal (failure :|: result).
    beval :: DT -> b (S p (b' (S p x) (S p y)) 
                          :&: (S p x)         )
                     (S p () :|: S p y)

{- I'll eventually want higher arities for asynchronous parameters 
   to achieve higher stability for the beval operation. 
    -- | evaluate with two asynchronous arguments, one result.
    beval2to1 :: DT -> b (S p (b' (S p x :&: S p y) (S p z)) 
                              :&: (S p x :&: S p y)         )
                         (S p () :|: S p z)
-}



-- Note: I probably need several more variations of beval:
--  more asynchronous inputs and outputs (how many? 3? 4?)
--  support for sums on inputs and outputs.
-- But these are all performance options. For now, just get it working!

-- | BEmbed - embed (or lift) behaviors. 
class BEmbed b b' where
    bembed :: b' x y -> b x y

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



---------------------------
-- Concrete Instances: B --
---------------------------

-- TUNING
-- dtScanAheadB: default lookahead for constB, adjeqfB. Must limit
--   scan ahead to avoid divergence. A larger lookahead reduces risk
--   of rework down the line but might cause more rework immediately.
--   I'd like this to be tunable in a higher layer for constructing
--   behaviors. (or alternatively define it based on the ln_peek 
--   values in the behavior). I don't want it cluttering the normal
--   interface, though.
-- dtStratB: maximum distance to compute ahead of stability, for the
--   bforce and bstrat behaviors. 
dtScanAheadB :: DT
dtScanAheadB = 2.0 -- seconds ahead of stability

dtStratB :: DT
dtStratB = 0.1 -- seconds ahead of stability

instance BFmap B where 
    bfmap    = fmapB
    bconst   = constB dtScanAheadB
    bforce   = forceB dtStratB
    bstrat   = stratB dtStratB
    badjeqf  = adjeqfB dtScanAheadB
instance BProd B where
    bfirst   = firstB
    bdup     = dupB
    bfst     = fstB
    bswap    = swapB
    bassoclp = assoclpB
instance BSum B where
    bleft    = leftB
    bmirror  = mirrorB
    bmerge   = mergeB
    binl     = inlB
    bassocls = assoclsB
instance BZip B where
    bzap     = zapB
instance BSplit B where
    bsplit   = splitB
instance BDisjoin B where 
    bdisjoin = disjoinB
instance BTemporal B where
    bdelay   = delayB
    bsynch   = synchB
instance BPeek B where
    bpeek    = peekB
instance Behavior B



