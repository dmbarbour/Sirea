
{-# LANGUAGE TypeOperators, MultiParamTypeClasses, Rank2Types #-}

-- | This module documents abstract behaviors used in Sirea. These
-- classes are implemented by the concrete behavior type B, but are
-- presented in typeclasses to support documentation and transforms.
-- (Users of Sirea are not expected to implement these typeclasses.)
--
-- This module focuses on the primary compositional behaviors plus a
-- few common performance annotations. Specific effects might be 
-- achieved via other classes.
--
module Sirea.Behavior  
    ( (:&:), (:|:), S, S0, S1, SigInP
    , (>>>) -- from Control.Category
    , bfwd
    , BFmap(..), bfseq, bseq, bspark, bstratf
    , BProd(..), bfst, bsecond, bsnd, bassocrp, (***), (&&&)
    , bvoid, (|*|)
    , BSum(..), binl, bright, binr, bassocrs, (+++), (|||)
    , bconjoin
    , BDisjoin(..)
    , bdisjoinry, bdisjoinryy, bdisjoinrz, bdisjoinrzz
    , bdisjoinly, bdisjoinlyy, bdisjoinlz, bdisjoinlzz
    , bIfThenElse, bUnless, bWhen -- utility
    , BZip(..), bzip, bzipWith, bunzip
    , BSplit(..), bsplitWith, bsplitOn, bunsplit, bsplitMaybe, bsplitBool
    , BTemporal(..)
    , BDynamic(..), beval, bexec, bbevalx, bbeval, bbexec
    , Behavior
    ) where

import Prelude hiding (id,(.))
import Control.Category
import Control.Parallel (pseq, par)
import Control.Parallel.Strategies (Eval)
import Sirea.Internal.STypes 
import Sirea.Time (DT)

infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||
infixr 1 |*|

-- | Behavior is a grouping of all basic behavior classes.
class ( Category b
      , BFmap b
      , BProd b, BZip b
      , BSum b, BSplit b
      , BDisjoin b
      , BTemporal b
      ) => Behavior b

-- | bfwd is just another name for Control.Category.id.
--     f >>> bfwd = f
--     bfwd >>> f = f
bfwd :: (Category b) => b x x
bfwd = id

-- | BFmap - pure operations on concrete signals. Includes common
-- performance annotations. BFmap lifts Haskell functions to process
-- signals, serving the role of `arr` from Control.Arrow. But bfmap
-- is constrained to operating on one concrete signal; functions 
-- cannot operate on asynchronous signals or across partitions.
--
class (Category b) => BFmap b where
    -- | bfmap applies a function to a concrete signal. This allows
    -- arbitrary Haskell functions to integrate with RDP.
    bfmap :: (x -> y) -> b (S p x) (S p y)

    -- | bconst maps a constant to the signal. The resulting signal
    -- will vary between active and inactive on the same schedule as
    -- the input signal, but its active value will be constant.
    --   bconst = bfmap . const
    -- This may be specialized for performance, like badjeqf.
    bconst :: y -> b (S p x) (S p y)
    -- bconst = bfmap . const

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
    -- bstrat = bfmap runEval

    -- | btouch will compute the signal slightly ahead of stability, 
    -- eliminating lazy thunks up to the Just x | Nothing spine of
    -- the signal. Compose with bstrat so evaluating `Just` will 
    -- evaluate parts of x.
    --
    -- This is a performance annotation, with no semantic effect on
    -- the signal or behavior. Developers shouldn't have divergent
    -- signal values.
    btouch :: b (S p x) (S p x)
    -- btouch = bfwd

    -- | The badjeqf behavior helps with stability in case of false
    -- updates, i.e. where the signal indicates a possible change,
    -- but the values are equivalent. (This is a common consequence
    -- of lossy functions, e.g. `Foo -> Bool`.) 
    --
    -- The signal values will be merged within each update; further,
    -- updates may be shifted into the future if there was no change
    -- in the observed values in the near term. The future cannot be
    -- tested indefinitely, so the update will be delivered. It may
    -- occasionally be useful to compose this with bfchoke.
    --
    -- badjeqf should have no effect on the signal's semantic value,
    -- and should be considered a performance annotation.
    badjeqf :: (Eq x) => b (S p x) (S p x)
    -- badjeqf = bfwd

-- | bfseq will sequence evaluation when the signal update occurs,
-- according to a provided sequential strategy. A useful idiom:
--   > import Control.DeepSeq (rnf)
--   > bfseq rnf
-- This would reduce a signal to normal form before further progress 
-- in the partition's thread. This can improve data parallelism by
-- making more efficient use of partition threads, can help control
-- memory overheads, and can achieve more predictable performance.
bfseq :: (BFmap b) => (x -> ()) -> b (S p x) (S p x)
bfseq f = bfmap seqf >>> bstrat >>> btouch
    where seqf x = (f x) `pseq` return x

-- | bseq applies the normal Haskell sequence operator (`seq`) to
-- reduce elements in a signal to weak head normal form. 
--    bseq = bfseq (`seq` ())
-- In many cases, weak head normal form is insufficient. But it is
-- useful for strict data types, and easy access to bseq can be
-- convenient.
bseq :: (BFmap b) => b (S p x) (S p x)
bseq = bfseq (`seq` ())

-- | `bspark` is the similar to `bfseq` except that it sparks each
-- computation rather than running it in the partition thread, and 
-- does not wait for the computation to complete. 
--
-- bspark is predictable, but not very composable. For example, in
--   > bspark foo >>> bspark bar -- (don't do this)
-- The bar reduction will occur in a spark that immediately waits
-- for the foo reduction to complete. This ensures the bar reduction
-- doesn't compete with the foo reduction, but limits parallelism.
-- Consequently, bspark is best used to just perform full reduction.
-- If you need more control, use bstrat directly.
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
-- Use with btouch to initiate computation relative to stability.
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
--     bswap - products are commutative
--     bassoclp - products are associative (move parens left)
--     b1i - introduce an S1 signal (identity for :&:)
--     b1e - eliminate an S1 signal
--     btrivial - convert anything to the S1 signal
--
-- The above operations should be free at runtime (after compile).
-- bdup has a trivial cost in Haskell (since we alias the value 
-- representation). The last three are to match categories, but
-- you'll typically use bfst or bsnd instead.
--
-- A few operations are defined from the primitives: 
--
--     bfst - keep the first signal, drop the second.
--     bsnd - keep second signal, drop first
--     bsecond - operate on the second signal
--     bassocrp - products are associative (move parens right)
--     (***) - operate on first and second in parallel
--     (&&&) - create and define multiple pipelines at once
--     bvoid - branch behavior just for side-effects, drop result
--
-- Various Laws or Properties (assuming valid RDP operands):
--
-- Factor First: bfirst f >>> bfirst g = bfirst (f >>> g)
-- Spatial Idempotence: bdup >>> (f *** f) = f >>> bdup
-- Spatial Commutativity: bfirst f >>> bsecond g = bsecond g >>> bfirst f
--     Lemma: (f *** g) >>> (f' *** g') = (f >>> f') *** (g >>> g')
-- Associative Identity (Product, Left): bassoclp >>> bassocrp = id
-- Associative Identity (Product, Right): bassocrp >>> bassoclp = id
-- Commutative Identity (Product): bswap >>> bswap = id
-- Duplicate Identity: bdup >>> bswap = bdup
--
class (Category b) => BProd b where
    bfirst   :: b x x' -> b (x :&: y) (x' :&: y)
    bdup     :: b x (x :&: x)
    bswap    :: b (x :&: y) (y :&: x)
    bassoclp :: b (x :&: (y :&: z)) ((x :&: y) :&: z)
    b1i      :: b x (S1 :&: x)
    b1e      :: b (S1 :&: x) x
    btrivial :: b x S1

bfst     :: (BProd b) => b (x :&: y) x
bsecond  :: (BProd b) => b y y' -> b (x :&: y) (x :&: y')
bsnd     :: (BProd b) => b (x :&: y) y
bassocrp :: (BProd b) => b ((x :&: y) :&: z) (x :&: (y :&: z))
(***)    :: (BProd b) => b x x' -> b y y' -> b (x :&: y) (x' :&: y')
(&&&)    :: (BProd b) => b x y  -> b x z  -> b x (y :&: z)
bswap3   :: (BProd b) => b ((x :&: y) :&: z) (z :&: (y :&: x))
bvoid    :: (BProd b) => b x y -> b x x

bsecond f = bswap >>> bfirst f >>> bswap
bsnd = bfirst btrivial >>> b1e
bfst = bswap >>> bsnd
bassocrp = bswap3 >>> bassoclp >>> bswap3
bswap3 = bfirst bswap >>> bswap
(***) f g = bfirst f >>> bsecond g
(&&&) f g = bdup >>> (f *** g)
bvoid f = bdup >>> bfirst f >>> bsnd

-- staticSelect :: (BProd b) => Bool -> b (x :&: x) x
-- staticSelect choice = if choice then bfst else bsnd

-- | It is often convenient to treat an RDP behavior as describing a
-- multi-agent system. Agents are concurrent and operate in a shared 
-- environment. Agents interact indirectly through shared resources,
-- such as shared state or demand monitors. The blackboard metaphor
-- is a useful model for achieving cooperation between agents.
--
-- Agents may provide services to other agents by publishing dynamic
-- behaviors to a shared space where other agents can discover them.
--
-- The |*| operator makes treating behaviors as agents (or threads,
-- or processes) very convenient:
--
--   > main = runSireaApp $ alice |*| bob |*| charlie |*| dana
--
-- The operator is commutative, associative, and idempotent, though
-- these properties are not utilized by any optimizer at the moment.
-- The definition is very simple:
--
--   > f |*| g = bvoid f >>> bvoid g
--
(|*|) :: (BProd b) => b x y -> b x z -> b x x
(|*|) f g = bvoid f >>> bvoid g

-- | BSum - data plumbing for asynchronous sums. Asynchronous sums
-- (x :|: y) means that x and y are active for different durations
-- and times, but may overlap slightly due to variation in delay. 
-- Sums model conditional expressions in RDP, and can work well at
-- smaller scales. If there are a large number of rare choices, or
-- an unbounded number of choices, BDynamic should be favored.
--
--     bleft - apply behavior only to left path.
--     bmerge - combine elements of a sum (implicit synch)
--     bmirror - sums are commutative; flip left and right
--     bassocls - sums are associative (shift parens left)
--     b0i - introduce the S0 signal (identity for :|:)
--     b0e - eliminate the S0 signal
--     bvacuous - convert S0 signal into anything
--
-- Excepting bmerge, the above operations should be free at runtime.
-- bmerge has an overhead similar to bzip (in some cases it might be
-- better for performance to simply apply the same operation to left
-- and right and forego merging). 
--
-- A few more utility operations are defined from the primitives:
--
--     binl - constant choose left; i.e. if true
--     binr - constant choose right; i.e. if false
--     bright - apply behavior only to the right path
--     bassocrs - sums are associative (shift parens right)
--     (+++) - apply operations to both paths 
--     (|||) - apply operations to both paths then merge them
--     bskip - behavior never performed, for symmetry with bvoid.
-- 
-- Various Laws or Properties: 
--
-- Factor Left: bleft f >>> bleft g = bleft (f >>> g)
-- Choice Commutativity: bleft f >>> bright g = bright g >>> bleft f
--     Lemma: (f +++ g) >>> (f' +++ g') = (f >>> f') +++ (g >>> g')
-- Merge Equivalence (*): (f +++ f) >>> bmerge = bmerge >>> f
--     (*): assuming synchronized signals
-- Dead Source Elim (Left):  binl >>> bright g = binl
-- Dead Source Elim (Right): binr >>> bleft f  = binr
-- Associative Identity (Sum, Left): bassocls >>> bassocrs = id
-- Associative Identity (Sum, Right): bassocrs >>> bassocls = id
-- Commutative Identity (Product): bmirror >>> bmirror = id
-- Merge Identity: bmirror >>> bmerge = bmerge
--
class (Category b) => BSum b where
    bleft    :: b x x' -> b (x :|: y) (x' :|: y)
    bmirror  :: b (x :|: y) (y :|: x)
    bmerge   :: b (x :|: x) x
    bassocls :: b (x :|: (y :|: z)) ((x :|: y) :|: z)
    b0i      :: b x (S0 :|: x)
    b0e      :: b (S0 :|: x) x
    bvacuous :: b S0 x


binl     :: (BSum b) => b x (x :|: y)
binr     :: (BSum b) => b y (x :|: y)
bright   :: (BSum b) => b y y' -> b (x :|: y) (x :|: y')
bassocrs :: (BSum b) => b ((x :|: y) :|: z) (x :|: (y :|: z))
(+++)    :: (BSum b) => b x x' -> b y y' -> b (x :|: y) (x' :|: y')
(|||)    :: (BSum b) => b x z  -> b y z  -> b (x :|: y) z
bmirror3 :: (BSum b) => b ((x :|: y) :|: z) (z :|: (y :|: x))
-- bskip    :: (BSum b) => b y x -> b x x

binr = b0i >>> bleft bvacuous
binl = binr >>> bmirror
bright f = bmirror >>> bleft f >>> bmirror
bassocrs = bmirror3 >>> bassocls >>> bmirror3
(+++) f g = bleft f >>> bright g
(|||) f g = (f +++ g) >>> bmerge
bmirror3 = bleft bmirror >>> bmirror
-- bskip f = binr >>> bleft f >>> bmerge

-- staticSwitch :: (BSum b) => Bool -> b x (x :|: x)
-- staticSwitch choice = if choice then binl else binr

-- | bconjoin (aka "factor") is a partial merge, factoring a common 
-- element from a sum. (The name refers to conjunctive form.)
bconjoin :: (BSum b, BProd b) => b ((x :&: y) :|: (x :&: z)) (x :&: (y :|: z))
bconjoin = getX &&& getYZ
    where getX = (bfst +++ bfst) >>> bmerge
          getYZ = (bsnd +++ bsnd)

-- | Disjoin (aka "distribute") will apply a split across elements
-- that are outside of that split. This doesn't happen magically; a
-- signal representing the split must start in the *same partition*
-- as the signals being split, i.e. a spatial constraint. 
--
-- I favor the word `disjoin` (refering to disjunctive form) because
-- distribution has other connotations for spatial semantics. To
-- perform disjoin across partitions requires explicit distribution
-- of the signal representing the split.
--
-- Disjoin is dual to conjoin, though this property is obfuscated by
-- the partition types and the explicit decision for which signal is
-- to represent the split. Utility disjoins (bdisjoin(l|r)(k?)(z?))
-- cover some duals to conjoin, albeit for more specific types.
--
--     bdisjoin - primitive disjoin; often painful to use directly
--     bdisjoin :: b (x :&: ((S p () :&: y) :|: z))
--                   ((x :&: y) :|: (x :&: z))
--
--        S p () - unit signal representing split 
--        x - signal being split, may be complex (for x in p)
--        y - preserved signal on left
--        z - preserved signal on right
--
--   Common case splitting on the "y" signal (S p y :|: z)
--
--     bdisjoinly  - disjoin left on y signal
--     bdisjoinry  - disjoin right on y signal
--     bdisjoinlyy - disjoin left on first y signal of many
--     bdisjoinryy - disjoin right on first y signal of many
--
--   Common case splitting on the "z" signal (y :|: S p z)
--
--     bdisjoinlz  - disjoin left on z signal
--     bdisjoinrz  - disjoin right on z signal
--     bdisjoinlzz - disjoin left on first z signal of many
--     bdisjoinrzz - disjoin right on first z signal of many
--
-- Disjoin is moderately expensive; it requires masking signals, and
-- which requires a some synchronization. However, you'll only pay
-- for the resulting `x` values that it seems you might use.
--
class (BSum b, BProd b) => BDisjoin b where
    bdisjoin :: (SigInP p x) => b (x :&: ((S p () :&: y) :|: z)) ((x :&: y) :|: (x :&: z))


-- helpers for `bdisjoin` in various arrangements of values. 
bdisjoinly  :: (BDisjoin b, BFmap b, SigInP p x) => b (x :&: (S p y :|: z))  ((x :&: S p y) :|: (x :&: z))
bdisjoinlyy :: (BDisjoin b, BFmap b, SigInP p x) => b (x :&: ((S p y :&: y') :|: z)) ((x :&: (S p y :&: y')) :|: (x :&: z))
bdisjoinlz  :: (BDisjoin b, BFmap b, SigInP p x) => b (x :&: (y :|: S p z)) ((x :&: y) :|: (x :&: S p z))
bdisjoinlzz :: (BDisjoin b, BFmap b, SigInP p x) => b (x :&: (y :|: (S p z :&: z'))) ((x :&: y) :|: (x :&: (S p z :&: z')))

bdisjoinry  :: (BDisjoin b, BFmap b, SigInP p x) => b ((S p y :|: z) :&: x) ((S p y :&: x) :|: (z :&: x))
bdisjoinryy :: (BDisjoin b, BFmap b, SigInP p x) => b (((S p y :&: y') :|: z) :&: x) (((S p y :&: y') :&: x) :|: (z :&: x))
bdisjoinrz  :: (BDisjoin b, BFmap b, SigInP p x) => b ((y :|: S p z) :&: x) ((y :&: x) :|: (S p z :&: x))
bdisjoinrzz :: (BDisjoin b, BFmap b, SigInP p x) => b ((y :|: (S p z :&: z')) :&: x) ((y :&: x) :|: ((S p z :&: z') :&: x))

bdisjoinly   = prep >>> bdisjoin
    where prep = (bsecond . bleft) $ bdup >>> (bfirst $ bconst ())
bdisjoinlyy  = prep >>> bdisjoin
    where prep = (bsecond . bleft) $ bfirst bdup >>> bassocrp >>> (bfirst $ bconst ())
bdisjoinlz   = (bsecond bmirror) >>> bdisjoinly  >>> bmirror
bdisjoinlzz  = (bsecond bmirror) >>> bdisjoinlyy >>> bmirror

bdisjoinry   = bswap >>> bdisjoinly  >>> (bswap +++ bswap)
bdisjoinryy  = bswap >>> bdisjoinlyy >>> (bswap +++ bswap)
bdisjoinrz   = bswap >>> bdisjoinlz  >>> (bswap +++ bswap)
bdisjoinrzz  = bswap >>> bdisjoinlzz >>> (bswap +++ bswap)

-- | bIfThenElse expresses a common pattern seen in many functional
-- languages, but in the context of RDP's reactive model. It will
-- test a condition in an environment `x`, choose the left or right
-- path (onTrue +++ onFalse), then merge the results. This is a lot
-- of responsibilities; often, it would be preferable to keep an 
-- open conditional expression (y :|: y), or to preserve information
-- computed while testing the condition. However, when we want a
-- quick and convenient conditional, this is available.
bIfThenElse :: (BDisjoin b, SigInP p x)
            => b x (S p () :|: S p ()) -- decision
            -> b x y -- onTrue
            -> b x y -- onFalse
            -> b x y -- total bIfThenElse expression
bIfThenElse cond onTrue onFalse =
    bdup >>> bfirst (cond >>> bleft (b1i >>> bswap)) >>> bswap >>> 
    -- at (x :&: ((S p () :&: S1) :|: S p ())
    bdisjoin >>> 
    -- at (x :&: S1) :|: (x :&: S p ())
    (bfst +++ bfst) >>>
    -- at (x :|: x)
    (onTrue +++ onFalse) >>>
    -- at (y :|: y)
    bmerge -- at y
    
-- | bUnless and bWhen serve a similar role to the unless and when
-- operations defined in Control.Monad. They are performed for
-- continuous side-effects based on a condition.
bUnless, bWhen :: (BDisjoin b, SigInP p x)
        => b x (S p () :|: S p ()) -- decision
        -> b x y_ -- action (drops response)
        -> b x x -- unless or when operation
bUnless cond = bWhen (bmirror . cond) 
bWhen cond action = bvoid $
    bdup >>> bfirst (cond >>> bleft (b1i >>> bswap)) >>> bswap >>>
    -- at (x :&: ((S p () :&: S1) :|: S p ())
    bdisjoin >>>
    -- at (x :&: S1) :|: (x :&: S p ())
    bleft (bfirst action)

-- | BZip is a behavior for combining elements of an asynchronous 
-- product. The main purpose is to combine them to apply a Haskell
-- function. The arguments must already be in the same partition to
-- zip them. The signals are implicitly synchronized. 
class (BProd b, BFmap b) => BZip b where
    -- | bzap describes an applicative structure. It applies a
    -- function while zipping the two signals. Usefully, this can
    -- support some partial reuse optimizations if the left element
    -- changes slower than the right element.
    bzap :: b (S p (x -> y) :&: S p x) (S p y)

-- | bzip is a traditional zip, albeit between signals. Values
-- of the same times are combined.
bzip :: (BZip b) => b (S p x :&: S p y) (S p (x,y))
bzip = bzipWith (,)

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

-- | bsplitWith is included to dual zipWith, and might be useful.
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

-- | bsplitMaybe is for convenience, with the obvious semantics.
bsplitMaybe :: (BSplit b) => b (S p (Maybe x)) (S p x :|: S p ())
bsplitMaybe = bsplitWith mb2e
    where mb2e Nothing  = Right ()
          mb2e (Just x) = Left x

-- | bsplitBool will divide on a boolean. Here the left branch is
-- true, while the right branch is false.
bsplitBool :: (BSplit b) => b (S p Bool) (S p () :|: S p ())
bsplitBool = bsplitWith b2e
    where b2e False = Right ()
          b2e True = Left ()

-- | BTemporal - operations for orchestrating signals in time.
-- (For spatial orchestration, see Sirea.Partition.)
--
-- For arrow laws, it would be ideal to model timing properties of
-- signals in the type system. But doing so in Haskell is awkward.
-- For Sirea, many operations implicitly synchronize signals: zip,
-- merge, disjoin, etc.. 
-- 
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

{- NOTE: Decided to auto-choke at demand monitors and other cyclic
   resources, such that explicit choke is unnecessary.
    -- | When interacting with resources, especially in combination
    -- with bdelay, one can model 'temporal recursive' behaviors in
    -- RDP, e.g. `monitor >>> bfmap fn >>> bdelay 0.1 >>> demand` 
    -- could continuously update a demand monitor relative to past
    -- values. This sort of behavior is discouraged where feasible,
    -- but cannot be avoided entirely in open systems. The problem
    -- is that the computation might run too far ahead - seconds and
    -- eventually minutes, consuming a growing amount of memory and
    -- burning CPU, and increasing risk of rework.
    --
    -- The bfchoke behavior will help regulate update cycles, cause
    -- them to backoff once they've run too far ahead of stability.
    -- The cost is a potential loss of snapshot consistency for a
    -- behavior's distant future values. bfchoke is a performance
    -- annotation but is essential for cyclic applications. Sirea
    -- does not include any automatic chokes at this time.
    bfchoke :: b x x
-}

-- | Dynamic behaviors are behaviors constructed or discovered at
-- runtime. They are useful for modeling resources, extensions,
-- service brokering, dynamic configuration (from script or XML), 
-- live programming, staged computations (compilation, linking), and
-- capability security patterns (behaviors as capabilities). 
--
-- Old dynamic behaviors are continuously expiring. If the behavior
-- is no longer provided by a signal, it will soon be disabled. A
-- good type-system would reflect this, ensuring dynamic behaviors
-- are used before they expire (relative to some latency from their
-- source). Behaviors should not be represented in persistent state.
-- This constraint simplifies RDP with respect to security, garbage 
-- collection, and resilience patterns after disruption. (Developers
-- can always use a script if they need persistent representation of
-- dynamic behaviors.)
--
-- In Sirea, dynamic behaviors are constrained to either operate in
-- one partition or cross a partition (point to point). The latter
-- is useful for dynamic networking. Since Sirea lacks type safety
-- for latency, an error signal is admitted.
--
-- NOTE: BDynamic has two behavior types, b b'. This is primarily
-- to support arrow transforms; not every transformed behavior type
-- can be used as dynamic behavior. For other behavior wrappers or
-- DSLs, I suggest compilation behavior separate from evaluation.
-- E.g. compile to `B w x y`, compose further if desired, then use
-- `beval` to execute.  
class (Behavior b, Behavior b') => BDynamic b b' where
    -- | continuously install and evaluate dynamic behaviors that
    -- may cross partitions. The first argument provides metadata
    -- about crossing and latency; it's a hack.
    bevalx :: (SigInP p x, SigInP p' y) => b' (S p ()) (S p' ())
           -> b (S p (b' x y) :&: x) (y :|: S p ())

-- | common case: install and evaluate in a single partition, given
-- a fixed latency budget for each behavor. Dynamic behaviors that 
-- don't fit the latency have the unit result.
beval :: (BDynamic b b', SigInP p x, SigInP p y) 
      => DT -> b (S p (b' x y) :&: x) (y :|: S p ())
beval = bevalx . bdelay

-- | apply a dynamic behavior, but drop the result. Conveniently, 
-- this is independent of latency.
bexec :: (BDynamic b b', SigInP p x) => b (S p (b' x y_) :&: x) S1
bexec = bfirst (bfmap (>>> btrivial)) >>> bevalx btrivial >>> btrivial

-- | bbevalx, bbeval, bbexec simply constrain the BDynamic type to
-- simplify type inference in the common case where the host type is
-- the same as the hosted behavior.
bbevalx :: (BDynamic b b, SigInP p x, SigInP p' y) 
        => b (S p ()) (S p' ()) -> b (S p (b x y) :&: x) (y :|: S p ())
bbeval :: (BDynamic b b, SigInP p x, SigInP p y) 
        => DT -> b (S p (b x y) :&: x) (y :|: S p ())
bbexec :: (BDynamic b b, SigInP p x) => b (S p (b x y_) :&: x) S1
bbevalx = bevalx
bbeval = beval
bbexec = bexec

-- WISHLIST: a behavior-level map operation.
--
--  I'd love to have a notion of performing a behavior on every
--  element in a collection, something like:
--
--    bforeach :: B (S p x) (S p y) -> B (S p [x]) (S p [y]) 
--  
--  Currently this can be achieved with beval, but would not be very 
--  efficient since it may need to rebuilt whenever an element in a
--  collection is modified. Native support could make it efficient.
--
--  I'm not sure HOW to do much better, except maybe to create types
--  for collections of behaviors. If V is a vector of complex signals
--  of a common type:
--     map       :: B x y -> B (V x) (V y)
--     singleton :: B x (V x)
--     cons      :: B (x :&: V x) (V x)
--     append    :: B (V x :&: V x) (V x)
--     foldl     :: B (y :&: x) y -> B (y :&: V x) y
--  But I don't want to complicate Sirea with a new signal type, and
--  it isn't clear that this would help. Might be better to stick
--  with type-level operators like:  (x :&: (x :&: (x :&: (x ...
--
--  Even without vector support like this, we can achieve efficient
--  large collections processing if we use intermediate state to 
--  index and restructure big data into a stable tree. I.e. then we
--  only need to rebuild small sections of that tree. 
--
-- NOTES:
--  Potentially, the vector type could work if it had a maximum static
--  size, i.e. such that we operate on 'up to' K elements, for some 
--  small K. This might be worth pursuing, as it could actually be 
--  useful in a wide range of applications (~ SIMD parallelism, music,
--  animation, real-time systems, probabilistic RDP).


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

"bvoid.bvoid" bvoid . bvoid = bvoid
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



-- TODO: Consider switching to:
--   (S p f x)
-- Such that we're operating on time-varying signals of kind `f x`.
-- This could impact:
--
--   bfmap, bzip, bsplit, btemporal
--
-- Unfortunately, it would also make many constraints difficult to
-- validate, e.g. for `bdelay :: DT -> b x x` I'd need a global 
-- constraint on `x` that says each element is delayable, or I'd
-- need to enforce that more uniformly. (I think Oleg had some 
-- answer for partial constraints.)
--
-- The alternative is use wrappers - `S p (f x)` technique, wrapping
-- the `x` type to enforce constraints locally. The main difficulty
-- here is that continous time-varying signals can't be ignorant of
-- `bdelay`. With this technique, I'll probably need to wrap the `S`
-- type as well, hide it behind another kind of behavior to enforce
-- tighter constraints or open a GADT to new options.
--
-- Potentially I can change this transparently, later, by defining
-- `S p x` as `SF Sig p x` or similar.
-- 


