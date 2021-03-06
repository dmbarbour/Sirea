{-# LANGUAGE TypeOperators, EmptyDataDecls, DeriveDataTypeable, 
    MultiParamTypeClasses, FlexibleInstances #-}

module Sirea.Internal.STypes
    ( (:&:)
    , (:|:)
    , S, S0, S1
    --, V
    , SigInP
    , SigMembr, BuildMembr(..), buildMembr
    ) where 

import Data.Typeable -- all are typeable

-- | (x :&: y). Product of asynchronous or partitioned signals, but
-- x and y will have equal and tightly coupled active periods. For
-- example, if x is active for 300ms, inactive 100ms, then active
-- 600ms, then y will have the same profile. However, asynchronous
-- delays enable a small divergence of exactly when these periods
-- occur. (They'll be synchronized before recombining signals.)
data (:&:) x y deriving(Typeable)
infixr 3 :&:

-- | (x :|: y). Union or Sum of asynchronous or partitioned signals.
-- Signals are active for different durations, i.e. if x is active
-- 100 ms, inactive 400 ms, then active 100 ms: then y is inactive
-- 100 ms, active up to 400 ms, then inactive 100 ms. (There may be
-- durations where both are inactive.) Due to asynchronous delays 
-- the active periods might overlap for statically known periods.
data (:|:) x y deriving(Typeable)
infixr 2 :|:

-- | (S p a) is a Sig a in partition p. 
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
-- as bcross.
--
-- Partitions must be Data.Typeable to support analysis of types
-- as values. Some types may have special meaning, indicating that
-- extra threads should be constructed when behavior is initiated.
data S p a deriving(Typeable)

-- a local version of Void (not exported)
data Void deriving(Typeable)

-- | S0 is the identity type for (:|:). It is a signal that is never
-- active. There are no valid values for it, so it cannot be active.
-- Its existence is for type-level operations, and match signatures
-- to various category and arrow models that need sum identity.
--
-- S0 is an non-existent signal. 
type S0 = S () Void

-- | S1 is the identity type for (:&:). It is a signal that is never
-- utilized. It's stuck in a limbo partition, so cannot be accessed.
-- Its existence is for type-level operations, and match signatures
-- to various category and arrow models that need product identity.
--
-- S1 is an imaginary signal.
type S1 = S Void ()

-- might add support for continuous signals? C p x? rather not, though.
-- might add support for collections? (L x) for list of x?


-- | (SigMembr x) supports construction of a membrane object given
-- only the type of signal x. This is necessary for BDynamic and
-- possibly other generic programs that process signals without much
-- knowledge of them. SigMembr can also enforce that a signal is a
-- valid instance of the signal type. SigMembr is entirely defined
-- by sirea-core, no ability to extend it in clients.
class SigMembr x where 
    sigMembr :: (BuildMembr m) => m x

-- | (BuildMembr m) describes construction of a particular membrane.
-- Membranes, in this case, are ignorant about their types. The
-- membrane type determines what is constructed. Note that GADTs
-- might be necessary to leverage this effectively. 
--
-- (developed for use by BDynamic, but client extensible).
class BuildMembr m where
    buildSigMembr :: m (S p a)
    buildSumMembr :: m x -> m y -> m (x :|: y)
    buildProdMembr :: m x -> m y -> m (x :&: y)

instance SigMembr (S p a) where 
    sigMembr = buildSigMembr
instance (SigMembr x, SigMembr y) => SigMembr (x :|: y) where
    sigMembr = buildSumMembr sigMembr sigMembr
instance (SigMembr x, SigMembr y) => SigMembr (x :&: y) where
    sigMembr = buildProdMembr sigMembr sigMembr

-- | Function to build a membrane m for a given signal type x.
buildMembr :: (BuildMembr m, SigMembr x) => m x
buildMembr = sigMembr

-- | (SigInP p x) constrains that complex signal x exists entirely
-- in partition p. This avoids need for implicit bcross in disjoin
-- and eval behaviors, while allowing them to be reasonably generic.
class (SigMembr x) => SigInP p x
instance SigInP p (S p x)
instance (SigInP p x, SigInP p y) => SigInP p (x :&: y)
instance (SigInP p x, SigInP p y) => SigInP p (x :|: y)

-- would like something that `selects` a signal in p, and extracts a
-- unit signal, in a generic way... that would certainly make disjoin
-- easier to express. This sort of type-driven program would be easy
-- to express in Coq. But I'd also want to optimize which signal is
-- selected (i.e. the cheapest one to receive).


-- (V x) lifts collection processing to Sirea's reactive layer.
-- The vector of signals is homogenous, but may have a time-varying 
-- finite size. A signal of collections may be converted to a vector
-- of signals and back, i.e.:
--
--    b (S p [x]) (V (S p x)) 
--    b (V (S p x) :&: S p ()) (S p [x])
--      or maybe
--    b (S p [Maybe x]) (V (S p x))
--    b (V (S p x) :&: S p ()) (S p [Maybe x])
--
-- Vectors are logically synchronous, with every element having the
-- same latency. Though for a complex signals of form `V (y :&: z)`
-- the y and z components may still have varying latencies.
--
-- THOUGHTS: would it be worthwhile to constrain V to process a
-- maximum static count (V k x), with k in the type-system? This 
-- would make V a little less widely useful, but a little more
-- suitable for real-time systems... and still quite useful for
-- dynamic systems, and easier to implement properly.
--
-- (STATUS: preliminary design, experimental)
-- data V k x

-- Related to V:
--
-- A concept of partial-signals might be useful, i.e. one half of an
-- 'if'. Formally, we can get this using S1:
--    type H x = x :|: S1 -- half-signals
--    (x :|: y) ~> H x -- bright btrivial
--    x ~> H x -- b0i >>> bleft bvacuous >>> mirror
--    y ~> H x -- b0i >>> (bvacuous +++ btrivial)
--    H x :|: H x ~> H x -- plumb >>> (bmerge +++ bmerge) where
--       plumb :: (x1 :|: y1) :|: (x2 :|: y2) ~> (x1 :|: x2) :|: (y1 :|: y2)
--       plumb = bassocrs >>> bright plumb2 >>> bassocls
--       plumb2 = bassocls >>> bleft bmirror >>> bassocrs
--     
-- But it is inconvenient to deal with products of partial signals,
-- since each signal might be partial in its own ways. I know of no
-- efficient, effective way to combine (H x :&: H y :&: H z).
--
-- If vectors have maximum dynamic size, then presumably we could 
-- understand them as a vector of H signals values. In that case,
-- it might be better to separate V from H (such that we can have
-- `V k (H x)` explicitly), but we might be able to provide some
-- useful, standard ways to operate on the signals, too.
-- 


