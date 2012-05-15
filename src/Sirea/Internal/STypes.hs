{-# LANGUAGE TypeOperators, EmptyDataDecls,   
    MultiParamTypeClasses, FlexibleInstances #-}

module Sirea.Internal.STypes
    ( (:&:)
    , (:|:)
    , S
    , SigInP
    ) where 

import Data.Typeable -- all are typeable

-- | (x :&: y). Product of asynchronous or partitioned signals, but
-- x and y will have equal and tightly coupled active periods. For
-- example, if x is active for 300ms, inactive 100ms, then active
-- 600ms, then y will have the same profile. However, asynchronous
-- delays enable a small divergence of exactly when these periods
-- occur. (They'll be synchronized before recombining signals.)
data (:&:) x y
infixr 3 :&:

-- | (x :|: y). Union or Sum of asynchronous or partitioned signals.
-- Signals are active for different durations, i.e. if x is active
-- 100 ms, inactive 400 ms, then active 100 ms: then y is inactive
-- 100 ms, active up to 400 ms, then inactive 100 ms. (There may be
-- durations where both are inactive.) Due to asynchronous delays 
-- the active periods might overlap for statically known periods.
data (:|:) x y
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
data S p a

{- Typeclass variation of SigInP 
   MultiParamTypeClasses, FlexibleInstances -}

-- | (SigInP p x) constrains that complex signal x exists entirely
-- in partition p. This avoids need for implicit bcross in disjoin
-- and eval behaviors, while allowing them to be reasonably generic.
-- 
-- This already has a complete definition. It is not for extension
-- by Sirea clients.
class SigInP p x where
    proofOfSigInP :: ProofOfSigInP p x 

data ProofOfSigInP p x 
trivialSigInP :: ProofOfSigInP p x
trivialSigInP = undefined

instance SigInP p (S p x) where
    proofOfSigInP = trivialSigInP

instance (SigInP p x, SigInP p y) => SigInP p (x :&: y) where
    proofOfSigInP = trivialSigInP

instance (SigInP p x, SigInP p y) => SigInP p (x :|: y) where
    proofOfSigInP = trivialSigInP

---------------------------
-- Data.Typeable Support --
---------------------------

tcSig, tcSum, tcProd :: TyCon
tcSig  = mkTyCon3 "Sirea" "Behavior" "S"
tcSum  = mkTyCon3 "Sirea" "Behavior" "(:|:)"
tcProd = mkTyCon3 "Sirea" "Behavior" "(:&:)"

instance Typeable2 S where
    typeOf2 _ = mkTyConApp tcSig []

instance Typeable2 (:|:) where
    typeOf2 _ = mkTyConApp tcSum []

instance Typeable2 (:&:) where
    typeOf2 _ = mkTyConApp tcProd []



