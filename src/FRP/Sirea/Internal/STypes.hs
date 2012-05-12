{-# LANGUAGE TypeOperators, EmptyDataDecls, TypeFamilies #-}
{- MultiParamTypeClasses, FlexibleInstances -}

module FRP.Sirea.Internal.STypes
    ( (:&:)
    , (:|:)
    , S

    -- type functions (via type families)
    , SigInP
    , SigToP
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

{- Typeclass variation of SigInP - 
     requires MultiParamTypeClasses, FlexibleInstances

-- | SigInP describes signals signals that exist in one partition p.
-- It is not intended for extension by Sirea clients; all the SigInP
-- types are defined by Sirea (using flexible instances).
class SigInP p x where
    proofOfSigInP :: ProofOfSigInP p x -- NOT exported. 

data ProofOfSigInP p x 
trivialSigInP :: ProofOfSigInP p x
trivialSigInP = undefined

instance SigInP p (S p x) where
    proofOfSigInP = trivialSigInP

instance (SigInP p x, SigInP p y) => SigInP p (x :&: y) where
    proofOfSigInP = trivialSigInP

instance (SigInP p x, SigInP p y) => SigInP p (x :|: y) where
    proofOfSigInP = trivialSigInP

-}

{- Type Family version of SigInP -}

-- | (SigInP p x) describes a complex signal x in partition p. It is an
-- identity function on signal types, but only for those that exist
-- entirely in one partition. This is necessary to achieve generic
-- behaviors for disjoin and eval.
--
-- Should NOT be extended by Sirea clients.
type family SigInP p x
type instance SigInP p (S p x)   = (S p x)
type instance SigInP p (x :&: y) = (SigInP p x :&: SigInP p y)
type instance SigInP p (x :|: y) = (SigInP p x :|: SigInP p y)

-- | (SigToP p x) describes a complex signal x AFTER crossing into 
-- partition p. That is, every component signal in x is rewritten as
-- existing in p.
--
-- Should NOT be extended by Sirea clients.
type family SigToP p x
type instance SigToP p (S p' x)  = (S p x)
type instance SigToP p (x :&: y) = (SigToP p x :&: SigToP p y)
type instance SigToP p (x :|: y) = (SigToP p x :|: SigToP p y)

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



