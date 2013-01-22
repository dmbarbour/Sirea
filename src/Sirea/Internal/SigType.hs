
-- | this module was separated primarily because I might want direct
-- access to the signal type when adding support for continuous
-- varying signals. It is not intended for access by regular clients
-- of the Sirea library.
module Sirea.Internal.SigType
    ( Sig(..)
    , mkSig
    ) where

import Sirea.Internal.DiscreteTimedSeq

-- | Sig is an abstract type for discrete-varying signals in Sirea.
-- A signal is defined for all times, but in practice the past is
-- dropped (collected) while the future is updated over time. 
data Sig a = Sig 
    { s_head :: !(Maybe a) 
    , s_tail :: !(DSeq (Maybe a)) 
    }

-- utility
mkSig :: Maybe a -> DSeq (Maybe a) -> Sig a
mkSig v0 ds = Sig { s_head = v0, s_tail = ds }

-- TODO:
-- Sig describes a discrete-varying signal. But continuous varying
-- signals might be modeled within it - preferably in a manner 
-- suitable for symbolic analysis. This likely means one of:
--   * trigonometric interpolation polynomial (sum of m = -n to n of c_m * e^(i*m*x))
--   * polynomial expressions (sum of i = 0..n of A_i * t^i)
-- Both would allow simple rep as vector of doubles for the coefficients.
-- (But there is a challenge of performing time-shifts on them. Maybe some sort
-- of matrix operation would be necessary.)
--
-- multi-dimensional curves would be desired anyway, i.e. vectors and 
-- matrices of curvatures. Some sort of time-varying bezier surface is 
-- also a possibility.
--
-- Will probably want to handle in separate modules. I don't have the 
-- mathematical knowledge for this at the moment, not even to efficiently


