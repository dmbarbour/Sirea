
-- where do I organize these types???
module FRP.Sirea.Link 
    ( MkLnk(..)
    , Lnk(..)
    , SigUp(..)
    , ln_fmap
    ) where

import FRP.Sirea.Signal
import FRP.Sirea.Behavior (S,(:&:),(:|:)) -- for Lnk family


-- | MkLnk - constructors and metadata for including a new behavior
-- primitive in Sirea. There are currently two metadata values:
--
--   time-sensitive - if false, may shift delays before or after
--     (affecting delay aggregation optimizations)
--   effectful - if false, assume behavior only needed for output
--     (affecting dead code elimination optimizations)
-- 
-- A pure behavior will be neither effectful nor time-sensitive. A
-- simple query behavior (e.g. for mouse position or file state) may
-- be time-sensitive but not effectful. 
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

-- MkLnk difficulties: I'm not sure that I can handle a flexible structure
-- for inputs to MkLnk, at least not without users providing dedicated
-- functions. Would it be possible to use typeclasses to select the function
-- for finding the right delay values?


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
-- Lnk is used indirectly by bUnsafeLnk to adapt foreign services.
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
-- At the moment the idea is to access Lnk via typeclasses.
-- If that doesn't work out, I'll change Lnk to a GADT.

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
ln_fmap :: (SigUp x0 -> SigUp xf) -> Lnk (S pf xf) -> Lnk (S p0 x0)
ln_fmap fn ln = LnkSig 
  { ln_touch = ln_touch ln
  , ln_update = ln_update ln . fn 
  }


