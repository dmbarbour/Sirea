
{-# LANGUAGE GADTs, TypeFamilies #-}

-- where do I organize these types???
module FRP.Sirea.Link 
    ( MkLnk(..)
    , LnkUp(..), SigUp(..)
    , Lnk, LnkW(..)
    , ln_zero
    , ln_left, ln_right, ln_fst, ln_snd
    , ln_null, ln_mbsig, ln_lnkup
    , ln_sumap, ln_mksigzip
    ) where

import FRP.Sirea.Signal
import FRP.Sirea.Behavior (S,(:&:),(:|:)) -- for Lnk
import FRP.Sirea.Internal.Types
import Data.IORef
import Control.Monad (unless)

-- | MkLnk - constructors and metadata for including a new behavior
-- primitive in Sirea. There are currently two metadata values:
--
--   time sensitive (tsen) - if false, may shift delays before or 
--     after MkLnk behavior (affecting delay aggregation)
--   desc - a string descriptor of the link, for debugging.
-- 
-- The primary operation is ln_build, which constructs a link in the
-- IO monad, accepting the response capability and generating the
-- demand capability. IO is for constructing intermediate caches and
-- any preparatory hooks to external resources, but should not have
-- observable side-effects (i.e. wait for the signal to activate).
--
-- Dead-code optimizations are handled as part of ln_build: if the
-- response target is LnkNull, one might return LnkNull for demand
-- capability. This isn't necessary, though - an effectful behavior
-- will accept a link even if it doesn't provide any meaningful
-- output. 
--   
data MkLnk x y = MkLnk 
    { ln_tsen  :: !Bool 
    , ln_desc  :: !String
    , ln_build :: !(Lnk y -> IO (Lnk x))
    }

-- MkLnk difficulties: I'm not sure that I can handle a flexible structure
-- for inputs to MkLnk, at least not without users providing dedicated
-- functions. Would it be possible to use typeclasses to select the function
-- for finding the right delay values?

-- | A Lnk describes a complex product of LnkUp values, to 
-- support all complex signal types - S, (:&:) and (:|:). 
--   type Lnk = LnkW LnkUp
type Lnk = LnkW LnkUp

-- | LnkW is a GADT for a complex product of signals. Note that
-- LnkNull is used to indicate dead code or unknown types. LnkW
-- ignores partitioning information about the original signal, 
-- which was used to constrain construction of the behavior.
--
data LnkW s a where
    LnkNull :: LnkW s a
    LnkSig  :: !(s a) -> LnkW s (S p a)
    LnkProd :: !(LnkW s a) -> !(LnkW s b) -> LnkW s (a :&: b)
    LnkSum  :: !(LnkW s a) -> !(LnkW s b) -> LnkW s (a :|: b)

-- | LnkUp processes updates to a concrete signal. Complex signals
-- can be represented ultimately as a complex product of LnkUp 
-- structures. 
--
--   ln_touch - call this if an update is guaranteed in the near
--      future but not immediately. Allows later stages in the pipe
--      to wait for the update.
--   ln_update - updates the entire future of a signal (see SigUp).
--      Note that shutdown is also modeled as an update (using the 
--      signal s_never).
-- 
-- Dead code is better represented by LnkNull than by trivial LnkUp.
data LnkUp a = LnkUp
    { ln_touch  :: !(IO ())
    , ln_update :: !(SigUp a -> IO ())
    }

-- | ln_zero is a trivial LnkUp state, similar to LnkNull but hides
-- that the input is dropped.
ln_zero :: LnkUp a
ln_zero = LnkUp 
    { ln_touch = return ()
    , ln_update = const $ return ()
    }

-- | ln_lnkup extracts from LnkSig or returns ln_zero from LnkNull
ln_lnkup  :: Lnk (S p a) -> (LnkUp a)
ln_lnkup (LnkSig lu) = lu
ln_lnkup _ = ln_zero

-- | Each signal update carries:
--    state - the new state of the signal, starting at a given time
--      which must be greater or equal to current stability. The
--      value Nothing here means that the state did not change.
--    stability - the new stability of the signal, after applying
--      the state update. A promise that all future updates happen
--      no earlier than the given instant in time, to support GC. 
--      The value Nothing here means stable forever.
-- Stability always updates. State might not update, i.e. to avoid
-- recomputing a signal when it is known it did not change.
data SigUp a = SigUp 
    { su_state :: !(Maybe (Sig a , T))
    , su_stable :: !(Maybe T)
    }
su_signal :: SigUp a -> Maybe (Sig a)
su_signal = fmap fst . su_state
su_time :: SigUp a -> Maybe T
su_time = fmap snd . su_state


---------------------------
-- UTILITY
-------------

ln_left   :: LnkW s (a :|: b) -> LnkW s a
ln_right  :: LnkW s (a :|: b) -> LnkW s b
ln_fst    :: LnkW s (a :&: b) -> LnkW s a
ln_snd    :: LnkW s (a :&: b) -> LnkW s b
ln_null   :: LnkW s a -> Bool
ln_mbsig  :: LnkW s (S p a) -> Maybe (s a)

ln_left (LnkSum a _) = a
ln_left _ = LnkNull

ln_right (LnkSum _ b) = b
ln_right _ = LnkNull

ln_fst (LnkProd a _) = a
ln_fst _ = LnkNull

ln_snd (LnkProd _ b) = b
ln_snd _ = LnkNull

ln_null LnkNull = True
ln_null (LnkSig _) = False
ln_null (LnkProd a b) = ln_null a && ln_null b
ln_null (LnkSum a b) = ln_null a && ln_null b

ln_mbsig (LnkSig sa) = Just sa
ln_mbsig _ = Nothing


-- | simple link update from a signal update transformer
-- (Not all SigUp transforms are safe for RDP. Most aren't.)
ln_sumap :: (SigUp x0 -> SigUp xf) -> LnkUp xf -> LnkUp x0
ln_sumap fn ln = LnkUp 
  { ln_touch = ln_touch ln -- forward touches
  , ln_update = ln_update ln . fn -- forward updates after map
  }

-- possibility: a stateful version of the above, via recursive 
-- structure (to encapsulate state), could be useful to model some
-- mechanisms such as choke, adjeqf, etc.

-- | for combining two signals; stores in an intermediate structure, 
-- and constructs update from given zip function. Will release any
-- unnecessary state based on updates to stability. Will hold update
-- if one input is touched but not yet updated.
--
-- This implementation assumes updates and touches for both inputs
-- are single-threaded, which should be enforced using partitions 
-- on behavior types.
ln_mksigzip :: (Sig x -> Sig y -> Sig z) -> LnkUp z -> IO (LnkUp x, LnkUp y)
ln_mksigzip jf luz =
    newIORef sm_zero >>= \ rfSigM ->
    return $! ln_mkSigM' rfSigM jf luz

ln_mkSigM' :: IORef (SigM x y) -> (Sig x -> Sig y -> Sig z) 
           -> LinkUp z -> (LnkUp x, LnkUp y)
ln_mkSigM' rfSigM jf luz = (lux,luy)
    where pokeX = 
            readIORef rfSigM >>= \ sm ->
            (writeIORef rfSigM $ sm_update_l st_poke sm) >>
            unless (sm_waiting sm) (ln_touch ln)
          pokeY = 
            readIORef rfSigM >>= \ sm ->
            (writeIORef rfSigM $ sm_update_r st_poke sm) >>
            unless (sm_waiting sm) (ln_touch ln)
          emit  = 
            readIORef rfSigM >>= \ sm ->
            unless (sm_waiting sm) $
              let su = sm_emit jf sm in
              (writeIORef rfSigM $! sm_cleanup (su_stable su) sm) >>
              ln_update luz su 
          updX su = modifyIORef rfSigM (sm_sigup_l su) >> emit
          updY su = modifyIORef rfSigM (sm_sigup_r su) >> emit
          lux = LnkUp { ln_touch = pokeX, ln_update = updX }
          luy = LnkUp { ln_touch = pokeY, ln_update = updY }


