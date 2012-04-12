
{-# LANGUAGE GADTs, TypeOperators #-}

module FRP.Sirea.Link 
    ( MkLnk(..)
    , LnkUp(..)
    , Lnk, LnkW(..)
    , ln_zero, ln_lnkup
    , ln_left, ln_right, ln_fst, ln_snd, ln_null
    , ln_sumap, ln_mksigzip, ln_lumap
    , ln_withSigM
    , SigUp(..), su_apply
    ) where

import FRP.Sirea.Internal.STypes
import FRP.Sirea.Internal.Types
--import FRP.Sirea.Context
import FRP.Sirea.Signal
import Data.IORef
import Control.Monad (unless)

-- | MkLnk - constructors and metadata for including a new behavior
-- primitive in Sirea. 
--
-- The primary operation is ln_build, which constructs a link in the
-- IO monad, accepting the response capability and generating the
-- demand capability. IO is for constructing intermediate caches and
-- any preparatory hooks to external resources, but should not have
-- observable side-effects (i.e. wait for the signal to activate).
--
-- Dead-code optimizations are handled as part of ln_build: if the
-- response target is LnkDead, one might return LnkDead for demand
-- capability. This isn't necessary, though - an effectful behavior
-- will accept a link even if it doesn't provide any meaningful
-- output.
--
-- Secondary data includes:
--   tsen - time sensitive: if true, prevents delay aggregation and
--     forces aggregated delay to apply prior to reaching link. 
--
data MkLnk x y = MkLnk 
    { ln_tsen  :: !Bool 
    , ln_build :: !(Lnk y -> IO (Lnk x))
    }

-- | A Lnk describes a complex product of LnkUp values, to 
-- support all complex signal types - S, (:&:) and (:|:). 
--   type Lnk = LnkW LnkUp
type Lnk = LnkW LnkUp

-- | LnkW is a GADT for a complex product of signals. Note that
-- LnkDead is used to indicate dead code. 

or unknown types. LnkW
-- ignores partitioning information about the signals. If that
-- is important info, catch it in the initial behavior. 
data LnkW s a where
    LnkDead :: LnkW s a
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
-- Dead code is better represented by LnkDead than by trivial LnkUp.
data LnkUp a = LnkUp
    { ln_touch  :: !(IO ())
    , ln_update :: !(SigUp a -> IO ())
    }

-- | ln_zero is a trivial LnkUp state, similar to LnkDead but hides
-- that the input is dropped.
ln_zero :: LnkUp a
ln_zero = LnkUp 
    { ln_touch = return ()
    , ln_update = const $ return ()
    }

-- | ln_lnkup extracts LnkUp (from LnkSig or ln_zero from LnkDead)
ln_lnkup  :: Lnk (S p a) -> (LnkUp a)
ln_lnkup (LnkSig lu) = lu
ln_lnkup _ = ln_zero

---------------------------
-- UTILITY
-------------

ln_left   :: LnkW s (a :|: b) -> LnkW s a
ln_right  :: LnkW s (a :|: b) -> LnkW s b
ln_fst    :: LnkW s (a :&: b) -> LnkW s a
ln_snd    :: LnkW s (a :&: b) -> LnkW s b
ln_null   :: LnkW s a -> Bool

ln_left (LnkSum a _) = a
ln_left _ = LnkDead

ln_right (LnkSum _ b) = b
ln_right _ = LnkDead

ln_fst (LnkProd a _) = a
ln_fst _ = LnkDead

ln_snd (LnkProd _ b) = b
ln_snd _ = LnkDead

ln_null LnkDead = True
ln_null (LnkSig _) = False
ln_null (LnkProd a b) = ln_null a && ln_null b
ln_null (LnkSum a b) = ln_null a && ln_null b

-- | simple link update from a signal update transformer
-- (Not all SigUp transforms are safe for RDP. Most aren't.)
ln_sumap :: (SigUp x -> SigUp y) -> LnkUp y -> LnkUp x
ln_sumap fn ln = LnkUp 
  { ln_touch = ln_touch ln -- forward touches
  , ln_update = ln_update ln . fn -- forward updates after map
  }

-- | simple transformer from LnkUp to Lnk
ln_lumap :: (LnkUp x -> LnkUp y) -> Lnk (S p x) -> Lnk (S p y)
ln_lumap _ LnkDead = LnkDead
ln_lumap fn (LnkSig l) = LnkSig (fn l)


-- | for combining two signals; stores in an intermediate structure, 
-- and constructs update from given zip function. Will release any
-- unnecessary state based on updates to stability. Will hold update
-- if one input is touched but not yet updated.
--
-- This implementation assumes updates and touches for both inputs
-- are single-threaded, which should be enforced using partitions 
-- on behavior types.
ln_mksigzip :: (Sig x -> Sig y -> Sig z) -> LnkUp z -> IO (LnkUp x, LnkUp y)
ln_mksigzip jf luz = ln_withSigM onTouch onEmit
    where onTouch = ln_touch luz 
          onEmit sm = ln_update luz (sm_emit jf sm)


-- | SigM is a utility type for combining two input signals. This
-- function sets up a SigM based composition and automatically 
-- handles all the propagation and cleanup requirements.
--
--     ln_withSigM onTouch onEmit
--
ln_withSigM :: IO () -> (SigM x y -> IO ()) -> IO (LnkUp x, LnkUp y)
ln_withSigM onTouch onEmit = 
    newIORef sm_zero >>= \ rfSigM ->
    return $! ln_withSigM' rfSigM onTouch onEmit

ln_withSigM' :: IORef (SigM x y) -> IO () -> (SigM x y -> IO ()) 
             -> (LnkUp x, LnkUp y)
ln_withSigM' rfSigM onTouch onEmit = (lux,luy)
    where pokeX =   readIORef rfSigM >>= \ sm ->
                    writeIORef rfSigM (sm_update_l st_poke sm) >>
                    unless (sm_waiting sm) onTouch
          pokeY =   readIORef rfSigM >>= \ sm ->
                    writeIORef rfSigM (sm_update_r st_poke sm) >>
                    unless (sm_waiting sm) onTouch
          emit  =   readIORef rfSigM >>= \ sm ->
                    unless (sm_waiting sm) $
                    let sm' = sm_cleanup (sm_stable sm) sm in
                    sm' `seq` 
                    writeIORef rfSigM sm' >>
                    onEmit sm
          updX su = modifyIORef rfSigM (sm_sigup_l su) >> emit
          updY su = modifyIORef rfSigM (sm_sigup_r su) >> emit
          lux =     LnkUp { ln_touch = pokeX, ln_update = updX }
          luy =     LnkUp { ln_touch = pokeY, ln_update = updY }



