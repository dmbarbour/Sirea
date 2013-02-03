{-# LANGUAGE Rank2Types, FlexibleContexts #-}

-- | PCaps represent the standard partition capabilities for every B
-- behavior, i.e. for computation and caching purposes. Partitions
-- support basic scheduling tasks, access to logical time of current
-- step, and construction of mutable references for cache or waits.
--
-- These capabilities are distinct from PCX (which instead supports
-- sensors, actuators, state, FFI, and IO). Basically, PCaps model 
-- minimal, ambient authority that every link needs to have.
--
-- Developers can generally assume there are no strong distinctions
-- between PCaps of the same type, i.e. the primary distinguishing
-- factor is the partition type.
--
module Sirea.PCaps 
    ( PCaps(..)
    , Ref(..)
    , modifyRef, writeRef', modifyRef'
    ) where

import Sirea.Time (T)
import Data.Typeable
import Data.Function (fix)

data PCaps m p = PCaps 
    { pc_schedNextPhase :: !(m () -> m ()) -- perform action next phase 
    , pc_schedEndOfStep :: !(m () -> m ()) -- perform action after last phase
    , pc_schedNextStep  :: !(m () -> m ()) -- perform action next step
    , pc_schedHeartbeat :: !(m () -> m ()) -- perform action next heartbeat
    , pc_stepTime       :: !(m T) -- time associated with current step
    , pc_newRef         :: !(forall a . a -> m (Ref m a)) -- obtain a new reference
    -- , pc_newKey :: !(m Key) -- obtain a fresh identifier (per partition)
    }

-- | Ref basically follows the IORef interface. 
--
-- Ref is simply defined by its read and write caps.
-- Ref must only be used within its origin partition.
data Ref m a = Ref 
    { readRef  :: !(m a)
    , writeRef :: !(a -> m ())
    }

-- | strict write
writeRef' :: Ref m a -> a -> m ()
writeRef' rf a = a `seq` writeRef rf a

modifyRef :: (Monad m) => Ref m a -> (a -> a) -> m ()
modifyRef rf fn = readRef rf >>= writeRef rf . fn

-- | strict modify
modifyRef' :: (Monad m) => Ref m a -> (a -> a) -> m ()
modifyRef' rf fn = readRef rf >>= writeRef' rf . fn

instance (Typeable1 m) => Typeable1 (PCaps m) where
    typeOf1 lc = mkTyConApp tyLC [tyM] where
        tyLC = mkTyCon3 "sirea" "Sirea.PCaps" "PCaps"
        tyM = (typeOf1 . fix . pc_schedHeartbeat) lc 

instance (Typeable1 m) => Typeable1 (Ref m) where
    typeOf1 rf = mkTyConApp tyRf [tyM] where
        tyRf = mkTyCon3 "sirea" "Sirea.PCaps" "Ref"
        tyM = (typeOf1 . readRef) rf


