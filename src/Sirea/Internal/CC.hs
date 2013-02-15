{-# LANGUAGE Rank2Types #-}

-- | RDP is designed for object capability security. Sirea attempts
-- to remain true to this design at least for the B0 type. Haskell,
-- unfortunately, makes it painful to remain faithful to object 
-- capability security model (due to inadequate support from the
-- module system), so the main B type provides access to general
-- IO. 
module Sirea.Internal.CC 
    ( CC(..)
    , Ref(..), writeRef', modifyRef, modifyRef'
    , Sched(..)
    }

import Sirea.Time (T)

-- | CC is a set of capabilities and similar features processed on
-- the forward compilation pass through a behavior. This includes
-- latencies (which also enables delay fusion optimizations) and
-- a set of local operations: scheduling, references.
--
-- Basically, these are the 'common computational capabilities' that
-- are considered ambient in an application.
data CC m = CC 
    { cc_sched     :: !(Sched m)
    , cc_newRef    :: !(forall a . a -> m (Ref m a))
    }

-- | A reference type modeled as a pair of read, write capabilities.
data Ref m a = Ref
    { readRef  :: !(m a)
    , writeRef :: !(a -> m ())
    }

-- | strict write.
writeRef' :: Ref m a -> a -> m ()
writeRef' rf a = a `seq` writeRef rf a

-- | modify a reference.
modifyRef :: Ref m a -> (a -> a) -> m ()
modifyRef rf fn = readRef rf >>= writeRef rf . fn

-- | strict modify reference
modifyRef' :: Ref m a -> (a -> a) -> m ()
modifyRef' rf fn = readRef rf >>= writeRef' rf . fn

-- | Sched is a useful set of capabilities for scheduling actions
-- for a thread or resource. 
--
--    stepTime   -- get time of current step; constant per step
--    onNextStep -- schedule an action to occur next step; trigger
--                  next step to run; runs in 'ln_touch' phase
--    onUpdPhase -- schedule action to occur on 'ln_update' phase
--    onStepEnd  -- schedule action to occur after update phase
--    eventually -- schedule action to occur in near future, but
--                  after some time has passed
--
-- `onNextStep` and `eventually` will be MT-safe (can be called from
-- helper threads or partitions). All others must be used from the
-- partition in which they apply. Eventual actions run in batches,
-- controlled by an external heartbeat; the timing is not precise,
-- but (subject to tuning and system health) runs at 10-20Hz. This
-- makes it useful for cleanup or time-insensitive periodic tasks.
--
-- Sirea currently runs three phases per step. The first phase will
-- run ln_touch operations to announce a future update, and possibly
-- an ln_cycle operation to detect and cut cycles. The second phase
-- propagates all ln_update operations, or ln_idle if the update is
-- just for stability. The final phase, onStepEnd, will propagate
-- batched updates to remote partitions, and maybe perform a little
-- cleanup or run any 'unsafeOnUpdate' actions.
--
data Sched m = Sched
    { stepTime   :: !(m T)
    , onNextStep :: !(m () -> m ())
    , onUpdPhase :: !(m () -> m ())
    , onStepEnd  :: !(m () -> m ())
    , eventually :: !(m () -> m ())
    } 


