{-# LANGUAGE Rank2Types #-}

-- | Whereas PCX provides access to arbitrary resources, a common
-- requirement for basic computation is for thread-local scheduling.
-- This is necessary for dynamic behaviors and other operations. 
-- So PSched is threaded through all B type behaviors.
--
-- This module just defines the capabilities.
-- 
-- See also: Sirea.Partition (for getPSched)
module Sirea.PSched 
    ( PSched(..)
    ) where

import Sirea.Time (T)

-- | PSched is a useful set of capabilities for scheduling actions
-- in a partition thread. 
--
--    p_stepTime   -- get time of current step; constant per step
--    p_onNextStep -- schedule an action to occur next step; trigger
--                    next step to run; runs in 'ln_touch' phase
--    p_onUpdPhase -- schedule action to occur on 'ln_update' phase
--    p_onStepEnd  -- schedule action to occur after update phase
--    p_eventually -- schedule action to occur in near future
--
-- `p_onNextStep` and `p_eventually` are MT-safe (can be called from
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
data PSched = PSched
    { p_stepTime   :: !(IO T)
    , p_onNextStep :: !(IO () -> IO ())
    , p_onUpdPhase :: !(IO () -> IO ())
    , p_onStepEnd  :: !(IO () -> IO ())
    , p_eventually :: !(IO () -> IO ())
    } 

