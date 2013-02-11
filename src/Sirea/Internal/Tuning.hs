
-- A single module for all those configuration tuning variables used by Sirea.
module Sirea.Internal.Tuning
    ( dtRestart, dtStability, dtHeartbeat, dtGrace
    , dtInsigStabilityUp, dtFutureChoke
    , dtEqf, dtSeq
    , dtCompileFuture
    , batchesInFlight
    , dtDaggrHist, dtMdistHist
    , dtFinalize
    , dtPrintExpire  
    , tAncient  
    ) where

import Sirea.Time (T,DT,mkTime)

-- The main Sirea application has a few tuning parameters related to
-- periodic updates, heartbeats, graceful startup and shutdown. Also
-- a reset period - if the main thread seems frozen too long.
dtRestart, dtStability, dtHeartbeat, dtGrace :: DT
dtRestart   = 3.00   -- how long a pause to cause a restart
dtStability = 0.30   -- stability of main signal (affects halting time)
dtHeartbeat = 0.06   -- heartbeat and periodic increase of stability
dtGrace     = dtHeartbeat -- time allotted for graceful start and stop

-- At certain locations, where updates are delivered via partition
-- stepper, we have opportunity to block or delay some updates. This
-- can help regulate behavior in case of cyclic feedback systems. 
--
-- A more explicit choke may also be useful, in cases where we want
-- block updates that apply too far in the future.
dtInsigStabilityUp, dtFutureChoke :: DT
dtInsigStabilityUp = dtHeartbeat - 0.001 -- largest insignificant pure-stability update
dtFutureChoke      = 30 * dtHeartbeat    -- slow down updates if far beyond stability

-- There are several cases where we'll want to evaluate signals into
-- their near future. For `bseq` we simply want to flatten signals 
-- to control performance. For equality filters, we want to seek the
-- first difference between two signals so we can tweak update times
-- accordingly and avoid rework downstream.
dtEqf, dtSeq :: DT
dtEqf   = 40 * dtHeartbeat  -- when seeking first difference in a signal
dtSeq   = dtEqf / 10 -- when simply evaluating a signal ahead

-- For dynamic behaviors, it's best to install behaviors a little 
-- before they're necessary. Doing so can improve system latency and
-- support better speculative evaluation downstream. The tradeoff is
-- potentially much more rework when signals change.
dtCompileFuture :: DT
dtCompileFuture = dtFutureChoke -- how far to anticipate dynamic behaviors

-- Communication between partitions in Sirea occurs via bcross, and
-- uses coarse-grained batches to support snapshot consistency and
-- improved efficiency. The number of "batches in flight" is limited
-- to ensure fairness and simplify performance reasoning. (Note that
-- this value is per directed edge between partitions, not global.)
--
-- Tuning here is a tradeoff. A large number of batches may improve
-- parallelism, piggybacking, and CPU efficiency. However, it may
-- cost memory, latency, and increases drift between partitions.
-- A small number of batches will require more thread switching but
-- may result in tighter tolerances.
--
-- Bounded buffers between partitions are the only waits in Sirea.
-- Sirea is potentially wait-free if threads keep up with workloads.
-- This can also be understood as a basis for fair scheduling.
--
batchesInFlight :: Int
batchesInFlight = 6

-- For demand monitors, and other resources based on DemandAggr, we
-- want to keep a small amount of history available to support late
-- arriving demand sources (relative to wall clock). We'll also keep
-- some historical data to accommodate late arriving observers. The
-- demand sources aspect impacts stability, so is more limited. 
dtDaggrHist, dtMdistHist :: DT
dtDaggrHist = dtHeartbeat -- how long to tolerate late-arriving demands
dtMdistHist = dtHeartbeat -- how long to tolerate late-arriving observers

-- 
-- Some behaviors depend on stability values to know how much to 
-- compute. UnsafeOnUpdate is one example, and BDynamic has another.
-- When these behaviors reach their 'final' stability value, it can
-- be confusing to decide how much more to compute. Presumably, the
-- final value should be an inactive signal. But it might be active
-- for a few seconds before finalizing. In those cases, dtFinalize
-- is used to ensure any remaining values are processed.
dtFinalize :: DT
dtFinalize = 1000 * dtHeartbeat -- guarantee completion in all but absurd cases


-- For console printing, currently I use a simple expiration model
-- for old sentences. (I'd like to eventually develop a rigorous
-- model for console output, but this is sufficient for now and
-- roughly models a tuple space with expirations.)
--  (note: this won't be used in near future)
dtPrintExpire :: DT
dtPrintExpire = 100 * dtHeartbeat 

-- In some cases, I want to initialize structures with a lower bound
-- for Time. But I don't want to pay code and performance overheads 
-- for a symbolic representation of this lower bound. So I'll just
-- use tAncient to represent a long before-time.
--
-- I'll model the ancient sentinel time as a billion days ago. 
tAncient :: T
tAncient = mkTime (negate aBillionDays) 0 where
    aBillionDays = 1000 * 1000 * 1000


