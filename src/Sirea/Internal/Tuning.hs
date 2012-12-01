
-- A single module for all those configuration tuning variables used by Sirea.
module Sirea.Internal.Tuning
    ( dtRestart, dtStability, dtHeartbeat, dtGrace
    , dtInsigStabilityUp, dtFutureChoke
    , dtEqf, dtSeq
    , dtCompileFuture
    , dtDaggrHist, dtMdistHist
    , dtFinalize
    ) where

-- The main Sirea application has a few tuning parameters related to
-- periodic updates, heartbeats, graceful startup and shutdown. Also
-- a reset period - if the main thread seems frozen too long.
dtRestart, dtStability, dtStep, dtGrace :: DT
dtRestart   = 1.20   -- how long a pause to cause a restart
dtStability = 0.30   -- stability of main signal (affects halting time)
dtHeartbeat = 0.06   -- heartbeat and periodic increase of stability
dtGrace     = dtHeartbeat -- time allotted for graceful start and stop


-- At certain locations, where updates are delivered via partition
-- stepper, we have opportunity to block or delay some updates. This
-- can help regulate behavior in case of cyclic feedback systems. 
dtInsigStabilityUp, dtFutureChoke :: DT
dtInsigStabilityUp = 0.05 -- smallest significant pure-stability update
dtFutureChoke = dtEqf - 0.001 -- try to choke updates if far beyond stability

-- There are several cases where we'll want to evaluate signals into
-- their near future. For `bseq` we simply want to flatten signals 
-- to control performance. For equality filters, we want to seek the
-- first difference between two signals so we can tweak update times
-- accordingly and avoid rework downstream.
dtEqf, dtSeq :: DT
dtEqf   = 3.6  -- when seeking first difference in a signal
dtSeq   = 0.36 -- when simply evaluating a signal ahead

-- For dynamic behaviors, it's best to install behaviors a little 
-- before they're necessary. Doing so can improve system latency and
-- support better speculative evaluation downstream. The tradeoff is
-- potentially more rework when signals change.
dtCompileFuture :: DT
dtCompileFuture = 2.7 -- how far to anticipate dynamic behaviors

-- For demand monitors, and other resources based on DemandAggr, we
-- want to keep a small amount of history available to support late
-- arriving demand sources (relative to wall clock). We'll also keep
-- some historical data to accommodate late arriving observers. The
-- demand sources aspect impacts stability, so is more limited. 
dtDaggrHist, dtMdistHist :: DT
dtDaggrHist = 0.036 -- how long to tolerate late-arriving demands
dtMdistHist = 0.144 -- how long to tolerate late-arriving observers

-- UnsafeOnUpdate behaviors will execute IO actions as they become
-- stable. But eventually they'll receive a final stability update.
-- At that point, the signal should become Nothing, but that might
-- be at some time well in the future of the current stability. This
-- tunes how far to search for that Nothing before aborting. 
dtFinalize :: DT
dtFinalize = 36.0 -- pretty much guarantees completion


-- For console printing, currently I use a simple expiration model
-- for old sentences. (I'd like to eventually develop a rigorous
-- model for console output, but this is sufficient for now and
-- roughly models a tuple space with expirations.)
dtPrintExpire :: DT
dtPrintExpire = 6.0 



