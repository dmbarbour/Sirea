
-- A single module for all those configuration tuning variables used by Sirea.
module Sirea.Internal.Tuning
    ( dtRestart, dtStability, dtHeartbeat, dtGrace
    , dtFutureChoke
    , dtEqShift, dtAlign
    , dtTouch
    , dtCompile
    , batchesInFlight    
    , dtDaggrHist, dtMdistHist
    , dtPrintExpire  
    , tAncient  
    ) where

import Sirea.Time (T,DT,mkTime)

-- The main Sirea application has a few tuning parameters related to
-- periodic updates, heartbeats, graceful startup and shutdown. Also
-- a reset period - if the main thread seems frozen too long, we'll
-- model this in the activity signal.
dtRestart, dtStability, dtHeartbeat, dtGrace :: DT
dtRestart   = 2.00   -- how long a pause to force a restart
dtStability = 0.30   -- stability of main signal (affects halting time)
dtHeartbeat = 0.06   -- heartbeat and periodic increase of stability
dtGrace     = dtHeartbeat -- time allotted for graceful start and stop

-- A small update to stability is not always worth sending. It must
-- be sent within a partition (after ln_touch, to indicate there is
-- no update), but across partitions or steps we are free to drop a
-- few if we deem them insignificant for GC purposes. 
--
-- (Note: this is problematic for high-frequency cycles. E.g. cycle 
-- at 40ms might stop updating stability and get 'stuck'.)
--dtInsigStabilityUp :: DT
--dtInsigStabilityUp = 0.05 -- largest insignificant pure-stability update

-- To control temporal feedback cycles through resources, Sirea will
-- choke processing of updates that apply to values in the distant
-- future.
dtFutureChoke :: DT
dtFutureChoke = 3 * dtHeartbeat

-- TODO: develop a combined choke*eqshift that can support some sort
-- of exponential backoff. Not critical for now, but could save much
-- rework if done properly.

-- For badjeqf and bconst, how far do we peek to find a first point
-- of non-equivalence? If we find no difference, how much further do
-- we seek for a point of ideal alignment to 'swap in' the updated
-- signal?
dtEqShift, dtAlign :: DT
dtEqShift = 4 * dtHeartbeat -- comparison of values
dtAlign = 2 * dtHeartbeat -- extra search for alignment

-- When we 'btouch', how far (relative to stability) do we cause the
-- signal to be evaluated. Forcing evaluation is mostly useful to 
-- control where latencies are introduced. If dtTouch is larger than
-- zero, some rework may be performed but more values are available
-- in real-time. 
dtTouch :: DT
dtTouch = dtEqShift / 10

-- For dynamic behaviors, it's best to install behaviors a little 
-- before they're necessary. Doing so can improve system latency and
-- support better speculative evaluation downstream. The tradeoff is
-- potentially much more rework when signals change. I plan to make
-- this more adaptive, eventually.
dtCompile :: DT
dtCompile = 3 * dtHeartbeat -- how far to anticipate dynamic behaviors

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
-- Bounded buffers provide synchronization and fair scheduling for
-- Sirea partition threads. By themselves, they cannot deadlock. All
-- incoming batches are processed in each 'step' so each step breaks
-- potential deadlocks. However, developers must avoid use of other
-- synchronization mechanisms between threads.
--
-- Sirea's parallelism is designed to be self-regulating, i.e. the
-- relative efficiency increases when a partition falls behind and
-- has a lot of batches to process. But regulation from batching and
-- piggybacking is soft and subtle compared to bounded buffers.
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

-- For console printing, currently I use a simple expiration model
-- for old sentences. (I'd like to eventually develop a rigorous
-- model for console output, but this is sufficient for now and
-- roughly models a tuple space with expirations.)
--  (note: this won't be used in near future)
dtPrintExpire :: DT
dtPrintExpire = 6.0

-- In some cases, I want to initialize structures with a lower bound
-- for Time. But I don't want to pay code and performance overheads 
-- for a symbolic representation of this lower bound. So I'll just
-- use tAncient to represent a long before-time.
tAncient :: T
tAncient = mkTime (negate aBillionDays) 0 where
    aBillionDays = 1000 * 1000 * 1000


