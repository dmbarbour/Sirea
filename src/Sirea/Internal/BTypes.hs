{-# LANGUAGE GADTs, TypeOperators #-}

-- Behavior types. 
module Sirea.Internal.BTypes
    ( B(..)

    -- support for time manipulations
    , TR, TS, LDT(..)
    , tr_unit, tr_fwd, tr_dead
    , LnkD(..)
    , lnd_fst -- :: LnkD d (x :&: y) -> LnkD d x
    , lnd_snd -- :: LnkD d (x :&: y) -> LnkD d y
    , lnd_left -- :: LnkD d (x :|: y) -> LnkD d x
    , lnd_right -- :: LnkD d (x :|: y) -> LnkD d y
    , lnd_sig -- :: LnkD d (S p x) -> d
    , lnd_fmap, lnd_aggr, lnd_zip
    , ldt_zero
    , ldt_maxGoal, ldt_minGoal
    , ldt_maxCurr, ldt_minCurr
    , ldt_anyLive, ldt_valid

    , latentOnTime, tshiftB
    ) where

import Sirea.Internal.STypes (S,(:&:),(:|:))
import Sirea.Internal.LTypes 
import Sirea.Time (DT)
import Control.Exception (assert)

-- | (B w x y) describes an RDP behavior - a signal transformer with
-- potential for declarative `demand effects`. Signal x is called
-- the demand, and y the response. Behaviors may be composed, so the
-- response from one behavior easily becomes demand on another.
--
-- Type `w` for the signal regards the scope of side-effects. 
--
-- A common effect on demand is to acquire resources, e.g. to power
-- up a sensor only while there is code interested in observing it.
-- But demands can also influence state, and thereby interact with
-- many other behaviors via shared state or stateful services.
--
-- RDP demand effects are constrained: 
--   * spatial idempotence - in any given instant, the same demand
--     twice, or a thousand times, has no additional effect. 
--   * spatial commutativity - the origin of a demand signal does
--     not affect its meaning; demands at any given instant must be
--     processed as an unordered set.
--   * duration coupling - the active periods of response y are
--     tightly coupled to the active periods of demand x. If x is
--     active 100 ms, inactive 300 ms, active 200 ms then so will
--     be y (albeit possibly delayed by a small constant).
--   * continuous & eventless - no instantaneous states or values, 
--     and conceptually infinite instants between times. Rather than
--     a button-press event, for example, developers will see the
--     button down state a few milliseconds then back in up-state.
--
-- These constraints make RDP very declarative. But developers must
-- learn new patterns, idioms, and state models.
--
-- Behaviors compose much like arrows (from Control.Arrow), but are
-- more constrained due to partitioning, asynchrony, and duration
-- coupling. 
data B w x y where
  -- most operations
  B_mkLnk   :: (TR x y) -> (MkLnk w x y) -> B w x y

  -- time modeling, logical delay; reports time shifts
  -- (does not cause time-shift; combine with B_latent for concrete delay) 
  B_tshift  :: (TS x) -> B w x x

  -- category
  B_pipe    :: (B w x y) -> (B w y z) -> B w x z

  -- arrows
  B_first   :: (B w x x') -> B w (x :&: y) (x' :&: y)
  B_left    :: (B w x x') -> B w (x :|: y) (x' :|: y)

  -- bmerge needed some extra info to perform critical
  -- dead code elimination. Basically, it needs compile
  -- data from the forward pass.
  B_latent  :: (LnkD LDT x -> B w x y) -> B w x y

  -- B_unique :: UniqueID -> (B w x y) -> B w x y

-- POSSIBILITY: Add the UniqueID automatically with unsafeLnkB 
-- (via unsafePerformIO) and for all the BImpl options. 
--
-- REASON: performance with dynamic behaviors? especially in case
-- where we switch between the same two or three dynamic behaviors.
-- Could leave the last few behaviors installed and cached.
--
-- It could work reasonably well in practice. But no pressing need
-- for it.

-- | delay computation of B x y until timing info is available
latentOnTime :: (LnkD LDT x -> B w x y) -> B w x y
latentOnTime fn = B_latent fn


-- | tshiftB combines B_tshift and B_latent to perform a concrete
-- delay if the reported time-shift indicates a delay should occur.
-- (based on ldt_curr; changes to ldt_goal don't cause computation)
tshiftB :: TS x -> B w x x
tshiftB fn = B_latent forceDelay `B_pipe` B_tshift fn
    where forceDelay t0 = applyDelayB t0 (fn t0)

applyDelayB :: LnkD LDT x -> LnkD LDT x -> B w x x
applyDelayB t0 tf = B_mkLnk id lnk
    where build = buildTshift t0 tf
          lnk = MkLnk { ln_build = return . build 
                      , ln_tsen = False, ln_peek = 0 }

-- buildTshift will apply delays based on before/after LDT values
--  asserts latency is non-decreasing
--  no post-compile cost for links that aren't delayed
buildTshift :: LnkD LDT x -> LnkD LDT x -> Lnk x -> Lnk x
buildTshift _ _ LnkDead = LnkDead
buildTshift t0 tf (LnkProd x y) =
    let opx = buildTshift (lnd_fst t0) (lnd_fst tf) x in
    let opy = buildTshift (lnd_snd t0) (lnd_snd tf) y in
    LnkProd opx opy
buildTshift t0 tf (LnkSum x y) =
    let opx = buildTshift (lnd_left  t0) (lnd_left  tf) x in
    let opy = buildTshift (lnd_right t0) (lnd_right tf) y in
    LnkSum opx opy
buildTshift t0 tf (LnkSig lu) = 
    let dt0 = lnd_sig t0 in
    let dtf = lnd_sig tf in
    let dtDiff = (ldt_curr dtf) - (ldt_curr dt0) in
    assert (dtDiff >= 0) $
    if (0 == dtDiff) then LnkSig lu else
    LnkSig (ln_sumap (su_delay dtDiff) lu)



---------------------------------------------------------
-- A simple model for time-shifts. We have a current delay and a
-- goal delay. A time-shift can move either or both. Differences
-- in current delay can be turned into a final behavior - i.e. 
-- we compare ldt_curr before and after, and delay accordingly. 
--
-- TR does not cause time-shifts, but controls how timing info is
-- preserved across MkLnk behaviors. Only a few special behaviors,
-- such as bdisjoin, need other than tr_unit, id, or tr_fwd.
type TR x y = LnkD LDT x -> LnkD LDT y
type TS x = TR x x
data LDT = LDT 
    { ldt_curr :: !DT   -- actual delay from start of behavior
    , ldt_goal :: !DT   -- aggregated but unapplied logical delay
    , ldt_live :: !Bool -- is this a live branch (not binl or binr?)
    }

-- ldt_zero: an LDT suitable for starting a compilation.
ldt_zero :: LDT
ldt_zero = LDT 
    { ldt_curr = 0
    , ldt_goal = 0
    , ldt_live = True 
    }

ldt_maxGoal, ldt_minGoal, ldt_maxCurr, ldt_minCurr :: LnkD LDT x -> DT
ldt_maxGoal = lnd_aggr max . lnd_fmap ldt_goal
ldt_minGoal = lnd_aggr min . lnd_fmap ldt_goal
ldt_maxCurr = lnd_aggr max . lnd_fmap ldt_curr
ldt_minCurr = lnd_aggr min . lnd_fmap ldt_curr

-- ldt_anyLive returns true if ANY inputs are alive.
ldt_anyLive :: LnkD LDT x -> Bool
ldt_anyLive = lnd_aggr (||) . lnd_fmap ldt_live

-- simple validation on LnkD LDT structure
-- not very efficient, but okay for assert
ldt_valid :: LnkD LDT x -> Bool
ldt_valid (LnkDUnit ldt) = (ldt_goal ldt >= ldt_curr ldt)
ldt_valid (LnkDSum x y) = ldt_valid x && ldt_valid y
ldt_valid (LnkDProd x y) = ldt_valid x && ldt_valid y && 
    (ldt_anyLive x == ldt_anyLive y)

-- tr_unit validates an assumption that all inputs have uniform
-- timing properties. It is the normal timing translator for MkLnk.
tr_unit :: TR x y
tr_unit x =
    assert (ldt_valid x) $
    assert (ldt_maxGoal x == ldt_minGoal x) $
    assert (ldt_maxCurr x == ldt_minCurr x) $
    LnkDUnit $ LDT 
        { ldt_curr = ldt_maxCurr x
        , ldt_goal = ldt_maxGoal x
        , ldt_live = ldt_anyLive x
        }

tr_fwd :: TR (S p1 x1) (S p2 x2)
tr_fwd = LnkDUnit . lnd_sig

tr_dead :: LnkD LDT x -> LnkD LDT y
tr_dead x = LnkDUnit ldtDead
    where ldtDead = LDT { ldt_curr = ldt_maxCurr x
                        , ldt_goal = ldt_maxGoal x
                        , ldt_live = False 
                        }


----------------------------------------------------------
-- LnkD is a more generic version of LnkW for metadata.
-- It allows a single value to represent a group. Usefully
-- it can be propagated even if the type is unknown.
data LnkD d x where
    LnkDUnit :: d -> LnkD d x
    LnkDProd :: (LnkD d x) -> (LnkD d y) -> LnkD d (x :&: y)
    LnkDSum  :: (LnkD d x) -> (LnkD d y) -> LnkD d (x :|: y)

lnd_fst :: LnkD d (x :&: y) -> LnkD d x
lnd_snd :: LnkD d (x :&: y) -> LnkD d y
lnd_left :: LnkD d (x :|: y) -> LnkD d x
lnd_right :: LnkD d (x :|: y) -> LnkD d y
lnd_sig  :: LnkD d (S p x) -> d

lnd_fst (LnkDUnit d) = LnkDUnit d
lnd_fst (LnkDProd x _) = x
lnd_snd (LnkDUnit d) = LnkDUnit d
lnd_snd (LnkDProd _ y) = y
lnd_left (LnkDUnit d) = LnkDUnit d
lnd_left (LnkDSum x _) = x
lnd_right (LnkDUnit d) = LnkDUnit d
lnd_right (LnkDSum _ y) = y 
lnd_sig (LnkDUnit d) = d

-- map a simple operation across all elements independently. 
lnd_fmap :: (a -> b) -> LnkD a x -> LnkD b x
lnd_fmap fn (LnkDUnit d) = LnkDUnit (fn d)
lnd_fmap fn (LnkDProd l r) = LnkDProd (lnd_fmap fn l) (lnd_fmap fn r)
lnd_fmap fn (LnkDSum l r) = LnkDSum (lnd_fmap fn l) (lnd_fmap fn r)

-- aggregate a value. Note that the aggregation function should be
-- idempotent and commutative. There aren't many applicable 
-- functions that are meaningful, except min and max.
lnd_aggr :: (b -> b -> b) -> LnkD b x -> b
lnd_aggr _ (LnkDUnit b) = b
lnd_aggr fn (LnkDProd l r) = fn (lnd_aggr fn l) (lnd_aggr fn r)
lnd_aggr fn (LnkDSum l r) = fn (lnd_aggr fn l) (lnd_aggr fn r)

-- apply a function pairwise between matching elements in structure.
lnd_zip :: (a -> b -> c) -> LnkD a x -> LnkD b x -> LnkD c x
lnd_zip fn (LnkDUnit a) (LnkDUnit b) = 
    LnkDUnit (fn a b)
lnd_zip fn (LnkDSum al ar) blr = 
    let cl = lnd_zip fn al (lnd_left blr) in
    let cr = lnd_zip fn ar (lnd_right blr) in
    LnkDSum cl cr
lnd_zip fn alr (LnkDSum bl br) =
    let cl = lnd_zip fn (lnd_left alr) bl in
    let cr = lnd_zip fn (lnd_right alr) br in
    LnkDSum cl cr
lnd_zip fn (LnkDProd a1 a2) b12 =
    let c1 = lnd_zip fn a1 (lnd_fst b12) in
    let c2 = lnd_zip fn a2 (lnd_snd b12) in
    LnkDProd c1 c2
lnd_zip fn a12 (LnkDProd b1 b2) =
    let c1 = lnd_zip fn (lnd_fst a12) b1 in
    let c2 = lnd_zip fn (lnd_snd a12) b2 in
    LnkDProd c1 c2




