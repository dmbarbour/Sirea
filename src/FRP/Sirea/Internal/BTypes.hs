{-# LANGUAGE GADTs, TypeOperators, EmptyDataDecls #-}

-- Behavior types. 
module FRP.Sirea.Internal.BTypes
    ( B(..)

    -- support for time manipulations
    , TR, TS, LDT(..)
    , tr_unit, tr_fwd
    , LnkD(..)
    , lnd_fst -- :: LnkD d (x :&: y) -> LnkD d x
    , lnd_snd -- :: LnkD d (x :&: y) -> LnkD d y
    , lnd_left -- :: LnkD d (x :|: y) -> LnkD d x
    , lnd_right -- :: LnkD d (x :|: y) -> LnkD d y
    , lnd_sig -- :: LnkD d (S p x) -> d
    , lnd_fmap
    , lnd_aggr
    , ldt_maxGoal, ldt_minGoal
    , ldt_maxCurr, ldt_minCurr
    ) where

import FRP.Sirea.Internal.STypes
import FRP.Sirea.Internal.LTypes (MkLnk)
import FRP.Sirea.Time (DT)
import Control.Exception (assert)
import Data.Function (on)

-- | (B x y) describes an RDP behavior - a signal transformer with
-- potential for declarative `demand effects`. Signal x is called
-- the demand, and y the response. Behaviors may be composed, so the
-- response from one behavior easily becomes demand on another.
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
--     be y (albeit, possibly delayed a little).
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
data B x y where
  -- most signal operations
  B_mkLnk   :: !(TR x y) -> !(MkLnk x y) -> B x y

  -- uniquified behaviors might eventually support some
  -- duplicate elimination optimizations. For now, though,
  -- I don't really have these.
  -- B_unique :: !UniqueID -> !(B x y) -> B x y

  -- time modeling, logical delay, concrete delay
  B_tshift  :: !(TS x) -> B x x

  -- category
  B_fwd     :: B x x
  B_pipe    :: !(B x y) -> !(B y z) -> B x z

  -- data plumbing (products)
  B_fst     :: B (x :&: y) x
  B_on_fst  :: !(B x x') -> B (x :&: y) (x' :&: y)
  B_swap    :: B (x :&: y) (y :&: x)
  B_dup     :: B x (x :&: x)
  B_asso_p  :: B (x :&: (y :&: z)) ((x :&: y) :&: z)
  
  -- data plumbing (choices)
  B_in_lft  :: B x (x :|: y)
  B_on_lft  :: !(B x x') -> B (x :|: y) (x' :|: y)
  B_mirror  :: B (x :|: y) (y :|: x)
  B_merge   :: B (x :|: x) x
  B_asso_s  :: B (x :|: (y :|: z)) ((x :|: y) :|: z) 


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
    { ldt_curr :: !DT -- actual delay from start of behavior
    , ldt_goal :: !DT -- aggregated but unapplied logical delay
    }

ldt_maxGoal, ldt_minGoal, ldt_maxCurr, ldt_minCurr :: LnkD LDT x -> DT
ldt_maxGoal = lnd_aggr max . lnd_fmap ldt_goal
ldt_minGoal = lnd_aggr min . lnd_fmap ldt_goal
ldt_maxCurr = lnd_aggr max . lnd_fmap ldt_curr
ldt_minCurr = lnd_aggr min . lnd_fmap ldt_curr

-- tr_unit validates an assumption that all inputs have uniform
-- timing properties. It is the normal timing translator for MkLnk.
tr_unit :: TR x y
tr_unit x =
    assert (ldt_maxGoal x == ldt_minGoal x) $
    assert (ldt_maxCurr x == ldt_minCurr x) $
    LnkDUnit $ LDT 
        { ldt_curr = ldt_maxCurr x
        , ldt_goal = ldt_maxGoal x
        }
tr_fwd :: TR (S p1 x1) (S p2 x2)
tr_fwd = LnkDUnit . lnd_sig

----------------------------------------------------------
-- LnkD is a more generic version of LnkW for metadata.
-- It allows a single value to represent a group. Usefully
-- it can be propagated even if the type is unknown.
data LnkD d x where
    LnkDUnit :: !d -> LnkD d x
    LnkDProd :: !(LnkD d x) -> !(LnkD d y) -> LnkD d (x :&: y)
    LnkDSum  :: !(LnkD d x) -> !(LnkD d y) -> LnkD d (x :|: y)

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




