{-# LANGUAGE GADTs, TypeOperators #-}

-- | Type B0 is the primitive behavior type in Sirea. It operates in
-- a hidden applicative monad of kind 'm'. Hiding the monad ensures 
-- effects are performed with secure capabilities.  
module Sirea.Internal.B0
    ( B0(..), MkB0(..)
    ) where

import Sirea.Internal.STypes (S,(:&:),(:|:))
import Sirea.Internal.LTypes 
import Sirea.Time (DT)
import Control.Exception (assert)

-- | MkB0 is the primary constructor for primitive Sirea behaviors.
data MkB0 m x y = MkB0
    { mkb_fwd :: LCaps m x -> LCaps m y
    , mkb_rev :: LCaps m x -> Lnk m y -> m (Lnk m x)
    }

-- | B0 m x y describes an RDP behavior that operates in an 
-- applicative monad m (which may be hidden), and processes
-- signals that are applied to it.

-- 
 - a signal transformer with
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
data B0 m x y where
  B0_mkLnk   :: MkB0 m x y -> B0 m x y
  B0_pipe    :: B0 m x y -> B0 m y z -> B0 m x z
  B0_first   :: B0 m x x' -> B0 m (x :&: y) (x' :&: y)
  B0_left    :: B0 m x x' -> B0 m (x :|: y) (x' :|: y)
  B0_latent  :: (LCaps m x -> B0 m x y) -> B0 m x y

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




