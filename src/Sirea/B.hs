
{-# LANGUAGE MultiParamTypeClasses #-}

-- | `B w x y` is the raw, primitive behavior type in Sirea. 
--
-- `B` assumes that necessary hooks (into environment and resources)
-- are already formed. However, it is inconvenient to achieve those
-- hooks by hand. To model environment and convenient type-driven
-- relationships (and multi-threading with bcross), use BCX.
--
-- See Also:
--   Sirea.Link for `unsafeLnkB` - new behavior primitives.
--   Sirea.BCX for behavior with resource context
module Sirea.B 
    ( B
    ) where

import Sirea.Time
import Sirea.Behavior
-- import Sirea.Partition
import Sirea.Internal.BTypes
import Sirea.Internal.BImpl
import Sirea.Internal.BDynamic
import Data.Typeable

instance Typeable2 B where
    typeOf2 _ = mkTyConApp tcB []
        where tcB = mkTyCon3 "sirea-core" "Sirea.Behavior" "B"

---------------------------
-- Concrete Instances: B --
---------------------------
-- TUNING
--   dtEqf: default lookahead for constB, adjeqfB.
--   dtSeq: compute ahead of stability for bseq.
--
-- Eventually I'd like to make these values adaptive, i.e. depending
-- on actual lookahead stability at runtime. The TCP-like algorithms
-- for congestion control seem applicable. 
--
-- For badjeqf/bconst, it may also be valuable to choke updates if
-- they do not appear to have changed. I.e. switch to heartbeat
-- updates if there is no observed change.
dtEqf, dtSeq :: DT
dtEqf   = 3.6  -- seconds ahead of stability to seek difference
dtSeq   = 0.36 -- seconds ahead of stability to force evaluation

eqfB :: (x -> x -> Bool) -> B (S p x) (S p x)
eqfB = unsafeEqShiftB dtEqf

instance BFmap B where 
    bfmap    = fmapB
    bconst c = constB c >>> eqfB ((const . const) True)
    bstrat   = stratB 
    bseq     = seqB dtSeq
    badjeqf  = adjeqfB >>> eqfB (==)
instance BProd B where
    bfirst   = firstB
    bdup     = dupB
    b1i      = s1iB
    b1e      = s1eB
    btrivial = trivialB
    bswap    = swapB
    bassoclp = assoclpB
instance BSum B where
    bleft    = leftB
    bmirror  = mirrorB
    bmerge   = mergeB
    b0i      = s0iB
    b0e      = s0eB
    bvacuous = vacuousB
    bassocls = assoclsB
instance BZip B where
    bzap     = zapB
instance BSplit B where
    bsplit   = splitB
instance BDisjoin B where 
    bdisjoin = disjoinB
instance BTemporal B where
    bdelay   = delayB
    bsynch   = synchB
instance BPeek B where
    bpeek    = peekB
instance Behavior B 

instance BDynamic B B where
    beval dt = evalB dt >>> bright bfst


-- TODO: Consider a behavior that slows the heartbeat.
--   Or maybe this could be done at `badjeqf` when we
--   realize a signal is constant for a while?

-- note: B does not support `bcross`, since B cannot 
-- track which partitions are in use. Need BCX for
-- bcross.
{-
instance BScope (B w) where
    bpushScope = unsafeChangeScopeB
    bpopScope  = unsafeChangeScopeB
-}






