
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
module Sirea.B (B) where

import Sirea.Time
import Sirea.Behavior
import Sirea.Partition
import Sirea.Internal.BTypes
import Sirea.Internal.BImpl

---------------------------
-- Concrete Instances: B --
---------------------------
-- TUNING
--   dtScanAheadB: default lookahead for constB, adjeqfB.
--   dtTouchB: compute ahead of stability for btouch.
-- Eventually I'd like to make these values adaptive, i.e. depending
-- on actual lookahead stability at runtime.
dtScanAheadB, dtTouchB :: DT
dtScanAheadB = 1.0 -- seconds ahead of stability
dtTouchB = 0 -- seconds ahead of stability

eqfB :: (x -> x -> Bool) -> B w (S p x) (S p x)
eqfB = unsafeEqShiftB dtScanAheadB

instance BFmap (B w) where 
    bfmap    = fmapB
    bconst c = constB c >>> eqfB (const $ const True)
    bstrat   = stratB 
    btouch   = touchB dtTouchB
    badjeqf  = adjeqfB >>> eqfB (==)
instance BProd (B w) where
    bfirst   = firstB
    bdup     = dupB
    bfst     = fstB
    bswap    = swapB
    bassoclp = assoclpB
instance BSum (B w) where
    bleft    = leftB
    bmirror  = mirrorB
    bmerge   = mergeB
    binl     = inlB
    bassocls = assoclsB
instance BZip (B w) where
    bzap     = zapB
instance BSplit (B w) where
    bsplit   = splitB
instance BDisjoin (B w) where 
    bdisjoin = disjoinB
instance BTemporal (B w) where
    bdelay   = delayB
    bsynch   = synchB
instance BPeek (B w) where
    bpeek    = peekB
instance Behavior (B w)

{-
instance BDynamic (B w) where
    beval    = evalB
    bexec    = execB
-}

-- note: B does not support `bcross`, since B cannot 
-- track which partitions are in use. Need BCX for
-- bcross.

instance BScope (B w) where
    bpushScope = unsafeChangeScopeB
    bpopScope  = unsafeChangeScopeB







