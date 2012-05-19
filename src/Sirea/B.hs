
-- | `B w x y` is a raw, primitive behavior type in Sirea. 
--
-- While flexible,
-- hooking `B` behaviors to resources is inconvenient. Non-trivial 
-- RDP behaviors requires a shared namespace of resources, and using
-- global Haskell state should be discouraged.
--
-- Use of `BCX` instead of `B` allows the behavior itself to thread
-- a resource context through a behavior, and enables behaviors that
-- require context (such as bcross from Sirea.Partition).
--
-- See Also:
--   Sirea.Link for `unsafeLnkB` - new behavior primitives.
--
module Sirea.B (B) where

import Sirea.Time
import Sirea.Behavior
import Sirea.Internal.BTypes
import Sirea.Internal.BImpl

---------------------------
-- Concrete Instances: B --
---------------------------
-- TUNING
--   dtScanAheadB: default lookahead for constB, adjeqfB.
--   dtTouchB: compute ahead of stability for btouch.
dtScanAheadB, dtTouchB :: DT
dtScanAheadB = 2.0 -- seconds ahead of stability
dtTouchB = 0.1 -- seconds ahead of stability

eqfB :: (x -> x -> Bool) -> B w (S p x) (S p x)
eqfB eq = unsafeEqShiftB dtScanAheadB eq

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



