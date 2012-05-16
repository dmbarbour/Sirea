
-- | LinkUnsafeIO is a simple behavior that executes IO from a Link
-- as an RDP behavior stabilizes. The IO action will be executed for
-- every unique value observed in the signal, and executes once 
-- there are no more takebacks. The motivation for LinkUnsafeIO was 
-- easy support for debugging, HUnit tests - stuff that is intended 
-- for easy removal. The signal itself is passed along untouched 
-- after performing the IO.
--
-- In terms of RDP semantics, LinkUnsafeIO is unsafe because IO is
-- not spatially idempotent or commutative. The IO actions might be
-- duplicated if developers take advantage of RDP's semantics for
-- refactoring and abstraction. (The *proper* way to integrate IO is
-- to leverage an external service, perhaps a blackboard metaphor to
-- describe actions to be performed.)
--
-- In practice, developers can generally ensure that the unsafe IO 
-- won't be duplicated (uniqueness), or that it would be safe enough
-- to do so (idempotence), and that collectively such operations are 
-- insensitive to order (commutative, or close enough). So this can 
-- be used safely, with careful discipline. 
--
-- The IO is not used to load input back into the behavior; doing so
-- would interfere with anticipation. Also, the IO should be locally
-- stateless. 
--
module Sirea.LinkUnsafeIO 
    ( unsafeOnUpdateB
    , unsafeOnUpdateBLazy
    ) where

import Sirea.Behavior
import Sirea.Link

-- | unsafeOnUpdateB - perform an IO action for every unique value
-- in a signal as it becomes stable, then forward the update (after
-- performing the IO). Only the initial `Nothing` value is skipped.
-- In some cases this may cause multiple updates in sequence if the
-- stability of the signal increases by a large steps.
-- 
-- unsafeOnUpdateB qualifies as an effectful sink, i.e. it will keep
-- a behavior alive. This will interfere with dead code elimination,
-- which may be unsuitable for debugging purposes.
unsafeOnUpdateB :: (Eq a) => (T -> Maybe a -> IO ()) -> B (S p a) (S p a)
unsafeOnUpdateB onUpdate = unsafeLnkB lnAction
    where lnAction = MkLnk { ln_build = buildAction
                           , ln_tsen = True
                           , ln_peek = 0
                           }

-- | unsafeOnUpdateBLazy - same as unsafeOnUpdateB, except that this
-- qualifies as "dead code" if certain other signals seem to also be
-- dead. This makes the lazy variation more suitable for debugging 
-- since it won't interfere with dead code elimination.
--
-- Often a signal would be unused past parameterizing an effect, so
-- the lazy variation forwards an arbitrary `x` signal. Any part of 
-- the full (S p a :&: x) output may be alive to activate IO. The
-- `x` signal itself is passed along unaltered (unsynchronized).
unsafeOnUpdateBLazy :: (Eq a) => (T -> Maybe a -> IO ()) 
                    -> B (S p a :&: x) (S p a :&: x)
unsafeOnUpdateBLazy onUpdate = (lazyActionB onUpdate) &&& bsnd


unsafeLnkB lnAction


unsafeOnSigUpB :: (SigUp a -> IO ()) -> B (S p a) (S p a)
unsafeOnSigUpB onUpdate = bvoid $ unsafeLnkB lnAssert
    where lnAssert = MkLnk { ln_build = return . buildAssert
                           , ln_tsen = True
                           , ln_peek = 0 
                           }
          buildAssert LnkDead = LnkSig luAssert
          buildAssert _ = error "should be void"
          luAssert = LnkUp { ln_touch = return ()
                           , ln_update = onUpdate
                           } 

unsafeOnTouchB


-- | keepAliveB will keep the first element alive so long as other
-- parts of the signal are alive. This would only be useful for 
-- performance debugging, and should probably be performed just
-- after `bfirst btouch`. 
keepAliveB  :: B (S p x :&: y) (S p x :&: y)
keepAliveB  = mkLnkB id $ mkLnkPure lnkMatchLiveness
    where lnkMatchLiveness xy =
            if (ln_dead xy) then LnkDead else   
            let x = (LnkSig . ln_lnkup . ln_fst) xy in
            let y = ln_snd xy in
            LnkProd x y


