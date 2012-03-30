
{-# LANGUAGE TypeOperators, EmptyDataDecls, GADTs, TypeFamilies #-}

module FRP.Sirea.Behavior  
  ( (:&:), (:|:), B, S
  , (>>>) -- from Control.Category
  , bfwd, bdelay, bsynch
  , bfirst, bsecond, (***), bswap, bcopy, bfst, bsnd, bassoclp, bassocrp
  , bleft, bright, (+++), bmirror, bmerge, binl, binr, bassocls, bassocrs
  , bconjoinl, bconjoinr
  , bvoid
  , bfmap, bzip, bzipWith, bsplit
  , bUnsafeLnk
  , MkLnk(..), Lnk(..), SigUp(..)
  , ln_fmap, su_fmap, su_delay
  ) where

import Prelude hiding (id,(.))
import Control.Category

import Data.IORef
import FRP.Sirea.Signal
import FRP.Sirea.Time
import FRP.Sirea.MiscTypes


infixr 3 ***
--infixr 3 &&&
infixr 2 +++
--infixr 2 |||

-- | (x :&: y). Product of asynchronous or partitioned signals, but
-- x and y will have equal and tightly coupled active periods. For
-- example, if x is active for 300ms, inactive 100ms, then active
-- 600ms, then y will have the same profile. However, asynchronous
-- delays enable a small divergence of exactly when these periods
-- occur. (They'll be synchronized before recombining signals.)
data (:&:) x y

-- | (x :|: y). Union or Sum of asynchronous or partitioned signals.
-- Signals are active for different durations, i.e. if x is active
-- 100 ms, inactive 400 ms, then active 100 ms: then y is inactive
-- 100 ms, active up to 400 ms, then inactive 100 ms. (There may be
-- durations where both are inactive.) Due to asynchronous delays 
-- the active periods might overlap for statically known periods.
data (:|:) x y

-- | (S p a) is a Sig in a blanket - Sig a in partition p. 
--
-- See FRP.Sirea.Signal for a description of signals. RDP developers
-- do not work directly with signals, but rather with behaviors that
-- transform signals. However, a Sirea developer might interact with
-- signals by the `bUnsafeLnk` behavior for FFI and legacy adapters.
--
-- Partitions represent the spatial distribution of signals, across
-- threads, processes, or heterogeneous systems. Developers can keep
-- certain functionality to certain partitions (or classes thereof).
-- Communication between partitions requires explicit behavior, such
-- as `bcross`.
--
-- Partitions should be Data.Typeable to support analysis of types
-- as values. Some types may have special meaning, indicating that
-- extra threads should be constructed when behavior is initiated.
data S p a

-- | PtHask is a simple partition class that indicates the partition
-- is local to the Haskell process and supports the full gamut of 
-- Haskell types and functions.
class PtHask p
instance PtHask ()


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
--     twice has no additional effect
--   * spatial commutativity - the origin of a demand signal does
--     not affect its meaning; demands are unordered
--   * duration coupling - the active periods of response y are
--     tightly coupled to the active periods of demand x. 
--   * continuous & eventless - no instantaneous states or values, 
--     and conceptually infinite instants between times.
--
-- These constraints make RDP very declarative. But developers must
-- learn new patterns, idioms, and state models.
--
-- Behaviors compose much like arrows (from Control.Arrow), but are
-- more constrained due to partitioning, asynchrony, and duration
-- coupling. 
--
data B x y where
  B_lnk     :: !(MkLnk x y) -> B x y

  B_fwd     :: B x x
  B_seq     :: !(B x y) -> !(B y z) -> B x z
  B_delay   :: !DT -> B x x
  B_synch   :: B x x

  B_fst     :: B (x :&: y) x
  B_on_fst  :: !(B x x') -> B (x :&: y) (x' :&: y)
  B_swap    :: B (x :&: y) (y :&: x)
  B_copy    :: B x (x :&: x)
  B_asso_p  :: B (x :&: (y :&: z)) ((x :&: y) :&: z)
  
  B_lft     :: B x (x :|: y)
  B_on_lft  :: !(B x x') -> B (x :|: y) (x' :|: y)
  B_mirror  :: B (x :|: y) (y :|: x)
  B_merge   :: B (x :|: x) x
  B_asso_s  :: B (x :|: (y :|: z)) ((x :|: y) :|: z) 
-- how am I to distribute commands and hooks?
--   Either GADT or Type Family + Typeclasses


instance Category B where
  id  = B_fwd
  (.) = flip B_seq

-- | RDP behaviors are arrows, but incompatible with Control.Arrow
-- due to `arr` being more powerful than RDP allows. A number of
-- behaviors support arrow composition, and several serve as basic
-- data plumbing that `arr` would perform in Control.Arrow. These
-- are also incompatible with Megacz's Generalized Arrows because no 
-- generic unit signal exists (cannot just create signals).
--
-- In Sirea, most data plumbing is essentially free at runtime. But
-- there are a few exceptions - bzip, bmerge, bconjoin, bsplit have
-- runtime overheads.
--
--  TRIVIAL
--   bfwd - identity behavior
--   (>>>) - forward composition (from Control.Category)
--
--  PRODUCTS
--   bfirst b - apply b on first element in product
--   bsecond b - apply b on second element in product
--   (b1 *** b2) = bfirst b1 >>> bsecond b2
--   bswap - flip first and second signals
--   bcopy - duplicate signal
--   bfst - keep first signal, drop second
--   bsnd - keep second signal, drop first
--   bassoclp - associate left on product
--   bassocrp - associate right on product
--   bzipWith, bzip - combine two concrete signals
-- 
--  SUM (CHOICE)
--   bleft b - apply b on left option in sum
--   bright b - apply b on right option in sum
--   (bl +++ br) - bleft bl >>> bright br
--   bmirror - flip left and right signals
--   bmerge - combine two choices into one signal (implicit synch)
--   binl - static choice of left option (~ if true) 
--   binr - static choice or right option (~ if false)
--   bassocls - associate left on sum
--   bassocrp - associate right on sum
--   bsplit - lift a decision in a signal to asynchronous layer
--
--  MISCELLANEOUS
--   bconjoin - partial merge on a product of sums
--   bvoid - execute a behavior for side-effects
--
--  SPATIAL or TEMPORAL
--   bdelay - delay signals (multi-part signals delayed equally)
--   bsynch - synch an asynchronous signal (to slowest)
--   bcross - communicate a signal between partitions
--
-- DATA OPERATIONS
--   bfmap - apply an arbitrary Haskell function to a signal 
--
bfwd     :: B x x

bfirst   :: B x x' -> B (x :&: y) (x' :&: y)
bsecond  :: B y y' -> B (x :&: y) (x :&: y')
(***)    :: B x x' -> B y y' -> B (x :&: y) (x' :&: y')
bswap    :: B (x :&: y) (y :&: x)
bcopy    :: B x (x :&: x)
bfst     :: B (x :&: y) x
bsnd     :: B (x :&: y) y
bassoclp :: B (x :&: (y :&: z)) ((x :&: y) :&: z)
bassocrp :: B ((x :&: y) :&: z) (x :&: (y :&: z))

bleft    :: B x x' -> B (x :|: y) (x' :|: y)
bright   :: B y y' -> B (x :|: y) (x :|: y')
(+++)    :: B x x' -> B y y' -> B (x :|: y) (x' :|: y')
bmirror  :: B (x :|: y) (y :|: x)
bmerge   :: B (x :|: x) x
binl     :: B x (x :|: y)
binr     :: B y (x :|: y)
bassocls :: B (x :|: (y :|: z)) ((x :|: y) :|: z)
bassocrs :: B ((x :|: y) :|: z) (x :|: (y :|: z))

bfwd = B_fwd

bfirst = B_on_fst
bsecond f = bswap >>> bfirst f >>> bswap 
(***) f g = bfirst f >>> bsecond g
bswap = B_swap
bcopy = B_copy
bfst  = B_fst
bsnd  = bswap >>> bfst
bassoclp = B_asso_p
bassocrp = bswap3 >>> bassoclp >>> bswap3

bleft = B_on_lft
bright f = bmirror >>> bleft f >>> bmirror
(+++) f g = bleft f >>> bright g
bmirror = B_mirror
bmerge = B_merge
binl = B_lft
binr = binl >>> bmirror
bassocls = B_asso_s
bassocrs = bmirr3 >>> bassocls >>> bmirr3

-- utility
bswap3 :: B ((x :&: y) :&: z) (z :&: (y :&: x))
bmirr3 :: B ((x :|: y) :|: z) (z :|: (y :|: x))
bswap3 = bfirst bswap >>> bswap
bmirr3 = bleft bmirror >>> bmirror

-- | bvoid executes a behavior for side-effects only,
-- and simply propagates its input. This is a common
-- pattern.
bvoid :: B x y -> B x x
bvoid b = bcopy >>> bfirst b >>> bsnd 

-- | Conjoin is a partial merge.
bconjoinl :: B ((x :&: y) :|: (x :&: z)) (x :&: (y :|: z))
bconjoinr :: B ((x :&: z) :|: (y :&: z)) ((x :|: y) :&: z)
bconjoinl = bcopy >>> (isolateX *** isolateYZ) 
   where isolateX = (bfst +++ bfst) >>> bmerge
         isolateYZ = (bsnd +++ bsnd)
bconjoinr = (bswap +++ bswap) >>> bconjoinl >>> bswap

-- berrseq - composition with error options.
-- todo: move to a arrow transformer...
berrseq :: B x (err :|: y) -> B y (err :|: z) -> B x (err :|: z)
berrseq bx by = bx >>> bright by >>> bassocls >>> bleft bmerge

-- benvseq - composition with environment (~reader)
-- todo: move to a arrow transfomer
benvseq :: B (env :&: x) y -> B (env :&: y) z -> B (env :&: x) z
benvseq bx by = bcopy >>> (bfst *** bx) >>> by

{- Disjoin would be a powerful behavior:

     bdisjoinl :: B (x :&: (y :|: z)) ((x :&: y) :|: (x :&: z))

   This essentially expresses a decision for remote processes on x
   based on a decision in another partition (y or z). However, it
   seems impossible to express while respecting a certain rule, that
   communication between partitions be explicit.
  
   So there is no disjoin in Sirea, not in the general case anyway.
   A weaker version of disjoin on particular signals will be viable.
     
-}

-- | Represent latency of calculation or communication by delaying
-- a signal a small, logical difftime. Appropriate use of delay can
-- greatly improve system consistency and efficiency. In case of a
-- complex signal, every signal receives the same delay.
bdelay :: DT -> B x x
bdelay = B_delay

-- | Synch automatically delays all signals to match the slowest in
-- a composite. Immediately after synchronization, you can be sure 
-- (x :&: y) products precisely overlap, and (x :|: y) sums handoff
-- smoothly without gap or overlap. For non-composte signals, bsynch
-- has no effect. bsynch twice has no extra effect. Synchronization
-- is logical in RDP, and the implementation is wait-free.
--
-- Signals even in different partitions may be synchronized. 
bsynch :: B x x
bsynch = B_synch

-- | map an arbitrary Haskell function across an input signal.
bfmap :: (PtHask p) => (a -> b) -> B (S p a) (S p b)
bfmap = bUnsafeFmap

-- | as bfmap, but not constrained to partition class
bUnsafeFmap :: (a -> b) -> B (S p a) (S p b)
bUnsafeFmap = bUnsafeSigUpMap . su_fmap . s_fmap

-- | simple map from update to update
bUnsafeSigUpMap :: (SigUp a -> SigUp b) -> B (S p a) (S p b)
bUnsafeSigUpMap fn = bUnsafeLnk $ MkLnk 
    { ln_time_sensitive = False
    , ln_effectful = False
    , ln_build  = return . ln_fmap fn
    }

-- | combine a product of signals into a signal of products
bzip :: (PtHask p) => B (S p a :&: S p b) (S p (a,b))
bzip = bzipWith (,)

-- | combine signals with a given function
bzipWith :: (PtHask p) => (a -> b -> c) -> B (S p a :&: S p b) (S p c)
bzipWith = bUnsafeZipWith

-- | as bzipWith, but not constrained to valid partition class
bUnsafeZipWith :: (a -> b -> c) -> B (S p a :&: S p b) (S p c)
bUnsafeZipWith = bUnsafeSigZip . s_zip

-- | generic combiner for two signals. This requires some intermediate
-- state to build the signals and recombine them. That state is GC'd as
-- it updates.
bUnsafeSigZip :: (Sig a -> Sig b -> Sig c) -> B (S p a :&: S p b) (S p c)
bUnsafeSigZip fn = bUnsafeLnk $ MkLnk 
    { ln_time_sensitive = False
    , ln_effectful = False
    , ln_build = mkln_zip fn
    }

-- | lift choice of data to choice of behaviors
bsplit :: (PtHask p) => B (S p (Either a b)) (S p a :|: S p b)
bsplit = bUnsafeSplit

-- | as bsplit, but not constrained to partition class
bUnsafeSplit :: B (S p (Either a b)) (S p a :|: S p b)
bUnsafeSplit = bUnsafeLnk $ MkLnk
    { ln_time_sensitive = False
    , ln_effectful = False
    , ln_build = return . ln_split
    }

-- | bUnsafeLnk extends Sirea with primitive behaviors, FFI, foreign
-- services, legacy adapters, access to state and IO. Some primitive
-- behaviors (bfmap, bzip, bsplit) are also implemented atop this. 
--
-- Each instance of this behavior (excluding dead code) results in
-- construction of one link (the ln_build operation) when the Sirea
-- behavior is initiated. This construction allows for intermediate
-- state, intended for caches and other `safe` uses. The link will
-- also have access to Haskell IO on each update. The updates may
-- specify future times for when they become active or expire.
--
-- As indicated in the name, bUnsafeLnk is unsafe - it can easily
-- violate the RDP abstraction. It can be used safely, but caution
-- is warranted. Developers must avoid violating the tenets of RDP:
-- duration coupling, spatial commutativity, spatial idempotence, 
-- no local state, eventless, eventual consistency and resilience.
-- One must also garbage-collect dead links (which are created by
-- dynamic behaviors) by testing for stability or s_fini.
--
-- Safe uses of bUnsafeLnk should be hidden behind a library API.
--
-- Note: Developers must not introduce delay by use of bUnsafeLnk.
-- Delay must be visible for analysis at bsynch, so use bdelay. Use
-- of bUnsafeLnk will force a synchronization for complex signals,
-- which should (with rare exceptions) all be in the same partition.
bUnsafeLnk :: MkLnk x y -> B x y
bUnsafeLnk mkLnk = bsynch >>> B_lnk mkLnk


-- combine two parallel signals. 
-- This works by constructing the two input signals (from updates)
mkln_zip :: (Sig a -> Sig b -> Sig c) -> Lnk (S p1 c) -> IO (Lnk (S p2 a :&: S p3 b))
mkln_zip = 
    newIORef emptyZipper >>= \ rz ->
    undefined
    -- create a signal accumulator for a
    -- create a signal accumulator for b
    -- touch forwards if both a,b are untouched

-- split a signal.
ln_split :: Lnk ((S p3 a) :|: (S p2 b)) -> Lnk (S p1 (Either a b))
ln_split = undefined

-- | modify the primary function of a signal update
--
-- Note that this function on signals should not delay the signal or
-- otherwise change its activity profile.
su_fmap :: (Sig a -> Sig b) -> SigUp a -> SigUp b
su_fmap fn su =
    let state' = fmap (\(s0,t) -> (fn s0, t)) (su_state su) in
    SigUp { su_state = state', su_stable = su_stable su }

-- | delay all aspects of a signal update
su_delay :: DT -> SigUp a -> SigUp a
su_delay dt = if (0 == dt) then id else \ su ->
    let state' = fmap (\(s0,t) -> (s_delay dt s0, addTime t dt)) (su_state su) in
    let stable' = fmap (flip addTime dt) (su_stable su) in
    SigUp { su_state = state', su_stable = stable' }


-- todo:
--   bcross: change partitions. Specific to signal and partition types.
--     Likely a typeclass!
--   bdrop
--   bpeek (anticipate)
--   
-- weaker disjoin?
-- initial stateful and pseudo-state ops
--- reactive term rewriting, reactive state transition, 
--- reactive constraint-logic


---------------------
 -- UTILITY TYPES --
---------------------

-- SigSt represents the state of one signal.
data SigSt a = SigSt
    { st_signal :: !(Sig a)    -- signal value
    , st_stable :: !(Maybe T)  -- signal stability
    , st_expect :: !Bool       -- recent `touch`
    }

st_update :: SigUp a -> SigSt a -> SigSt a
st_update su s0 = 
    assert (monotonicStability t0 tf) $
    SigSt { st_signal = sf, st_stable = tf, st_expect = False }
    where t0 = st_stable s0
          tf = su_stable su
          s0 = st_signal s0
          sf = case su_state su of
                Nothing -> s0
                Just (su',tu') -> s_switch s0 tu' su'

st_zero :: SigSt a
st_zero = SigSt { st_signal = empty, st_stable = Nothing, st_expect = False }

st_poke :: SigSt a -> SigSt a
st_poke st = st { st_expect = True }

-- clear history up to T
st_clear :: T -> SigSt a -> SigSt a 
st_clear tt st = sf `seq` st { st_signal = sf }
    where (_,sf) = s_sample (st_signal st) tt

-- for some extra validation and debugging, ensure that stability
-- increases (excepting when it is Forever; then it may decrease).
monotonicStability :: Maybe T -> Maybe T -> Bool
monotonicStability (Just t0) (Just tf) = (tf >= t0)
monotonicStability _ _ = True

-- SigM represents states for two signals, useful for merge.
-- 

-- SigMerge is mutable data for merging two signals in one vat.
-- Merging signals always requires building intermediate state.
-- The goal is to keep this intermediate state to a minimum. 
data SigM t s x y = SigM
    { sm_lsig :: SigSt t s x    -- state for left signal
    , sm_rsig :: SigSt t s y    -- state for right signal
    , sm_tmup :: Maybe t        -- earliest update (how far to look back)
    }

zeroSigM :: (Signal s t) => SigM t s x y
zeroSigM = SigM 
    { sm_lsig = zeroSigSt
    , sm_rsig = zeroSigSt
    , sm_tmup = Nothing 
    }

updlSigM :: (SigSt t s x -> SigSt t s x) -> (SigM t s x y -> SigM t s x y)
updrSigM :: (SigSt t s y -> SigSt t s y) -> (SigM t s x y -> SigM t s x y)
updlSigM fn sm = sm { sm_lsig = fn (sm_lsig sm) }
updrSigM fn sm = sm { sm_rsig = fn (sm_rsig sm) }
luUpdlSigM :: (Signal s t) => LinkUp t s x -> (SigM t s x y -> SigM t s x y)
luUpdrSigM :: (Signal s t) => LinkUp t s y -> (SigM t s x y -> SigM t s x y)
luUpdlSigM lu = luUpdtSigM lu . updlSigM (luUpdSigSt lu)
luUpdrSigM lu = luUpdtSigM lu . updrSigM (luUpdSigSt lu)

updtSigM :: (SigTime t) => t -> (SigM t s x y -> SigM t s x y)
updtSigM tm sm = sm { sm_tmup = tm' }
    where tm' = case sm_tmup sm of
                    Nothing -> (Just tm)
                    Just tx -> Just $! (min tm tx)

luUpdtSigM :: (Signal s t) => LinkUp t s xy -> (SigM t s x y -> SigM t s x y)
luUpdtSigM lu =
    case lu_update lu of
        Nothing -> id
        Just up -> updtSigM (su_time up)


-- For update aggregation: poke and wait
waitSigM :: SigM t s x y -> Bool
waitSigM sm = (st_expect $ sm_lsig sm) || (st_expect $ sm_rsig sm)


-- current stability. Note that 'Nothing' means 'infinite' stability.
-- But I don't locally enforce this; instead, I just report stability
-- based on the most recent updates.
stableSigM :: (SigTime t) => SigM t s x y -> Maybe t
stableSigM sm = mbs ltm rtm
  where mbs Nothing r = r
        mbs l Nothing = l
        mbs (Just lt) (Just rt) = Just (min lt rt)
        ltm = st_stable (sm_lsig sm)
        rtm = st_stable (sm_rsig sm)

-- generate a link-update from a SigM
emitSigM :: (Signal s t) => (s x -> s y -> s xy) 
         -> SigM t s x y -> Maybe (SigUp s xy)
emitSigM fn sm =
    case sm_tmup sm of
        Nothing -> Nothing
        Just tt ->
            let sx = st_signal (sm_lsig sm) in
            let sy = st_signal (sm_rsig sm) in
            Just (s_future tt (fn sx sy))

-- clear a signal up to a given stability.
clearSigM :: (Signal s t) => Maybe t -> SigM t s x y -> SigM t s x y
clearSigM Nothing _    = zeroSigM -- 'Nothing' means infinite stability
clearSigM (Just tt) sm = 
    let lsig' = clearSigSt tt (sm_lsig sm) in
    let rsig' = clearSigSt tt (sm_rsig sm) in
    lsig' `seq` rsig' `seq` SigM 
        { sm_lsig = lsig'
        , sm_rsig = rsig'
        , sm_tmup = Nothing
        }

-- This is the main body for zip and merge, which combine two signals.
-- Some intermediate state is created based on the destination vat,
-- and this is potentially subject to 'poking' to indicate that an
-- update will be coming soon on a given path. Use of poke can avoid
-- redundant operations in most dataflow graphs - essentially, it 
-- slows down the fast path to wait for the slow path within a vat.
mkSigM :: (Signal s t) => (s x -> s y -> s xy)
       -> LB h t s xy -> RDPIO h (LB h t s x, LB h t s y)
mkSigM jf lb =
    let vtMerge = lb_vat lb
        lcFwd   = lb_cap lb vtMerge
    in 
    newVarInVat vtMerge zeroSigM >>= \ rfSigM ->
    newVarInVat vtMerge False    >>= \ rfSched ->
    let pkl    = readVar rfSigM >>= \ sm ->
                 if (st_expect . sm_lsig) sm then return () else
                 writeVar rfSigM (updlSigM pokeSigSt sm) >>
                 lc_poke lcFwd
        pkr    = readVar rfSigM >>= \ sm ->
                 if (st_expect . sm_rsig) sm then return () else
                 writeVar rfSigM (updrSigM pokeSigSt sm) >>
                 lc_poke lcFwd
        sched  = readVar rfSched >>= \ bSched ->
                 if bSched then return () else
                 readVar rfSigM >>= \ sm ->
                 if waitSigM sm then return () else
                 writeVar rfSched True >>
                 lc_poke lcFwd >>
                 schedRdpIn vtMerge deliver
        deliver= writeVar rfSched False >>
                 readVar rfSigM >>= \ sm ->
                 if waitSigM sm then return () else
                 let su  = emitSigM jf sm
                     tm  = stableSigM sm
                     lu  = LinkUp { lu_update = su, lu_stable = tm }
                     sm' = clearSigM tm sm
                 in 
                 sm' `seq` writeVar rfSigM sm' >>
                 lc_send lcFwd lu
        upl lu = modifyVar rfSigM (luUpdlSigM lu) >> sched
        upr lu = modifyVar rfSigM (luUpdrSigM lu) >> sched
        cpl vb = if (vtMerge == vb)
                   then LC { lc_poke = pkl, lc_send = upl }
                   else LC { lc_poke = return ()
                           , lc_send = runRdpIn vtMerge upl }
        cpr vb = if (vtMerge == vb)
                   then LC { lc_poke = pkr, lc_send = upr }
                   else LC { lc_poke = return ()
                           , lc_send = runRdpIn vtMerge upr }
        lbl = LB { lb_vat = vtMerge, lb_cap = cpl }
        lbr = LB { lb_vat = vtMerge, lb_cap = cpr }
    in return (lbl,lbr)

 



