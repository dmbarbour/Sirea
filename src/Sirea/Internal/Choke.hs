
-- | RDP is able to model feedback cycles with shared resources. For
-- demand monitors, a cycle might be:
--
--   monitor >>> bfmap foo >>> bdelay 0.1 >>> demand
--
-- Logically, the above behavior will cycle at 10Hz. Without choke,
-- Sirea would compute it as fast as possible, perhaps at 10kHz, and
-- thus be 9990 cycles ahead by the time one second had passed. That
-- would be an inefficient use of CPU and memory, and risks rework
-- for values speculated several minutes ahead.
--
-- Cyclic feedback can model interactive systems and coordination 
-- patterns. However, cycles are expensive to compute and speculate.
-- Due to RDP's support for effects, there is no magical fixpoint.
-- Each full cycle involves at least one 'runStepper' call at each
-- partition involved in the cycle.
--
-- Developers are encouraged to avoid cycles where possible, instead
-- favor animated state models, which compute fixpoints of futures.
-- But cycles cannot be avoided entirely in open systems, so RDP and
-- Sirea must handle them robustly and efficiently. Potential cycles
-- are choked at every resource that might introduce them (state and
-- demand monitors, mostly).
--
-- Choking ensures a sane behavior: we speculate just fractions of a
-- second, and the equilibrium rate for updates is same as logical 
-- frequency. The physical computation is not tightly coupled to the
-- logical frequency - e.g. it would run in bursts for frequencies
-- much higher than Sirea's heartbeat rate.
--
-- To improve anticipation, choke uses a heuristic equilibrium such
-- that speculation runs a rough number of cycles ahead. Choke is
-- introduced implicitly at resources that need it, e.g. all demand
-- monitors are choked.
--
-- A second concern regarding cycles is interaction with ln_touch.
--
-- A cycle within a partition is broken across steps. This ensures
-- each step performs a predictable amount of computation before
-- returning, though this does hurt snapshot consistency across the
-- cyclic resource. To keep more computation in the step, cycles are
-- detected within each partition (using ln_cycle) and breaks occur
-- at most once per cycle.
--
-- TODO: consider developing some sort of EqChoke to eliminate false
-- updates, to avoid unnecessary rework.
--
module Sirea.Internal.Choke
    ( newChoke
    ) where

import Data.IORef
import Data.Unique
import qualified Data.Set as S
import Control.Monad (unless, when)
import Control.Exception (assert)
import Sirea.Signal
import Sirea.Time
import Sirea.UnsafeLink
import Sirea.Internal.Tuning (tAncient, dtFutureChoke)
import Sirea.Internal.LTypes 
import Sirea.Partition

-- Q: When do I clear CycSt?
-- A: I can clear when the model becomes inactive.

data Choke z = Choke 
    { ck_link   :: !(LnkUp z)
    , ck_ident  :: !Unique
    , ck_cycle  :: !(IORef CycSt)
    , ck_data   :: !(IORef (CKD z))
    , ck_psched :: !PSched
    }
data CKD z = CKD 
    { ckd_stable :: !StableT  -- last reported stability
    , ckd_expect :: !Bool     -- touched by main link 
    , ckd_flush  :: !Bool     -- flush active this step?
    , ckd_update :: !(UPD z)  -- pending update
    }
data CycSt = CycUntested | CycTested | CycDetected
data UPD z = Idle | Update !(Sig z) {-# UNPACK #-} !T

ckdZero :: CKD z
ckdZero = CKD (StableT tAncient) False False Idle

newChoke :: PSched -> LnkUp z -> IO (LnkUp z)
newChoke pd lu =
    newUnique >>= \ u ->
    newIORef CycUntested >>= \ rfCyc ->
    newIORef ckdZero >>= \ rfDat ->
    let ck = Choke lu u rfCyc rfDat pd in
    return (chokeLnk ck)

-- the main choke behavior
chokeLnk :: Choke z -> LnkUp z
chokeLnk ck = LnkUp touch update idle cyc where
    touch = chokeTouch ck
    cyc = chokeCyc ck  
    update tS tU su = chokeLinkUpdate ck tS $ applyUpd tU su
    idle tS = chokeLinkUpdate ck tS id

-- compose or piggyback updates 
applyUpd :: T -> Sig z -> UPD z -> UPD z
applyUpd tU su Idle = Update su tU
applyUpd tU su (Update s0 tU0) =
    if (tU > tU0) then Update (s_switch' s0 tU su) tU0
                  else Update su tU

-- chokeTouch records and reports a touch on the main link.
chokeTouch :: Choke z -> IO () 
chokeTouch ck =
    readIORef (ck_data ck) >>= \ ckd ->
    unless (ckd_expect ckd) $
        let ckd' = ckd { ckd_expect = True } in        
        writeIORef (ck_data ck) ckd' >>
        unless (ckdActive ckd) (chokeInit ck)

ckdActive :: CKD z -> Bool
ckdActive ckd = ckd_expect ckd || ckd_flush ckd

-- flush forces an update to be emitted downstream even when there
-- is no upstream update. This is used to break cycles or deliver
-- the updates on the next step. chokeFlush must be executed during
-- the touch phase.
chokeFlush :: Choke z -> IO ()
chokeFlush ck = 
    readIORef (ck_data ck) >>= \ ckd ->
    unless (ckd_flush ckd) $
        let ckd' = ckd { ckd_flush = True } in
        writeIORef (ck_data ck) ckd' >>
        onUpdPhase (ck_psched ck) (chokeFlushUpdate ck) >>
        unless (ckdActive ckd) (chokeInit ck)

-- When choke is activated for any reason, we'll test whether it is
-- part of a cycle, and we'll touch the link to indicate an upcoming
-- update. This happens once per step at most.
--
-- I'd like to explore techniques to minimize the number of cycle
-- tests, i.e. to avoid this 'flood'. 
chokeInit :: Choke z -> IO ()
chokeInit ck = tstCyc >> touch where
    tstCyc = 
        readIORef (ck_cycle ck) >>= \ cycSt ->
        when (cycUntested cycSt) $
            writeIORef (ck_cycle ck) CycTested >>
            ln_cycle (ck_link ck) (S.singleton (ck_ident ck))
    touch = ln_touch (ck_link ck)

cycUntested :: CycSt -> Bool
cycUntested CycUntested = True
cycUntested _ = False

cycDetected :: CycSt -> Bool
cycDetected CycDetected = True
cycDetected _ = False
    
-- chokeCyc detects partition-local cycles in a step
--
-- If we're also waiting on an upstream update, we'll need to use 
-- flush to break the cycle. We'll assume a cycle.
chokeCyc :: Choke z -> CycleSet -> IO ()
chokeCyc ck ns = 
    readIORef (ck_cycle ck) >>= \ cycSt ->
    unless (cycDetected cycSt) $
        if (S.member (ck_ident ck) ns) 
           then writeIORef (ck_cycle ck) CycDetected >>
                chokeFlush ck -- break cycle with flush
           else writeIORef (ck_cycle ck) CycTested >>
                let ns' = S.insert (ck_ident ck) ns in
                ln_cycle (ck_link ck) ns'

-- update initiated by chokeFlush, runs in update phase
chokeFlushUpdate :: Choke z -> IO ()
chokeFlushUpdate ck =
    readIORef (ck_data ck) >>= \ ckd ->
    readIORef (ck_cycle ck) >>= \ cycSt ->
    assert (ckd_flush ckd) $
    let ckd' = ckd { ckd_flush = False } in 
    let bDone = not (ckdActive ckd') in
    let bDeliver = bDone || cycDetected cycSt in
    writeIORef (ck_data ck) ckd' >>
    when bDone (writeIORef (ck_cycle ck) CycUntested) >>
    when bDeliver (chokeDeliver ck)


-- main link update or idle
--
-- If we're in a cycle, we need to decide whether to flush update on
-- next step. Otherwise, we should update this step.
chokeLinkUpdate :: Choke z -> StableT -> (UPD z -> UPD z) -> IO ()
chokeLinkUpdate ck tS fn =
    readIORef (ck_data ck) >>= \ ckd ->
    readIORef (ck_cycle ck) >>= \ cycSt -> 
    writeIORef (ck_cycle ck) CycUntested >>= \ _ ->
    assert (ckd_expect ckd) $ -- should be touched by link
    assert (tS >= ckd_stable ckd) $ -- non-decreasing stabiliy
    assert (not (cycDetected cycSt && ckd_flush ckd)) $ -- flush runs to break cycle
    let upd' = fn (ckd_update ckd) in
    let ckd' = ckd { ckd_stable = tS, ckd_expect = False, ckd_update = upd' } in
    writeIORef (ck_data ck) ckd' >>
    let bWaitForFlush = ckd_flush ckd' in
    unless bWaitForFlush $ -- wait for flush to avoid double update
        if cycDetected cycSt
           then let bSched = (ckd_stable ckd /= tS) || timeToDeliverU tS upd' in
                when bSched (onNextStep (ck_psched ck) (chokeFlush ck)) 
           else chokeDeliver ck

timeToDeliverU :: StableT -> UPD z -> Bool
timeToDeliverU _ Idle = False
timeToDeliverU tS (Update _ tU) = timeToDeliver tS tU

timeToDeliver :: StableT -> T -> Bool
timeToDeliver (StableT tS) tU = tU < (tS `addTime` dtFutureChoke)
    
-- Deliver the standing update. Called by chokeFlushUpdate or chokeLinkUpdate.
-- Deliver should run at most once per step. But it may run before link update
-- when a partition-local cycle is detected.
--
-- The caller is responsible for clearing any cycle information as needed.
chokeDeliver :: Choke z -> IO ()
chokeDeliver ck =
    readIORef (ck_data ck) >>= \ ckd ->
    assert (not (ckd_flush ckd)) $
    let tS = ckd_stable ckd in
    let upd = ckd_update ckd in
    case upd of
        Idle -> ln_idle (ck_link ck) tS
        Update su tU ->
            if timeToDeliver tS tU
               then let ckd' = ckd { ckd_update = Idle } in
                    ckd' `seq` writeIORef (ck_data ck) ckd' >>
                    ln_update (ck_link ck) tS tU su
               else ln_idle (ck_link ck) tS


