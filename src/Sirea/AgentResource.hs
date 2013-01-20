{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}

-- | AgentResource supports developers of FFI resource adapters in
-- expressing some logic with RDP code. Essentially, it allows 
-- partition resources to be wrapped with singleton RDP behaviors.
-- Behaviors are described by AgentBehavior class, then instantiated 
-- with the invokeAgent behavior. Concurrent demands may result in
-- the agent being kept around longer.
--
-- Safe RDP behaviors are idempotent. Use of AgentResource will not
-- impact them, except for performance (eliminating redundant 
-- computations). But AgentResource primarily benefits unsafe RDP
-- behaviors - e.g. it doesn't matter that your UnsafeOnUpdate
-- behavior is non-idempotent if it is invoked as a singleton.
--
-- Communication with the invoked behaviors is performed through
-- demand monitors or shared state (blackboard metaphors).
--
module Sirea.AgentResource
    ( invokeAgent
    , AgentBehavior(..)
    ) where

import Control.Applicative
import Control.Monad (unless, when)
import Control.Exception (assert)

import Data.IORef
import Data.Typeable
import Data.Function (fix) 
import Data.Maybe

import Sirea.Signal
import Sirea.Behavior
import Sirea.BCX
import Sirea.PCX
import Sirea.Partition
import Sirea.Link
import Sirea.Time

import Sirea.Internal.BTypes
import Sirea.Internal.BCompile (compileB)
import Sirea.Internal.BImpl (wrapEqFilter)
import Sirea.Internal.DemandMonitorData
import Sirea.Internal.LTypes
import Sirea.Internal.Tuning (dtEqf, dtDaggrHist)

-- | The RDP behaviors of AgentResources are defined in a typeclass.
-- The behaviors are indexed by partition and a `duty` type. Use the
-- `invokeAgent` behavior to compile and install a unique instance
-- of the AgentResource (even if invoked many times concurrently).
-- While an agentBehaviorSpec will start in a particular partition,
-- it is free to cross into related partitions to perform its duty.
--
-- Recommendation is to keep the `duty` types hidden, and export the
-- behaviors that use invokeAgent directly. This ensures uniqueness.
--
class (Partition p, Typeable duty) => AgentBehavior p duty where
    -- | This should be instantiated as: agentBehaviorSpec _ = ...
    -- The `duty` parameter is undefined, used only for type. 
    agentBehaviorSpec :: duty -> BCX w (S p ()) (S p ())

-- AgentResource ensures single instance for invokeAgent.
--
--  NOTE: AgentResource is modeled as a global resource, since the
--  agentBehavior may cross into multiple partitions. However, the
--  AgentResource is still identified by its starting partition.
data AgentResource p duty = AgentResource
    { ar_daggr :: !(DemandAggr () ())       -- track invocations
    , ar_make  :: !(IO (LnkUp ()))          -- how to instantiate agent
    , ar_data  :: !(IORef ARD)              -- state and instance tracking 
    , ar_pd    :: !PartD                    -- to schedule cleanup
    } deriving (Typeable)

data ARD = ARD 
    { ard_signal     :: !(SigSt ())         -- current demand signal
    , ard_link       :: !(Maybe (LnkUp ())) -- current instance
    , ard_flush      :: !Bool               -- GC flush desired?
    , ard_flushSched :: !Bool               -- GC flush scheduled?
    } 

ardZero :: ARD
ardZero = ARD st_zero Nothing False False

instance (AgentBehavior p duty) => Resource w (AgentResource p duty) where
    locateResource _ = newAgentResource

newAgentResource :: (AgentBehavior p duty) => PCX w -> IO (AgentResource p duty)
newAgentResource cw = fix $ \ mkar ->
    let b = unwrapBCX (getABS mkar) cw in -- behavior
    let make = ln_lnkup <$> snd (compileB b (LnkDUnit ldt_zero) LnkDead) in 
    let cp = getPCX mkar cw in
    let pd = mkPartD cp in
    newIORef ardZero >>= \ rf ->
    let lnAgent = LnkUp { ln_touch = touchAR make rf
                        , ln_update = updateAR pd rf } 
    in
    wrapEqFilter dtEqf (==) lnAgent >>= \ lu ->
    newDemandAggr cp lu sigActive >>= \ da ->
    let ar = AgentResource { ar_daggr = da, ar_data = rf
                           , ar_make = make, ar_pd = pd }
    in
    ar `seq` return ar
    
-- functions getABS, getDuty, getPCX mostly for type wrangling
getABS :: (AgentBehavior p duty) => IO (AgentResource p duty) -> BCX w (S p ()) (S p ())
getABS = agentBehaviorSpec . getDuty

getDuty :: (AgentBehavior p duty) => IO (AgentResource p duty) -> duty
getDuty _ = undefined

getPCX :: (AgentBehavior p duty) => IO (AgentResource p duty) -> PCX w -> PCX p 
getPCX _ = findInPCX
    
-- simple merge of activity signals
sigActive :: [Sig ()] -> Sig ()
sigActive [] = s_never
sigActive (s:ss) = foldl (<|>) s ss

-- Touch on AgentResource will forward touch to the agent.
-- It also instantiates the agent, if needed.
touchAR :: IO (LnkUp ()) -> IORef ARD -> IO ()
touchAR make rf = 
    readIORef rf >>= \ ard ->
    let st = ard_signal ard in
    unless (st_expect st) $
        let st' = st_poke st in
        maybe make return (ard_link ard) >>= \ lu ->
        let bTouchedForFlush = touchedByFlushARD ard in
        let ard' = ard { ard_signal = st', ard_link = Just lu
                       , ard_flush = False } 
        in
        writeIORef rf ard' >> -- record agent and touched signal
        unless bTouchedForFlush (ln_touch lu) -- forward touch to agent

deliverUpdateARD :: ARD -> SigUp () -> IO ()
deliverUpdateARD ard = 
    assert ((not . isNothing . ard_link) ard) $
    maybe ignore ln_update (ard_link ard)
    where ignore _ = return ()
    
-- handle update of collective demand for the agent, and possibly
-- schedule a flush to ensure the agent is cleared. This operation
-- will not actually clear the agent.
updateAR :: PartD -> IORef ARD -> SigUp () -> IO ()
updateAR pd rf su = 
    readIORef rf >>= \ ard ->
    assert ((st_expect . ard_signal) ard) $ -- we have a signal
    maybe getTStable pure (su_stable su) >>= \ tStable ->    
    let st' = st_clear tStable (st_sigup su (ard_signal ard)) in
    let bNeedFlush = isNothing (su_stable su) in
    let bFlushSched = ard_flushSched ard || bNeedFlush in
    let bSchedFlush = (not . ard_flushSched) ard && bNeedFlush in
    let ard' = ard { ard_signal = st', ard_flush = bNeedFlush   
                   , ard_flushSched = bFlushSched } 
    in
    ard' `seq` writeIORef rf ard' >>
    when bSchedFlush (pd_eventually pd (flushAR pd rf)) >>
    let su' = su { su_stable = Just tStable } in 
    deliverUpdateARD ard' su'
    where getTStable = (`subtractTime` dtDaggrHist) <$> pd_time pd

-- flush is invoked when it seems we won't be receiving updates from
-- the demand aggregator, enabling the agent to be removed when it
-- is no longer necessary. Flush may be disabled if an update occurs
-- on the same round. 
--
-- Flush will automatically repeat as necessary (on heartbeat).
flushAR :: PartD -> IORef ARD -> IO ()
flushAR pd rf = initFlush where
    initFlush =
        readIORef rf >>= \ ard ->
        assert (ard_flushSched ard) $
        let ard' = ard { ard_flushSched = False } in
        writeIORef rf ard' >>
        let bFlush = ard_flush ard' in
        when bFlush $
            assert (touchedByFlushARD ard') $
            pd_phaseDelay pd finiFlush >>
            maybe (return ()) (ln_touch) (ard_link ard')
    finiFlush =
        readIORef rf >>= \ ard ->
        when (ard_flush ard) $
            assert ((isNothing . st_stable . ard_signal) ard) $
            pd_time pd >>= \ tNow ->
            let tStable = tNow `subtractTime` dtDaggrHist in
            let st' = st_clear tStable (ard_signal ard) in
            let bDone = s_term (st_signal st') tStable in
            let bNeedFlush = isNothing (st_stable st') && not bDone in
            let deliverUpdate = deliverUpdateARD ard in
            let ard' = if bDone then ardZero else 
                       ard { ard_signal = st', ard_flush = bNeedFlush
                           , ard_flushSched = bNeedFlush }
            in
            let suStable = if bDone then Nothing else Just tStable in
            let su = SigUp { su_state = Nothing, su_stable = suStable } in
            ard' `seq` writeIORef rf ard' >>
            when bNeedFlush (pd_eventually pd (flushAR pd rf)) >>
            deliverUpdate su 

-- An AgentResource can be touched by flush or touched for update.
-- But we only want to touch once. If we touch for flush first, we
-- can tell because flush will be active but flushSched will be
-- inactive. 
touchedByFlushARD :: ARD -> Bool
touchedByFlushARD ard = ard_flush ard && (not . ard_flushSched) ard

-- | `invokeAgent` will install a unique instance of agent behavior
-- (one for each unique partition-duty pair). This behavior is built
-- and installed on demand, then uninstalled and GC'd when there is
-- no active demand, potentially many times in the Haskell process.
--
-- Logically, use of `invokeAgent` should have the same results as
-- many concurrent instances due to RDP's idempotence. However, the
-- unique installation may be much more efficient and will simplify
-- safe use of non-idempotent adapters (e.g. UnsafeOnUpdate). 
--
invokeAgent :: (AgentBehavior p duty) => duty -> BCX w (S p ()) (S p ())
invokeAgent duty = fix $ \ b -> wrapBCX $ \ cw ->
    let ar = getAR duty b cw in
    invokeDutyAR ar

invokeDutyAR :: AgentResource p duty -> B (S p ()) (S p ())
invokeDutyAR ar = bvoid (unsafeLinkB lnInvoke) where
    lnInvoke ln = assert (ln_dead ln) $ 
        LnkSig <$> newDemandLnk (ar_daggr ar)

getAR :: (AgentBehavior p duty) => duty -> BCX w (S p ()) (S p ()) 
      -> PCX w -> AgentResource p duty
getAR _ _ = findInPCX


