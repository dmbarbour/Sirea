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
import Control.Monad.Fix (mfix)

import Data.IORef
import Data.Typeable
import Data.Maybe
import Data.Function (fix)

import Sirea.Signal
import Sirea.Behavior
import Sirea.B
import Sirea.PCX
import Sirea.Partition
import Sirea.UnsafeLink
import Sirea.Time

import Sirea.Internal.B0Compile (compileB0)
import Sirea.Internal.B0Impl (wrapEqFilter)
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
    -- The `duty` parameter is undefined, used only for typeful
    -- construction. 
    agentBehaviorSpec :: duty -> B (S p ()) (S p ())

-- AgentResource ensures single instance for invokeAgent. A record
-- of the signal is maintained here in order to ensure an old agent
-- is kept in memory until it is no longer necessary.
--
--  NOTE: AgentResource is modeled as a global resource, since the
--  agentBehavior may cross into multiple partitions. However, the
--  AgentResource is still identified by its starting partition.
data AR p duty = AR
    { ar_daggr  :: !(DemandAggr () ())      -- track invocations
    , ar_make   :: !(IO (LnkUp ()))         -- how to instantiate agent
    , ar_data   :: !(IORef ARD)             -- state and instance tracking 
    , ar_psched :: !PSched                  -- to schedule cleanup
    } deriving (Typeable)

data ARD = ARD 
    { ard_signal     :: !(Sig ())           -- record of signal
    , ard_link       :: !(Maybe (LnkUp ())) -- current instance
    , ard_flush      :: !Bool               -- GC flush desired?
    , ard_flushSched :: !Bool               -- GC flush scheduled?
    } 

ardZero :: ARD
ardZero = ARD s_never Nothing False False

instance (AgentBehavior p duty) => Resource W (AR p duty) where
    locateResource _ = newAR

newAR :: (AgentBehavior p duty) => PCX W -> IO (AR p duty)
newAR cw = mfix $ \ ar -> -- fix cyclic dependencies
    getPCX ar cw >>= \ cp ->
    getPSched cp >>= \ pd ->
    newIORef ardZero >>= \ rf ->
    let b0 = unwrapB (getABS ar) cw in -- behavior
    let cc0 = CC { cc_getSched = return pd, cc_newRef = newRefIO } in
    let lc0 = LC { lc_dtCurr = 0, lc_dtGoal = 0, lc_cc = cc0 } in
    let lcaps = LnkSig (LCX lc0) in
    let make = ln_lnkup <$> snd (compileB0 b0 lcaps LnkDead) in 
    let touch = touchAR ar in
    let update = updateAR ar in
    let idle = idleAR ar in
    let lnAgent = LnkUp touch update idle in
    wrapEqFilter dtEqf (==) lnAgent >>= \ lu ->
    newDemandAggr cp lu sigActive >>= \ da ->
    return (AR da make rf pd)
    
-- functions getABS, getDuty, getPCX mostly for type wrangling
getABS :: (AgentBehavior p duty) => AR p duty -> B (S p ()) (S p ())
getABS = agentBehaviorSpec . getDuty

getDuty :: AR p duty -> duty
getDuty _ = undefined

getPCX :: (Partition p) => AR p duty -> PCX W -> IO (PCX p) 
getPCX _ = findInPCX
    
-- simple merge of activity signals
sigActive :: [Sig a] -> Sig ()
sigActive [] = s_never
sigActive (s:ss) = s_const () $ foldl (<|>) s ss

-- Touch on AgentResource will forward touch to the agent.
-- It also instantiates the agent, if needed.
touchAR :: AR p duty -> IO ()
touchAR ar =
    readIORef (ar_data ar) >>= \ ard ->
    maybe (ar_make ar) return (ard_link ard) >>= \ lu ->
    let bTouchedAlready = touchedForFlush ard in
    let ard' = ard { ard_link = Just lu, ard_flush = False } in
    writeIORef (ar_data ar) ard' >>
    unless bTouchedAlready (ln_touch lu)

-- Touch can also occur for flush. This is detectable by a state
-- where the agent resource has flush active but not scheduled.
touchedForFlush :: ARD -> Bool
touchedForFlush ard = ard_flush ard && (not . ard_flushSched) ard

getDaggrStability :: PSched -> StableT -> IO T
getDaggrStability _ (StableT tm) = pure tm
getDaggrStability pd DoneT = (`subtractTime` dtDaggrHist) <$> p_stepTime pd

idleAR :: AR p duty -> StableT -> IO ()
idleAR ar tS = 
    readIORef (ar_data ar) >>= \ ard ->
    assert ((not . isNothing . ard_link) ard) $
    let lu = fromMaybe ln_zero (ard_link ard) in
    getDaggrStability (ar_psched ar) tS >>= \ tCut ->
    let s' = s_trim (ard_signal ard) tCut in
    let ard' = ard { ard_signal = s' } in
    ard' `seq` writeIORef (ar_data ar) ard' >>
    ln_idle lu (StableT tCut) >>
    when (isDoneT tS) (initFlushAR ar)    
     
updateAR :: AR p duty -> StableT -> T -> Sig () -> IO ()
updateAR ar tS tU su =
    readIORef (ar_data ar) >>= \ ard ->
    assert ((not . isNothing . ard_link) ard) $
    let lu = fromMaybe ln_zero (ard_link ard) in
    getDaggrStability (ar_psched ar) tS >>= \ tCut ->
    let s1 = s_switch (ard_signal ard) tU su in
    let s' = s_trim s1 tCut in
    let ard' = ard { ard_signal = s' } in
    ard' `seq` writeIORef (ar_data ar) ard' >>
    ln_update lu (StableT tCut) tU su >>
    when (isDoneT tS) (initFlushAR ar)

initFlushAR :: AR p duty -> IO ()
initFlushAR ar =
    readIORef (ar_data ar) >>= \ ard ->
    let bSchedFlush = not (ard_flushSched ard) in
    let ard' = ard { ard_flush = True, ard_flushSched = True } in
    ard' `seq` writeIORef (ar_data ar) ard' >>
    when bSchedFlush (p_eventually (ar_psched ar) (flushAR ar))
    
flushAR :: AR p duty -> IO ()
flushAR ar = beginFlush where
    beginFlush =
        readIORef (ar_data ar) >>= \ ard ->
        assert (ard_flushSched ard) $ 
        let ard' = ard { ard_flushSched = False } in
        writeIORef (ar_data ar) ard' >>= \ _ ->
        when (ard_flush ard') $
            assert (touchedForFlush ard') $
            let lu = fromMaybe ln_zero (ard_link ard') in
            p_onUpdPhase (ar_psched ar) deliverFlush >>
            ln_touch lu
    deliverFlush =
        readIORef (ar_data ar) >>= \ ard ->
        when (touchedForFlush ard) $ -- blocked by touch or initFlushAR
            assert (ard_flush ard) $
            p_stepTime (ar_psched ar) >>= \ tNow ->
            let tCut = tNow `subtractTime` dtDaggrHist in
            let s' = s_trim (ard_signal ard) tCut in
            let lu = fromMaybe ln_zero (ard_link ard) in
            let bDone = s_term s' tCut in
            let ard' = if bDone then ardZero else
                       ard { ard_signal = s', ard_flushSched = True } 
            in
            let tS = if bDone then DoneT else StableT tCut in
            ard' `seq` writeIORef (ar_data ar) ardZero >>
            unless bDone (p_eventually (ar_psched ar) (flushAR ar)) >>
            ln_idle lu tS

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
invokeAgent :: (AgentBehavior p duty) => duty -> B (S p ()) (S p ())
invokeAgent duty = fix $ \ b -> invokeDutyAR (getAR duty b)

invokeDutyAR :: (PCX W -> IO (AR p duty)) -> B (S p ()) (S p ())
invokeDutyAR findAR = bvoid (unsafeLinkB lnInvoke) where
    lnInvoke cw ln = assert (ln_dead ln) $ 
        findAR cw >>= \ ar ->
        newDemandLnk (ar_daggr ar) >>= \ lu ->
        return (LnkSig lu)

getAR :: (AgentBehavior p duty) 
      => duty -> B (S p ()) (S p ()) -> PCX W -> IO (AR p duty)
getAR _ _ = findInPCX


