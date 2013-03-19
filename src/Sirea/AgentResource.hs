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
    ( unsafeInvokeAgent
    , AgentBehavior(..)
    ) where

import Control.Applicative
import Control.Monad (when)
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
import Sirea.Internal.DemandMonitorData
import Sirea.Internal.B0Impl (wrapLnEqShift)
import Sirea.Internal.LTypes

--import Debug.Trace

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
-- Caution: AgentBehavior should not do anything that might invoke
-- itself or it will start keeping itself alive, out of control.
--
class (Partition p, Typeable duty) => AgentBehavior p duty where
    -- | This should be instantiated as: agentBehaviorSpec _ = ...
    -- The `duty` parameter is undefined, used only for typeful
    -- construction. 
    agentBehaviorSpec :: duty -> B (S p ()) S1

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
    } deriving (Typeable)

data ARD = ARD 
    { ard_signal     :: !(Sig ())           -- record of signal
    , ard_link       :: !(Maybe (LnkUp ())) -- current instance
    } 

ardZero :: ARD
ardZero = ARD s_never Nothing

instance (AgentBehavior p duty) => Resource W (AR p duty) where
    locateResource _ = newAR

newAR :: (AgentBehavior p duty) => PCX W -> IO (AR p duty)
newAR cw = mfix $ \ ar -> -- fix cyclic dependencies
    getPCX ar cw >>= \ cp ->
    newIORef ardZero >>= \ rf ->
    getPSched cp >>= \ pd ->
    let b0 = unwrapB (getABS ar) cw in -- behavior
    let cc0 = CC { cc_getSched = return pd, cc_newRef = newRefIO } in
    let lc0 = LC { lc_dtCurr = 0, lc_dtGoal = 0, lc_cc = cc0 } in
    let lcaps = LnkSig (LCX lc0) in
    let make = ln_lnkup <$> snd (compileB0 b0 lcaps LnkDead) in 
    let lu = LnkUp (touchAR ar) (updateAR ar) (idleAR ar) (cycleAR ar) in
    wrapLnEqShift (==) lu >>= \ luEq ->
    newDemandAggr pd luEq sigActive >>= \ da ->
    return (AR da make rf)
    
-- functions getABS, getDuty, getPCX mostly for type wrangling
getABS :: (AgentBehavior p duty) => AR p duty -> B (S p ()) S1
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
touchAR ar = getARLink ar >>= \ lu -> ln_touch lu

-- load or create agent link
getARLink :: AR p duty -> IO (LnkUp ())
getARLink ar = 
    readIORef (ar_data ar) >>= \ ard ->
    case ard_link ard of
        Just lu ->  
            return lu
        Nothing ->
            --traceIO ("new Agent") >>
            ar_make ar >>= \ lu ->
            let ard' = ard { ard_link = Just lu } in
            writeIORef (ar_data ar) ard' >>
            return lu

-- test the agent for cyclic dependencies; creates the agent if it
-- is necessary to do so.
cycleAR :: AR p duty -> CycleSet -> IO ()
cycleAR ar ns = getARLink ar >>= \ lu -> ln_cycle lu ns

idleAR :: AR p duty -> StableT -> IO ()
idleAR ar tS = 
    readIORef (ar_data ar) >>= \ ard ->
    assert ((not . isNothing . ard_link) ard) $
    let lu = fromMaybe ln_zero (ard_link ard) in
    let s' = s_trim (ard_signal ard) (inStableT tS) in
    let bDone = s_term s' (inStableT tS) in
    let ard' = ard { ard_signal = s' } in
    ard' `seq` writeIORef (ar_data ar) ard' >>
    --traceIO ("agent idle tS = " ++ show tS) >>
    ln_idle lu tS >>
    when bDone (clearAR ar)
     
updateAR :: AR p duty -> StableT -> T -> Sig () -> IO ()
updateAR ar tS tU su =
    readIORef (ar_data ar) >>= \ ard ->
    assert ((not . isNothing . ard_link) ard) $
    let lu = fromMaybe ln_zero (ard_link ard) in
    let s1 = s_switch (ard_signal ard) tU su in
    let s' = s_trim s1 (inStableT tS) in
    let bDone = s_term s' (inStableT tS) in
    let ard' = ard { ard_signal = s' } in
    ard' `seq` writeIORef (ar_data ar) ard' >>
    --let ssu = sigToList s1 (tU `subtractTime` 1) (tU `addTime` 60) in
    --traceIO ("agent update tS = " ++ show tS ++ " tU = " ++ show tU ++ show ssu) >>
    ln_update lu tS tU su >>
    when bDone (clearAR ar)

-- clearAR is called whenever an agent stabilizes on inactivity. It
-- immediately removes the link from memory and resets the agent to
-- its initial state. If the agent is needed again, it is rebuilt.
-- In practice, this is conservative because the agent isn't cleared
-- if the signal is ambiguous about future activity.
clearAR :: AR p duty -> IO ()
clearAR ar = 
    --traceIO ("clearAR") >>
    writeIORef (ar_data ar) ardZero
    
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
-- Caution: invokeAgent is unsafe because cyclic invocations could
-- ultimately cause the agent to keep itself alive. This is usually
-- not a problem, since developers have pretty good control over 
-- agent behavior. But if an agent uses dynamic behavior or invokes
-- another agent, one must be cautious.
--
unsafeInvokeAgent :: (AgentBehavior p duty) => duty -> B (S p ()) (S p ())
unsafeInvokeAgent duty = fix $ \ b -> invokeDutyAR (getAR duty b)

invokeDutyAR :: (PCX W -> IO (AR p duty)) -> B (S p ()) (S p ())
invokeDutyAR findAR = bvoid (unsafeLinkB_ lnInvoke) where
    lnInvoke cw = findAR cw >>= newDemandLnk . ar_daggr

getAR :: (AgentBehavior p duty) 
      => duty -> B (S p ()) (S p ()) -> PCX W -> IO (AR p duty)
getAR _ _ = findInPCX


