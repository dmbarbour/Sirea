{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}

-- | AgentResource supports developers of FFI resource adapters in
-- expressing some logic with RDP code.
--  
-- Conceptually, we want some resources to be controlled by agents
-- external to the Sirea application. Instead of direct influence on
-- resources, we communicate with the agents, which will influence
-- the resources on our behalf. The agents can be coded in an RDP
-- style, allowing more logic to be expressed declaratively.
-- 
-- AgentResource supports this pattern in Sirea by ensuring that
-- only one instance of a particular agent is active at a time. The
-- agent is activated by signal to 'invokeAgent', and deactivates
-- when there is no more demand for it. Since only one instance of
-- an agent is present, reasoning about idempotence and safe use of
-- UnsafeOnUpdate, SRefs, etc. become easier.
--
-- Communication with agents is typically indirect via shared state,
-- demand monitor, or other blackboard metaphor. 
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

import Sirea.Signal
import Sirea.Behavior
import Sirea.BCX
import Sirea.PCX
import Sirea.Partition
import Sirea.Link

import Sirea.Internal.BTypes
import Sirea.Internal.BCompile (compileB)
import Sirea.Internal.DemandMonitorData
import Sirea.Internal.LTypes
import Sirea.Internal.Tuning (dtEqf, dtInsigStabilityUp)

-- | The RDP behaviors of AgentResources are defined in a typeclass.
-- The behaviors are indexed by partition and a `duty` type. Use the
-- `invokeAgent` behavior to compile and install a unique instance
-- of the AgentResource (even if invoked many times concurrently).
--
-- Recommendation is to keep the `duty` types hidden, and export the
-- behaviors that use invokeAgent directly. This ensures uniqueness.
--
class (Partition p, Typeable duty) => AgentBehavior p duty where
    -- | This should be instantiated as: agentBehaviorSpec _ = ...
    -- The `duty` parameter is undefined, used only for type. 
    agentBehaviorSpec :: duty -> BCX w (S p ()) (S p ())

-- AgentResource ensures single instance for invokeAgent
data AgentResource duty = AgentResource
    { ar_daggr :: !(DemandAggr () ())       -- track invocations
    , ar_data  :: !(IORef ARD)              -- state and instance tracking 
    , ar_pd    :: !PartD                    -- to schedule cleanup
    } deriving (Typeable)


data ARD = ARD 
    { ard_sig   :: !(SigSt ())              -- current demand signal
    , ard_flush :: !Bool                    -- is a GC flush scheduled?
    , ard_link  :: !(Maybe (LnkUp ()))      -- current agent instance
    } 

ardZero :: ARD
ardZero = ARD st_zero False Nothing

instance (AgentBehavior p duty) => Resource p (AgentResource duty) where
    locateResource _ = newAgentResource

-- Note that an AgentResource initially lacks an agent instance. The
-- instance is provided by invokeAgent (upon providing an update).
newAgentResource :: (AgentBehavior p duty) => PCX p -> IO (AgentResource duty)
newAgentResource cp = 
    newIORef ardZero >>= \ rf ->
    let update = updateAR rf in
    let lnAgent = LnkUp { ln_touch = return (), ln_update = update } in
    wrapEqFilter dtEqf (==) lnAgent >>= \ lu ->
    newDemandAggr cp lu sigActive >>= \ da ->
    let pd = mkPartD cp in
    return $ AgentResource { ar_daggr = da, ar_data = rf, ar_pd = pd } 
    
-- simple merge of activity signals
sigActive :: [Sig ()] -> Sig ()
sigActive [] = s_never
sigActive (s:ss) = foldl (<|>) s ss

-- drop insignificant stability updates
dropUpdate :: ARD -> SigUp z -> Bool
dropUpdate ard su = isNothing (su_state su) 
    && insigDiff (st_stable (ard_sig ard)) (su_stable su) 
    where insigDiff (Just t0) (Just tf) = tf < (t0 `addTime` dtInsigStabilityUp)
          insigDiff _ _ = False

-- handle update for collective invokeAgent.
updateAR :: (AgentBehavior p duty) => IORef ARD -> SigUp su -> IO ()
updateAR rf su = 
    readIORef rf >>= \ ard ->
    unless (dropUpdate ard su) $
        let st' = st_sigup su (ard_sig ard) in
        undefined
{-
        let bDone = maybe True
        
        let ard' = ard { ard_sig = st' } in
        
    
    let su' = 
    
    agentBehavior = agentBehaviorSpec d cp
    update = error "TODO!"
-}



-- | `invokeAgent` will install a unique instance of agent behavior
-- (one for each unique partition-duty pair). This behavior is built
-- and installed on demand, then uninstalled and GC'd when there is
-- no active demand, potentially many times in a Haskell process. 
-- Concurrent invocations do not result in extra instances, just one
-- instance that survives until all active demand disappears.
--
-- Logically, use of `invokeAgent` should have the same results as
-- many concurrent instances due to RDP's idempotence. However, the
-- unique installation may be much more efficient and will simplify
-- safe use of non-idempotent expressions (e.g. UnsafeOnUpdate). 
--
invokeAgent :: (AgentBehavior p duty) => duty -> BCX w (S p ()) (S p ())
invokeAgent d = fix $ \ b -> wrapBCX $ \ cw ->
    let cp = getPCX b cw in
    let ar = getAR d cp in
    let da = ar_daggr ar in
    let ab = unwrapBCX (agentBehaviorSpec duty) cw in
    let make = snd <$> compileB ab ldt_zero LnkDead in
    let autoMake = 
            -- compile and install the agent behavior (when needed)
            readIORef (ar_data ar) >>= \ ard ->
            when (isNothing (ard_link ard)) $
                make >>= \ link ->
                let ard' = ard { ard_link = Just link } in
                writeIORef (ar_data ar) ard' 
    in
    let lnInvoke ln = assert (ln_dead ln) $
            newDemandLnk da >>= \ lu ->
            let touch' = autoMake >> ln_touch lu in
            let lnOut = lu { ln_touch = touch' } in
            return (LnkSig lnOut)
    in
    bvoid (unsafeLinkB lnInvoke) >>> bconst ()


getPCX :: (Partition p) => BCX w (S p ()) (S p ()) -> PCX w -> PCX p
getPCX _ = findInPCX     

getAR :: (AgentBehavior p duty) => duty -> PCX p -> AgentResource duty
getAR _ = findInPCX

-- 
--   1. AgentResource essentially needs an activity monitor.
--   2. Agents must exist in a partition. (Multi-param typeclass?)
--   3. Agents cannot be parameterized, since


