
-- Utilities for tracking demands and monitors of demand.
module Sirea.Internal.DemandMonitorData
    ( DemandAggr(..), DD(..)
    ) where

import qualified Data.HashTable as HT
import Data.IORef
import Control.Applicative
import Control.Monad (unless, when)
import Control.Exception (assert)
import Sirea.Signal
import Sirea.Time
import Sirea.Link

-- | DemandAggr: keep track of incoming demand updates. 
--
-- REGARDING FEEDBACKS CYCLES:
--
-- Touches and updates to a demand monitor may be cyclic in nature
-- due to natural "open feedback loops". An example you have likely
-- experienced is placing a microphone near its amplifier, which oft
-- results in a shrill screech unless there is a chip dedicated to
-- eliminating it. Developers are discouraged from modeling cycles.
-- Often, cycles can be eliminated by using finer-grained resources,
-- e.g. multiple demand monitors for different roles. However: 
--
--   * Cycles cannot always be prevented in an open system. 
--   * RDP and Sirea should be robust to cycles when they occur.
--
-- Here, robust means: no deadlock, no livelock, and developers can
-- reason about soft real-time performance and resource consumption.
-- This is achieved by choking updates, as a damping mechanism for
-- the feedback. (In addition to demand monitors, state models need
-- such mechanisms.)
--
-- If cycles occur, the natural case involves latency between update
-- and feedback. In this case, the damping works very well since we
-- only delay processing of an anticipated future until we're a bit
-- closer to that future. This is called "temporal recursion". If no
-- latency exists, then we have instantaneous feedback which ideally
-- should be computed as an open network fixpoint. In that case, the
-- damping can damage consistency, and a warning will be printed.
--
-- By choking with temporal recursion in combination with adjacent
-- equality filters, a demand monitor can regain a lot of stability
-- that would otherwise be lost to cycles. 
-- 
data DemandAggr e z = DemandAggr 
    { de_active     :: !(IORef Bool)        -- to detect cyclic touch or update
    , de_touchCt    :: !(IORef Int)         -- count of non-cyclic touches
    , de_nextid     :: !(IORef Int)         -- next hashtable ID
    , de_table      :: !(HT.HashTable Int (DD e)) -- tracking demand data
    , de_hist       :: !(DT)                -- buffer to support straggling demands
    , de_fchoke     :: !(DT)                -- choke for temporal recursion...
    , de_nzip       :: !([Sig e] -> Sig z)  -- compute the result signal
    , de_peq        :: !(z -> z -> Bool)    -- for adjacency equality filters
    , de_update     :: !(IORef (Maybe T))   -- lowest update time (at the moment)
    , de_next       :: !(LnkUp z)           -- 
    }

data DD e = DD
    { dd_signal :: !(Sig e)
    , dd_expect :: !Bool
    , dd_stable :: !(Maybe T)
    }

{-

newDemandAggr :: ([Sig e] -> Sig z) 
              -> 
IO (DemandAggr e z)



-- DemandMonitorData
--   currently a couple tables and some stateful summary data
--   (the stateful data is used to keep algorithmic costs down)
--
-- zipfn: a function to combine signal sources; must be commutative
-- peqfn: a partial equality function; return true iff two values 
--   are known to be equal; may return false if unknown
-- history: buffer to accommodate straggling (newcoming) monitors or
--   demand sources. (Might later want to split so that new monitors
--   are supported longer than new sources? think about it.)
--
-- The DemandMonitorData currently assumes (possibly incorrectly) 
-- that the current demand stability is a decent estimate of new
-- demand or monitor times. 
-- 
data DemandMonitorData e z = DemandMonitor
    { dmd_zipfn     :: !([Sig e] -> Sig z)     -- zip the demand signals
    , dmd_peqfn     :: !(z -> z -> Bool)       -- clean up redundant updates
    , dmd_history   :: !(DT)                   -- for eventual consistency
    , dmd_signal    :: !(IORef (Sig z))        -- last reported signal
    , dmd_stable    :: !(IORef (Maybe T))      -- last reported stability
    , dmd_update    :: !(IORef (Maybe T))      -- earliest demand update
    , dmd_dtouch    :: !(IORef Int)            -- # expected updates on demand facets
    , dmd_mtouch    :: !(IORef Int)            -- # expected updates on monitor facets
    , dmd_nxtid     :: !(IORef Int32)          -- create local unique ID for table
    , dmd_dTable    :: !(HT.HashTable (DemandData e))
    , dmd_mTable    :: !(HT.HashTable (MonitorData z))
    }
data DemandData e = DemandData
    { dd_sig :: !(SigSt e) -- current demand status
    }
data MonitorData z = MonitorData
    { md_sig :: !(SigSt ()) -- monitoring status 
    , md_lnk :: !(LnkUp z)  -- to receive monitor updates
    , md_upd :: !(Maybe T)  -- recent update time (if any)
    }

dd_zero :: DemandData e
dd_zero = DemandData st_zero

md_zero :: LnkUp z -> MonitorData z
md_zero lu = MonitorData st_zero lu Nothing

touchMD :: MonitorData z -> MonitorData z 
touchMD md = 
    let st = md_sig md in
    let st' = st_poke st in
    md { md_sig = st' }

newDemandMonitorData 
    :: ([Sig e] -> Sig z) -- nzip
    -> (z -> z -> Bool)   -- peq
    -> DT                 -- how much to accommodate stragglers?
    -> IO (DemandMonitorData e z)
newDemandMonitorData nzip peq dtHist =
    assert (dtHist >= 0) $ 
    DemandMonitorData
        <$> pure nzip 
        <*> pure peq
        <*> pure dtHist
        <*> newIORef (nzip []) -- current signal
        <*> newIORef (Nothing) -- current signal stability
        <*> newIORef (Nothing) -- earliest demand update
        <*> newIORef 0      -- initial touch count
        <*> newIORef 400000 -- arbitrary starting index
        <*> HT.new (==) HT.hashInt
        <*> HT.new (==) HT.hashInt

-- obtain the index for a new demand or monitor facet 
-- (must be performed in demand monitor's thread)
getIndex :: DemandMonitorData e z -> IORef (Maybe Int) -> IO Int
getIndex dmd rf = readIORef rf >>= maybe newIndex return
    where rfNxt = dmd_nxtid dmd 
          newIndex = readIORef rfNxt >>= \ n ->
                     let n' = succ n in
                     n' `seq` 
                     writeIORef rfNxt n' >>
                     writeIORef rf (Just n') >>
                     return n'

-- touch monitor data (if it is not already recorded as touched)
-- this will touch a specific monitor, and add it if necessary.
touchMonitorData :: DemandMonitorData e z -> Int -> LnkUp z -> IO ()
touchMonitorData dmd ix lu =
    HT.lookup (dmd_mTable dmd) ix >>= \ mbmd ->
    let md = maybe (md_zero lu) id mbmd in
    unless ((st_expect . md_sig) md) $
        let md' = touchMD md in
        let rfct = dmd_mtouch dmd in
        HT.update (dmd_mTable dmd) ix md' >>
        readIORef rfct >>= \ ct ->
        (writeIORef rfct $! succ ct) >>
        ln_touch (md_lnk md') 

-- touch demand data (if it is not already touched)
-- first touch will cause touch of all present monitors
touchDemandData :: DemandMonitorData e z -> Int -> IO ()
touchDemandData dmd ix =
    HT.lookup (dmd_dTable dmd) ix >>= \ mbdd ->
    let dd = maybe (dd_zero) id mbdd in
    unless ((st_expect . dd_sig) dd) $
        let dd' = touchDD dd in
        HT.update (dmd_dTable dmd) ix dd' >>
        readIORef (dmd_dtouch dmd) >>= \ ct ->
        let ct' = succ ct in
        ct' `seq` writeIORef (dmd_dtouch dmd) ct' >>
        when (1 == ct') (touchAllMonitors dmd)

-- whenever we first touch a demand-data, we'll deliver a touch on
-- all monitors. This is not recorded anywhere, so may be redundant
-- with touchMonitorData. (That is okay; touch is idempotent; it 
-- matters more that the touch happens.)
touchAllMonitors            
        
        

    case mbmd of
        Nothing -> addTouched (md_zero lu)
        Just md -> 
            unless ((st_expect . md_sig) md) $
                addTouch
                let md' = touchMD md in
                
            let md = m
HT.insert (dmd_mTable 

-- strict modifyIORef (why isn't this standard?)
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' rf fn =

getDemandData :: DemandMonitorData e z -> Int -> IO (DemandData e)
getDemandData dmd ix = maybe dd_zero id `fmap` HT.lookup (dmd_dTable dmd) ix

expectingDemandUpdates :: DemandMonitorData e z -> IO Bool
expectingDemandUpdates dmd = (> 0) <$> readIORef (dmd_dtouch dmd)

expectingMonitorUpdates :: DemandMonitorData e z -> IO Bool
expectingMonitorUpdates dmd = (> 0) <$> readIORef (dmd_mtouch dmd)

expectingUpdates :: DemandMonitorData e z -> IO Bool
expectingUpdates dmd = 
    expectingDemandUpdates dmd >>= \ bD ->
    if bD then return True else
    expectingMonitorUpdates dmd

-- getMonitorData may need to `touch` a new monitor if the touch
-- count for demands is > 0. The LnkUp is only used when a fresh
-- MonitorData is needed.
getMonitorData :: DemandMonitorData e z -> Int -> LnkUp z -> IO (MonitorData z)
getMonitorData dmd ix lu = HT.lookup (dmd_mTable dmd) ix >>= maybe newMon return 
    where newMon = 
            expectingDemandUpdates dmd >>= \ bAutoTouch ->
            if (not bAutoTouch) then return md else
            HT.insert (dmd_mTable dmd) ix md >>
            touchMonitorData dmd ix >>
            

            
            
 let md = md_zero lu in
                    HT.insert (dmd_mTable dmd) ix md >>
                    readIORef (dmd_dtouch dmd) >>= \ nTouchCtD ->
                    when (nTouchCtD > 0) (touchMonitorData dmd ix lu) >>
                    readIORef (dmd_dtouch dmd) >>= \ nTouchCtD ->
                    when (nTouchCtD > 0) $
                        
                        touchMonitorData dmd ix lu
                    let bTouch = (nTouch > 0) in
                    if (not btouch) then 

-- get the demand data associated with a particular demand source.
dmd_getDemandData :: DemandMonitorData e z -> Int -> IO (DemandData e)
dmd_getDemandData dmd ix = maybe dd_zero id `fmap` HT.lookup (dmd_demand dmd) ix

-- get monitor data (subscription times, 
dmd_getMonitorData :: DemandMonitorData e z -> Int -> LnkUp z -> IO (MonitorData z)
dmd_getMonitorData dmd ix lu = 
    HT.lookup (dmd_monitor dmd) ix >>= \ mmd ->
    case mmd of
        Just md -> return md
        Nothing -> 
            let md = md_zero lu in

            



dmd_touchDemandFacet :: DemandMonitorData e z -> Int -> IO ()
dmd_touchDemandFacet dmd ix = 
    dmd_getDemandData dmd ix >>= \ dd ->
    let st = dd_sig dd in
    let bTouched = st_expect st in
    unless bTouched $
        let st' = st_poke st in
        let dd' = dd { dd_sig = st' } in
        HT.update (dmd_demand dmd) ix dd' >>
        readIORef (dmd_expect dmd) >>= \ nTC ->
            
        let dd' = dd { dd_sig = st
    if bTouched
        then 
error "TODO!"
 {-            loadIndex >>= \ ix ->
            dmd_getDemandData dmd ix >>= \ dd ->
            let st = dd_sig dd in
            unless (st_expect st) $  -}

dmd_updateDemandFacet :: DemandMonitorData e z -> Int -> SigUp e -> IO ()
dmd_updateDemandFacet dmd n su = error "TODO!"



dmd_touchMonitorFacet :: DemandMonitorData e z -> Int -> IO ()
dmd_touchMonitorFacet = error "TODO!"

dmd_updateMonitorFacet :: DemandMonitorData e z -> Int -> SigUp () -> IO ()
dmd_updateMonitorFacet = error "TODO!"


-- a demand facet will receive demand updates and touches. It does 
-- not propagate the signal! This is a sink. Client should apply a 
-- process for proper duration coupling. 
newDemandFacet :: DemandMonitorData e z -> IO (LnkUp e)
newDemandFacet dmd =
    newIORef Nothing >>= \ rfIndex -> -- GUID would be better for distributed system
    let loadIndex = getIndex (dmd_dTable dmd) rfIndex in
    let touch = loadIndex >>= dmd_touchDemandFacet dmd in
    let update su = loadIndex >>= \ ix -> dmd_updateDemandFacet dmd ix su in
    return $ LnkUp { ln_touch = touch, ln_update = update }

-- a newMonitorFacet will create a monitor facet. This tracks the
-- demand signal for masking against the active signal. 
newMonitorFacet :: DemandMonitorData e z -> LnkUp z -> IO (LnkUp ())
newMonitorFacet dmd lu =
    newIORef Nothing >>= \ rfIndex -> -- GUID would be better for distributed system
    let loadIndex = getIndex (dmd_mTable dmd) rfIndex in
    let touch = loadIndex >>= \ ix -> dmd_touchMonitorFacet dmd ix in
    let update su = loadIndex >>= \ ix -> dmd_updateMonitorFacet dmd ix su in
    return $ LnkUp { ln_touch = touch, ln_update = update }

-}



