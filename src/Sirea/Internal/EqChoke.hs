
-- | EqChoke is an extension of Choke. Choke ensures temporal cycles
-- are processed robustly. EqChoke does the same, but also protects
-- against false updates.
--
-- A false update occurs when a change in an upstream signal has no
-- effect on a downstream signal, e.g. for a behavior that returns
-- only part of its input, or a simplified view of it. False updates
-- can have a negative impact on stability and efficiency, causing
-- rework downstream.
--
-- The `badjeqf` behavior will delay a false update just a little,
-- which helps eliminate rework because most processing of signals
-- regards the near-term rather than the distant future. EqChoke
-- goes further, enabling indefinite delay. (The reason `badjeqf`
-- limits delay is to protect snapshot consistency.)
--
--
module Sirea.Internal.EqChoke
    ( newEqChoke
    ) where

import Data.IORef
import Data.Unique
import qualified Data.Set as S
import Control.Applicative
import Control.Monad (unless, when)
import Control.Exception (assert)
import Sirea.Signal
import Sirea.Time
import Sirea.UnsafeLink
import Sirea.Internal.Tuning (dtDaggrHist, dtMdistHist, tAncient)
import Sirea.Internal.LTypes -- for convenient SigSt, et. al.


import Sirea.Partition
import Sirea.Internal.LTypes



-- identity 
-- cycle cut
-- psched
-- state
-- note: might need to check s_is_final to assume final updates
--   (of course, in that case stability should be at or above the
--   update time, so it might not be necessary).
--
--   But can't depend on DoneT since it's gone, nor s_term due to 
--   use with demand monitors (which always report active).
-- include any update for current instant

data Choke z = Choke
    { ck_link  :: !(LnkUp z)
    , ck_data  :: !(IORef (CKD z))
    , ck_ident :: !Unique
    }
data CKD z = CKD



-- | EqChoke will Choke and also eliminate many false updates.
newEqChoke :: (Ord z) => PSched -> LnkUp z -> IO (LnkUp z)
newEqChoke pd lu = error "TODO!"

{-

testCycle where
    testCycle =  
        readIORef (da_data da) >>= \ dd ->
        unless (dd_cutCyc dd) $
            let bCycleDetected = S.member (da_ident da) ns in
            if bCycleDetected 
                then onCycleDetect dd
                else fwdTestCycle
    fwdTestCycle =
        let ns' = S.insert (da_ident da) ns in
        ln_cycle (da_link da) ns'
    onCycleDetect dd =
        -- add one touch to block normal updates
        let tc' = succ (dd_touchCt dd) in
        let dd' = dd { dd_cutCyc = True, dd_touchCt = tc' } in
        writeIORef (da_data da) dd' >>
        onUpdPhase (da_psched da) cycleUpdate
    cycleUpdate = 
        -- A cycleUpdate will usually just idle, but it may deliver
        -- a pending update from a cycle computed in a prior step.
        error "TODO: handle cycles detected in DemandAggr"



-- choke data
data CK z = CK 
    { ck_sendby :: {-# UNPACK #-} !T
    , _ck_tmup   :: {-# UNPACK #-} !T
    , _ck_signal :: !(Sig z)
    }

-- | choke a signal so updates won't run too far ahead of stability.
-- This is modeled by translating some updates into idles until the
-- stability catches up. Rather than an absolute choke, this uses a
-- backoff algorithm: update rate will diminish with the distance to
-- the next update (past a given dt cutoff).
fchokeB0 :: (Monad m) => B0 m x x
fchokeB0 = mkLnkB0 id mkLnChoke

-- chokeT is a heuristic decision for when to deliver an update
-- relative to a future deadline. If this time is a growing function
-- of the difference, then cycles of any size can stabilize. For now
-- I'll just use a fraction of the difference.
--
-- The deadline itself is already offset from the 
chokeT :: T -> T -> T
chokeT tNow tDeadline =
    assert (tDeadline > tNow) $ -- deadline in future
    let dtMaxDelay = tDeadline `diffTime` tNow in
    tNow `addTime` (dtMaxDelay * 0.25)

mkLnChoke :: (Monad m) => LCapsM m x -> LnkM m x -> m (LnkM m x)
mkLnChoke _ LnkDead = return LnkDead
mkLnChoke lc (LnkProd x y) = 
    mkLnChoke (ln_fst lc) x >>= \ x' ->
    mkLnChoke (ln_snd lc) y >>= \ y' ->
    return (LnkProd x' y')
mkLnChoke lc (LnkSum x y) =
    mkLnChoke (ln_left lc) x >>= \ x' ->
    mkLnChoke (ln_right lc) y >>= \ y' ->
    return (LnkSum x' y')
mkLnChoke (LnkSig (LCX lc)) (LnkSig lu) =
    cc_newRef (lc_cc lc) Nothing >>= \ rf ->
    return (LnkSig (luChoke rf lu))
mkLnChoke lc _ = assert (ln_dead lc) $ return LnkDead

-- | In case I want to choke an external resource using same code.
wrapLnChoke :: LnkUp z -> IO (LnkUp z)
wrapLnChoke lu =
    newRefIO Nothing >>= \ rf ->
    return (luChoke rf lu)

luChoke :: (Monad m) => Ref m (Maybe (CK z)) -> LnkUpM m z -> LnkUpM m z
luChoke rf lu = LnkUp touch update idle cycle where
    cycle = ln_cycle lu
    touch = ln_touch lu
    idle tS =
        readRef rf >>= \ mbck ->
        case mbck of
            Nothing -> ln_idle lu tS
            Just (CK tDeliver tU0 s0) ->
                let bDeliver = inStableT tS >= tDeliver in
                if bDeliver
                    then writeRef rf Nothing >>
                         ln_update lu tS tU0 s0
                    else ln_idle lu tS
    update tS tU su =
        readRef rf >>= \ mbck ->
        let tNow = inStableT tS in
        let tCut = tU `subtractTime` dtFutureChoke in
        let tSendBy = maybe tCut (min tCut . ck_sendby) mbck in
        if (tNow >= tSendBy)
            then deliverCK mbck tS tU su
            else let ck' = delayCK mbck (chokeT tNow tCut) tU su in
                 writeRef' rf (Just $! ck') >>
                 ln_idle lu tS
    deliverCK mbck tS tU su = 
        writeRef rf Nothing >>
        case mbck of
            Nothing -> ln_update lu tS tU su
            Just (CK _ tU0 s0) ->
                if (tU > tU0)
                    then ln_update lu tS tU0 (s_switch' s0 tU su)
                    else ln_update lu tS tU su
    delayCK Nothing tChoke tU su = CK tChoke tU su
    delayCK (Just (CK tChoke0 tU0 s0)) tChoke tU su =
        let tChoke' = min tChoke0 tChoke in
        if (tU > tU0) then CK tChoke' tU0 (s_switch' s0 tU su)
                      else CK tChoke' tU su


-- | wrapLnEqShift enables IO resources to access the same logic as
-- the eqShiftB0 behavior.
wrapLnEqShift :: (a -> a -> Bool) -> LnkUp a -> IO (LnkUp a)
wrapLnEqShift eq lu =
    newRefIO s_never >>= \ rf ->
    return (luEqShift eq rf lu)

luEqShift :: (Monad m) => (a -> a -> Bool) -> Ref m (Sig a) 
         -> LnkUpM m a -> LnkUpM m a
luEqShift eq rf lu = LnkUp touch update idle cycle where
    touch = ln_touch lu
    cycle = ln_cycle lu
    idle tS = 
        modifyRef' rf (gcSig tS) >>
        ln_idle lu tS
    update tS tU su =
        let tSeek = inStableT tS `addTime` dtEqShift in
        readRef rf >>= \ s0 -> -- old signal for comparison
        let mbDiffT = firstDiffT eq s0 su tU tSeek in
        case mbDiffT of
            Nothing -> -- signals are equal forever
                writeRef' rf (gcSig tS s0) >>
                ln_idle lu tS
            Just tU' -> 
                let su' = s_trim su tU' in
                let sf = s_switch' s0 tU' su' in
                writeRef' rf (gcSig tS sf) >>
                ln_update lu tS tU' su'

-- find time of first difference between two signals in a region. OR
-- if we don't find a difference, seek any existing point of change 
-- in the signals to get an 'aligned' update. An aligned update will
-- avoid increasing frequency of updates within limits of a search.
-- 
-- If there is no aligned update, this will test for a final state.
-- If the signals are equal up to the final state (not just queried
-- domain) then this will return Nothing; otherwise, it returns the
-- alignment time up to which the signals are known to be equal.
firstDiffT :: (a -> b -> Bool) -> Sig a -> Sig b -> T -> T -> Maybe T
firstDiffT eq as bs tLower tUpper =
    if (tLower >= tUpper) then Just tLower else -- aligned on tLower
    let sigEq = s_full_zip activeWhileEq as bs in -- compare signals
    let sigEqList = sigToList sigEq tLower tUpper in 
    let cutL = L.dropWhile sampleActive sigEqList in
    case cutL of
        (x:_) -> Just $! fst x -- found a difference
        [] -> let tAlign = tUpper `addTime` dtAlign in
              let (x,xs) = s_sample_d sigEq tUpper tAlign in
              case x of
                Just (tU,_) -> Just tU -- align with an update
                Nothing ->
                    -- final test for whether signal is constant
                    if s_is_final xs tAlign 
                        then Nothing 
                        else Just tAlign 
    where activeWhileEq (Just x) (Just y) = 
                if (eq x y) then Just () 
                            else Nothing
          activeWhileEq Nothing Nothing = Just ()
          activeWhileEq _ _ = Nothing
          sampleActive = (/= Nothing) . snd

-- TODO: consider an additional optimization to recognize when we've
-- reached the end of the current signal (s_is_final) and avoid the
-- update entirely. This would only need to apply if the tAlign
-- test fails.
-}


