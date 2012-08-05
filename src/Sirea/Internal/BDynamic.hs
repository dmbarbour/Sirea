{-# LANGUAGE TypeOperators, GADTs #-}

-- | Implementation of the Dynamic behavior type for B.
-- Evaluation will logically sync the inputs, then 
-- physically sync the outputs (y). The inputs are not
-- immediately 
module Sirea.Internal.BDynamic 
    ( evalB
    ) where

import Prelude hiding(id,(.))
import Control.Category
import Control.Monad (unless)
import Control.Applicative
import Control.Exception (assert)
import Data.IORef
import Data.List (foldl')
import Data.Maybe (isNothing)
import Sirea.Internal.BTypes
import Sirea.Internal.STypes
import Sirea.Internal.LTypes
import Sirea.Internal.BImpl 
import Sirea.Internal.BCompile
import Sirea.Time
import Sirea.Signal
--import Sirea.Internal.BImpl

-- Evaluate a behavior provided dynamically.
--
-- If the evaluated behavior does not fit into dt seconds, it is
-- rejected and the error signal is returned. evalB reports a 
-- delay of dt seconds between inputs and y outputs, and a delay
-- of 0 between inputs and error.
--
-- Multiple dynamic behaviors are maintained in parallel, i.e. both
-- present and future. Ability to anticipate and compile behaviors
-- slightly ahead of needing them is valuable for managing latency
-- with dynamic behaviors. `touch` is used extensively to combine
-- updates that occur at the same instant.
--
-- evalB can respect some dead code optimizations:
--   dead on input (due to binl/binr reaching x)
--   dead on output (signal is unused at y)
-- however, evalB
--   cannot predict dead input in y based on dead input in x
--   cannot predict dead sink in x based on dead sink in y
-- consequently, eval tends to hinder optimizations.
-- 
evalB :: (SigInP p x) => DT -> B w (S p (B w x y) :&: x) (y :|: S p ())
evalB dt = 
    -- synchronization and preparation (& initial compile step)
    synchB >>> forceDelayB >>> evalPrepB dt >>>
    -- after evalPrep, have (S p (Either (B w x y) ()) :&: x)
    B_first (splitB >>> mirrorB >>> B_left dupB) >>> swapB >>>
    -- have (x :&: ((S p () :&: S p ()) :|: S p (B w x y)))
    disjoinB >>>
    -- have ((x :&: S p ()) :|: (x :&: S p (B w x y))
    B_left sndB >>> mirrorB >>> B_left swapB >>> 
    -- now have (S p (B w x y) :&: x) :|: S p (); will eval on left
    B_left (B_mkLnk (trEval dt) mkLnkEval)

-- apply first compilation stage to the input behavior, and filter
-- behaviors that won't fit into given time `dt`.
evalPrepB :: DT -> B w (S p (B w x y) :&: x) ((S p (Either (B w x y) ())) :&: x)
evalPrepB dt = B_latent $ \ tbx ->
    let tx = lnd_snd tbx in
    B_first (fmapB (evalPrep dt tx))

-- local implementation of sndB (lack access to Sirea.Behavior)
sndB :: B w (x :&: y) y
sndB = B_first trivialB >>> s1eB

-- trEval reports the delay incurred by the eval process
-- i.e. so that operations using `y` are properly timed
trEval :: DT -> LnkD LDT x -> LnkD LDT y
trEval dt t0 = 
    assert (evalSynched t0) $
    let dt0 = ldt_maxCurr t0 in
    let dtf = dt0 + dt in
    trDTF dtf t0

-- evalPrep will partially evaluate the inner behaviors, and decide 
-- whether they can be evaluated further (e.g. if they do not fit in
-- the alloted timeslot, they cannot be evaluated).
--
-- The output `B` is not quite the same as the input `B` due to the
-- first compilation phase eliminating B_latent expressions, and
-- applying any fixed delays and dead-code-on-input optimizations.
-- Basically, the `B` after this point is highly context dependent.
evalPrep :: DT -> LnkD LDT x -> B w x y -> Either (B w x y) ()
evalPrep dt ldtx =
    assert (evalSynched ldtx) $
    evalFitDelay dtf . precompile
    where dt0 = ldt_maxCurr ldtx
          dtf = dt0 + dt 
          precompile = flip compileBC0 ldtx

-- evalFitDelay is applied to each dynamic behavior. If it succeeds,
-- we can guarantee the resulting delay is equal to dtf on every value
-- entering `y`. It may fail if the behavior is too large for dtf.
evalFitDelay :: DT -> (B w x y, LnkD LDT y) -> Either (B w x y) ()
evalFitDelay dtf (b,t0) =
    if (ldt_maxGoal t0 > dtf) 
        then Right () -- cannot fit to delay
        else Left (b >>> delayToFit) 
    where tf = trDTF dtf t0
          delayToFit = B_mkLnk (trDTF dtf) lnkDTF
          lnkDTF = MkLnk { ln_build = return . buildTshift t0 tf
                         , ln_tsen = False, ln_peek = 0 }

-- delay to fit a particular time
trDTF :: DT -> LnkD LDT x -> LnkD LDT y
trDTF dtf t0 = 
    assert (ldt_valid t0) $
    assert (dtf >= ldt_maxGoal t0) $
    LnkDUnit $ LDT { ldt_curr = dtf
                   , ldt_goal = dtf
                   , ldt_live = ldt_anyLive t0 }

-- delay to fit (very similar to buildTshift)
buildDTF :: DT -> LnkD LDT y -> Lnk y -> Lnk y
buildDTF dtf t0 = buildTshift t0 (trDTF dtf t0)

-- a sanity test for assertions
evalSynched :: LnkD LDT x -> Bool
evalSynched ldt =
    (ldt_valid ldt) &&
    (ldt_maxGoal ldt == ldt_minGoal ldt) &&
    (ldt_maxCurr ldt == ldt_minCurr ldt) &&
    (ldt_maxGoal ldt == ldt_minCurr ldt)

    
mkLnkEval :: (SigMembr x) => MkLnk w (S p (B w x y) :&: x) y
mkLnkEval = MkLnk { ln_build = buildEval
                  , ln_tsen = True, ln_peek = 0 }

buildEval :: (SigMembr x) => Lnk y -> IO (Lnk (S p (B w x y) :&: x))
buildEval lnyFinal = 
    -- I cannot use lny directly; I need to merge the inputs from
    -- each dynamic behavior. So I construct many lny values, one
    -- for each behavior, that merge into lny.
    mkMergeLnkFactory lnyFinal >>= \ lnyFac ->
    newIORef 0 >>= \ rfIdx ->
    let mkLny = takeIdx rfIdx >>= lnyFac in
    let compile b = mkLny >>= compileBC1 b in

    -- I'll certainly need a record of the compiled behaviors.
    -- This will be [(T,Lnk x)], with the last signal in the
    -- list always receiving the full future of the `x` input
    -- signals. We'll anticipate several seconds of future for
    -- dynamic behaviors.
    newIORef [] >>= \ rfBLnk -> 
    undefined

{-

    -- TODO: I generically need a membrane for the full set of `x`
    -- inputs (and may as well include the B w x y input...). This
    -- membrane must indicate for each signal:
    --  the current record of that signal
    --  the update time for that signal (if updated since last send)
    --  whether signal has been touched
    -- For simplicity, go ahead and delay updates of `beval` until
    -- all input signals are available. 
    --  whether the signal has been 

    -- I need a few records with respect to Sig (B w x y) in 
    -- particular:
    --  (a) of the current and future behavior signal (SigSt)
    --  (b) of the currently compiled, active behaviors [(T,Lnk x)]
    -- After any update to (a) I'll generally need to update every
    -- element in (b). 
    newIORef st_zero >>= \ rfBSig ->
    newIORef [] >>= \ rfBComp ->

    -- only need to compile the last phase of the
    -- behaviors (compileBC0 is performed by evalB).

    -- I need to store some information about 
    newIORef st_zero >>


        
     
                
    

    -- 
    undefined
    -}

-- a trivial function for sequential indexes
takeIdx :: IORef Int -> IO Int
takeIdx rf =
    readIORef rf >>= \ n0 ->
    let n = succ n0 in
    n `seq` 
    writeIORef rf n >>
    return n



-- RESULTS LINK FACTORY.
--   Call factory for each dynamic behavior.
--   Factory should be called with different integer each time
--     (may safely reuse integers after GC eliminates old behaviors)
--   One result link for each dynamic behavior.
--   All links are merged into target `Lnk y`.
--   Dead outputs are preserved. 
--   Touches on multiple merge targets:
--     clear touch status on update
--     propagate only if no touched elements remain
--   GC from collection only on propagation
--     Q: when can I remove signal from list?
--     A: when s_term holds at stability 
--
-- This design would be reasonable even in a distributed system, but
-- with a slight variation that we'd be creating URLs/IDs for remote
-- hosts (with special support for merge-links). Authorizations and
-- expirations for those links could be achieved by HMAC provided on
-- establishing the merge-link.
mkMergeLnkFactory :: Lnk y -> IO (Int -> IO (Lnk y))
mkMergeLnkFactory LnkDead = 
    return (const $ return LnkDead)
mkMergeLnkFactory (LnkProd f s) = 
    mkMergeLnkFactory f >>= \ mkF ->
    mkMergeLnkFactory s >>= \ mkS ->
    let mkProd n = mkF n >>= \ f' ->
                   mkS n >>= \ s' ->
                   return (LnkProd f' s')
    in return mkProd
mkMergeLnkFactory (LnkSum l r) =
    mkMergeLnkFactory l >>= \ mkL ->
    mkMergeLnkFactory r >>= \ mkR ->
    let mkSum n = mkL n >>= \ l' ->
                  mkR n >>= \ r' ->
                  return (LnkSum l' r')
    in return mkSum
mkMergeLnkFactory (LnkSig lu) =
    newIORef [] >>= \ rfSig ->
    newIORef Nothing >>= \ rfT ->
    let mkLnk n = 
          let lu' = fnMergeEvaluator n rfSig rfT lu in
          return (LnkSig lu')
    in return mkLnk

-- merge signal updates for a given index. 
-- this is performed single-threaded in some partition 
-- (the destination partition, not necessarily where eval starts)
-- touch can ensure all updates are in place before propagating
--
-- Current implementation doesn't optimize for large merge lists,
-- so if there are a lot of rapid behavior changes in a short 
-- period of time this could become expensive. I.e. cost ~ frequency^2
fnMergeEvaluator :: Int -> IORef [(Int, SigSt a)] -> IORef (Maybe T) 
                 -> LnkUp a -> LnkUp a
fnMergeEvaluator idx rfSt rfT lu = LnkUp { ln_touch = touch, ln_update = update }
    where touch = 
            -- poke the given SigSt
            readIORef rfSt >>= \ lSt ->
            let lSt' = fnModifyEvaluationIdx idx st_poke lSt in
            let wasAlreadyExpecting = any (st_expect . snd) lSt in
            writeIORef rfSt lSt' >>
            -- forward the touch if this is the first one
            -- once touched, must send at least a stability update!
            unless wasAlreadyExpecting (ln_touch lu)
          update su = 
            -- track the signals and stability
            readIORef rfSt >>= \ lSt ->
            let lSt' = fnModifyEvaluationIdx idx (st_sigup su) lSt in
            let stillExpectingUpdates = any (st_expect . snd) lSt' in
            writeIORef rfSt lSt' >>
            -- track the earliest update time for all updates
            readIORef rfT >>= \ tUp ->
            let tu = snd <$> su_state su in 
            let tUp' = leastTime tUp tu in
            writeIORef rfT tUp' >>
            -- if no longer expecting updates, emit merged signal
            unless stillExpectingUpdates emitMergedSignal
          emitMergedSignal =
            -- take and reset the time of earliest update
            readIORef rfT >>= \ tUp ->
            writeIORef rfT Nothing >>
            -- merge past, present, future evaluations; GC past evaluations
            readIORef rfSt >>= \ lSt ->
            let sMerged = foldr s_merge s_never $ map (st_signal . snd) lSt in
            let tStable = foldl' leastTime Nothing $ map (st_stable . snd) lSt in
            let lSt' = case tStable of
                          Nothing -> [] -- all elements stable forever.
                          Just tm -> foldr (fnEvalGC tm) [] lSt in
            lSt' `seq` 
            writeIORef rfSt lSt' >>
            -- generate and deliver the update
            case tUp of
                Nothing -> -- stability update only
                    let su = SigUp { su_state = Nothing
                                   , su_stable = tStable } in
                    ln_update lu su
                Just tu -> -- signal update starting at tu
                    let sMergedAndTrimmed = s_trim sMerged tu in
                    let su = SigUp { su_state = Just(sMergedAndTrimmed,tu)
                                   , su_stable = tStable } in
                    ln_update lu su

-- leastTime where `Nothing` is forever (upper bound)
leastTime :: Maybe T -> Maybe T -> Maybe T
leastTime l r = (min <$> l <*> r) <|> l <|> r

-- fnEvalGC - rebuilds the list assuming don't need information
-- prior to given time
fnEvalGC :: T -> (Int,SigSt a) -> [(Int,SigSt a)] -> [(Int,SigSt a)]
fnEvalGC tm (ix,st) lst =
    let (x,s') = s_sample (st_signal st) tm in
    if (isNothing x && s_is_final s' tm)
        then lst -- GC the whole index (may be added again by update)
        else let st' = st { st_signal = s' } in
             st' `seq` (ix,st'):lst -- GC signal at index

-- modify the SigSt at given index (substituting st_zero if first reference)
--
-- maintains an ordering from highest index to lowest index, which can help
-- resist glitches from concurrency during transitions between behaviors 
-- (but is not essential for correct behavior)
fnModifyEvaluationIdx :: Int -> (SigSt a -> SigSt a) -> [(Int,SigSt a)] -> [(Int,SigSt a)]
fnModifyEvaluationIdx idx fn [] = (idx,fn st_zero):[]
fnModifyEvaluationIdx idx fn (x@(n,st):r) =
    if (n > idx) then x:(fnModifyEvaluationIdx idx fn r) else
    if (n < idx) then (idx,fn st_zero):x:r else
    (n,fn st):r
 

            
-- CONCERN:
--   At moment, don't have any way to produce an `Lnk x` from
--   knowledge `SigInP x`. 
-- Probably need a typeclass for constructing Lnk x.
--   some sort of membrane class similar to SigInP



