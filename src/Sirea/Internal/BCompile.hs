{-# LANGUAGE TypeOperators, GADTs #-}

module Sirea.Internal.BCompile
    ( compileB
--    , latencyB
    ) where

import Sirea.Internal.BTypes
import Sirea.Internal.LTypes
import Control.Exception (assert)


-- | Compilation of Sirea `B` type behaviors. 
--
-- INPUTS:
--   B x y       - behavior to compile
--   LnkD LDT x  - timing of inputs
--   Lnk y       - destination, outputs
-- OUTPUTS:
--   LnkD LDT y  - timings of outputs
--   IO (Lnk x)  - constructor for input capability
--
-- Note: there is no integration with the Stepper at this point. Any
-- behaviors that need staging via the Stepper should have achieved 
-- it via MkLnk.
compileB :: B w x y -> LnkD LDT x -> Lnk y -> (LnkD LDT y, IO (Lnk x))
compileB bxy dtx lny =
    assert (ldt_valid dtx) $
    let (bxy', dty) = compileBC0 bxy dtx in
    let lnx = compileBC1 bxy' lny in
    assert (ldt_valid dty) $
    (dty, lnx)

-- | This is an initial left-to-right compile within a behavior. It
-- computes the timing properties of the resulting signal, applies 
-- time-dependent transforms (B_latent), and eliminates dead code on
-- input (at B_left).
compileBC0 :: B w x z -> LnkD LDT x -> (B w x z, LnkD LDT z)
compileBC0 (B_pipe bxy byz) dtx =
    let (bxy', dty) = compileBC0 bxy dtx in
    let (byz', dtz) = compileBC0 byz dty in
    assert (ldt_valid dty) $
    (B_pipe bxy' byz', dtz)
compileBC0 (B_first bef) dtx =
    let dte = lnd_fst dtx in
    let (bef', dtf) = compileBC0 bef dte in
    let dtz = LnkDProd dtf (lnd_snd dtx) in
    assert (ldt_anyLive dte == ldt_anyLive dtf) $
    (B_first bef', dtz)
compileBC0 (B_left bef) dtx =
    let dte = lnd_left dtx in
    if (ldt_anyLive dte) 
        then let (bef', dtf) = compileBC0 bef dte in
             let dtz = LnkDSum dtf (lnd_right dtx) in
             assert (ldt_anyLive dtf) $
             (B_left bef', dtz)
        else let dtf = tr_dead dte in
             let dtz = LnkDSum dtf (lnd_right dtx) in
             (B_left deadOnInputB, dtz)
compileBC0 (B_latent fn) dtx =
    compileBC0 (fn dtx) dtx
compileBC0 (B_tshift fn) dtx =
    let dtx' = fn dtx in
    (tshiftB dtx dtx', dtx')
compileBC0 bxz@(B_mkLnk fn _) dtx =
    (bxz, fn dtx)

-- | This is the right-to-left pass to build the behavior. It assumes
-- that B_latent and B_tshift have been handled by compileBC0.
compileBC1 :: B w x z -> Lnk z -> IO (Lnk x)
compileBC1 (B_pipe bxy byz) lnz = 
    compileBC1 byz lnz >>= compileBC1 bxy 
compileBC1 (B_first bef) lnz =
    compileBC1 bef (ln_fst lnz) >>= \ e ->
    return (LnkProd e (ln_snd lnz))
compileBC1 (B_left bef) lnz =
    compileBC1 bef (ln_left lnz) >>= \ e ->
    return (LnkSum e (ln_right lnz))
compileBC1 (B_latent _) _ =
    error "B_latent must be handled by compileBC0!"
compileBC1 (B_tshift _) _ =
    error "B_tshift must be handled by compileBC0!"
compileBC1 (B_mkLnk _ mkLnk) lnz =
    ln_build mkLnk lnz

-- | tshiftB turns a difference of `tshift` values into a MkLnk behavior.
-- This is used by the compiler to apply delays. 
tshiftB :: LnkD LDT x -> LnkD LDT x -> B w x x
tshiftB t0 tf = B_mkLnk id lnk
    where build = buildTshift t0 tf
          lnk = MkLnk { ln_build = return . build 
                      , ln_tsen = False
                      , ln_peek = 0
                      }

-- buildTshift will apply delays based on before/after LDT values
buildTshift :: LnkD LDT x -> LnkD LDT x -> Lnk x -> Lnk x
buildTshift _ _ LnkDead = LnkDead
buildTshift t0 tf (LnkProd x y) =
    let opx = buildTshift (lnd_fst t0) (lnd_fst tf) x in
    let opy = buildTshift (lnd_snd t0) (lnd_snd tf) y in
    LnkProd opx opy
buildTshift t0 tf (LnkSum x y) =
    let opx = buildTshift (lnd_left  t0) (lnd_left  tf) x in
    let opy = buildTshift (lnd_right t0) (lnd_right tf) y in
    LnkSum opx opy
buildTshift t0 tf (LnkSig lu) = 
    let dt0 = lnd_sig t0 in
    let dtf = lnd_sig tf in
    let dtDiff = (ldt_curr dtf) - (ldt_curr dt0) in
    assert (dtDiff >= 0) $
    if (0 == dtDiff) then LnkSig lu else
    LnkSig (ln_sumap (su_delay dtDiff) lu)


-- | deadOnInputB simply returns LnkDead. Assumption: already have
-- proven the input is dead. Injected by compileBC0 when B_left is
-- dead on input; goal is to prevent unnecessary construction of 
-- resources (such as partition threads).
deadOnInputB :: B w x y
deadOnInputB = B_mkLnk tr_dead lnkDead
    where lnkDead = MkLnk { ln_build = const (return LnkDead)
                          , ln_tsen = False, ln_peek = 0 } 



-- TODO (Maybe):
--  Compute maximum internal latency for a behavior.
--  Could be used for shutdown behavior.




