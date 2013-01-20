{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}

-- | In most cases, point-free programming is quite lovely. However,
-- when I find mysefl expressing deep structural manipulations, such
-- as `(bfirst . bsecond . bfirst . bfirst . bsecond) action`, it 
-- can feel verbose. Use of local `where` clauses will help, but I
-- would often prefer to have pre-defined names for quick access.
--
-- This module, BStruct, provides many convenience operations. These
-- can be grouped into four classes:
--
--    deep application of behaviors (similar to Lisp setcaddr!)
--    deep extraction of signals from products (like Lisp caddr)
--    deep injection of signals that model choices
--    treating complex signals as a FORTH-like stack.
--
-- BStruct thus provides convenient deep structural abstractions.
--
-- See Also:                  
--   FRP.Sirea.Behavior       
--                            
module Sirea.BDeep
    ( bxf,    bxs,    bxff,   bxfs,   bxsf,   bxss -- DEEP EXTRACTION (30)
    , bxfff,  bxffs,  bxfsf,  bxfss,  bxsff,  bxsfs,  bxssf,  bxsss
    , bxffff, bxfffs, bxffsf, bxffss, bxfsff, bxfsfs, bxfssf, bxfsss
    , bxsfff, bxsffs, bxsfsf, bxsfss, bxssff, bxssfs, bxsssf, bxssss

    , binl,    binr,    binll,   binlr,   binrl,   binrr -- DEEP INJECTION (30)
    , binlll,  binllr,  binlrl,  binlrr,  binrll,  binrlr,  binrrl,  binrrr
    , binllll, binlllr, binllrl, binllrr, binlrll, binlrlr, binlrrl, binlrrr
    , binrlll, binrllr, binrlrl, binrlrr, binrrll, binrrlr, binrrrl, binrrrr

    , bonf,    bons,    bonff,   bonfs,   bonsf,   bonss -- DEEP (:&:) APPLICATION (30)
    , bonfff,  bonffs,  bonfsf,  bonfss,  bonsff,  bonsfs,  bonssf,  bonsss
    , bonffff, bonfffs, bonffsf, bonffss, bonfsff, bonfsfs, bonfssf, bonfsss
    , bonsfff, bonsffs, bonsfsf, bonsfss, bonssff, bonssfs, bonsssf, bonssss

    , bonl,    bonr,    bonll,   bonlr,   bonrl,   bonrr -- DEEP (:|:) APPLICATION (30)
    , bonlll,  bonllr,  bonlrl,  bonlrr,  bonrll,  bonrlr,  bonrrl,  bonrrr
    , bonllll, bonlllr, bonllrl, bonllrr, bonlrll, bonlrlr, bonlrrl, bonlrrr
    , bonrlll, bonrllr, bonrlrl, bonrlrr, bonrrll, bonrrlr, bonrrrl, bonrrrr

    ) where 

-- TODO: Also consider design of auto-wiring based on type tags. 

import Control.Category ((<<<))
import Sirea.Behavior 

-- | DEEP EXTRACTION FOR PRODUCTS (:&:)

bxf :: (BProd b) => b (e  :&: s0) e
bxs :: (BProd b) => b (f0 :&: e ) e
bxff :: (BProd b) => b ((e  :&: s0) :&: s1) e
bxfs :: (BProd b) => b ((f0 :&: e ) :&: s1) e
bxsf :: (BProd b) => b (f1 :&: (e  :&: s0)) e
bxss :: (BProd b) => b (f1 :&: (f0 :&: e )) e
bxfff :: (BProd b) => b (((e  :&: s0) :&: s1) :&: s2) e
bxffs :: (BProd b) => b (((f0 :&: e ) :&: s1) :&: s2) e
bxfsf :: (BProd b) => b ((f1 :&: (e  :&: s0)) :&: s2) e
bxfss :: (BProd b) => b ((f1 :&: (f0 :&: e )) :&: s2) e
bxsff :: (BProd b) => b (f2 :&: ((e  :&: s0) :&: s1)) e
bxsfs :: (BProd b) => b (f2 :&: ((f0 :&: e ) :&: s1)) e
bxssf :: (BProd b) => b (f2 :&: (f1 :&: (e  :&: s0))) e
bxsss :: (BProd b) => b (f2 :&: (f1 :&: (f0 :&: e ))) e
bxffff :: (BProd b) => b ((((e  :&: s0) :&: s1) :&: s2) :&: s3) e
bxfffs :: (BProd b) => b ((((f0 :&: e ) :&: s1) :&: s2) :&: s3) e
bxffsf :: (BProd b) => b (((f1 :&: (e  :&: s0)) :&: s2) :&: s3) e
bxffss :: (BProd b) => b (((f1 :&: (f0 :&: e )) :&: s2) :&: s3) e
bxfsff :: (BProd b) => b ((f2 :&: ((e  :&: s0) :&: s1)) :&: s3) e
bxfsfs :: (BProd b) => b ((f2 :&: ((f0 :&: e ) :&: s1)) :&: s3) e
bxfssf :: (BProd b) => b ((f2 :&: (f1 :&: (e  :&: s0))) :&: s3) e
bxfsss :: (BProd b) => b ((f2 :&: (f1 :&: (f0 :&: e ))) :&: s3) e
bxsfff :: (BProd b) => b (f3 :&: (((e  :&: s0) :&: s1) :&: s2)) e
bxsffs :: (BProd b) => b (f3 :&: (((f0 :&: e ) :&: s1) :&: s2)) e
bxsfsf :: (BProd b) => b (f3 :&: ((f1 :&: (e  :&: s0)) :&: s2)) e
bxsfss :: (BProd b) => b (f3 :&: ((f1 :&: (f0 :&: e )) :&: s2)) e
bxssff :: (BProd b) => b (f3 :&: (f2 :&: ((e  :&: s0) :&: s1))) e
bxssfs :: (BProd b) => b (f3 :&: (f2 :&: ((f0 :&: e ) :&: s1))) e
bxsssf :: (BProd b) => b (f3 :&: (f2 :&: (f1 :&: (e  :&: s0)))) e
bxssss :: (BProd b) => b (f3 :&: (f2 :&: (f1 :&: (f0 :&: e )))) e

bxf = bfst     -- for consistent naming
bxs = bsnd     -- for consistent naming
bxff = bxf >>> bxf
bxfs = bxf >>> bxs
bxsf = bxs >>> bxf
bxss = bxs >>> bxs
bxfff = bxf >>> bxff
bxffs = bxf >>> bxfs
bxfsf = bxf >>> bxsf
bxfss = bxf >>> bxss
bxsff = bxs >>> bxff
bxsfs = bxs >>> bxfs
bxssf = bxs >>> bxsf
bxsss = bxs >>> bxss
bxffff = bxf >>> bxfff
bxfffs = bxf >>> bxffs
bxffsf = bxf >>> bxfsf
bxffss = bxf >>> bxfss
bxfsff = bxf >>> bxsff
bxfsfs = bxf >>> bxsfs
bxfssf = bxf >>> bxssf
bxfsss = bxf >>> bxsss
bxsfff = bxs >>> bxfff
bxsffs = bxs >>> bxffs
bxsfsf = bxs >>> bxfsf
bxsfss = bxs >>> bxfss
bxssff = bxs >>> bxsff
bxssfs = bxs >>> bxsfs
bxsssf = bxs >>> bxssf
bxssss = bxs >>> bxsss

-- | DEEP INJECTION FOR SUMS (:|:)

-- binl is defined in FRP.Sirea.Behavior.
-- binr is defined in FRP.Sirea.Behavior.
binll :: (BSum b) => b e ((e  :|: r0) :|: r1)
binlr :: (BSum b) => b e ((l0 :|: e ) :|: r1)
binrl :: (BSum b) => b e (l1 :|: (e  :|: r0))
binrr :: (BSum b) => b e (l1 :|: (l0 :|: e ))
binlll :: (BSum b) => b e (((e  :|: r0) :|: r1) :|: r2)
binllr :: (BSum b) => b e (((l0 :|: e ) :|: r1) :|: r2)
binlrl :: (BSum b) => b e ((l1 :|: (e  :|: r0)) :|: r2)
binlrr :: (BSum b) => b e ((l1 :|: (l0 :|: e )) :|: r2)
binrll :: (BSum b) => b e (l2 :|: ((e  :|: r0) :|: r1))
binrlr :: (BSum b) => b e (l2 :|: ((l0 :|: e ) :|: r1))
binrrl :: (BSum b) => b e (l2 :|: (l1 :|: (e  :|: r0)))
binrrr :: (BSum b) => b e (l2 :|: (l1 :|: (l0 :|: e )))
binllll :: (BSum b) => b e ((((e  :|: r0) :|: r1) :|: r2) :|: r3)
binlllr :: (BSum b) => b e ((((l0 :|: e ) :|: r1) :|: r2) :|: r3)
binllrl :: (BSum b) => b e (((l1 :|: (e  :|: r0)) :|: r2) :|: r3)
binllrr :: (BSum b) => b e (((l1 :|: (l0 :|: e )) :|: r2) :|: r3)
binlrll :: (BSum b) => b e ((l2 :|: ((e  :|: r0) :|: r1)) :|: r3)
binlrlr :: (BSum b) => b e ((l2 :|: ((l0 :|: e ) :|: r1)) :|: r3)
binlrrl :: (BSum b) => b e ((l2 :|: (l1 :|: (e  :|: r0))) :|: r3)
binlrrr :: (BSum b) => b e ((l2 :|: (l1 :|: (l0 :|: e ))) :|: r3)
binrlll :: (BSum b) => b e (l3 :|: (((e  :|: r0) :|: r1) :|: r2))
binrllr :: (BSum b) => b e (l3 :|: (((l0 :|: e ) :|: r1) :|: r2))
binrlrl :: (BSum b) => b e (l3 :|: ((l1 :|: (e  :|: r0)) :|: r2))
binrlrr :: (BSum b) => b e (l3 :|: ((l1 :|: (l0 :|: e )) :|: r2))
binrrll :: (BSum b) => b e (l3 :|: (l2 :|: ((e  :|: r0) :|: r1)))
binrrlr :: (BSum b) => b e (l3 :|: (l2 :|: ((l0 :|: e ) :|: r1)))
binrrrl :: (BSum b) => b e (l3 :|: (l2 :|: (l1 :|: (e  :|: r0))))
binrrrr :: (BSum b) => b e (l3 :|: (l2 :|: (l1 :|: (l0 :|: e ))))

-- binl already defined in FRP.Sirea.Behavior.
-- binr already defined in FRP.Sirea.Behavior.
binll = binl <<< binl
binlr = binl <<< binr
binrl = binr <<< binl
binrr = binr <<< binr
binlll = binl <<< binll
binllr = binl <<< binlr
binlrl = binl <<< binrl
binlrr = binl <<< binrr
binrll = binr <<< binll
binrlr = binr <<< binlr
binrrl = binr <<< binrl
binrrr = binr <<< binrr
binllll = binl <<< binlll
binlllr = binl <<< binllr
binllrl = binl <<< binlrl
binllrr = binl <<< binlrr
binlrll = binl <<< binrll
binlrlr = binl <<< binrlr
binlrrl = binl <<< binrrl
binlrrr = binl <<< binrrr
binrlll = binr <<< binlll
binrllr = binr <<< binllr
binrlrl = binr <<< binlrl
binrlrr = binr <<< binlrr
binrrll = binr <<< binrll
binrrlr = binr <<< binrlr
binrrrl = binr <<< binrrl
binrrrr = binr <<< binrrr

-- | DEEP APPLICATIONS FOR SUMS (:|:)

bonl    :: (BSum b)  => b e e' -> b (e :|: r0) {- ~> -} (e' :|: r0)
bonr    :: (BSum b)  => b e e' -> b (l0 :|: e) {- ~> -} (l0 :|: e')
bonll   :: (BSum b)  => b e e' -> b ((e  :|: r0) :|: r1) {- ~> -} ((e' :|: r0) :|: r1)
bonlr   :: (BSum b)  => b e e' -> b ((l0 :|: e ) :|: r1) {- ~> -} ((l0 :|: e') :|: r1)
bonrl   :: (BSum b)  => b e e' -> b (l1 :|: (e  :|: r0)) {- ~> -} (l1 :|: (e' :|: r0))
bonrr   :: (BSum b)  => b e e' -> b (l1 :|: (l0 :|: e )) {- ~> -} (l1 :|: (l0 :|: e'))
bonlll  :: (BSum b)  => b e e' -> b (((e  :|: r0) :|: r1) :|: r2) {- ~> -} (((e' :|: r0) :|: r1) :|: r2)
bonllr  :: (BSum b)  => b e e' -> b (((l0 :|: e ) :|: r1) :|: r2) {- ~> -} (((l0 :|: e') :|: r1) :|: r2)
bonlrl  :: (BSum b)  => b e e' -> b ((l1 :|: (e  :|: r0)) :|: r2) {- ~> -} ((l1 :|: (e' :|: r0)) :|: r2)
bonlrr  :: (BSum b)  => b e e' -> b ((l1 :|: (l0 :|: e )) :|: r2) {- ~> -} ((l1 :|: (l0 :|: e')) :|: r2)
bonrll  :: (BSum b)  => b e e' -> b (l2 :|: ((e  :|: r0) :|: r1)) {- ~> -} (l2 :|: ((e' :|: r0) :|: r1))
bonrlr  :: (BSum b)  => b e e' -> b (l2 :|: ((l0 :|: e ) :|: r1)) {- ~> -} (l2 :|: ((l0 :|: e') :|: r1))
bonrrl  :: (BSum b)  => b e e' -> b (l2 :|: (l1 :|: (e  :|: r0))) {- ~> -} (l2 :|: (l1 :|: (e' :|: r0)))
bonrrr  :: (BSum b)  => b e e' -> b (l2 :|: (l1 :|: (l0 :|: e ))) {- ~> -} (l2 :|: (l1 :|: (l0 :|: e')))
bonllll :: (BSum b)  => b e e' -> b ((((e  :|: r0) :|: r1) :|: r2) :|: r3) {- ~> -} ((((e' :|: r0) :|: r1) :|: r2) :|: r3)
bonlllr :: (BSum b)  => b e e' -> b ((((l0 :|: e ) :|: r1) :|: r2) :|: r3) {- ~> -} ((((l0 :|: e') :|: r1) :|: r2) :|: r3)
bonllrl :: (BSum b)  => b e e' -> b (((l1 :|: (e  :|: r0)) :|: r2) :|: r3) {- ~> -} (((l1 :|: (e' :|: r0)) :|: r2) :|: r3)
bonllrr :: (BSum b)  => b e e' -> b (((l1 :|: (l0 :|: e )) :|: r2) :|: r3) {- ~> -} (((l1 :|: (l0 :|: e')) :|: r2) :|: r3)
bonlrll :: (BSum b)  => b e e' -> b ((l2 :|: ((e  :|: r0) :|: r1)) :|: r3) {- ~> -} ((l2 :|: ((e' :|: r0) :|: r1)) :|: r3)
bonlrlr :: (BSum b)  => b e e' -> b ((l2 :|: ((l0 :|: e ) :|: r1)) :|: r3) {- ~> -} ((l2 :|: ((l0 :|: e') :|: r1)) :|: r3)
bonlrrl :: (BSum b)  => b e e' -> b ((l2 :|: (l1 :|: (e  :|: r0))) :|: r3) {- ~> -} ((l2 :|: (l1 :|: (e' :|: r0))) :|: r3)
bonlrrr :: (BSum b)  => b e e' -> b ((l2 :|: (l1 :|: (l0 :|: e ))) :|: r3) {- ~> -} ((l2 :|: (l1 :|: (l0 :|: e'))) :|: r3)
bonrlll :: (BSum b)  => b e e' -> b (l3 :|: (((e  :|: r0) :|: r1) :|: r2)) {- ~> -} (l3 :|: (((e' :|: r0) :|: r1) :|: r2))
bonrllr :: (BSum b)  => b e e' -> b (l3 :|: (((l0 :|: e ) :|: r1) :|: r2)) {- ~> -} (l3 :|: (((l0 :|: e') :|: r1) :|: r2))
bonrlrl :: (BSum b)  => b e e' -> b (l3 :|: ((l1 :|: (e  :|: r0)) :|: r2)) {- ~> -} (l3 :|: ((l1 :|: (e' :|: r0)) :|: r2))
bonrlrr :: (BSum b)  => b e e' -> b (l3 :|: ((l1 :|: (l0 :|: e )) :|: r2)) {- ~> -} (l3 :|: ((l1 :|: (l0 :|: e')) :|: r2))
bonrrll :: (BSum b)  => b e e' -> b (l3 :|: (l2 :|: ((e  :|: r0) :|: r1))) {- ~> -} (l3 :|: (l2 :|: ((e' :|: r0) :|: r1)))
bonrrlr :: (BSum b)  => b e e' -> b (l3 :|: (l2 :|: ((l0 :|: e ) :|: r1))) {- ~> -} (l3 :|: (l2 :|: ((l0 :|: e') :|: r1)))
bonrrrl :: (BSum b)  => b e e' -> b (l3 :|: (l2 :|: (l1 :|: (e  :|: r0)))) {- ~> -} (l3 :|: (l2 :|: (l1 :|: (e' :|: r0))))
bonrrrr :: (BSum b)  => b e e' -> b (l3 :|: (l2 :|: (l1 :|: (l0 :|: e )))) {- ~> -} (l3 :|: (l2 :|: (l1 :|: (l0 :|: e'))))

bonl = bleft   -- for consistent naming
bonr = bright  -- for consistent naming
bonll = bonl . bonl
bonlr = bonl . bonr
bonrl = bonr . bonl
bonrr = bonr . bonr
bonlll = bonl . bonll
bonllr = bonl . bonlr
bonlrl = bonl . bonrl
bonlrr = bonl . bonrr 
bonrll = bonr . bonll
bonrlr = bonr . bonlr
bonrrl = bonr . bonrl
bonrrr = bonr . bonrr 
bonllll = bonl . bonlll
bonlllr = bonl . bonllr
bonllrl = bonl . bonlrl
bonllrr = bonl . bonlrr 
bonlrll = bonl . bonrll
bonlrlr = bonl . bonrlr
bonlrrl = bonl . bonrrl
bonlrrr = bonl . bonrrr 
bonrlll = bonr . bonlll
bonrllr = bonr . bonllr
bonrlrl = bonr . bonlrl
bonrlrr = bonr . bonlrr 
bonrrll = bonr . bonrll
bonrrlr = bonr . bonrlr
bonrrrl = bonr . bonrrl
bonrrrr = bonr . bonrrr 

-- | DEEP APPLICATIONS FOR PRODUCTS (:&:)

bonf    :: (BProd b) => b e e' -> b (e :&: s0) {- ~> -} (e' :&: s0)
bons    :: (BProd b) => b e e' -> b (f0 :&: e) {- ~> -} (f0 :&: e')
bonff   :: (BProd b) => b e e' -> b ((e  :&: s0) :&: s1) {- ~> -} ((e' :&: s0) :&: s1)
bonfs   :: (BProd b) => b e e' -> b ((f0 :&: e ) :&: s1) {- ~> -} ((f0 :&: e') :&: s1)
bonsf   :: (BProd b) => b e e' -> b (f1 :&: (e  :&: s0)) {- ~> -} (f1 :&: (e' :&: s0))
bonss   :: (BProd b) => b e e' -> b (f1 :&: (f0 :&: e )) {- ~> -} (f1 :&: (f0 :&: e'))
bonfff  :: (BProd b) => b e e' -> b (((e  :&: s0) :&: s1) :&: s2) {- ~> -} (((e' :&: s0) :&: s1) :&: s2)
bonffs  :: (BProd b) => b e e' -> b (((f0 :&: e ) :&: s1) :&: s2) {- ~> -} (((f0 :&: e') :&: s1) :&: s2)
bonfsf  :: (BProd b) => b e e' -> b ((f1 :&: (e  :&: s0)) :&: s2) {- ~> -} ((f1 :&: (e' :&: s0)) :&: s2)
bonfss  :: (BProd b) => b e e' -> b ((f1 :&: (f0 :&: e )) :&: s2) {- ~> -} ((f1 :&: (f0 :&: e')) :&: s2)
bonsff  :: (BProd b) => b e e' -> b (f2 :&: ((e  :&: s0) :&: s1)) {- ~> -} (f2 :&: ((e' :&: s0) :&: s1))
bonsfs  :: (BProd b) => b e e' -> b (f2 :&: ((f0 :&: e ) :&: s1)) {- ~> -} (f2 :&: ((f0 :&: e') :&: s1))
bonssf  :: (BProd b) => b e e' -> b (f2 :&: (f1 :&: (e  :&: s0))) {- ~> -} (f2 :&: (f1 :&: (e' :&: s0)))
bonsss  :: (BProd b) => b e e' -> b (f2 :&: (f1 :&: (f0 :&: e ))) {- ~> -} (f2 :&: (f1 :&: (f0 :&: e')))
bonffff :: (BProd b) => b e e' -> b ((((e  :&: s0) :&: s1) :&: s2) :&: s3) {- ~> -} ((((e' :&: s0) :&: s1) :&: s2) :&: s3)
bonfffs :: (BProd b) => b e e' -> b ((((f0 :&: e ) :&: s1) :&: s2) :&: s3) {- ~> -} ((((f0 :&: e') :&: s1) :&: s2) :&: s3)
bonffsf :: (BProd b) => b e e' -> b (((f1 :&: (e  :&: s0)) :&: s2) :&: s3) {- ~> -} (((f1 :&: (e' :&: s0)) :&: s2) :&: s3)
bonffss :: (BProd b) => b e e' -> b (((f1 :&: (f0 :&: e )) :&: s2) :&: s3) {- ~> -} (((f1 :&: (f0 :&: e')) :&: s2) :&: s3)
bonfsff :: (BProd b) => b e e' -> b ((f2 :&: ((e  :&: s0) :&: s1)) :&: s3) {- ~> -} ((f2 :&: ((e' :&: s0) :&: s1)) :&: s3)
bonfsfs :: (BProd b) => b e e' -> b ((f2 :&: ((f0 :&: e ) :&: s1)) :&: s3) {- ~> -} ((f2 :&: ((f0 :&: e') :&: s1)) :&: s3)
bonfssf :: (BProd b) => b e e' -> b ((f2 :&: (f1 :&: (e  :&: s0))) :&: s3) {- ~> -} ((f2 :&: (f1 :&: (e' :&: s0))) :&: s3)
bonfsss :: (BProd b) => b e e' -> b ((f2 :&: (f1 :&: (f0 :&: e ))) :&: s3) {- ~> -} ((f2 :&: (f1 :&: (f0 :&: e'))) :&: s3)
bonsfff :: (BProd b) => b e e' -> b (f3 :&: (((e  :&: s0) :&: s1) :&: s2)) {- ~> -} (f3 :&: (((e' :&: s0) :&: s1) :&: s2))
bonsffs :: (BProd b) => b e e' -> b (f3 :&: (((f0 :&: e ) :&: s1) :&: s2)) {- ~> -} (f3 :&: (((f0 :&: e') :&: s1) :&: s2))
bonsfsf :: (BProd b) => b e e' -> b (f3 :&: ((f1 :&: (e  :&: s0)) :&: s2)) {- ~> -} (f3 :&: ((f1 :&: (e' :&: s0)) :&: s2))
bonsfss :: (BProd b) => b e e' -> b (f3 :&: ((f1 :&: (f0 :&: e )) :&: s2)) {- ~> -} (f3 :&: ((f1 :&: (f0 :&: e')) :&: s2))
bonssff :: (BProd b) => b e e' -> b (f3 :&: (f2 :&: ((e  :&: s0) :&: s1))) {- ~> -} (f3 :&: (f2 :&: ((e' :&: s0) :&: s1)))
bonssfs :: (BProd b) => b e e' -> b (f3 :&: (f2 :&: ((f0 :&: e ) :&: s1))) {- ~> -} (f3 :&: (f2 :&: ((f0 :&: e') :&: s1)))
bonsssf :: (BProd b) => b e e' -> b (f3 :&: (f2 :&: (f1 :&: (e  :&: s0)))) {- ~> -} (f3 :&: (f2 :&: (f1 :&: (e' :&: s0))))
bonssss :: (BProd b) => b e e' -> b (f3 :&: (f2 :&: (f1 :&: (f0 :&: e )))) {- ~> -} (f3 :&: (f2 :&: (f1 :&: (f0 :&: e'))))

bonf = bfirst   -- for consistent naming
bons = bsecond  -- for consistent naming
bonff = bonf . bonf
bonfs = bonf . bons
bonsf = bons . bonf
bonss = bons . bons
bonfff = bonf . bonff
bonffs = bonf . bonfs
bonfsf = bonf . bonsf
bonfss = bonf . bonss 
bonsff = bons . bonff
bonsfs = bons . bonfs
bonssf = bons . bonsf
bonsss = bons . bonss 
bonffff = bonf . bonfff
bonfffs = bonf . bonffs
bonffsf = bonf . bonfsf
bonffss = bonf . bonfss 
bonfsff = bonf . bonsff
bonfsfs = bonf . bonsfs
bonfssf = bonf . bonssf
bonfsss = bonf . bonsss 
bonsfff = bons . bonfff
bonsffs = bons . bonffs
bonsfsf = bons . bonfsf
bonsfss = bons . bonfss 
bonssff = bons . bonsff
bonssfs = bons . bonsfs
bonsssf = bons . bonssf
bonssss = bons . bonsss 



-- | The stack-based operations treat complex signals like a stack.
-- The complex signal:
--
--    (x :&: (y :&: (z :&: ...
--
-- Is treated as a stack with at least 3 elements. The last element
-- is not accessible, so if we have a stack with exactly three 
-- elements:
--     
--    (x :&: (y :&: (z :&: omega)))
--
-- Then the omega element is not accessible to stack operations. The
-- usual action is to "create" the initial stack by duplicating
-- some behavior. 



-- TODO: convenience operators?
--  I've added Bdeep - eqvs. of bcadadr and setf bcadadr from Lisp
--  Need some stack-like operators
--      on (x :&: (y :&: (z :& ...
--      kswap, krotl(3,4,5,6,7), krotr(3,4,5,6,7), kdup, kover, 
--      kdisjoin would be feasible for some number of arguments.
--      ktake,kput
--  Maybe some support for data-driven dynamic patterns.
--      folds, recursion
--  



