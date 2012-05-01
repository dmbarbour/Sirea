-- NOTE: This file is generated programmatically, and should not be 
-- modified by hand. Modify and rerun BdeepGen instead.

                           
-- | Bdeep provides convenient shorthand utility behaviors,  simple
-- composites of RDP behaviors for: 
--                            
--   deep application of behaviors 
--   deep extraction of products 
--                            
-- For example, rather than:  
--                            
-- > (bfirst . bsecond . bleft . bsecond) action 
--                            
-- Developers can simply write: 
--                            
-- > bonfsls action           
--                            
-- This reads as `behavior on first second left second`. Note that
-- the order of characters corresponds to the path of application.
-- These are defined up to 4 deep, for a total 340 functions. The
-- shallow elements bonf (= bfirst), bons, bonl, bonr are included
-- for consistency.           
--                            
-- Similarly, rather than:    
--                            
-- > bfst >>> bsnd >>> bfst >>> bfst
--                            
-- Developers can write:      
--                            
-- > bxfsff                   
--                            
-- This reads as `behavior extract first second first first second.
-- The forward order of characters corresponds to the path. This is
-- reverse the order of Lisp's car,cdr,cadadr conventions. 
--                            
-- To extract multiple elements, use the (&&&) behavior: 
--                            
--  > bxfsfs &&& bxssfs       
--                            
-- Note: extraction is NOT possible for left/right, due to duration 
-- coupling constraints. These are also defined up to 4 depth, for 
-- total 32 functions.       
--                           
-- The dual of bx* is also provided, for completeness and another 30
-- functions for injection - `binlrr` is `binl <<< binr <<< binr`.
-- These might be useful if a behavior is a big switch, but that may 
-- be an anti-pattern (better to use a lot of small behaviors).
--                            
-- If 4 depth isn't enough, these operations are readily composed.
-- E.g. consider a stack-like environment:
--   extract 8th element: bxssss >>> bxsssf 
--   operate on 8th element: (bonssss . bonsssf) op 
-- Though FRP.Sirea.Bstack provides richer Forth-inspired operators
-- for pointfree stack operations.
--                            
-- Unfortunately, RDP behaviors are not compatible with Haskell's 
-- Arrow syntax, which would make it easier to provide local names
-- to complex parameters. At the moment, Sirea must be used in a
-- point-free style. These deep operations should keep this from
-- becoming too tedious.      
--                            
-- See Also:                  
--   FRP.Sirea.Behavior       
--   FRP.Sirea.Bstack         
--                            
{-# LANGUAGE TypeOperators #-} 
module FRP.Sirea.Bdeep 
    ( bxf, bxs, bxff, bxfs, bxsf, bxss
    , bxfff, bxffs, bxfsf, bxfss, bxsff, bxsfs
    , bxssf, bxsss, bxffff, bxfffs, bxffsf, bxffss
    , bxfsff, bxfsfs, bxfssf, bxfsss, bxsfff, bxsffs
    , bxsfsf, bxsfss, bxssff, bxssfs, bxsssf, bxssss
    , binl, binr, binll, binlr, binrl, binrr
    , binlll, binllr, binlrl, binlrr, binrll, binrlr
    , binrrl, binrrr, binllll, binlllr, binllrl, binllrr
    , binlrll, binlrlr, binlrrl, binlrrr, binrlll, binrllr
    , binrlrl, binrlrr, binrrll, binrrlr, binrrrl, binrrrr
    , bonf, bons, bonl, bonr, bonff, bonfs
    , bonfl, bonfr, bonsf, bonss, bonsl, bonsr
    , bonlf, bonls, bonll, bonlr, bonrf, bonrs
    , bonrl, bonrr, bonfff, bonffs, bonffl, bonffr
    , bonfsf, bonfss, bonfsl, bonfsr, bonflf, bonfls
    , bonfll, bonflr, bonfrf, bonfrs, bonfrl, bonfrr
    , bonsff, bonsfs, bonsfl, bonsfr, bonssf, bonsss
    , bonssl, bonssr, bonslf, bonsls, bonsll, bonslr
    , bonsrf, bonsrs, bonsrl, bonsrr, bonlff, bonlfs
    , bonlfl, bonlfr, bonlsf, bonlss, bonlsl, bonlsr
    , bonllf, bonlls, bonlll, bonllr, bonlrf, bonlrs
    , bonlrl, bonlrr, bonrff, bonrfs, bonrfl, bonrfr
    , bonrsf, bonrss, bonrsl, bonrsr, bonrlf, bonrls
    , bonrll, bonrlr, bonrrf, bonrrs, bonrrl, bonrrr
    , bonffff, bonfffs, bonfffl, bonfffr, bonffsf, bonffss
    , bonffsl, bonffsr, bonfflf, bonffls, bonffll, bonfflr
    , bonffrf, bonffrs, bonffrl, bonffrr, bonfsff, bonfsfs
    , bonfsfl, bonfsfr, bonfssf, bonfsss, bonfssl, bonfssr
    , bonfslf, bonfsls, bonfsll, bonfslr, bonfsrf, bonfsrs
    , bonfsrl, bonfsrr, bonflff, bonflfs, bonflfl, bonflfr
    , bonflsf, bonflss, bonflsl, bonflsr, bonfllf, bonflls
    , bonflll, bonfllr, bonflrf, bonflrs, bonflrl, bonflrr
    , bonfrff, bonfrfs, bonfrfl, bonfrfr, bonfrsf, bonfrss
    , bonfrsl, bonfrsr, bonfrlf, bonfrls, bonfrll, bonfrlr
    , bonfrrf, bonfrrs, bonfrrl, bonfrrr, bonsfff, bonsffs
    , bonsffl, bonsffr, bonsfsf, bonsfss, bonsfsl, bonsfsr
    , bonsflf, bonsfls, bonsfll, bonsflr, bonsfrf, bonsfrs
    , bonsfrl, bonsfrr, bonssff, bonssfs, bonssfl, bonssfr
    , bonsssf, bonssss, bonsssl, bonsssr, bonsslf, bonssls
    , bonssll, bonsslr, bonssrf, bonssrs, bonssrl, bonssrr
    , bonslff, bonslfs, bonslfl, bonslfr, bonslsf, bonslss
    , bonslsl, bonslsr, bonsllf, bonslls, bonslll, bonsllr
    , bonslrf, bonslrs, bonslrl, bonslrr, bonsrff, bonsrfs
    , bonsrfl, bonsrfr, bonsrsf, bonsrss, bonsrsl, bonsrsr
    , bonsrlf, bonsrls, bonsrll, bonsrlr, bonsrrf, bonsrrs
    , bonsrrl, bonsrrr, bonlfff, bonlffs, bonlffl, bonlffr
    , bonlfsf, bonlfss, bonlfsl, bonlfsr, bonlflf, bonlfls
    , bonlfll, bonlflr, bonlfrf, bonlfrs, bonlfrl, bonlfrr
    , bonlsff, bonlsfs, bonlsfl, bonlsfr, bonlssf, bonlsss
    , bonlssl, bonlssr, bonlslf, bonlsls, bonlsll, bonlslr
    , bonlsrf, bonlsrs, bonlsrl, bonlsrr, bonllff, bonllfs
    , bonllfl, bonllfr, bonllsf, bonllss, bonllsl, bonllsr
    , bonlllf, bonllls, bonllll, bonlllr, bonllrf, bonllrs
    , bonllrl, bonllrr, bonlrff, bonlrfs, bonlrfl, bonlrfr
    , bonlrsf, bonlrss, bonlrsl, bonlrsr, bonlrlf, bonlrls
    , bonlrll, bonlrlr, bonlrrf, bonlrrs, bonlrrl, bonlrrr
    , bonrfff, bonrffs, bonrffl, bonrffr, bonrfsf, bonrfss
    , bonrfsl, bonrfsr, bonrflf, bonrfls, bonrfll, bonrflr
    , bonrfrf, bonrfrs, bonrfrl, bonrfrr, bonrsff, bonrsfs
    , bonrsfl, bonrsfr, bonrssf, bonrsss, bonrssl, bonrssr
    , bonrslf, bonrsls, bonrsll, bonrslr, bonrsrf, bonrsrs
    , bonrsrl, bonrsrr, bonrlff, bonrlfs, bonrlfl, bonrlfr
    , bonrlsf, bonrlss, bonrlsl, bonrlsr, bonrllf, bonrlls
    , bonrlll, bonrllr, bonrlrf, bonrlrs, bonrlrl, bonrlrr
    , bonrrff, bonrrfs, bonrrfl, bonrrfr, bonrrsf, bonrrss
    , bonrrsl, bonrrsr, bonrrlf, bonrrls, bonrrll, bonrrlr
    , bonrrrf, bonrrrs, bonrrrl, bonrrrr
    ) where 
import Control.Category ((<<<))
import FRP.Sirea.Behavior 


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
bonf :: (BProd b        ) => b e e' -> b (e  :&: s0) (e' :&: s0)
bons :: (BProd b        ) => b e e' -> b (f0 :&: e ) (f0 :&: e')
bonl :: (         BSum b) => b e e' -> b (e  :|: r0) (e' :|: r0)
bonr :: (         BSum b) => b e e' -> b (l0 :|: e ) (l0 :|: e')
bonff :: (BProd b        ) => b e e' -> b ((e  :&: s0) :&: s1) ((e' :&: s0) :&: s1)
bonfs :: (BProd b        ) => b e e' -> b ((f0 :&: e ) :&: s1) ((f0 :&: e') :&: s1)
bonfl :: (BProd b, BSum b) => b e e' -> b ((e  :|: r0) :&: s1) ((e' :|: r0) :&: s1)
bonfr :: (BProd b, BSum b) => b e e' -> b ((l0 :|: e ) :&: s1) ((l0 :|: e') :&: s1)
bonsf :: (BProd b        ) => b e e' -> b (f1 :&: (e  :&: s0)) (f1 :&: (e' :&: s0))
bonss :: (BProd b        ) => b e e' -> b (f1 :&: (f0 :&: e )) (f1 :&: (f0 :&: e'))
bonsl :: (BProd b, BSum b) => b e e' -> b (f1 :&: (e  :|: r0)) (f1 :&: (e' :|: r0))
bonsr :: (BProd b, BSum b) => b e e' -> b (f1 :&: (l0 :|: e )) (f1 :&: (l0 :|: e'))
bonlf :: (BProd b, BSum b) => b e e' -> b ((e  :&: s0) :|: r1) ((e' :&: s0) :|: r1)
bonls :: (BProd b, BSum b) => b e e' -> b ((f0 :&: e ) :|: r1) ((f0 :&: e') :|: r1)
bonll :: (         BSum b) => b e e' -> b ((e  :|: r0) :|: r1) ((e' :|: r0) :|: r1)
bonlr :: (         BSum b) => b e e' -> b ((l0 :|: e ) :|: r1) ((l0 :|: e') :|: r1)
bonrf :: (BProd b, BSum b) => b e e' -> b (l1 :|: (e  :&: s0)) (l1 :|: (e' :&: s0))
bonrs :: (BProd b, BSum b) => b e e' -> b (l1 :|: (f0 :&: e )) (l1 :|: (f0 :&: e'))
bonrl :: (         BSum b) => b e e' -> b (l1 :|: (e  :|: r0)) (l1 :|: (e' :|: r0))
bonrr :: (         BSum b) => b e e' -> b (l1 :|: (l0 :|: e )) (l1 :|: (l0 :|: e'))
bonfff :: (BProd b        ) => b e e' -> b (((e  :&: s0) :&: s1) :&: s2) (((e' :&: s0) :&: s1) :&: s2)
bonffs :: (BProd b        ) => b e e' -> b (((f0 :&: e ) :&: s1) :&: s2) (((f0 :&: e') :&: s1) :&: s2)
bonffl :: (BProd b, BSum b) => b e e' -> b (((e  :|: r0) :&: s1) :&: s2) (((e' :|: r0) :&: s1) :&: s2)
bonffr :: (BProd b, BSum b) => b e e' -> b (((l0 :|: e ) :&: s1) :&: s2) (((l0 :|: e') :&: s1) :&: s2)
bonfsf :: (BProd b        ) => b e e' -> b ((f1 :&: (e  :&: s0)) :&: s2) ((f1 :&: (e' :&: s0)) :&: s2)
bonfss :: (BProd b        ) => b e e' -> b ((f1 :&: (f0 :&: e )) :&: s2) ((f1 :&: (f0 :&: e')) :&: s2)
bonfsl :: (BProd b, BSum b) => b e e' -> b ((f1 :&: (e  :|: r0)) :&: s2) ((f1 :&: (e' :|: r0)) :&: s2)
bonfsr :: (BProd b, BSum b) => b e e' -> b ((f1 :&: (l0 :|: e )) :&: s2) ((f1 :&: (l0 :|: e')) :&: s2)
bonflf :: (BProd b, BSum b) => b e e' -> b (((e  :&: s0) :|: r1) :&: s2) (((e' :&: s0) :|: r1) :&: s2)
bonfls :: (BProd b, BSum b) => b e e' -> b (((f0 :&: e ) :|: r1) :&: s2) (((f0 :&: e') :|: r1) :&: s2)
bonfll :: (BProd b, BSum b) => b e e' -> b (((e  :|: r0) :|: r1) :&: s2) (((e' :|: r0) :|: r1) :&: s2)
bonflr :: (BProd b, BSum b) => b e e' -> b (((l0 :|: e ) :|: r1) :&: s2) (((l0 :|: e') :|: r1) :&: s2)
bonfrf :: (BProd b, BSum b) => b e e' -> b ((l1 :|: (e  :&: s0)) :&: s2) ((l1 :|: (e' :&: s0)) :&: s2)
bonfrs :: (BProd b, BSum b) => b e e' -> b ((l1 :|: (f0 :&: e )) :&: s2) ((l1 :|: (f0 :&: e')) :&: s2)
bonfrl :: (BProd b, BSum b) => b e e' -> b ((l1 :|: (e  :|: r0)) :&: s2) ((l1 :|: (e' :|: r0)) :&: s2)
bonfrr :: (BProd b, BSum b) => b e e' -> b ((l1 :|: (l0 :|: e )) :&: s2) ((l1 :|: (l0 :|: e')) :&: s2)
bonsff :: (BProd b        ) => b e e' -> b (f2 :&: ((e  :&: s0) :&: s1)) (f2 :&: ((e' :&: s0) :&: s1))
bonsfs :: (BProd b        ) => b e e' -> b (f2 :&: ((f0 :&: e ) :&: s1)) (f2 :&: ((f0 :&: e') :&: s1))
bonsfl :: (BProd b, BSum b) => b e e' -> b (f2 :&: ((e  :|: r0) :&: s1)) (f2 :&: ((e' :|: r0) :&: s1))
bonsfr :: (BProd b, BSum b) => b e e' -> b (f2 :&: ((l0 :|: e ) :&: s1)) (f2 :&: ((l0 :|: e') :&: s1))
bonssf :: (BProd b        ) => b e e' -> b (f2 :&: (f1 :&: (e  :&: s0))) (f2 :&: (f1 :&: (e' :&: s0)))
bonsss :: (BProd b        ) => b e e' -> b (f2 :&: (f1 :&: (f0 :&: e ))) (f2 :&: (f1 :&: (f0 :&: e')))
bonssl :: (BProd b, BSum b) => b e e' -> b (f2 :&: (f1 :&: (e  :|: r0))) (f2 :&: (f1 :&: (e' :|: r0)))
bonssr :: (BProd b, BSum b) => b e e' -> b (f2 :&: (f1 :&: (l0 :|: e ))) (f2 :&: (f1 :&: (l0 :|: e')))
bonslf :: (BProd b, BSum b) => b e e' -> b (f2 :&: ((e  :&: s0) :|: r1)) (f2 :&: ((e' :&: s0) :|: r1))
bonsls :: (BProd b, BSum b) => b e e' -> b (f2 :&: ((f0 :&: e ) :|: r1)) (f2 :&: ((f0 :&: e') :|: r1))
bonsll :: (BProd b, BSum b) => b e e' -> b (f2 :&: ((e  :|: r0) :|: r1)) (f2 :&: ((e' :|: r0) :|: r1))
bonslr :: (BProd b, BSum b) => b e e' -> b (f2 :&: ((l0 :|: e ) :|: r1)) (f2 :&: ((l0 :|: e') :|: r1))
bonsrf :: (BProd b, BSum b) => b e e' -> b (f2 :&: (l1 :|: (e  :&: s0))) (f2 :&: (l1 :|: (e' :&: s0)))
bonsrs :: (BProd b, BSum b) => b e e' -> b (f2 :&: (l1 :|: (f0 :&: e ))) (f2 :&: (l1 :|: (f0 :&: e')))
bonsrl :: (BProd b, BSum b) => b e e' -> b (f2 :&: (l1 :|: (e  :|: r0))) (f2 :&: (l1 :|: (e' :|: r0)))
bonsrr :: (BProd b, BSum b) => b e e' -> b (f2 :&: (l1 :|: (l0 :|: e ))) (f2 :&: (l1 :|: (l0 :|: e')))
bonlff :: (BProd b, BSum b) => b e e' -> b (((e  :&: s0) :&: s1) :|: r2) (((e' :&: s0) :&: s1) :|: r2)
bonlfs :: (BProd b, BSum b) => b e e' -> b (((f0 :&: e ) :&: s1) :|: r2) (((f0 :&: e') :&: s1) :|: r2)
bonlfl :: (BProd b, BSum b) => b e e' -> b (((e  :|: r0) :&: s1) :|: r2) (((e' :|: r0) :&: s1) :|: r2)
bonlfr :: (BProd b, BSum b) => b e e' -> b (((l0 :|: e ) :&: s1) :|: r2) (((l0 :|: e') :&: s1) :|: r2)
bonlsf :: (BProd b, BSum b) => b e e' -> b ((f1 :&: (e  :&: s0)) :|: r2) ((f1 :&: (e' :&: s0)) :|: r2)
bonlss :: (BProd b, BSum b) => b e e' -> b ((f1 :&: (f0 :&: e )) :|: r2) ((f1 :&: (f0 :&: e')) :|: r2)
bonlsl :: (BProd b, BSum b) => b e e' -> b ((f1 :&: (e  :|: r0)) :|: r2) ((f1 :&: (e' :|: r0)) :|: r2)
bonlsr :: (BProd b, BSum b) => b e e' -> b ((f1 :&: (l0 :|: e )) :|: r2) ((f1 :&: (l0 :|: e')) :|: r2)
bonllf :: (BProd b, BSum b) => b e e' -> b (((e  :&: s0) :|: r1) :|: r2) (((e' :&: s0) :|: r1) :|: r2)
bonlls :: (BProd b, BSum b) => b e e' -> b (((f0 :&: e ) :|: r1) :|: r2) (((f0 :&: e') :|: r1) :|: r2)
bonlll :: (         BSum b) => b e e' -> b (((e  :|: r0) :|: r1) :|: r2) (((e' :|: r0) :|: r1) :|: r2)
bonllr :: (         BSum b) => b e e' -> b (((l0 :|: e ) :|: r1) :|: r2) (((l0 :|: e') :|: r1) :|: r2)
bonlrf :: (BProd b, BSum b) => b e e' -> b ((l1 :|: (e  :&: s0)) :|: r2) ((l1 :|: (e' :&: s0)) :|: r2)
bonlrs :: (BProd b, BSum b) => b e e' -> b ((l1 :|: (f0 :&: e )) :|: r2) ((l1 :|: (f0 :&: e')) :|: r2)
bonlrl :: (         BSum b) => b e e' -> b ((l1 :|: (e  :|: r0)) :|: r2) ((l1 :|: (e' :|: r0)) :|: r2)
bonlrr :: (         BSum b) => b e e' -> b ((l1 :|: (l0 :|: e )) :|: r2) ((l1 :|: (l0 :|: e')) :|: r2)
bonrff :: (BProd b, BSum b) => b e e' -> b (l2 :|: ((e  :&: s0) :&: s1)) (l2 :|: ((e' :&: s0) :&: s1))
bonrfs :: (BProd b, BSum b) => b e e' -> b (l2 :|: ((f0 :&: e ) :&: s1)) (l2 :|: ((f0 :&: e') :&: s1))
bonrfl :: (BProd b, BSum b) => b e e' -> b (l2 :|: ((e  :|: r0) :&: s1)) (l2 :|: ((e' :|: r0) :&: s1))
bonrfr :: (BProd b, BSum b) => b e e' -> b (l2 :|: ((l0 :|: e ) :&: s1)) (l2 :|: ((l0 :|: e') :&: s1))
bonrsf :: (BProd b, BSum b) => b e e' -> b (l2 :|: (f1 :&: (e  :&: s0))) (l2 :|: (f1 :&: (e' :&: s0)))
bonrss :: (BProd b, BSum b) => b e e' -> b (l2 :|: (f1 :&: (f0 :&: e ))) (l2 :|: (f1 :&: (f0 :&: e')))
bonrsl :: (BProd b, BSum b) => b e e' -> b (l2 :|: (f1 :&: (e  :|: r0))) (l2 :|: (f1 :&: (e' :|: r0)))
bonrsr :: (BProd b, BSum b) => b e e' -> b (l2 :|: (f1 :&: (l0 :|: e ))) (l2 :|: (f1 :&: (l0 :|: e')))
bonrlf :: (BProd b, BSum b) => b e e' -> b (l2 :|: ((e  :&: s0) :|: r1)) (l2 :|: ((e' :&: s0) :|: r1))
bonrls :: (BProd b, BSum b) => b e e' -> b (l2 :|: ((f0 :&: e ) :|: r1)) (l2 :|: ((f0 :&: e') :|: r1))
bonrll :: (         BSum b) => b e e' -> b (l2 :|: ((e  :|: r0) :|: r1)) (l2 :|: ((e' :|: r0) :|: r1))
bonrlr :: (         BSum b) => b e e' -> b (l2 :|: ((l0 :|: e ) :|: r1)) (l2 :|: ((l0 :|: e') :|: r1))
bonrrf :: (BProd b, BSum b) => b e e' -> b (l2 :|: (l1 :|: (e  :&: s0))) (l2 :|: (l1 :|: (e' :&: s0)))
bonrrs :: (BProd b, BSum b) => b e e' -> b (l2 :|: (l1 :|: (f0 :&: e ))) (l2 :|: (l1 :|: (f0 :&: e')))
bonrrl :: (         BSum b) => b e e' -> b (l2 :|: (l1 :|: (e  :|: r0))) (l2 :|: (l1 :|: (e' :|: r0)))
bonrrr :: (         BSum b) => b e e' -> b (l2 :|: (l1 :|: (l0 :|: e ))) (l2 :|: (l1 :|: (l0 :|: e')))
bonffff :: (BProd b        ) => b e e' -> b ((((e  :&: s0) :&: s1) :&: s2) :&: s3) ((((e' :&: s0) :&: s1) :&: s2) :&: s3)
bonfffs :: (BProd b        ) => b e e' -> b ((((f0 :&: e ) :&: s1) :&: s2) :&: s3) ((((f0 :&: e') :&: s1) :&: s2) :&: s3)
bonfffl :: (BProd b, BSum b) => b e e' -> b ((((e  :|: r0) :&: s1) :&: s2) :&: s3) ((((e' :|: r0) :&: s1) :&: s2) :&: s3)
bonfffr :: (BProd b, BSum b) => b e e' -> b ((((l0 :|: e ) :&: s1) :&: s2) :&: s3) ((((l0 :|: e') :&: s1) :&: s2) :&: s3)
bonffsf :: (BProd b        ) => b e e' -> b (((f1 :&: (e  :&: s0)) :&: s2) :&: s3) (((f1 :&: (e' :&: s0)) :&: s2) :&: s3)
bonffss :: (BProd b        ) => b e e' -> b (((f1 :&: (f0 :&: e )) :&: s2) :&: s3) (((f1 :&: (f0 :&: e')) :&: s2) :&: s3)
bonffsl :: (BProd b, BSum b) => b e e' -> b (((f1 :&: (e  :|: r0)) :&: s2) :&: s3) (((f1 :&: (e' :|: r0)) :&: s2) :&: s3)
bonffsr :: (BProd b, BSum b) => b e e' -> b (((f1 :&: (l0 :|: e )) :&: s2) :&: s3) (((f1 :&: (l0 :|: e')) :&: s2) :&: s3)
bonfflf :: (BProd b, BSum b) => b e e' -> b ((((e  :&: s0) :|: r1) :&: s2) :&: s3) ((((e' :&: s0) :|: r1) :&: s2) :&: s3)
bonffls :: (BProd b, BSum b) => b e e' -> b ((((f0 :&: e ) :|: r1) :&: s2) :&: s3) ((((f0 :&: e') :|: r1) :&: s2) :&: s3)
bonffll :: (BProd b, BSum b) => b e e' -> b ((((e  :|: r0) :|: r1) :&: s2) :&: s3) ((((e' :|: r0) :|: r1) :&: s2) :&: s3)
bonfflr :: (BProd b, BSum b) => b e e' -> b ((((l0 :|: e ) :|: r1) :&: s2) :&: s3) ((((l0 :|: e') :|: r1) :&: s2) :&: s3)
bonffrf :: (BProd b, BSum b) => b e e' -> b (((l1 :|: (e  :&: s0)) :&: s2) :&: s3) (((l1 :|: (e' :&: s0)) :&: s2) :&: s3)
bonffrs :: (BProd b, BSum b) => b e e' -> b (((l1 :|: (f0 :&: e )) :&: s2) :&: s3) (((l1 :|: (f0 :&: e')) :&: s2) :&: s3)
bonffrl :: (BProd b, BSum b) => b e e' -> b (((l1 :|: (e  :|: r0)) :&: s2) :&: s3) (((l1 :|: (e' :|: r0)) :&: s2) :&: s3)
bonffrr :: (BProd b, BSum b) => b e e' -> b (((l1 :|: (l0 :|: e )) :&: s2) :&: s3) (((l1 :|: (l0 :|: e')) :&: s2) :&: s3)
bonfsff :: (BProd b        ) => b e e' -> b ((f2 :&: ((e  :&: s0) :&: s1)) :&: s3) ((f2 :&: ((e' :&: s0) :&: s1)) :&: s3)
bonfsfs :: (BProd b        ) => b e e' -> b ((f2 :&: ((f0 :&: e ) :&: s1)) :&: s3) ((f2 :&: ((f0 :&: e') :&: s1)) :&: s3)
bonfsfl :: (BProd b, BSum b) => b e e' -> b ((f2 :&: ((e  :|: r0) :&: s1)) :&: s3) ((f2 :&: ((e' :|: r0) :&: s1)) :&: s3)
bonfsfr :: (BProd b, BSum b) => b e e' -> b ((f2 :&: ((l0 :|: e ) :&: s1)) :&: s3) ((f2 :&: ((l0 :|: e') :&: s1)) :&: s3)
bonfssf :: (BProd b        ) => b e e' -> b ((f2 :&: (f1 :&: (e  :&: s0))) :&: s3) ((f2 :&: (f1 :&: (e' :&: s0))) :&: s3)
bonfsss :: (BProd b        ) => b e e' -> b ((f2 :&: (f1 :&: (f0 :&: e ))) :&: s3) ((f2 :&: (f1 :&: (f0 :&: e'))) :&: s3)
bonfssl :: (BProd b, BSum b) => b e e' -> b ((f2 :&: (f1 :&: (e  :|: r0))) :&: s3) ((f2 :&: (f1 :&: (e' :|: r0))) :&: s3)
bonfssr :: (BProd b, BSum b) => b e e' -> b ((f2 :&: (f1 :&: (l0 :|: e ))) :&: s3) ((f2 :&: (f1 :&: (l0 :|: e'))) :&: s3)
bonfslf :: (BProd b, BSum b) => b e e' -> b ((f2 :&: ((e  :&: s0) :|: r1)) :&: s3) ((f2 :&: ((e' :&: s0) :|: r1)) :&: s3)
bonfsls :: (BProd b, BSum b) => b e e' -> b ((f2 :&: ((f0 :&: e ) :|: r1)) :&: s3) ((f2 :&: ((f0 :&: e') :|: r1)) :&: s3)
bonfsll :: (BProd b, BSum b) => b e e' -> b ((f2 :&: ((e  :|: r0) :|: r1)) :&: s3) ((f2 :&: ((e' :|: r0) :|: r1)) :&: s3)
bonfslr :: (BProd b, BSum b) => b e e' -> b ((f2 :&: ((l0 :|: e ) :|: r1)) :&: s3) ((f2 :&: ((l0 :|: e') :|: r1)) :&: s3)
bonfsrf :: (BProd b, BSum b) => b e e' -> b ((f2 :&: (l1 :|: (e  :&: s0))) :&: s3) ((f2 :&: (l1 :|: (e' :&: s0))) :&: s3)
bonfsrs :: (BProd b, BSum b) => b e e' -> b ((f2 :&: (l1 :|: (f0 :&: e ))) :&: s3) ((f2 :&: (l1 :|: (f0 :&: e'))) :&: s3)
bonfsrl :: (BProd b, BSum b) => b e e' -> b ((f2 :&: (l1 :|: (e  :|: r0))) :&: s3) ((f2 :&: (l1 :|: (e' :|: r0))) :&: s3)
bonfsrr :: (BProd b, BSum b) => b e e' -> b ((f2 :&: (l1 :|: (l0 :|: e ))) :&: s3) ((f2 :&: (l1 :|: (l0 :|: e'))) :&: s3)
bonflff :: (BProd b, BSum b) => b e e' -> b ((((e  :&: s0) :&: s1) :|: r2) :&: s3) ((((e' :&: s0) :&: s1) :|: r2) :&: s3)
bonflfs :: (BProd b, BSum b) => b e e' -> b ((((f0 :&: e ) :&: s1) :|: r2) :&: s3) ((((f0 :&: e') :&: s1) :|: r2) :&: s3)
bonflfl :: (BProd b, BSum b) => b e e' -> b ((((e  :|: r0) :&: s1) :|: r2) :&: s3) ((((e' :|: r0) :&: s1) :|: r2) :&: s3)
bonflfr :: (BProd b, BSum b) => b e e' -> b ((((l0 :|: e ) :&: s1) :|: r2) :&: s3) ((((l0 :|: e') :&: s1) :|: r2) :&: s3)
bonflsf :: (BProd b, BSum b) => b e e' -> b (((f1 :&: (e  :&: s0)) :|: r2) :&: s3) (((f1 :&: (e' :&: s0)) :|: r2) :&: s3)
bonflss :: (BProd b, BSum b) => b e e' -> b (((f1 :&: (f0 :&: e )) :|: r2) :&: s3) (((f1 :&: (f0 :&: e')) :|: r2) :&: s3)
bonflsl :: (BProd b, BSum b) => b e e' -> b (((f1 :&: (e  :|: r0)) :|: r2) :&: s3) (((f1 :&: (e' :|: r0)) :|: r2) :&: s3)
bonflsr :: (BProd b, BSum b) => b e e' -> b (((f1 :&: (l0 :|: e )) :|: r2) :&: s3) (((f1 :&: (l0 :|: e')) :|: r2) :&: s3)
bonfllf :: (BProd b, BSum b) => b e e' -> b ((((e  :&: s0) :|: r1) :|: r2) :&: s3) ((((e' :&: s0) :|: r1) :|: r2) :&: s3)
bonflls :: (BProd b, BSum b) => b e e' -> b ((((f0 :&: e ) :|: r1) :|: r2) :&: s3) ((((f0 :&: e') :|: r1) :|: r2) :&: s3)
bonflll :: (BProd b, BSum b) => b e e' -> b ((((e  :|: r0) :|: r1) :|: r2) :&: s3) ((((e' :|: r0) :|: r1) :|: r2) :&: s3)
bonfllr :: (BProd b, BSum b) => b e e' -> b ((((l0 :|: e ) :|: r1) :|: r2) :&: s3) ((((l0 :|: e') :|: r1) :|: r2) :&: s3)
bonflrf :: (BProd b, BSum b) => b e e' -> b (((l1 :|: (e  :&: s0)) :|: r2) :&: s3) (((l1 :|: (e' :&: s0)) :|: r2) :&: s3)
bonflrs :: (BProd b, BSum b) => b e e' -> b (((l1 :|: (f0 :&: e )) :|: r2) :&: s3) (((l1 :|: (f0 :&: e')) :|: r2) :&: s3)
bonflrl :: (BProd b, BSum b) => b e e' -> b (((l1 :|: (e  :|: r0)) :|: r2) :&: s3) (((l1 :|: (e' :|: r0)) :|: r2) :&: s3)
bonflrr :: (BProd b, BSum b) => b e e' -> b (((l1 :|: (l0 :|: e )) :|: r2) :&: s3) (((l1 :|: (l0 :|: e')) :|: r2) :&: s3)
bonfrff :: (BProd b, BSum b) => b e e' -> b ((l2 :|: ((e  :&: s0) :&: s1)) :&: s3) ((l2 :|: ((e' :&: s0) :&: s1)) :&: s3)
bonfrfs :: (BProd b, BSum b) => b e e' -> b ((l2 :|: ((f0 :&: e ) :&: s1)) :&: s3) ((l2 :|: ((f0 :&: e') :&: s1)) :&: s3)
bonfrfl :: (BProd b, BSum b) => b e e' -> b ((l2 :|: ((e  :|: r0) :&: s1)) :&: s3) ((l2 :|: ((e' :|: r0) :&: s1)) :&: s3)
bonfrfr :: (BProd b, BSum b) => b e e' -> b ((l2 :|: ((l0 :|: e ) :&: s1)) :&: s3) ((l2 :|: ((l0 :|: e') :&: s1)) :&: s3)
bonfrsf :: (BProd b, BSum b) => b e e' -> b ((l2 :|: (f1 :&: (e  :&: s0))) :&: s3) ((l2 :|: (f1 :&: (e' :&: s0))) :&: s3)
bonfrss :: (BProd b, BSum b) => b e e' -> b ((l2 :|: (f1 :&: (f0 :&: e ))) :&: s3) ((l2 :|: (f1 :&: (f0 :&: e'))) :&: s3)
bonfrsl :: (BProd b, BSum b) => b e e' -> b ((l2 :|: (f1 :&: (e  :|: r0))) :&: s3) ((l2 :|: (f1 :&: (e' :|: r0))) :&: s3)
bonfrsr :: (BProd b, BSum b) => b e e' -> b ((l2 :|: (f1 :&: (l0 :|: e ))) :&: s3) ((l2 :|: (f1 :&: (l0 :|: e'))) :&: s3)
bonfrlf :: (BProd b, BSum b) => b e e' -> b ((l2 :|: ((e  :&: s0) :|: r1)) :&: s3) ((l2 :|: ((e' :&: s0) :|: r1)) :&: s3)
bonfrls :: (BProd b, BSum b) => b e e' -> b ((l2 :|: ((f0 :&: e ) :|: r1)) :&: s3) ((l2 :|: ((f0 :&: e') :|: r1)) :&: s3)
bonfrll :: (BProd b, BSum b) => b e e' -> b ((l2 :|: ((e  :|: r0) :|: r1)) :&: s3) ((l2 :|: ((e' :|: r0) :|: r1)) :&: s3)
bonfrlr :: (BProd b, BSum b) => b e e' -> b ((l2 :|: ((l0 :|: e ) :|: r1)) :&: s3) ((l2 :|: ((l0 :|: e') :|: r1)) :&: s3)
bonfrrf :: (BProd b, BSum b) => b e e' -> b ((l2 :|: (l1 :|: (e  :&: s0))) :&: s3) ((l2 :|: (l1 :|: (e' :&: s0))) :&: s3)
bonfrrs :: (BProd b, BSum b) => b e e' -> b ((l2 :|: (l1 :|: (f0 :&: e ))) :&: s3) ((l2 :|: (l1 :|: (f0 :&: e'))) :&: s3)
bonfrrl :: (BProd b, BSum b) => b e e' -> b ((l2 :|: (l1 :|: (e  :|: r0))) :&: s3) ((l2 :|: (l1 :|: (e' :|: r0))) :&: s3)
bonfrrr :: (BProd b, BSum b) => b e e' -> b ((l2 :|: (l1 :|: (l0 :|: e ))) :&: s3) ((l2 :|: (l1 :|: (l0 :|: e'))) :&: s3)
bonsfff :: (BProd b        ) => b e e' -> b (f3 :&: (((e  :&: s0) :&: s1) :&: s2)) (f3 :&: (((e' :&: s0) :&: s1) :&: s2))
bonsffs :: (BProd b        ) => b e e' -> b (f3 :&: (((f0 :&: e ) :&: s1) :&: s2)) (f3 :&: (((f0 :&: e') :&: s1) :&: s2))
bonsffl :: (BProd b, BSum b) => b e e' -> b (f3 :&: (((e  :|: r0) :&: s1) :&: s2)) (f3 :&: (((e' :|: r0) :&: s1) :&: s2))
bonsffr :: (BProd b, BSum b) => b e e' -> b (f3 :&: (((l0 :|: e ) :&: s1) :&: s2)) (f3 :&: (((l0 :|: e') :&: s1) :&: s2))
bonsfsf :: (BProd b        ) => b e e' -> b (f3 :&: ((f1 :&: (e  :&: s0)) :&: s2)) (f3 :&: ((f1 :&: (e' :&: s0)) :&: s2))
bonsfss :: (BProd b        ) => b e e' -> b (f3 :&: ((f1 :&: (f0 :&: e )) :&: s2)) (f3 :&: ((f1 :&: (f0 :&: e')) :&: s2))
bonsfsl :: (BProd b, BSum b) => b e e' -> b (f3 :&: ((f1 :&: (e  :|: r0)) :&: s2)) (f3 :&: ((f1 :&: (e' :|: r0)) :&: s2))
bonsfsr :: (BProd b, BSum b) => b e e' -> b (f3 :&: ((f1 :&: (l0 :|: e )) :&: s2)) (f3 :&: ((f1 :&: (l0 :|: e')) :&: s2))
bonsflf :: (BProd b, BSum b) => b e e' -> b (f3 :&: (((e  :&: s0) :|: r1) :&: s2)) (f3 :&: (((e' :&: s0) :|: r1) :&: s2))
bonsfls :: (BProd b, BSum b) => b e e' -> b (f3 :&: (((f0 :&: e ) :|: r1) :&: s2)) (f3 :&: (((f0 :&: e') :|: r1) :&: s2))
bonsfll :: (BProd b, BSum b) => b e e' -> b (f3 :&: (((e  :|: r0) :|: r1) :&: s2)) (f3 :&: (((e' :|: r0) :|: r1) :&: s2))
bonsflr :: (BProd b, BSum b) => b e e' -> b (f3 :&: (((l0 :|: e ) :|: r1) :&: s2)) (f3 :&: (((l0 :|: e') :|: r1) :&: s2))
bonsfrf :: (BProd b, BSum b) => b e e' -> b (f3 :&: ((l1 :|: (e  :&: s0)) :&: s2)) (f3 :&: ((l1 :|: (e' :&: s0)) :&: s2))
bonsfrs :: (BProd b, BSum b) => b e e' -> b (f3 :&: ((l1 :|: (f0 :&: e )) :&: s2)) (f3 :&: ((l1 :|: (f0 :&: e')) :&: s2))
bonsfrl :: (BProd b, BSum b) => b e e' -> b (f3 :&: ((l1 :|: (e  :|: r0)) :&: s2)) (f3 :&: ((l1 :|: (e' :|: r0)) :&: s2))
bonsfrr :: (BProd b, BSum b) => b e e' -> b (f3 :&: ((l1 :|: (l0 :|: e )) :&: s2)) (f3 :&: ((l1 :|: (l0 :|: e')) :&: s2))
bonssff :: (BProd b        ) => b e e' -> b (f3 :&: (f2 :&: ((e  :&: s0) :&: s1))) (f3 :&: (f2 :&: ((e' :&: s0) :&: s1)))
bonssfs :: (BProd b        ) => b e e' -> b (f3 :&: (f2 :&: ((f0 :&: e ) :&: s1))) (f3 :&: (f2 :&: ((f0 :&: e') :&: s1)))
bonssfl :: (BProd b, BSum b) => b e e' -> b (f3 :&: (f2 :&: ((e  :|: r0) :&: s1))) (f3 :&: (f2 :&: ((e' :|: r0) :&: s1)))
bonssfr :: (BProd b, BSum b) => b e e' -> b (f3 :&: (f2 :&: ((l0 :|: e ) :&: s1))) (f3 :&: (f2 :&: ((l0 :|: e') :&: s1)))
bonsssf :: (BProd b        ) => b e e' -> b (f3 :&: (f2 :&: (f1 :&: (e  :&: s0)))) (f3 :&: (f2 :&: (f1 :&: (e' :&: s0))))
bonssss :: (BProd b        ) => b e e' -> b (f3 :&: (f2 :&: (f1 :&: (f0 :&: e )))) (f3 :&: (f2 :&: (f1 :&: (f0 :&: e'))))
bonsssl :: (BProd b, BSum b) => b e e' -> b (f3 :&: (f2 :&: (f1 :&: (e  :|: r0)))) (f3 :&: (f2 :&: (f1 :&: (e' :|: r0))))
bonsssr :: (BProd b, BSum b) => b e e' -> b (f3 :&: (f2 :&: (f1 :&: (l0 :|: e )))) (f3 :&: (f2 :&: (f1 :&: (l0 :|: e'))))
bonsslf :: (BProd b, BSum b) => b e e' -> b (f3 :&: (f2 :&: ((e  :&: s0) :|: r1))) (f3 :&: (f2 :&: ((e' :&: s0) :|: r1)))
bonssls :: (BProd b, BSum b) => b e e' -> b (f3 :&: (f2 :&: ((f0 :&: e ) :|: r1))) (f3 :&: (f2 :&: ((f0 :&: e') :|: r1)))
bonssll :: (BProd b, BSum b) => b e e' -> b (f3 :&: (f2 :&: ((e  :|: r0) :|: r1))) (f3 :&: (f2 :&: ((e' :|: r0) :|: r1)))
bonsslr :: (BProd b, BSum b) => b e e' -> b (f3 :&: (f2 :&: ((l0 :|: e ) :|: r1))) (f3 :&: (f2 :&: ((l0 :|: e') :|: r1)))
bonssrf :: (BProd b, BSum b) => b e e' -> b (f3 :&: (f2 :&: (l1 :|: (e  :&: s0)))) (f3 :&: (f2 :&: (l1 :|: (e' :&: s0))))
bonssrs :: (BProd b, BSum b) => b e e' -> b (f3 :&: (f2 :&: (l1 :|: (f0 :&: e )))) (f3 :&: (f2 :&: (l1 :|: (f0 :&: e'))))
bonssrl :: (BProd b, BSum b) => b e e' -> b (f3 :&: (f2 :&: (l1 :|: (e  :|: r0)))) (f3 :&: (f2 :&: (l1 :|: (e' :|: r0))))
bonssrr :: (BProd b, BSum b) => b e e' -> b (f3 :&: (f2 :&: (l1 :|: (l0 :|: e )))) (f3 :&: (f2 :&: (l1 :|: (l0 :|: e'))))
bonslff :: (BProd b, BSum b) => b e e' -> b (f3 :&: (((e  :&: s0) :&: s1) :|: r2)) (f3 :&: (((e' :&: s0) :&: s1) :|: r2))
bonslfs :: (BProd b, BSum b) => b e e' -> b (f3 :&: (((f0 :&: e ) :&: s1) :|: r2)) (f3 :&: (((f0 :&: e') :&: s1) :|: r2))
bonslfl :: (BProd b, BSum b) => b e e' -> b (f3 :&: (((e  :|: r0) :&: s1) :|: r2)) (f3 :&: (((e' :|: r0) :&: s1) :|: r2))
bonslfr :: (BProd b, BSum b) => b e e' -> b (f3 :&: (((l0 :|: e ) :&: s1) :|: r2)) (f3 :&: (((l0 :|: e') :&: s1) :|: r2))
bonslsf :: (BProd b, BSum b) => b e e' -> b (f3 :&: ((f1 :&: (e  :&: s0)) :|: r2)) (f3 :&: ((f1 :&: (e' :&: s0)) :|: r2))
bonslss :: (BProd b, BSum b) => b e e' -> b (f3 :&: ((f1 :&: (f0 :&: e )) :|: r2)) (f3 :&: ((f1 :&: (f0 :&: e')) :|: r2))
bonslsl :: (BProd b, BSum b) => b e e' -> b (f3 :&: ((f1 :&: (e  :|: r0)) :|: r2)) (f3 :&: ((f1 :&: (e' :|: r0)) :|: r2))
bonslsr :: (BProd b, BSum b) => b e e' -> b (f3 :&: ((f1 :&: (l0 :|: e )) :|: r2)) (f3 :&: ((f1 :&: (l0 :|: e')) :|: r2))
bonsllf :: (BProd b, BSum b) => b e e' -> b (f3 :&: (((e  :&: s0) :|: r1) :|: r2)) (f3 :&: (((e' :&: s0) :|: r1) :|: r2))
bonslls :: (BProd b, BSum b) => b e e' -> b (f3 :&: (((f0 :&: e ) :|: r1) :|: r2)) (f3 :&: (((f0 :&: e') :|: r1) :|: r2))
bonslll :: (BProd b, BSum b) => b e e' -> b (f3 :&: (((e  :|: r0) :|: r1) :|: r2)) (f3 :&: (((e' :|: r0) :|: r1) :|: r2))
bonsllr :: (BProd b, BSum b) => b e e' -> b (f3 :&: (((l0 :|: e ) :|: r1) :|: r2)) (f3 :&: (((l0 :|: e') :|: r1) :|: r2))
bonslrf :: (BProd b, BSum b) => b e e' -> b (f3 :&: ((l1 :|: (e  :&: s0)) :|: r2)) (f3 :&: ((l1 :|: (e' :&: s0)) :|: r2))
bonslrs :: (BProd b, BSum b) => b e e' -> b (f3 :&: ((l1 :|: (f0 :&: e )) :|: r2)) (f3 :&: ((l1 :|: (f0 :&: e')) :|: r2))
bonslrl :: (BProd b, BSum b) => b e e' -> b (f3 :&: ((l1 :|: (e  :|: r0)) :|: r2)) (f3 :&: ((l1 :|: (e' :|: r0)) :|: r2))
bonslrr :: (BProd b, BSum b) => b e e' -> b (f3 :&: ((l1 :|: (l0 :|: e )) :|: r2)) (f3 :&: ((l1 :|: (l0 :|: e')) :|: r2))
bonsrff :: (BProd b, BSum b) => b e e' -> b (f3 :&: (l2 :|: ((e  :&: s0) :&: s1))) (f3 :&: (l2 :|: ((e' :&: s0) :&: s1)))
bonsrfs :: (BProd b, BSum b) => b e e' -> b (f3 :&: (l2 :|: ((f0 :&: e ) :&: s1))) (f3 :&: (l2 :|: ((f0 :&: e') :&: s1)))
bonsrfl :: (BProd b, BSum b) => b e e' -> b (f3 :&: (l2 :|: ((e  :|: r0) :&: s1))) (f3 :&: (l2 :|: ((e' :|: r0) :&: s1)))
bonsrfr :: (BProd b, BSum b) => b e e' -> b (f3 :&: (l2 :|: ((l0 :|: e ) :&: s1))) (f3 :&: (l2 :|: ((l0 :|: e') :&: s1)))
bonsrsf :: (BProd b, BSum b) => b e e' -> b (f3 :&: (l2 :|: (f1 :&: (e  :&: s0)))) (f3 :&: (l2 :|: (f1 :&: (e' :&: s0))))
bonsrss :: (BProd b, BSum b) => b e e' -> b (f3 :&: (l2 :|: (f1 :&: (f0 :&: e )))) (f3 :&: (l2 :|: (f1 :&: (f0 :&: e'))))
bonsrsl :: (BProd b, BSum b) => b e e' -> b (f3 :&: (l2 :|: (f1 :&: (e  :|: r0)))) (f3 :&: (l2 :|: (f1 :&: (e' :|: r0))))
bonsrsr :: (BProd b, BSum b) => b e e' -> b (f3 :&: (l2 :|: (f1 :&: (l0 :|: e )))) (f3 :&: (l2 :|: (f1 :&: (l0 :|: e'))))
bonsrlf :: (BProd b, BSum b) => b e e' -> b (f3 :&: (l2 :|: ((e  :&: s0) :|: r1))) (f3 :&: (l2 :|: ((e' :&: s0) :|: r1)))
bonsrls :: (BProd b, BSum b) => b e e' -> b (f3 :&: (l2 :|: ((f0 :&: e ) :|: r1))) (f3 :&: (l2 :|: ((f0 :&: e') :|: r1)))
bonsrll :: (BProd b, BSum b) => b e e' -> b (f3 :&: (l2 :|: ((e  :|: r0) :|: r1))) (f3 :&: (l2 :|: ((e' :|: r0) :|: r1)))
bonsrlr :: (BProd b, BSum b) => b e e' -> b (f3 :&: (l2 :|: ((l0 :|: e ) :|: r1))) (f3 :&: (l2 :|: ((l0 :|: e') :|: r1)))
bonsrrf :: (BProd b, BSum b) => b e e' -> b (f3 :&: (l2 :|: (l1 :|: (e  :&: s0)))) (f3 :&: (l2 :|: (l1 :|: (e' :&: s0))))
bonsrrs :: (BProd b, BSum b) => b e e' -> b (f3 :&: (l2 :|: (l1 :|: (f0 :&: e )))) (f3 :&: (l2 :|: (l1 :|: (f0 :&: e'))))
bonsrrl :: (BProd b, BSum b) => b e e' -> b (f3 :&: (l2 :|: (l1 :|: (e  :|: r0)))) (f3 :&: (l2 :|: (l1 :|: (e' :|: r0))))
bonsrrr :: (BProd b, BSum b) => b e e' -> b (f3 :&: (l2 :|: (l1 :|: (l0 :|: e )))) (f3 :&: (l2 :|: (l1 :|: (l0 :|: e'))))
bonlfff :: (BProd b, BSum b) => b e e' -> b ((((e  :&: s0) :&: s1) :&: s2) :|: r3) ((((e' :&: s0) :&: s1) :&: s2) :|: r3)
bonlffs :: (BProd b, BSum b) => b e e' -> b ((((f0 :&: e ) :&: s1) :&: s2) :|: r3) ((((f0 :&: e') :&: s1) :&: s2) :|: r3)
bonlffl :: (BProd b, BSum b) => b e e' -> b ((((e  :|: r0) :&: s1) :&: s2) :|: r3) ((((e' :|: r0) :&: s1) :&: s2) :|: r3)
bonlffr :: (BProd b, BSum b) => b e e' -> b ((((l0 :|: e ) :&: s1) :&: s2) :|: r3) ((((l0 :|: e') :&: s1) :&: s2) :|: r3)
bonlfsf :: (BProd b, BSum b) => b e e' -> b (((f1 :&: (e  :&: s0)) :&: s2) :|: r3) (((f1 :&: (e' :&: s0)) :&: s2) :|: r3)
bonlfss :: (BProd b, BSum b) => b e e' -> b (((f1 :&: (f0 :&: e )) :&: s2) :|: r3) (((f1 :&: (f0 :&: e')) :&: s2) :|: r3)
bonlfsl :: (BProd b, BSum b) => b e e' -> b (((f1 :&: (e  :|: r0)) :&: s2) :|: r3) (((f1 :&: (e' :|: r0)) :&: s2) :|: r3)
bonlfsr :: (BProd b, BSum b) => b e e' -> b (((f1 :&: (l0 :|: e )) :&: s2) :|: r3) (((f1 :&: (l0 :|: e')) :&: s2) :|: r3)
bonlflf :: (BProd b, BSum b) => b e e' -> b ((((e  :&: s0) :|: r1) :&: s2) :|: r3) ((((e' :&: s0) :|: r1) :&: s2) :|: r3)
bonlfls :: (BProd b, BSum b) => b e e' -> b ((((f0 :&: e ) :|: r1) :&: s2) :|: r3) ((((f0 :&: e') :|: r1) :&: s2) :|: r3)
bonlfll :: (BProd b, BSum b) => b e e' -> b ((((e  :|: r0) :|: r1) :&: s2) :|: r3) ((((e' :|: r0) :|: r1) :&: s2) :|: r3)
bonlflr :: (BProd b, BSum b) => b e e' -> b ((((l0 :|: e ) :|: r1) :&: s2) :|: r3) ((((l0 :|: e') :|: r1) :&: s2) :|: r3)
bonlfrf :: (BProd b, BSum b) => b e e' -> b (((l1 :|: (e  :&: s0)) :&: s2) :|: r3) (((l1 :|: (e' :&: s0)) :&: s2) :|: r3)
bonlfrs :: (BProd b, BSum b) => b e e' -> b (((l1 :|: (f0 :&: e )) :&: s2) :|: r3) (((l1 :|: (f0 :&: e')) :&: s2) :|: r3)
bonlfrl :: (BProd b, BSum b) => b e e' -> b (((l1 :|: (e  :|: r0)) :&: s2) :|: r3) (((l1 :|: (e' :|: r0)) :&: s2) :|: r3)
bonlfrr :: (BProd b, BSum b) => b e e' -> b (((l1 :|: (l0 :|: e )) :&: s2) :|: r3) (((l1 :|: (l0 :|: e')) :&: s2) :|: r3)
bonlsff :: (BProd b, BSum b) => b e e' -> b ((f2 :&: ((e  :&: s0) :&: s1)) :|: r3) ((f2 :&: ((e' :&: s0) :&: s1)) :|: r3)
bonlsfs :: (BProd b, BSum b) => b e e' -> b ((f2 :&: ((f0 :&: e ) :&: s1)) :|: r3) ((f2 :&: ((f0 :&: e') :&: s1)) :|: r3)
bonlsfl :: (BProd b, BSum b) => b e e' -> b ((f2 :&: ((e  :|: r0) :&: s1)) :|: r3) ((f2 :&: ((e' :|: r0) :&: s1)) :|: r3)
bonlsfr :: (BProd b, BSum b) => b e e' -> b ((f2 :&: ((l0 :|: e ) :&: s1)) :|: r3) ((f2 :&: ((l0 :|: e') :&: s1)) :|: r3)
bonlssf :: (BProd b, BSum b) => b e e' -> b ((f2 :&: (f1 :&: (e  :&: s0))) :|: r3) ((f2 :&: (f1 :&: (e' :&: s0))) :|: r3)
bonlsss :: (BProd b, BSum b) => b e e' -> b ((f2 :&: (f1 :&: (f0 :&: e ))) :|: r3) ((f2 :&: (f1 :&: (f0 :&: e'))) :|: r3)
bonlssl :: (BProd b, BSum b) => b e e' -> b ((f2 :&: (f1 :&: (e  :|: r0))) :|: r3) ((f2 :&: (f1 :&: (e' :|: r0))) :|: r3)
bonlssr :: (BProd b, BSum b) => b e e' -> b ((f2 :&: (f1 :&: (l0 :|: e ))) :|: r3) ((f2 :&: (f1 :&: (l0 :|: e'))) :|: r3)
bonlslf :: (BProd b, BSum b) => b e e' -> b ((f2 :&: ((e  :&: s0) :|: r1)) :|: r3) ((f2 :&: ((e' :&: s0) :|: r1)) :|: r3)
bonlsls :: (BProd b, BSum b) => b e e' -> b ((f2 :&: ((f0 :&: e ) :|: r1)) :|: r3) ((f2 :&: ((f0 :&: e') :|: r1)) :|: r3)
bonlsll :: (BProd b, BSum b) => b e e' -> b ((f2 :&: ((e  :|: r0) :|: r1)) :|: r3) ((f2 :&: ((e' :|: r0) :|: r1)) :|: r3)
bonlslr :: (BProd b, BSum b) => b e e' -> b ((f2 :&: ((l0 :|: e ) :|: r1)) :|: r3) ((f2 :&: ((l0 :|: e') :|: r1)) :|: r3)
bonlsrf :: (BProd b, BSum b) => b e e' -> b ((f2 :&: (l1 :|: (e  :&: s0))) :|: r3) ((f2 :&: (l1 :|: (e' :&: s0))) :|: r3)
bonlsrs :: (BProd b, BSum b) => b e e' -> b ((f2 :&: (l1 :|: (f0 :&: e ))) :|: r3) ((f2 :&: (l1 :|: (f0 :&: e'))) :|: r3)
bonlsrl :: (BProd b, BSum b) => b e e' -> b ((f2 :&: (l1 :|: (e  :|: r0))) :|: r3) ((f2 :&: (l1 :|: (e' :|: r0))) :|: r3)
bonlsrr :: (BProd b, BSum b) => b e e' -> b ((f2 :&: (l1 :|: (l0 :|: e ))) :|: r3) ((f2 :&: (l1 :|: (l0 :|: e'))) :|: r3)
bonllff :: (BProd b, BSum b) => b e e' -> b ((((e  :&: s0) :&: s1) :|: r2) :|: r3) ((((e' :&: s0) :&: s1) :|: r2) :|: r3)
bonllfs :: (BProd b, BSum b) => b e e' -> b ((((f0 :&: e ) :&: s1) :|: r2) :|: r3) ((((f0 :&: e') :&: s1) :|: r2) :|: r3)
bonllfl :: (BProd b, BSum b) => b e e' -> b ((((e  :|: r0) :&: s1) :|: r2) :|: r3) ((((e' :|: r0) :&: s1) :|: r2) :|: r3)
bonllfr :: (BProd b, BSum b) => b e e' -> b ((((l0 :|: e ) :&: s1) :|: r2) :|: r3) ((((l0 :|: e') :&: s1) :|: r2) :|: r3)
bonllsf :: (BProd b, BSum b) => b e e' -> b (((f1 :&: (e  :&: s0)) :|: r2) :|: r3) (((f1 :&: (e' :&: s0)) :|: r2) :|: r3)
bonllss :: (BProd b, BSum b) => b e e' -> b (((f1 :&: (f0 :&: e )) :|: r2) :|: r3) (((f1 :&: (f0 :&: e')) :|: r2) :|: r3)
bonllsl :: (BProd b, BSum b) => b e e' -> b (((f1 :&: (e  :|: r0)) :|: r2) :|: r3) (((f1 :&: (e' :|: r0)) :|: r2) :|: r3)
bonllsr :: (BProd b, BSum b) => b e e' -> b (((f1 :&: (l0 :|: e )) :|: r2) :|: r3) (((f1 :&: (l0 :|: e')) :|: r2) :|: r3)
bonlllf :: (BProd b, BSum b) => b e e' -> b ((((e  :&: s0) :|: r1) :|: r2) :|: r3) ((((e' :&: s0) :|: r1) :|: r2) :|: r3)
bonllls :: (BProd b, BSum b) => b e e' -> b ((((f0 :&: e ) :|: r1) :|: r2) :|: r3) ((((f0 :&: e') :|: r1) :|: r2) :|: r3)
bonllll :: (         BSum b) => b e e' -> b ((((e  :|: r0) :|: r1) :|: r2) :|: r3) ((((e' :|: r0) :|: r1) :|: r2) :|: r3)
bonlllr :: (         BSum b) => b e e' -> b ((((l0 :|: e ) :|: r1) :|: r2) :|: r3) ((((l0 :|: e') :|: r1) :|: r2) :|: r3)
bonllrf :: (BProd b, BSum b) => b e e' -> b (((l1 :|: (e  :&: s0)) :|: r2) :|: r3) (((l1 :|: (e' :&: s0)) :|: r2) :|: r3)
bonllrs :: (BProd b, BSum b) => b e e' -> b (((l1 :|: (f0 :&: e )) :|: r2) :|: r3) (((l1 :|: (f0 :&: e')) :|: r2) :|: r3)
bonllrl :: (         BSum b) => b e e' -> b (((l1 :|: (e  :|: r0)) :|: r2) :|: r3) (((l1 :|: (e' :|: r0)) :|: r2) :|: r3)
bonllrr :: (         BSum b) => b e e' -> b (((l1 :|: (l0 :|: e )) :|: r2) :|: r3) (((l1 :|: (l0 :|: e')) :|: r2) :|: r3)
bonlrff :: (BProd b, BSum b) => b e e' -> b ((l2 :|: ((e  :&: s0) :&: s1)) :|: r3) ((l2 :|: ((e' :&: s0) :&: s1)) :|: r3)
bonlrfs :: (BProd b, BSum b) => b e e' -> b ((l2 :|: ((f0 :&: e ) :&: s1)) :|: r3) ((l2 :|: ((f0 :&: e') :&: s1)) :|: r3)
bonlrfl :: (BProd b, BSum b) => b e e' -> b ((l2 :|: ((e  :|: r0) :&: s1)) :|: r3) ((l2 :|: ((e' :|: r0) :&: s1)) :|: r3)
bonlrfr :: (BProd b, BSum b) => b e e' -> b ((l2 :|: ((l0 :|: e ) :&: s1)) :|: r3) ((l2 :|: ((l0 :|: e') :&: s1)) :|: r3)
bonlrsf :: (BProd b, BSum b) => b e e' -> b ((l2 :|: (f1 :&: (e  :&: s0))) :|: r3) ((l2 :|: (f1 :&: (e' :&: s0))) :|: r3)
bonlrss :: (BProd b, BSum b) => b e e' -> b ((l2 :|: (f1 :&: (f0 :&: e ))) :|: r3) ((l2 :|: (f1 :&: (f0 :&: e'))) :|: r3)
bonlrsl :: (BProd b, BSum b) => b e e' -> b ((l2 :|: (f1 :&: (e  :|: r0))) :|: r3) ((l2 :|: (f1 :&: (e' :|: r0))) :|: r3)
bonlrsr :: (BProd b, BSum b) => b e e' -> b ((l2 :|: (f1 :&: (l0 :|: e ))) :|: r3) ((l2 :|: (f1 :&: (l0 :|: e'))) :|: r3)
bonlrlf :: (BProd b, BSum b) => b e e' -> b ((l2 :|: ((e  :&: s0) :|: r1)) :|: r3) ((l2 :|: ((e' :&: s0) :|: r1)) :|: r3)
bonlrls :: (BProd b, BSum b) => b e e' -> b ((l2 :|: ((f0 :&: e ) :|: r1)) :|: r3) ((l2 :|: ((f0 :&: e') :|: r1)) :|: r3)
bonlrll :: (         BSum b) => b e e' -> b ((l2 :|: ((e  :|: r0) :|: r1)) :|: r3) ((l2 :|: ((e' :|: r0) :|: r1)) :|: r3)
bonlrlr :: (         BSum b) => b e e' -> b ((l2 :|: ((l0 :|: e ) :|: r1)) :|: r3) ((l2 :|: ((l0 :|: e') :|: r1)) :|: r3)
bonlrrf :: (BProd b, BSum b) => b e e' -> b ((l2 :|: (l1 :|: (e  :&: s0))) :|: r3) ((l2 :|: (l1 :|: (e' :&: s0))) :|: r3)
bonlrrs :: (BProd b, BSum b) => b e e' -> b ((l2 :|: (l1 :|: (f0 :&: e ))) :|: r3) ((l2 :|: (l1 :|: (f0 :&: e'))) :|: r3)
bonlrrl :: (         BSum b) => b e e' -> b ((l2 :|: (l1 :|: (e  :|: r0))) :|: r3) ((l2 :|: (l1 :|: (e' :|: r0))) :|: r3)
bonlrrr :: (         BSum b) => b e e' -> b ((l2 :|: (l1 :|: (l0 :|: e ))) :|: r3) ((l2 :|: (l1 :|: (l0 :|: e'))) :|: r3)
bonrfff :: (BProd b, BSum b) => b e e' -> b (l3 :|: (((e  :&: s0) :&: s1) :&: s2)) (l3 :|: (((e' :&: s0) :&: s1) :&: s2))
bonrffs :: (BProd b, BSum b) => b e e' -> b (l3 :|: (((f0 :&: e ) :&: s1) :&: s2)) (l3 :|: (((f0 :&: e') :&: s1) :&: s2))
bonrffl :: (BProd b, BSum b) => b e e' -> b (l3 :|: (((e  :|: r0) :&: s1) :&: s2)) (l3 :|: (((e' :|: r0) :&: s1) :&: s2))
bonrffr :: (BProd b, BSum b) => b e e' -> b (l3 :|: (((l0 :|: e ) :&: s1) :&: s2)) (l3 :|: (((l0 :|: e') :&: s1) :&: s2))
bonrfsf :: (BProd b, BSum b) => b e e' -> b (l3 :|: ((f1 :&: (e  :&: s0)) :&: s2)) (l3 :|: ((f1 :&: (e' :&: s0)) :&: s2))
bonrfss :: (BProd b, BSum b) => b e e' -> b (l3 :|: ((f1 :&: (f0 :&: e )) :&: s2)) (l3 :|: ((f1 :&: (f0 :&: e')) :&: s2))
bonrfsl :: (BProd b, BSum b) => b e e' -> b (l3 :|: ((f1 :&: (e  :|: r0)) :&: s2)) (l3 :|: ((f1 :&: (e' :|: r0)) :&: s2))
bonrfsr :: (BProd b, BSum b) => b e e' -> b (l3 :|: ((f1 :&: (l0 :|: e )) :&: s2)) (l3 :|: ((f1 :&: (l0 :|: e')) :&: s2))
bonrflf :: (BProd b, BSum b) => b e e' -> b (l3 :|: (((e  :&: s0) :|: r1) :&: s2)) (l3 :|: (((e' :&: s0) :|: r1) :&: s2))
bonrfls :: (BProd b, BSum b) => b e e' -> b (l3 :|: (((f0 :&: e ) :|: r1) :&: s2)) (l3 :|: (((f0 :&: e') :|: r1) :&: s2))
bonrfll :: (BProd b, BSum b) => b e e' -> b (l3 :|: (((e  :|: r0) :|: r1) :&: s2)) (l3 :|: (((e' :|: r0) :|: r1) :&: s2))
bonrflr :: (BProd b, BSum b) => b e e' -> b (l3 :|: (((l0 :|: e ) :|: r1) :&: s2)) (l3 :|: (((l0 :|: e') :|: r1) :&: s2))
bonrfrf :: (BProd b, BSum b) => b e e' -> b (l3 :|: ((l1 :|: (e  :&: s0)) :&: s2)) (l3 :|: ((l1 :|: (e' :&: s0)) :&: s2))
bonrfrs :: (BProd b, BSum b) => b e e' -> b (l3 :|: ((l1 :|: (f0 :&: e )) :&: s2)) (l3 :|: ((l1 :|: (f0 :&: e')) :&: s2))
bonrfrl :: (BProd b, BSum b) => b e e' -> b (l3 :|: ((l1 :|: (e  :|: r0)) :&: s2)) (l3 :|: ((l1 :|: (e' :|: r0)) :&: s2))
bonrfrr :: (BProd b, BSum b) => b e e' -> b (l3 :|: ((l1 :|: (l0 :|: e )) :&: s2)) (l3 :|: ((l1 :|: (l0 :|: e')) :&: s2))
bonrsff :: (BProd b, BSum b) => b e e' -> b (l3 :|: (f2 :&: ((e  :&: s0) :&: s1))) (l3 :|: (f2 :&: ((e' :&: s0) :&: s1)))
bonrsfs :: (BProd b, BSum b) => b e e' -> b (l3 :|: (f2 :&: ((f0 :&: e ) :&: s1))) (l3 :|: (f2 :&: ((f0 :&: e') :&: s1)))
bonrsfl :: (BProd b, BSum b) => b e e' -> b (l3 :|: (f2 :&: ((e  :|: r0) :&: s1))) (l3 :|: (f2 :&: ((e' :|: r0) :&: s1)))
bonrsfr :: (BProd b, BSum b) => b e e' -> b (l3 :|: (f2 :&: ((l0 :|: e ) :&: s1))) (l3 :|: (f2 :&: ((l0 :|: e') :&: s1)))
bonrssf :: (BProd b, BSum b) => b e e' -> b (l3 :|: (f2 :&: (f1 :&: (e  :&: s0)))) (l3 :|: (f2 :&: (f1 :&: (e' :&: s0))))
bonrsss :: (BProd b, BSum b) => b e e' -> b (l3 :|: (f2 :&: (f1 :&: (f0 :&: e )))) (l3 :|: (f2 :&: (f1 :&: (f0 :&: e'))))
bonrssl :: (BProd b, BSum b) => b e e' -> b (l3 :|: (f2 :&: (f1 :&: (e  :|: r0)))) (l3 :|: (f2 :&: (f1 :&: (e' :|: r0))))
bonrssr :: (BProd b, BSum b) => b e e' -> b (l3 :|: (f2 :&: (f1 :&: (l0 :|: e )))) (l3 :|: (f2 :&: (f1 :&: (l0 :|: e'))))
bonrslf :: (BProd b, BSum b) => b e e' -> b (l3 :|: (f2 :&: ((e  :&: s0) :|: r1))) (l3 :|: (f2 :&: ((e' :&: s0) :|: r1)))
bonrsls :: (BProd b, BSum b) => b e e' -> b (l3 :|: (f2 :&: ((f0 :&: e ) :|: r1))) (l3 :|: (f2 :&: ((f0 :&: e') :|: r1)))
bonrsll :: (BProd b, BSum b) => b e e' -> b (l3 :|: (f2 :&: ((e  :|: r0) :|: r1))) (l3 :|: (f2 :&: ((e' :|: r0) :|: r1)))
bonrslr :: (BProd b, BSum b) => b e e' -> b (l3 :|: (f2 :&: ((l0 :|: e ) :|: r1))) (l3 :|: (f2 :&: ((l0 :|: e') :|: r1)))
bonrsrf :: (BProd b, BSum b) => b e e' -> b (l3 :|: (f2 :&: (l1 :|: (e  :&: s0)))) (l3 :|: (f2 :&: (l1 :|: (e' :&: s0))))
bonrsrs :: (BProd b, BSum b) => b e e' -> b (l3 :|: (f2 :&: (l1 :|: (f0 :&: e )))) (l3 :|: (f2 :&: (l1 :|: (f0 :&: e'))))
bonrsrl :: (BProd b, BSum b) => b e e' -> b (l3 :|: (f2 :&: (l1 :|: (e  :|: r0)))) (l3 :|: (f2 :&: (l1 :|: (e' :|: r0))))
bonrsrr :: (BProd b, BSum b) => b e e' -> b (l3 :|: (f2 :&: (l1 :|: (l0 :|: e )))) (l3 :|: (f2 :&: (l1 :|: (l0 :|: e'))))
bonrlff :: (BProd b, BSum b) => b e e' -> b (l3 :|: (((e  :&: s0) :&: s1) :|: r2)) (l3 :|: (((e' :&: s0) :&: s1) :|: r2))
bonrlfs :: (BProd b, BSum b) => b e e' -> b (l3 :|: (((f0 :&: e ) :&: s1) :|: r2)) (l3 :|: (((f0 :&: e') :&: s1) :|: r2))
bonrlfl :: (BProd b, BSum b) => b e e' -> b (l3 :|: (((e  :|: r0) :&: s1) :|: r2)) (l3 :|: (((e' :|: r0) :&: s1) :|: r2))
bonrlfr :: (BProd b, BSum b) => b e e' -> b (l3 :|: (((l0 :|: e ) :&: s1) :|: r2)) (l3 :|: (((l0 :|: e') :&: s1) :|: r2))
bonrlsf :: (BProd b, BSum b) => b e e' -> b (l3 :|: ((f1 :&: (e  :&: s0)) :|: r2)) (l3 :|: ((f1 :&: (e' :&: s0)) :|: r2))
bonrlss :: (BProd b, BSum b) => b e e' -> b (l3 :|: ((f1 :&: (f0 :&: e )) :|: r2)) (l3 :|: ((f1 :&: (f0 :&: e')) :|: r2))
bonrlsl :: (BProd b, BSum b) => b e e' -> b (l3 :|: ((f1 :&: (e  :|: r0)) :|: r2)) (l3 :|: ((f1 :&: (e' :|: r0)) :|: r2))
bonrlsr :: (BProd b, BSum b) => b e e' -> b (l3 :|: ((f1 :&: (l0 :|: e )) :|: r2)) (l3 :|: ((f1 :&: (l0 :|: e')) :|: r2))
bonrllf :: (BProd b, BSum b) => b e e' -> b (l3 :|: (((e  :&: s0) :|: r1) :|: r2)) (l3 :|: (((e' :&: s0) :|: r1) :|: r2))
bonrlls :: (BProd b, BSum b) => b e e' -> b (l3 :|: (((f0 :&: e ) :|: r1) :|: r2)) (l3 :|: (((f0 :&: e') :|: r1) :|: r2))
bonrlll :: (         BSum b) => b e e' -> b (l3 :|: (((e  :|: r0) :|: r1) :|: r2)) (l3 :|: (((e' :|: r0) :|: r1) :|: r2))
bonrllr :: (         BSum b) => b e e' -> b (l3 :|: (((l0 :|: e ) :|: r1) :|: r2)) (l3 :|: (((l0 :|: e') :|: r1) :|: r2))
bonrlrf :: (BProd b, BSum b) => b e e' -> b (l3 :|: ((l1 :|: (e  :&: s0)) :|: r2)) (l3 :|: ((l1 :|: (e' :&: s0)) :|: r2))
bonrlrs :: (BProd b, BSum b) => b e e' -> b (l3 :|: ((l1 :|: (f0 :&: e )) :|: r2)) (l3 :|: ((l1 :|: (f0 :&: e')) :|: r2))
bonrlrl :: (         BSum b) => b e e' -> b (l3 :|: ((l1 :|: (e  :|: r0)) :|: r2)) (l3 :|: ((l1 :|: (e' :|: r0)) :|: r2))
bonrlrr :: (         BSum b) => b e e' -> b (l3 :|: ((l1 :|: (l0 :|: e )) :|: r2)) (l3 :|: ((l1 :|: (l0 :|: e')) :|: r2))
bonrrff :: (BProd b, BSum b) => b e e' -> b (l3 :|: (l2 :|: ((e  :&: s0) :&: s1))) (l3 :|: (l2 :|: ((e' :&: s0) :&: s1)))
bonrrfs :: (BProd b, BSum b) => b e e' -> b (l3 :|: (l2 :|: ((f0 :&: e ) :&: s1))) (l3 :|: (l2 :|: ((f0 :&: e') :&: s1)))
bonrrfl :: (BProd b, BSum b) => b e e' -> b (l3 :|: (l2 :|: ((e  :|: r0) :&: s1))) (l3 :|: (l2 :|: ((e' :|: r0) :&: s1)))
bonrrfr :: (BProd b, BSum b) => b e e' -> b (l3 :|: (l2 :|: ((l0 :|: e ) :&: s1))) (l3 :|: (l2 :|: ((l0 :|: e') :&: s1)))
bonrrsf :: (BProd b, BSum b) => b e e' -> b (l3 :|: (l2 :|: (f1 :&: (e  :&: s0)))) (l3 :|: (l2 :|: (f1 :&: (e' :&: s0))))
bonrrss :: (BProd b, BSum b) => b e e' -> b (l3 :|: (l2 :|: (f1 :&: (f0 :&: e )))) (l3 :|: (l2 :|: (f1 :&: (f0 :&: e'))))
bonrrsl :: (BProd b, BSum b) => b e e' -> b (l3 :|: (l2 :|: (f1 :&: (e  :|: r0)))) (l3 :|: (l2 :|: (f1 :&: (e' :|: r0))))
bonrrsr :: (BProd b, BSum b) => b e e' -> b (l3 :|: (l2 :|: (f1 :&: (l0 :|: e )))) (l3 :|: (l2 :|: (f1 :&: (l0 :|: e'))))
bonrrlf :: (BProd b, BSum b) => b e e' -> b (l3 :|: (l2 :|: ((e  :&: s0) :|: r1))) (l3 :|: (l2 :|: ((e' :&: s0) :|: r1)))
bonrrls :: (BProd b, BSum b) => b e e' -> b (l3 :|: (l2 :|: ((f0 :&: e ) :|: r1))) (l3 :|: (l2 :|: ((f0 :&: e') :|: r1)))
bonrrll :: (         BSum b) => b e e' -> b (l3 :|: (l2 :|: ((e  :|: r0) :|: r1))) (l3 :|: (l2 :|: ((e' :|: r0) :|: r1)))
bonrrlr :: (         BSum b) => b e e' -> b (l3 :|: (l2 :|: ((l0 :|: e ) :|: r1))) (l3 :|: (l2 :|: ((l0 :|: e') :|: r1)))
bonrrrf :: (BProd b, BSum b) => b e e' -> b (l3 :|: (l2 :|: (l1 :|: (e  :&: s0)))) (l3 :|: (l2 :|: (l1 :|: (e' :&: s0))))
bonrrrs :: (BProd b, BSum b) => b e e' -> b (l3 :|: (l2 :|: (l1 :|: (f0 :&: e )))) (l3 :|: (l2 :|: (l1 :|: (f0 :&: e'))))
bonrrrl :: (         BSum b) => b e e' -> b (l3 :|: (l2 :|: (l1 :|: (e  :|: r0)))) (l3 :|: (l2 :|: (l1 :|: (e' :|: r0))))
bonrrrr :: (         BSum b) => b e e' -> b (l3 :|: (l2 :|: (l1 :|: (l0 :|: e )))) (l3 :|: (l2 :|: (l1 :|: (l0 :|: e'))))

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
bonf = bfirst  -- for consistent naming
bons = bsecond -- for consistent naming
bonl = bleft   -- for consistent naming
bonr = bright  -- for consistent naming
bonff = bonf . bonf
bonfs = bonf . bons
bonfl = bonf . bonl
bonfr = bonf . bonr
bonsf = bons . bonf
bonss = bons . bons
bonsl = bons . bonl
bonsr = bons . bonr
bonlf = bonl . bonf
bonls = bonl . bons
bonll = bonl . bonl
bonlr = bonl . bonr
bonrf = bonr . bonf
bonrs = bonr . bons
bonrl = bonr . bonl
bonrr = bonr . bonr
bonfff = bonf . bonff
bonffs = bonf . bonfs
bonffl = bonf . bonfl
bonffr = bonf . bonfr
bonfsf = bonf . bonsf
bonfss = bonf . bonss
bonfsl = bonf . bonsl
bonfsr = bonf . bonsr
bonflf = bonf . bonlf
bonfls = bonf . bonls
bonfll = bonf . bonll
bonflr = bonf . bonlr
bonfrf = bonf . bonrf
bonfrs = bonf . bonrs
bonfrl = bonf . bonrl
bonfrr = bonf . bonrr
bonsff = bons . bonff
bonsfs = bons . bonfs
bonsfl = bons . bonfl
bonsfr = bons . bonfr
bonssf = bons . bonsf
bonsss = bons . bonss
bonssl = bons . bonsl
bonssr = bons . bonsr
bonslf = bons . bonlf
bonsls = bons . bonls
bonsll = bons . bonll
bonslr = bons . bonlr
bonsrf = bons . bonrf
bonsrs = bons . bonrs
bonsrl = bons . bonrl
bonsrr = bons . bonrr
bonlff = bonl . bonff
bonlfs = bonl . bonfs
bonlfl = bonl . bonfl
bonlfr = bonl . bonfr
bonlsf = bonl . bonsf
bonlss = bonl . bonss
bonlsl = bonl . bonsl
bonlsr = bonl . bonsr
bonllf = bonl . bonlf
bonlls = bonl . bonls
bonlll = bonl . bonll
bonllr = bonl . bonlr
bonlrf = bonl . bonrf
bonlrs = bonl . bonrs
bonlrl = bonl . bonrl
bonlrr = bonl . bonrr
bonrff = bonr . bonff
bonrfs = bonr . bonfs
bonrfl = bonr . bonfl
bonrfr = bonr . bonfr
bonrsf = bonr . bonsf
bonrss = bonr . bonss
bonrsl = bonr . bonsl
bonrsr = bonr . bonsr
bonrlf = bonr . bonlf
bonrls = bonr . bonls
bonrll = bonr . bonll
bonrlr = bonr . bonlr
bonrrf = bonr . bonrf
bonrrs = bonr . bonrs
bonrrl = bonr . bonrl
bonrrr = bonr . bonrr
bonffff = bonf . bonfff
bonfffs = bonf . bonffs
bonfffl = bonf . bonffl
bonfffr = bonf . bonffr
bonffsf = bonf . bonfsf
bonffss = bonf . bonfss
bonffsl = bonf . bonfsl
bonffsr = bonf . bonfsr
bonfflf = bonf . bonflf
bonffls = bonf . bonfls
bonffll = bonf . bonfll
bonfflr = bonf . bonflr
bonffrf = bonf . bonfrf
bonffrs = bonf . bonfrs
bonffrl = bonf . bonfrl
bonffrr = bonf . bonfrr
bonfsff = bonf . bonsff
bonfsfs = bonf . bonsfs
bonfsfl = bonf . bonsfl
bonfsfr = bonf . bonsfr
bonfssf = bonf . bonssf
bonfsss = bonf . bonsss
bonfssl = bonf . bonssl
bonfssr = bonf . bonssr
bonfslf = bonf . bonslf
bonfsls = bonf . bonsls
bonfsll = bonf . bonsll
bonfslr = bonf . bonslr
bonfsrf = bonf . bonsrf
bonfsrs = bonf . bonsrs
bonfsrl = bonf . bonsrl
bonfsrr = bonf . bonsrr
bonflff = bonf . bonlff
bonflfs = bonf . bonlfs
bonflfl = bonf . bonlfl
bonflfr = bonf . bonlfr
bonflsf = bonf . bonlsf
bonflss = bonf . bonlss
bonflsl = bonf . bonlsl
bonflsr = bonf . bonlsr
bonfllf = bonf . bonllf
bonflls = bonf . bonlls
bonflll = bonf . bonlll
bonfllr = bonf . bonllr
bonflrf = bonf . bonlrf
bonflrs = bonf . bonlrs
bonflrl = bonf . bonlrl
bonflrr = bonf . bonlrr
bonfrff = bonf . bonrff
bonfrfs = bonf . bonrfs
bonfrfl = bonf . bonrfl
bonfrfr = bonf . bonrfr
bonfrsf = bonf . bonrsf
bonfrss = bonf . bonrss
bonfrsl = bonf . bonrsl
bonfrsr = bonf . bonrsr
bonfrlf = bonf . bonrlf
bonfrls = bonf . bonrls
bonfrll = bonf . bonrll
bonfrlr = bonf . bonrlr
bonfrrf = bonf . bonrrf
bonfrrs = bonf . bonrrs
bonfrrl = bonf . bonrrl
bonfrrr = bonf . bonrrr
bonsfff = bons . bonfff
bonsffs = bons . bonffs
bonsffl = bons . bonffl
bonsffr = bons . bonffr
bonsfsf = bons . bonfsf
bonsfss = bons . bonfss
bonsfsl = bons . bonfsl
bonsfsr = bons . bonfsr
bonsflf = bons . bonflf
bonsfls = bons . bonfls
bonsfll = bons . bonfll
bonsflr = bons . bonflr
bonsfrf = bons . bonfrf
bonsfrs = bons . bonfrs
bonsfrl = bons . bonfrl
bonsfrr = bons . bonfrr
bonssff = bons . bonsff
bonssfs = bons . bonsfs
bonssfl = bons . bonsfl
bonssfr = bons . bonsfr
bonsssf = bons . bonssf
bonssss = bons . bonsss
bonsssl = bons . bonssl
bonsssr = bons . bonssr
bonsslf = bons . bonslf
bonssls = bons . bonsls
bonssll = bons . bonsll
bonsslr = bons . bonslr
bonssrf = bons . bonsrf
bonssrs = bons . bonsrs
bonssrl = bons . bonsrl
bonssrr = bons . bonsrr
bonslff = bons . bonlff
bonslfs = bons . bonlfs
bonslfl = bons . bonlfl
bonslfr = bons . bonlfr
bonslsf = bons . bonlsf
bonslss = bons . bonlss
bonslsl = bons . bonlsl
bonslsr = bons . bonlsr
bonsllf = bons . bonllf
bonslls = bons . bonlls
bonslll = bons . bonlll
bonsllr = bons . bonllr
bonslrf = bons . bonlrf
bonslrs = bons . bonlrs
bonslrl = bons . bonlrl
bonslrr = bons . bonlrr
bonsrff = bons . bonrff
bonsrfs = bons . bonrfs
bonsrfl = bons . bonrfl
bonsrfr = bons . bonrfr
bonsrsf = bons . bonrsf
bonsrss = bons . bonrss
bonsrsl = bons . bonrsl
bonsrsr = bons . bonrsr
bonsrlf = bons . bonrlf
bonsrls = bons . bonrls
bonsrll = bons . bonrll
bonsrlr = bons . bonrlr
bonsrrf = bons . bonrrf
bonsrrs = bons . bonrrs
bonsrrl = bons . bonrrl
bonsrrr = bons . bonrrr
bonlfff = bonl . bonfff
bonlffs = bonl . bonffs
bonlffl = bonl . bonffl
bonlffr = bonl . bonffr
bonlfsf = bonl . bonfsf
bonlfss = bonl . bonfss
bonlfsl = bonl . bonfsl
bonlfsr = bonl . bonfsr
bonlflf = bonl . bonflf
bonlfls = bonl . bonfls
bonlfll = bonl . bonfll
bonlflr = bonl . bonflr
bonlfrf = bonl . bonfrf
bonlfrs = bonl . bonfrs
bonlfrl = bonl . bonfrl
bonlfrr = bonl . bonfrr
bonlsff = bonl . bonsff
bonlsfs = bonl . bonsfs
bonlsfl = bonl . bonsfl
bonlsfr = bonl . bonsfr
bonlssf = bonl . bonssf
bonlsss = bonl . bonsss
bonlssl = bonl . bonssl
bonlssr = bonl . bonssr
bonlslf = bonl . bonslf
bonlsls = bonl . bonsls
bonlsll = bonl . bonsll
bonlslr = bonl . bonslr
bonlsrf = bonl . bonsrf
bonlsrs = bonl . bonsrs
bonlsrl = bonl . bonsrl
bonlsrr = bonl . bonsrr
bonllff = bonl . bonlff
bonllfs = bonl . bonlfs
bonllfl = bonl . bonlfl
bonllfr = bonl . bonlfr
bonllsf = bonl . bonlsf
bonllss = bonl . bonlss
bonllsl = bonl . bonlsl
bonllsr = bonl . bonlsr
bonlllf = bonl . bonllf
bonllls = bonl . bonlls
bonllll = bonl . bonlll
bonlllr = bonl . bonllr
bonllrf = bonl . bonlrf
bonllrs = bonl . bonlrs
bonllrl = bonl . bonlrl
bonllrr = bonl . bonlrr
bonlrff = bonl . bonrff
bonlrfs = bonl . bonrfs
bonlrfl = bonl . bonrfl
bonlrfr = bonl . bonrfr
bonlrsf = bonl . bonrsf
bonlrss = bonl . bonrss
bonlrsl = bonl . bonrsl
bonlrsr = bonl . bonrsr
bonlrlf = bonl . bonrlf
bonlrls = bonl . bonrls
bonlrll = bonl . bonrll
bonlrlr = bonl . bonrlr
bonlrrf = bonl . bonrrf
bonlrrs = bonl . bonrrs
bonlrrl = bonl . bonrrl
bonlrrr = bonl . bonrrr
bonrfff = bonr . bonfff
bonrffs = bonr . bonffs
bonrffl = bonr . bonffl
bonrffr = bonr . bonffr
bonrfsf = bonr . bonfsf
bonrfss = bonr . bonfss
bonrfsl = bonr . bonfsl
bonrfsr = bonr . bonfsr
bonrflf = bonr . bonflf
bonrfls = bonr . bonfls
bonrfll = bonr . bonfll
bonrflr = bonr . bonflr
bonrfrf = bonr . bonfrf
bonrfrs = bonr . bonfrs
bonrfrl = bonr . bonfrl
bonrfrr = bonr . bonfrr
bonrsff = bonr . bonsff
bonrsfs = bonr . bonsfs
bonrsfl = bonr . bonsfl
bonrsfr = bonr . bonsfr
bonrssf = bonr . bonssf
bonrsss = bonr . bonsss
bonrssl = bonr . bonssl
bonrssr = bonr . bonssr
bonrslf = bonr . bonslf
bonrsls = bonr . bonsls
bonrsll = bonr . bonsll
bonrslr = bonr . bonslr
bonrsrf = bonr . bonsrf
bonrsrs = bonr . bonsrs
bonrsrl = bonr . bonsrl
bonrsrr = bonr . bonsrr
bonrlff = bonr . bonlff
bonrlfs = bonr . bonlfs
bonrlfl = bonr . bonlfl
bonrlfr = bonr . bonlfr
bonrlsf = bonr . bonlsf
bonrlss = bonr . bonlss
bonrlsl = bonr . bonlsl
bonrlsr = bonr . bonlsr
bonrllf = bonr . bonllf
bonrlls = bonr . bonlls
bonrlll = bonr . bonlll
bonrllr = bonr . bonllr
bonrlrf = bonr . bonlrf
bonrlrs = bonr . bonlrs
bonrlrl = bonr . bonlrl
bonrlrr = bonr . bonlrr
bonrrff = bonr . bonrff
bonrrfs = bonr . bonrfs
bonrrfl = bonr . bonrfl
bonrrfr = bonr . bonrfr
bonrrsf = bonr . bonrsf
bonrrss = bonr . bonrss
bonrrsl = bonr . bonrsl
bonrrsr = bonr . bonrsr
bonrrlf = bonr . bonrlf
bonrrls = bonr . bonrls
bonrrll = bonr . bonrll
bonrrlr = bonr . bonrlr
bonrrrf = bonr . bonrrf
bonrrrs = bonr . bonrrs
bonrrrl = bonr . bonrrl
bonrrrr = bonr . bonrrr

