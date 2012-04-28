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
-- These are defined up to 5 deep, for a total 1364 functions. The
-- shallow elements bonf (= bfirst), bons, bonl, bonr are included
-- for consistency.           
--                            
-- Similarly, rather than:    
--                            
-- > bfst >>> bsnd >>> bfst >>> bfst >>> bsnd 
--                            
-- Developers can write:      
--                            
-- > bxfsffs                  
--                            
-- This reads as `behavior extract first second first first second.
-- The forward order of characters corresponds to the path. This is
-- reverse the order of Lisp's car,cdr,cadadr conventions. 
--                            
-- To extract multiple elements, use the (&&&) behavior: 
--                            
--  > bxfsffs &&& bxssffs     
--                            
-- Note: extraction is NOT possible for left/right, due to duration 
-- coupling constraints. These are also defined up to 5 depth, for 
-- total 62 functions.       
--                           
-- The dual to bx* is also provided, just for completeness. This is 
-- another 60 functions for injection - `binlr` is `binl >>> binr`.
-- These might be useful if a behavior is a big switch, but that may 
-- be an anti-pattern (better to use a lot of small behaviors).
--                            
-- If 5 depth isn't enough, these operations are readily composed.
-- E.g. consider a stack-like environment:
--   extract 8th element: bxsssss >>> bxssf 
--   operate on 8th element: (bonsssss . bonssf) op 
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
module FRP.Sirea.Bdeep 
    ( bxf, bxs, bxff, bxfs, bxsf, bxss
    , bxfff, bxffs, bxfsf, bxfss, bxsff, bxsfs
    , bxssf, bxsss, bxffff, bxfffs, bxffsf, bxffss
    , bxfsff, bxfsfs, bxfssf, bxfsss, bxsfff, bxsffs
    , bxsfsf, bxsfss, bxssff, bxssfs, bxsssf, bxssss
    , bxfffff, bxffffs, bxfffsf, bxfffss, bxffsff, bxffsfs
    , bxffssf, bxffsss, bxfsfff, bxfsffs, bxfsfsf, bxfsfss
    , bxfssff, bxfssfs, bxfsssf, bxfssss, bxsffff, bxsfffs
    , bxsffsf, bxsffss, bxsfsff, bxsfsfs, bxsfssf, bxsfsss
    , bxssfff, bxssffs, bxssfsf, bxssfss, bxsssff, bxsssfs
    , bxssssf, bxsssss, bonf, bons, bonl, bonr
    , bonff, bonfs, bonfl, bonfr, bonsf, bonss
    , bonsl, bonsr, bonlf, bonls, bonll, bonlr
    , bonrf, bonrs, bonrl, bonrr, bonfff, bonffs
    , bonffl, bonffr, bonfsf, bonfss, bonfsl, bonfsr
    , bonflf, bonfls, bonfll, bonflr, bonfrf, bonfrs
    , bonfrl, bonfrr, bonsff, bonsfs, bonsfl, bonsfr
    , bonssf, bonsss, bonssl, bonssr, bonslf, bonsls
    , bonsll, bonslr, bonsrf, bonsrs, bonsrl, bonsrr
    , bonlff, bonlfs, bonlfl, bonlfr, bonlsf, bonlss
    , bonlsl, bonlsr, bonllf, bonlls, bonlll, bonllr
    , bonlrf, bonlrs, bonlrl, bonlrr, bonrff, bonrfs
    , bonrfl, bonrfr, bonrsf, bonrss, bonrsl, bonrsr
    , bonrlf, bonrls, bonrll, bonrlr, bonrrf, bonrrs
    , bonrrl, bonrrr, bonffff, bonfffs, bonfffl, bonfffr
    , bonffsf, bonffss, bonffsl, bonffsr, bonfflf, bonffls
    , bonffll, bonfflr, bonffrf, bonffrs, bonffrl, bonffrr
    , bonfsff, bonfsfs, bonfsfl, bonfsfr, bonfssf, bonfsss
    , bonfssl, bonfssr, bonfslf, bonfsls, bonfsll, bonfslr
    , bonfsrf, bonfsrs, bonfsrl, bonfsrr, bonflff, bonflfs
    , bonflfl, bonflfr, bonflsf, bonflss, bonflsl, bonflsr
    , bonfllf, bonflls, bonflll, bonfllr, bonflrf, bonflrs
    , bonflrl, bonflrr, bonfrff, bonfrfs, bonfrfl, bonfrfr
    , bonfrsf, bonfrss, bonfrsl, bonfrsr, bonfrlf, bonfrls
    , bonfrll, bonfrlr, bonfrrf, bonfrrs, bonfrrl, bonfrrr
    , bonsfff, bonsffs, bonsffl, bonsffr, bonsfsf, bonsfss
    , bonsfsl, bonsfsr, bonsflf, bonsfls, bonsfll, bonsflr
    , bonsfrf, bonsfrs, bonsfrl, bonsfrr, bonssff, bonssfs
    , bonssfl, bonssfr, bonsssf, bonssss, bonsssl, bonsssr
    , bonsslf, bonssls, bonssll, bonsslr, bonssrf, bonssrs
    , bonssrl, bonssrr, bonslff, bonslfs, bonslfl, bonslfr
    , bonslsf, bonslss, bonslsl, bonslsr, bonsllf, bonslls
    , bonslll, bonsllr, bonslrf, bonslrs, bonslrl, bonslrr
    , bonsrff, bonsrfs, bonsrfl, bonsrfr, bonsrsf, bonsrss
    , bonsrsl, bonsrsr, bonsrlf, bonsrls, bonsrll, bonsrlr
    , bonsrrf, bonsrrs, bonsrrl, bonsrrr, bonlfff, bonlffs
    , bonlffl, bonlffr, bonlfsf, bonlfss, bonlfsl, bonlfsr
    , bonlflf, bonlfls, bonlfll, bonlflr, bonlfrf, bonlfrs
    , bonlfrl, bonlfrr, bonlsff, bonlsfs, bonlsfl, bonlsfr
    , bonlssf, bonlsss, bonlssl, bonlssr, bonlslf, bonlsls
    , bonlsll, bonlslr, bonlsrf, bonlsrs, bonlsrl, bonlsrr
    , bonllff, bonllfs, bonllfl, bonllfr, bonllsf, bonllss
    , bonllsl, bonllsr, bonlllf, bonllls, bonllll, bonlllr
    , bonllrf, bonllrs, bonllrl, bonllrr, bonlrff, bonlrfs
    , bonlrfl, bonlrfr, bonlrsf, bonlrss, bonlrsl, bonlrsr
    , bonlrlf, bonlrls, bonlrll, bonlrlr, bonlrrf, bonlrrs
    , bonlrrl, bonlrrr, bonrfff, bonrffs, bonrffl, bonrffr
    , bonrfsf, bonrfss, bonrfsl, bonrfsr, bonrflf, bonrfls
    , bonrfll, bonrflr, bonrfrf, bonrfrs, bonrfrl, bonrfrr
    , bonrsff, bonrsfs, bonrsfl, bonrsfr, bonrssf, bonrsss
    , bonrssl, bonrssr, bonrslf, bonrsls, bonrsll, bonrslr
    , bonrsrf, bonrsrs, bonrsrl, bonrsrr, bonrlff, bonrlfs
    , bonrlfl, bonrlfr, bonrlsf, bonrlss, bonrlsl, bonrlsr
    , bonrllf, bonrlls, bonrlll, bonrllr, bonrlrf, bonrlrs
    , bonrlrl, bonrlrr, bonrrff, bonrrfs, bonrrfl, bonrrfr
    , bonrrsf, bonrrss, bonrrsl, bonrrsr, bonrrlf, bonrrls
    , bonrrll, bonrrlr, bonrrrf, bonrrrs, bonrrrl, bonrrrr
    , bonfffff, bonffffs, bonffffl, bonffffr, bonfffsf, bonfffss
    , bonfffsl, bonfffsr, bonffflf, bonfffls, bonfffll, bonffflr
    , bonfffrf, bonfffrs, bonfffrl, bonfffrr, bonffsff, bonffsfs
    , bonffsfl, bonffsfr, bonffssf, bonffsss, bonffssl, bonffssr
    , bonffslf, bonffsls, bonffsll, bonffslr, bonffsrf, bonffsrs
    , bonffsrl, bonffsrr, bonfflff, bonfflfs, bonfflfl, bonfflfr
    , bonfflsf, bonfflss, bonfflsl, bonfflsr, bonffllf, bonfflls
    , bonfflll, bonffllr, bonfflrf, bonfflrs, bonfflrl, bonfflrr
    , bonffrff, bonffrfs, bonffrfl, bonffrfr, bonffrsf, bonffrss
    , bonffrsl, bonffrsr, bonffrlf, bonffrls, bonffrll, bonffrlr
    , bonffrrf, bonffrrs, bonffrrl, bonffrrr, bonfsfff, bonfsffs
    , bonfsffl, bonfsffr, bonfsfsf, bonfsfss, bonfsfsl, bonfsfsr
    , bonfsflf, bonfsfls, bonfsfll, bonfsflr, bonfsfrf, bonfsfrs
    , bonfsfrl, bonfsfrr, bonfssff, bonfssfs, bonfssfl, bonfssfr
    , bonfsssf, bonfssss, bonfsssl, bonfsssr, bonfsslf, bonfssls
    , bonfssll, bonfsslr, bonfssrf, bonfssrs, bonfssrl, bonfssrr
    , bonfslff, bonfslfs, bonfslfl, bonfslfr, bonfslsf, bonfslss
    , bonfslsl, bonfslsr, bonfsllf, bonfslls, bonfslll, bonfsllr
    , bonfslrf, bonfslrs, bonfslrl, bonfslrr, bonfsrff, bonfsrfs
    , bonfsrfl, bonfsrfr, bonfsrsf, bonfsrss, bonfsrsl, bonfsrsr
    , bonfsrlf, bonfsrls, bonfsrll, bonfsrlr, bonfsrrf, bonfsrrs
    , bonfsrrl, bonfsrrr, bonflfff, bonflffs, bonflffl, bonflffr
    , bonflfsf, bonflfss, bonflfsl, bonflfsr, bonflflf, bonflfls
    , bonflfll, bonflflr, bonflfrf, bonflfrs, bonflfrl, bonflfrr
    , bonflsff, bonflsfs, bonflsfl, bonflsfr, bonflssf, bonflsss
    , bonflssl, bonflssr, bonflslf, bonflsls, bonflsll, bonflslr
    , bonflsrf, bonflsrs, bonflsrl, bonflsrr, bonfllff, bonfllfs
    , bonfllfl, bonfllfr, bonfllsf, bonfllss, bonfllsl, bonfllsr
    , bonflllf, bonfllls, bonfllll, bonflllr, bonfllrf, bonfllrs
    , bonfllrl, bonfllrr, bonflrff, bonflrfs, bonflrfl, bonflrfr
    , bonflrsf, bonflrss, bonflrsl, bonflrsr, bonflrlf, bonflrls
    , bonflrll, bonflrlr, bonflrrf, bonflrrs, bonflrrl, bonflrrr
    , bonfrfff, bonfrffs, bonfrffl, bonfrffr, bonfrfsf, bonfrfss
    , bonfrfsl, bonfrfsr, bonfrflf, bonfrfls, bonfrfll, bonfrflr
    , bonfrfrf, bonfrfrs, bonfrfrl, bonfrfrr, bonfrsff, bonfrsfs
    , bonfrsfl, bonfrsfr, bonfrssf, bonfrsss, bonfrssl, bonfrssr
    , bonfrslf, bonfrsls, bonfrsll, bonfrslr, bonfrsrf, bonfrsrs
    , bonfrsrl, bonfrsrr, bonfrlff, bonfrlfs, bonfrlfl, bonfrlfr
    , bonfrlsf, bonfrlss, bonfrlsl, bonfrlsr, bonfrllf, bonfrlls
    , bonfrlll, bonfrllr, bonfrlrf, bonfrlrs, bonfrlrl, bonfrlrr
    , bonfrrff, bonfrrfs, bonfrrfl, bonfrrfr, bonfrrsf, bonfrrss
    , bonfrrsl, bonfrrsr, bonfrrlf, bonfrrls, bonfrrll, bonfrrlr
    , bonfrrrf, bonfrrrs, bonfrrrl, bonfrrrr, bonsffff, bonsfffs
    , bonsfffl, bonsfffr, bonsffsf, bonsffss, bonsffsl, bonsffsr
    , bonsfflf, bonsffls, bonsffll, bonsfflr, bonsffrf, bonsffrs
    , bonsffrl, bonsffrr, bonsfsff, bonsfsfs, bonsfsfl, bonsfsfr
    , bonsfssf, bonsfsss, bonsfssl, bonsfssr, bonsfslf, bonsfsls
    , bonsfsll, bonsfslr, bonsfsrf, bonsfsrs, bonsfsrl, bonsfsrr
    , bonsflff, bonsflfs, bonsflfl, bonsflfr, bonsflsf, bonsflss
    , bonsflsl, bonsflsr, bonsfllf, bonsflls, bonsflll, bonsfllr
    , bonsflrf, bonsflrs, bonsflrl, bonsflrr, bonsfrff, bonsfrfs
    , bonsfrfl, bonsfrfr, bonsfrsf, bonsfrss, bonsfrsl, bonsfrsr
    , bonsfrlf, bonsfrls, bonsfrll, bonsfrlr, bonsfrrf, bonsfrrs
    , bonsfrrl, bonsfrrr, bonssfff, bonssffs, bonssffl, bonssffr
    , bonssfsf, bonssfss, bonssfsl, bonssfsr, bonssflf, bonssfls
    , bonssfll, bonssflr, bonssfrf, bonssfrs, bonssfrl, bonssfrr
    , bonsssff, bonsssfs, bonsssfl, bonsssfr, bonssssf, bonsssss
    , bonssssl, bonssssr, bonssslf, bonsssls, bonsssll, bonssslr
    , bonsssrf, bonsssrs, bonsssrl, bonsssrr, bonsslff, bonsslfs
    , bonsslfl, bonsslfr, bonsslsf, bonsslss, bonsslsl, bonsslsr
    , bonssllf, bonsslls, bonsslll, bonssllr, bonsslrf, bonsslrs
    , bonsslrl, bonsslrr, bonssrff, bonssrfs, bonssrfl, bonssrfr
    , bonssrsf, bonssrss, bonssrsl, bonssrsr, bonssrlf, bonssrls
    , bonssrll, bonssrlr, bonssrrf, bonssrrs, bonssrrl, bonssrrr
    , bonslfff, bonslffs, bonslffl, bonslffr, bonslfsf, bonslfss
    , bonslfsl, bonslfsr, bonslflf, bonslfls, bonslfll, bonslflr
    , bonslfrf, bonslfrs, bonslfrl, bonslfrr, bonslsff, bonslsfs
    , bonslsfl, bonslsfr, bonslssf, bonslsss, bonslssl, bonslssr
    , bonslslf, bonslsls, bonslsll, bonslslr, bonslsrf, bonslsrs
    , bonslsrl, bonslsrr, bonsllff, bonsllfs, bonsllfl, bonsllfr
    , bonsllsf, bonsllss, bonsllsl, bonsllsr, bonslllf, bonsllls
    , bonsllll, bonslllr, bonsllrf, bonsllrs, bonsllrl, bonsllrr
    , bonslrff, bonslrfs, bonslrfl, bonslrfr, bonslrsf, bonslrss
    , bonslrsl, bonslrsr, bonslrlf, bonslrls, bonslrll, bonslrlr
    , bonslrrf, bonslrrs, bonslrrl, bonslrrr, bonsrfff, bonsrffs
    , bonsrffl, bonsrffr, bonsrfsf, bonsrfss, bonsrfsl, bonsrfsr
    , bonsrflf, bonsrfls, bonsrfll, bonsrflr, bonsrfrf, bonsrfrs
    , bonsrfrl, bonsrfrr, bonsrsff, bonsrsfs, bonsrsfl, bonsrsfr
    , bonsrssf, bonsrsss, bonsrssl, bonsrssr, bonsrslf, bonsrsls
    , bonsrsll, bonsrslr, bonsrsrf, bonsrsrs, bonsrsrl, bonsrsrr
    , bonsrlff, bonsrlfs, bonsrlfl, bonsrlfr, bonsrlsf, bonsrlss
    , bonsrlsl, bonsrlsr, bonsrllf, bonsrlls, bonsrlll, bonsrllr
    , bonsrlrf, bonsrlrs, bonsrlrl, bonsrlrr, bonsrrff, bonsrrfs
    , bonsrrfl, bonsrrfr, bonsrrsf, bonsrrss, bonsrrsl, bonsrrsr
    , bonsrrlf, bonsrrls, bonsrrll, bonsrrlr, bonsrrrf, bonsrrrs
    , bonsrrrl, bonsrrrr, bonlffff, bonlfffs, bonlfffl, bonlfffr
    , bonlffsf, bonlffss, bonlffsl, bonlffsr, bonlfflf, bonlffls
    , bonlffll, bonlfflr, bonlffrf, bonlffrs, bonlffrl, bonlffrr
    , bonlfsff, bonlfsfs, bonlfsfl, bonlfsfr, bonlfssf, bonlfsss
    , bonlfssl, bonlfssr, bonlfslf, bonlfsls, bonlfsll, bonlfslr
    , bonlfsrf, bonlfsrs, bonlfsrl, bonlfsrr, bonlflff, bonlflfs
    , bonlflfl, bonlflfr, bonlflsf, bonlflss, bonlflsl, bonlflsr
    , bonlfllf, bonlflls, bonlflll, bonlfllr, bonlflrf, bonlflrs
    , bonlflrl, bonlflrr, bonlfrff, bonlfrfs, bonlfrfl, bonlfrfr
    , bonlfrsf, bonlfrss, bonlfrsl, bonlfrsr, bonlfrlf, bonlfrls
    , bonlfrll, bonlfrlr, bonlfrrf, bonlfrrs, bonlfrrl, bonlfrrr
    , bonlsfff, bonlsffs, bonlsffl, bonlsffr, bonlsfsf, bonlsfss
    , bonlsfsl, bonlsfsr, bonlsflf, bonlsfls, bonlsfll, bonlsflr
    , bonlsfrf, bonlsfrs, bonlsfrl, bonlsfrr, bonlssff, bonlssfs
    , bonlssfl, bonlssfr, bonlsssf, bonlssss, bonlsssl, bonlsssr
    , bonlsslf, bonlssls, bonlssll, bonlsslr, bonlssrf, bonlssrs
    , bonlssrl, bonlssrr, bonlslff, bonlslfs, bonlslfl, bonlslfr
    , bonlslsf, bonlslss, bonlslsl, bonlslsr, bonlsllf, bonlslls
    , bonlslll, bonlsllr, bonlslrf, bonlslrs, bonlslrl, bonlslrr
    , bonlsrff, bonlsrfs, bonlsrfl, bonlsrfr, bonlsrsf, bonlsrss
    , bonlsrsl, bonlsrsr, bonlsrlf, bonlsrls, bonlsrll, bonlsrlr
    , bonlsrrf, bonlsrrs, bonlsrrl, bonlsrrr, bonllfff, bonllffs
    , bonllffl, bonllffr, bonllfsf, bonllfss, bonllfsl, bonllfsr
    , bonllflf, bonllfls, bonllfll, bonllflr, bonllfrf, bonllfrs
    , bonllfrl, bonllfrr, bonllsff, bonllsfs, bonllsfl, bonllsfr
    , bonllssf, bonllsss, bonllssl, bonllssr, bonllslf, bonllsls
    , bonllsll, bonllslr, bonllsrf, bonllsrs, bonllsrl, bonllsrr
    , bonlllff, bonlllfs, bonlllfl, bonlllfr, bonlllsf, bonlllss
    , bonlllsl, bonlllsr, bonllllf, bonlllls, bonlllll, bonllllr
    , bonlllrf, bonlllrs, bonlllrl, bonlllrr, bonllrff, bonllrfs
    , bonllrfl, bonllrfr, bonllrsf, bonllrss, bonllrsl, bonllrsr
    , bonllrlf, bonllrls, bonllrll, bonllrlr, bonllrrf, bonllrrs
    , bonllrrl, bonllrrr, bonlrfff, bonlrffs, bonlrffl, bonlrffr
    , bonlrfsf, bonlrfss, bonlrfsl, bonlrfsr, bonlrflf, bonlrfls
    , bonlrfll, bonlrflr, bonlrfrf, bonlrfrs, bonlrfrl, bonlrfrr
    , bonlrsff, bonlrsfs, bonlrsfl, bonlrsfr, bonlrssf, bonlrsss
    , bonlrssl, bonlrssr, bonlrslf, bonlrsls, bonlrsll, bonlrslr
    , bonlrsrf, bonlrsrs, bonlrsrl, bonlrsrr, bonlrlff, bonlrlfs
    , bonlrlfl, bonlrlfr, bonlrlsf, bonlrlss, bonlrlsl, bonlrlsr
    , bonlrllf, bonlrlls, bonlrlll, bonlrllr, bonlrlrf, bonlrlrs
    , bonlrlrl, bonlrlrr, bonlrrff, bonlrrfs, bonlrrfl, bonlrrfr
    , bonlrrsf, bonlrrss, bonlrrsl, bonlrrsr, bonlrrlf, bonlrrls
    , bonlrrll, bonlrrlr, bonlrrrf, bonlrrrs, bonlrrrl, bonlrrrr
    , bonrffff, bonrfffs, bonrfffl, bonrfffr, bonrffsf, bonrffss
    , bonrffsl, bonrffsr, bonrfflf, bonrffls, bonrffll, bonrfflr
    , bonrffrf, bonrffrs, bonrffrl, bonrffrr, bonrfsff, bonrfsfs
    , bonrfsfl, bonrfsfr, bonrfssf, bonrfsss, bonrfssl, bonrfssr
    , bonrfslf, bonrfsls, bonrfsll, bonrfslr, bonrfsrf, bonrfsrs
    , bonrfsrl, bonrfsrr, bonrflff, bonrflfs, bonrflfl, bonrflfr
    , bonrflsf, bonrflss, bonrflsl, bonrflsr, bonrfllf, bonrflls
    , bonrflll, bonrfllr, bonrflrf, bonrflrs, bonrflrl, bonrflrr
    , bonrfrff, bonrfrfs, bonrfrfl, bonrfrfr, bonrfrsf, bonrfrss
    , bonrfrsl, bonrfrsr, bonrfrlf, bonrfrls, bonrfrll, bonrfrlr
    , bonrfrrf, bonrfrrs, bonrfrrl, bonrfrrr, bonrsfff, bonrsffs
    , bonrsffl, bonrsffr, bonrsfsf, bonrsfss, bonrsfsl, bonrsfsr
    , bonrsflf, bonrsfls, bonrsfll, bonrsflr, bonrsfrf, bonrsfrs
    , bonrsfrl, bonrsfrr, bonrssff, bonrssfs, bonrssfl, bonrssfr
    , bonrsssf, bonrssss, bonrsssl, bonrsssr, bonrsslf, bonrssls
    , bonrssll, bonrsslr, bonrssrf, bonrssrs, bonrssrl, bonrssrr
    , bonrslff, bonrslfs, bonrslfl, bonrslfr, bonrslsf, bonrslss
    , bonrslsl, bonrslsr, bonrsllf, bonrslls, bonrslll, bonrsllr
    , bonrslrf, bonrslrs, bonrslrl, bonrslrr, bonrsrff, bonrsrfs
    , bonrsrfl, bonrsrfr, bonrsrsf, bonrsrss, bonrsrsl, bonrsrsr
    , bonrsrlf, bonrsrls, bonrsrll, bonrsrlr, bonrsrrf, bonrsrrs
    , bonrsrrl, bonrsrrr, bonrlfff, bonrlffs, bonrlffl, bonrlffr
    , bonrlfsf, bonrlfss, bonrlfsl, bonrlfsr, bonrlflf, bonrlfls
    , bonrlfll, bonrlflr, bonrlfrf, bonrlfrs, bonrlfrl, bonrlfrr
    , bonrlsff, bonrlsfs, bonrlsfl, bonrlsfr, bonrlssf, bonrlsss
    , bonrlssl, bonrlssr, bonrlslf, bonrlsls, bonrlsll, bonrlslr
    , bonrlsrf, bonrlsrs, bonrlsrl, bonrlsrr, bonrllff, bonrllfs
    , bonrllfl, bonrllfr, bonrllsf, bonrllss, bonrllsl, bonrllsr
    , bonrlllf, bonrllls, bonrllll, bonrlllr, bonrllrf, bonrllrs
    , bonrllrl, bonrllrr, bonrlrff, bonrlrfs, bonrlrfl, bonrlrfr
    , bonrlrsf, bonrlrss, bonrlrsl, bonrlrsr, bonrlrlf, bonrlrls
    , bonrlrll, bonrlrlr, bonrlrrf, bonrlrrs, bonrlrrl, bonrlrrr
    , bonrrfff, bonrrffs, bonrrffl, bonrrffr, bonrrfsf, bonrrfss
    , bonrrfsl, bonrrfsr, bonrrflf, bonrrfls, bonrrfll, bonrrflr
    , bonrrfrf, bonrrfrs, bonrrfrl, bonrrfrr, bonrrsff, bonrrsfs
    , bonrrsfl, bonrrsfr, bonrrssf, bonrrsss, bonrrssl, bonrrssr
    , bonrrslf, bonrrsls, bonrrsll, bonrrslr, bonrrsrf, bonrrsrs
    , bonrrsrl, bonrrsrr, bonrrlff, bonrrlfs, bonrrlfl, bonrrlfr
    , bonrrlsf, bonrrlss, bonrrlsl, bonrrlsr, bonrrllf, bonrrlls
    , bonrrlll, bonrrllr, bonrrlrf, bonrrlrs, bonrrlrl, bonrrlrr
    , bonrrrff, bonrrrfs, bonrrrfl, bonrrrfr, bonrrrsf, bonrrrss
    , bonrrrsl, bonrrrsr, bonrrrlf, bonrrrls, bonrrrll, bonrrrlr
    , bonrrrrf, bonrrrrs, bonrrrrl, bonrrrrr
    ) where 
import FRP.Sirea.Behavior 


bxf :: b (e  :&: s0) e
bxs :: b (f0 :&: e ) e
bxff :: b ((e  :&: s0) :&: s1) e
bxfs :: b ((f0 :&: e ) :&: s1) e
bxsf :: b (f1 :&: (e  :&: s0)) e
bxss :: b (f1 :&: (f0 :&: e )) e
bxfff :: b (((e  :&: s0) :&: s1) :&: s2) e
bxffs :: b (((f0 :&: e ) :&: s1) :&: s2) e
bxfsf :: b ((f1 :&: (e  :&: s0)) :&: s2) e
bxfss :: b ((f1 :&: (f0 :&: e )) :&: s2) e
bxsff :: b (f2 :&: ((e  :&: s0) :&: s1)) e
bxsfs :: b (f2 :&: ((f0 :&: e ) :&: s1)) e
bxssf :: b (f2 :&: (f1 :&: (e  :&: s0))) e
bxsss :: b (f2 :&: (f1 :&: (f0 :&: e ))) e
bxffff :: b ((((e  :&: s0) :&: s1) :&: s2) :&: s3) e
bxfffs :: b ((((f0 :&: e ) :&: s1) :&: s2) :&: s3) e
bxffsf :: b (((f1 :&: (e  :&: s0)) :&: s2) :&: s3) e
bxffss :: b (((f1 :&: (f0 :&: e )) :&: s2) :&: s3) e
bxfsff :: b ((f2 :&: ((e  :&: s0) :&: s1)) :&: s3) e
bxfsfs :: b ((f2 :&: ((f0 :&: e ) :&: s1)) :&: s3) e
bxfssf :: b ((f2 :&: (f1 :&: (e  :&: s0))) :&: s3) e
bxfsss :: b ((f2 :&: (f1 :&: (f0 :&: e ))) :&: s3) e
bxsfff :: b (f3 :&: (((e  :&: s0) :&: s1) :&: s2)) e
bxsffs :: b (f3 :&: (((f0 :&: e ) :&: s1) :&: s2)) e
bxsfsf :: b (f3 :&: ((f1 :&: (e  :&: s0)) :&: s2)) e
bxsfss :: b (f3 :&: ((f1 :&: (f0 :&: e )) :&: s2)) e
bxssff :: b (f3 :&: (f2 :&: ((e  :&: s0) :&: s1))) e
bxssfs :: b (f3 :&: (f2 :&: ((f0 :&: e ) :&: s1))) e
bxsssf :: b (f3 :&: (f2 :&: (f1 :&: (e  :&: s0)))) e
bxssss :: b (f3 :&: (f2 :&: (f1 :&: (f0 :&: e )))) e
bxfffff :: b (((((e  :&: s0) :&: s1) :&: s2) :&: s3) :&: s4) e
bxffffs :: b (((((f0 :&: e ) :&: s1) :&: s2) :&: s3) :&: s4) e
bxfffsf :: b ((((f1 :&: (e  :&: s0)) :&: s2) :&: s3) :&: s4) e
bxfffss :: b ((((f1 :&: (f0 :&: e )) :&: s2) :&: s3) :&: s4) e
bxffsff :: b (((f2 :&: ((e  :&: s0) :&: s1)) :&: s3) :&: s4) e
bxffsfs :: b (((f2 :&: ((f0 :&: e ) :&: s1)) :&: s3) :&: s4) e
bxffssf :: b (((f2 :&: (f1 :&: (e  :&: s0))) :&: s3) :&: s4) e
bxffsss :: b (((f2 :&: (f1 :&: (f0 :&: e ))) :&: s3) :&: s4) e
bxfsfff :: b ((f3 :&: (((e  :&: s0) :&: s1) :&: s2)) :&: s4) e
bxfsffs :: b ((f3 :&: (((f0 :&: e ) :&: s1) :&: s2)) :&: s4) e
bxfsfsf :: b ((f3 :&: ((f1 :&: (e  :&: s0)) :&: s2)) :&: s4) e
bxfsfss :: b ((f3 :&: ((f1 :&: (f0 :&: e )) :&: s2)) :&: s4) e
bxfssff :: b ((f3 :&: (f2 :&: ((e  :&: s0) :&: s1))) :&: s4) e
bxfssfs :: b ((f3 :&: (f2 :&: ((f0 :&: e ) :&: s1))) :&: s4) e
bxfsssf :: b ((f3 :&: (f2 :&: (f1 :&: (e  :&: s0)))) :&: s4) e
bxfssss :: b ((f3 :&: (f2 :&: (f1 :&: (f0 :&: e )))) :&: s4) e
bxsffff :: b (f4 :&: ((((e  :&: s0) :&: s1) :&: s2) :&: s3)) e
bxsfffs :: b (f4 :&: ((((f0 :&: e ) :&: s1) :&: s2) :&: s3)) e
bxsffsf :: b (f4 :&: (((f1 :&: (e  :&: s0)) :&: s2) :&: s3)) e
bxsffss :: b (f4 :&: (((f1 :&: (f0 :&: e )) :&: s2) :&: s3)) e
bxsfsff :: b (f4 :&: ((f2 :&: ((e  :&: s0) :&: s1)) :&: s3)) e
bxsfsfs :: b (f4 :&: ((f2 :&: ((f0 :&: e ) :&: s1)) :&: s3)) e
bxsfssf :: b (f4 :&: ((f2 :&: (f1 :&: (e  :&: s0))) :&: s3)) e
bxsfsss :: b (f4 :&: ((f2 :&: (f1 :&: (f0 :&: e ))) :&: s3)) e
bxssfff :: b (f4 :&: (f3 :&: (((e  :&: s0) :&: s1) :&: s2))) e
bxssffs :: b (f4 :&: (f3 :&: (((f0 :&: e ) :&: s1) :&: s2))) e
bxssfsf :: b (f4 :&: (f3 :&: ((f1 :&: (e  :&: s0)) :&: s2))) e
bxssfss :: b (f4 :&: (f3 :&: ((f1 :&: (f0 :&: e )) :&: s2))) e
bxsssff :: b (f4 :&: (f3 :&: (f2 :&: ((e  :&: s0) :&: s1)))) e
bxsssfs :: b (f4 :&: (f3 :&: (f2 :&: ((f0 :&: e ) :&: s1)))) e
bxssssf :: b (f4 :&: (f3 :&: (f2 :&: (f1 :&: (e  :&: s0))))) e
bxsssss :: b (f4 :&: (f3 :&: (f2 :&: (f1 :&: (f0 :&: e ))))) e
-- binl is defined in FRP.Sirea.Behavior.
-- binr is defined in FRP.Sirea.Behavior.
binll :: b e ((e  :|: r0) :|: r1)
binlr :: b e ((l0 :|: e ) :|: r1)
binrl :: b e (l1 :|: (e  :|: r0))
binrr :: b e (l1 :|: (l0 :|: e ))
binlll :: b e (((e  :|: r0) :|: r1) :|: r2)
binllr :: b e (((l0 :|: e ) :|: r1) :|: r2)
binlrl :: b e ((l1 :|: (e  :|: r0)) :|: r2)
binlrr :: b e ((l1 :|: (l0 :|: e )) :|: r2)
binrll :: b e (l2 :|: ((e  :|: r0) :|: r1))
binrlr :: b e (l2 :|: ((l0 :|: e ) :|: r1))
binrrl :: b e (l2 :|: (l1 :|: (e  :|: r0)))
binrrr :: b e (l2 :|: (l1 :|: (l0 :|: e )))
binllll :: b e ((((e  :|: r0) :|: r1) :|: r2) :|: r3)
binlllr :: b e ((((l0 :|: e ) :|: r1) :|: r2) :|: r3)
binllrl :: b e (((l1 :|: (e  :|: r0)) :|: r2) :|: r3)
binllrr :: b e (((l1 :|: (l0 :|: e )) :|: r2) :|: r3)
binlrll :: b e ((l2 :|: ((e  :|: r0) :|: r1)) :|: r3)
binlrlr :: b e ((l2 :|: ((l0 :|: e ) :|: r1)) :|: r3)
binlrrl :: b e ((l2 :|: (l1 :|: (e  :|: r0))) :|: r3)
binlrrr :: b e ((l2 :|: (l1 :|: (l0 :|: e ))) :|: r3)
binrlll :: b e (l3 :|: (((e  :|: r0) :|: r1) :|: r2))
binrllr :: b e (l3 :|: (((l0 :|: e ) :|: r1) :|: r2))
binrlrl :: b e (l3 :|: ((l1 :|: (e  :|: r0)) :|: r2))
binrlrr :: b e (l3 :|: ((l1 :|: (l0 :|: e )) :|: r2))
binrrll :: b e (l3 :|: (l2 :|: ((e  :|: r0) :|: r1)))
binrrlr :: b e (l3 :|: (l2 :|: ((l0 :|: e ) :|: r1)))
binrrrl :: b e (l3 :|: (l2 :|: (l1 :|: (e  :|: r0))))
binrrrr :: b e (l3 :|: (l2 :|: (l1 :|: (l0 :|: e ))))
binlllll :: b e (((((e  :|: r0) :|: r1) :|: r2) :|: r3) :|: r4)
binllllr :: b e (((((l0 :|: e ) :|: r1) :|: r2) :|: r3) :|: r4)
binlllrl :: b e ((((l1 :|: (e  :|: r0)) :|: r2) :|: r3) :|: r4)
binlllrr :: b e ((((l1 :|: (l0 :|: e )) :|: r2) :|: r3) :|: r4)
binllrll :: b e (((l2 :|: ((e  :|: r0) :|: r1)) :|: r3) :|: r4)
binllrlr :: b e (((l2 :|: ((l0 :|: e ) :|: r1)) :|: r3) :|: r4)
binllrrl :: b e (((l2 :|: (l1 :|: (e  :|: r0))) :|: r3) :|: r4)
binllrrr :: b e (((l2 :|: (l1 :|: (l0 :|: e ))) :|: r3) :|: r4)
binlrlll :: b e ((l3 :|: (((e  :|: r0) :|: r1) :|: r2)) :|: r4)
binlrllr :: b e ((l3 :|: (((l0 :|: e ) :|: r1) :|: r2)) :|: r4)
binlrlrl :: b e ((l3 :|: ((l1 :|: (e  :|: r0)) :|: r2)) :|: r4)
binlrlrr :: b e ((l3 :|: ((l1 :|: (l0 :|: e )) :|: r2)) :|: r4)
binlrrll :: b e ((l3 :|: (l2 :|: ((e  :|: r0) :|: r1))) :|: r4)
binlrrlr :: b e ((l3 :|: (l2 :|: ((l0 :|: e ) :|: r1))) :|: r4)
binlrrrl :: b e ((l3 :|: (l2 :|: (l1 :|: (e  :|: r0)))) :|: r4)
binlrrrr :: b e ((l3 :|: (l2 :|: (l1 :|: (l0 :|: e )))) :|: r4)
binrllll :: b e (l4 :|: ((((e  :|: r0) :|: r1) :|: r2) :|: r3))
binrlllr :: b e (l4 :|: ((((l0 :|: e ) :|: r1) :|: r2) :|: r3))
binrllrl :: b e (l4 :|: (((l1 :|: (e  :|: r0)) :|: r2) :|: r3))
binrllrr :: b e (l4 :|: (((l1 :|: (l0 :|: e )) :|: r2) :|: r3))
binrlrll :: b e (l4 :|: ((l2 :|: ((e  :|: r0) :|: r1)) :|: r3))
binrlrlr :: b e (l4 :|: ((l2 :|: ((l0 :|: e ) :|: r1)) :|: r3))
binrlrrl :: b e (l4 :|: ((l2 :|: (l1 :|: (e  :|: r0))) :|: r3))
binrlrrr :: b e (l4 :|: ((l2 :|: (l1 :|: (l0 :|: e ))) :|: r3))
binrrlll :: b e (l4 :|: (l3 :|: (((e  :|: r0) :|: r1) :|: r2)))
binrrllr :: b e (l4 :|: (l3 :|: (((l0 :|: e ) :|: r1) :|: r2)))
binrrlrl :: b e (l4 :|: (l3 :|: ((l1 :|: (e  :|: r0)) :|: r2)))
binrrlrr :: b e (l4 :|: (l3 :|: ((l1 :|: (l0 :|: e )) :|: r2)))
binrrrll :: b e (l4 :|: (l3 :|: (l2 :|: ((e  :|: r0) :|: r1))))
binrrrlr :: b e (l4 :|: (l3 :|: (l2 :|: ((l0 :|: e ) :|: r1))))
binrrrrl :: b e (l4 :|: (l3 :|: (l2 :|: (l1 :|: (e  :|: r0)))))
binrrrrr :: b e (l4 :|: (l3 :|: (l2 :|: (l1 :|: (l0 :|: e )))))
bonf :: b e e' -> b (e  :&: s0) (e' :&: s0)
bons :: b e e' -> b (f0 :&: e ) (f0 :&: e')
bonl :: b e e' -> b (e  :|: r0) (e' :|: r0)
bonr :: b e e' -> b (l0 :|: e ) (l0 :|: e')
bonff :: b e e' -> b ((e  :&: s0) :&: s1) ((e' :&: s0) :&: s1)
bonfs :: b e e' -> b ((f0 :&: e ) :&: s1) ((f0 :&: e') :&: s1)
bonfl :: b e e' -> b ((e  :|: r0) :&: s1) ((e' :|: r0) :&: s1)
bonfr :: b e e' -> b ((l0 :|: e ) :&: s1) ((l0 :|: e') :&: s1)
bonsf :: b e e' -> b (f1 :&: (e  :&: s0)) (f1 :&: (e' :&: s0))
bonss :: b e e' -> b (f1 :&: (f0 :&: e )) (f1 :&: (f0 :&: e'))
bonsl :: b e e' -> b (f1 :&: (e  :|: r0)) (f1 :&: (e' :|: r0))
bonsr :: b e e' -> b (f1 :&: (l0 :|: e )) (f1 :&: (l0 :|: e'))
bonlf :: b e e' -> b ((e  :&: s0) :|: r1) ((e' :&: s0) :|: r1)
bonls :: b e e' -> b ((f0 :&: e ) :|: r1) ((f0 :&: e') :|: r1)
bonll :: b e e' -> b ((e  :|: r0) :|: r1) ((e' :|: r0) :|: r1)
bonlr :: b e e' -> b ((l0 :|: e ) :|: r1) ((l0 :|: e') :|: r1)
bonrf :: b e e' -> b (l1 :|: (e  :&: s0)) (l1 :|: (e' :&: s0))
bonrs :: b e e' -> b (l1 :|: (f0 :&: e )) (l1 :|: (f0 :&: e'))
bonrl :: b e e' -> b (l1 :|: (e  :|: r0)) (l1 :|: (e' :|: r0))
bonrr :: b e e' -> b (l1 :|: (l0 :|: e )) (l1 :|: (l0 :|: e'))
bonfff :: b e e' -> b (((e  :&: s0) :&: s1) :&: s2) (((e' :&: s0) :&: s1) :&: s2)
bonffs :: b e e' -> b (((f0 :&: e ) :&: s1) :&: s2) (((f0 :&: e') :&: s1) :&: s2)
bonffl :: b e e' -> b (((e  :|: r0) :&: s1) :&: s2) (((e' :|: r0) :&: s1) :&: s2)
bonffr :: b e e' -> b (((l0 :|: e ) :&: s1) :&: s2) (((l0 :|: e') :&: s1) :&: s2)
bonfsf :: b e e' -> b ((f1 :&: (e  :&: s0)) :&: s2) ((f1 :&: (e' :&: s0)) :&: s2)
bonfss :: b e e' -> b ((f1 :&: (f0 :&: e )) :&: s2) ((f1 :&: (f0 :&: e')) :&: s2)
bonfsl :: b e e' -> b ((f1 :&: (e  :|: r0)) :&: s2) ((f1 :&: (e' :|: r0)) :&: s2)
bonfsr :: b e e' -> b ((f1 :&: (l0 :|: e )) :&: s2) ((f1 :&: (l0 :|: e')) :&: s2)
bonflf :: b e e' -> b (((e  :&: s0) :|: r1) :&: s2) (((e' :&: s0) :|: r1) :&: s2)
bonfls :: b e e' -> b (((f0 :&: e ) :|: r1) :&: s2) (((f0 :&: e') :|: r1) :&: s2)
bonfll :: b e e' -> b (((e  :|: r0) :|: r1) :&: s2) (((e' :|: r0) :|: r1) :&: s2)
bonflr :: b e e' -> b (((l0 :|: e ) :|: r1) :&: s2) (((l0 :|: e') :|: r1) :&: s2)
bonfrf :: b e e' -> b ((l1 :|: (e  :&: s0)) :&: s2) ((l1 :|: (e' :&: s0)) :&: s2)
bonfrs :: b e e' -> b ((l1 :|: (f0 :&: e )) :&: s2) ((l1 :|: (f0 :&: e')) :&: s2)
bonfrl :: b e e' -> b ((l1 :|: (e  :|: r0)) :&: s2) ((l1 :|: (e' :|: r0)) :&: s2)
bonfrr :: b e e' -> b ((l1 :|: (l0 :|: e )) :&: s2) ((l1 :|: (l0 :|: e')) :&: s2)
bonsff :: b e e' -> b (f2 :&: ((e  :&: s0) :&: s1)) (f2 :&: ((e' :&: s0) :&: s1))
bonsfs :: b e e' -> b (f2 :&: ((f0 :&: e ) :&: s1)) (f2 :&: ((f0 :&: e') :&: s1))
bonsfl :: b e e' -> b (f2 :&: ((e  :|: r0) :&: s1)) (f2 :&: ((e' :|: r0) :&: s1))
bonsfr :: b e e' -> b (f2 :&: ((l0 :|: e ) :&: s1)) (f2 :&: ((l0 :|: e') :&: s1))
bonssf :: b e e' -> b (f2 :&: (f1 :&: (e  :&: s0))) (f2 :&: (f1 :&: (e' :&: s0)))
bonsss :: b e e' -> b (f2 :&: (f1 :&: (f0 :&: e ))) (f2 :&: (f1 :&: (f0 :&: e')))
bonssl :: b e e' -> b (f2 :&: (f1 :&: (e  :|: r0))) (f2 :&: (f1 :&: (e' :|: r0)))
bonssr :: b e e' -> b (f2 :&: (f1 :&: (l0 :|: e ))) (f2 :&: (f1 :&: (l0 :|: e')))
bonslf :: b e e' -> b (f2 :&: ((e  :&: s0) :|: r1)) (f2 :&: ((e' :&: s0) :|: r1))
bonsls :: b e e' -> b (f2 :&: ((f0 :&: e ) :|: r1)) (f2 :&: ((f0 :&: e') :|: r1))
bonsll :: b e e' -> b (f2 :&: ((e  :|: r0) :|: r1)) (f2 :&: ((e' :|: r0) :|: r1))
bonslr :: b e e' -> b (f2 :&: ((l0 :|: e ) :|: r1)) (f2 :&: ((l0 :|: e') :|: r1))
bonsrf :: b e e' -> b (f2 :&: (l1 :|: (e  :&: s0))) (f2 :&: (l1 :|: (e' :&: s0)))
bonsrs :: b e e' -> b (f2 :&: (l1 :|: (f0 :&: e ))) (f2 :&: (l1 :|: (f0 :&: e')))
bonsrl :: b e e' -> b (f2 :&: (l1 :|: (e  :|: r0))) (f2 :&: (l1 :|: (e' :|: r0)))
bonsrr :: b e e' -> b (f2 :&: (l1 :|: (l0 :|: e ))) (f2 :&: (l1 :|: (l0 :|: e')))
bonlff :: b e e' -> b (((e  :&: s0) :&: s1) :|: r2) (((e' :&: s0) :&: s1) :|: r2)
bonlfs :: b e e' -> b (((f0 :&: e ) :&: s1) :|: r2) (((f0 :&: e') :&: s1) :|: r2)
bonlfl :: b e e' -> b (((e  :|: r0) :&: s1) :|: r2) (((e' :|: r0) :&: s1) :|: r2)
bonlfr :: b e e' -> b (((l0 :|: e ) :&: s1) :|: r2) (((l0 :|: e') :&: s1) :|: r2)
bonlsf :: b e e' -> b ((f1 :&: (e  :&: s0)) :|: r2) ((f1 :&: (e' :&: s0)) :|: r2)
bonlss :: b e e' -> b ((f1 :&: (f0 :&: e )) :|: r2) ((f1 :&: (f0 :&: e')) :|: r2)
bonlsl :: b e e' -> b ((f1 :&: (e  :|: r0)) :|: r2) ((f1 :&: (e' :|: r0)) :|: r2)
bonlsr :: b e e' -> b ((f1 :&: (l0 :|: e )) :|: r2) ((f1 :&: (l0 :|: e')) :|: r2)
bonllf :: b e e' -> b (((e  :&: s0) :|: r1) :|: r2) (((e' :&: s0) :|: r1) :|: r2)
bonlls :: b e e' -> b (((f0 :&: e ) :|: r1) :|: r2) (((f0 :&: e') :|: r1) :|: r2)
bonlll :: b e e' -> b (((e  :|: r0) :|: r1) :|: r2) (((e' :|: r0) :|: r1) :|: r2)
bonllr :: b e e' -> b (((l0 :|: e ) :|: r1) :|: r2) (((l0 :|: e') :|: r1) :|: r2)
bonlrf :: b e e' -> b ((l1 :|: (e  :&: s0)) :|: r2) ((l1 :|: (e' :&: s0)) :|: r2)
bonlrs :: b e e' -> b ((l1 :|: (f0 :&: e )) :|: r2) ((l1 :|: (f0 :&: e')) :|: r2)
bonlrl :: b e e' -> b ((l1 :|: (e  :|: r0)) :|: r2) ((l1 :|: (e' :|: r0)) :|: r2)
bonlrr :: b e e' -> b ((l1 :|: (l0 :|: e )) :|: r2) ((l1 :|: (l0 :|: e')) :|: r2)
bonrff :: b e e' -> b (l2 :|: ((e  :&: s0) :&: s1)) (l2 :|: ((e' :&: s0) :&: s1))
bonrfs :: b e e' -> b (l2 :|: ((f0 :&: e ) :&: s1)) (l2 :|: ((f0 :&: e') :&: s1))
bonrfl :: b e e' -> b (l2 :|: ((e  :|: r0) :&: s1)) (l2 :|: ((e' :|: r0) :&: s1))
bonrfr :: b e e' -> b (l2 :|: ((l0 :|: e ) :&: s1)) (l2 :|: ((l0 :|: e') :&: s1))
bonrsf :: b e e' -> b (l2 :|: (f1 :&: (e  :&: s0))) (l2 :|: (f1 :&: (e' :&: s0)))
bonrss :: b e e' -> b (l2 :|: (f1 :&: (f0 :&: e ))) (l2 :|: (f1 :&: (f0 :&: e')))
bonrsl :: b e e' -> b (l2 :|: (f1 :&: (e  :|: r0))) (l2 :|: (f1 :&: (e' :|: r0)))
bonrsr :: b e e' -> b (l2 :|: (f1 :&: (l0 :|: e ))) (l2 :|: (f1 :&: (l0 :|: e')))
bonrlf :: b e e' -> b (l2 :|: ((e  :&: s0) :|: r1)) (l2 :|: ((e' :&: s0) :|: r1))
bonrls :: b e e' -> b (l2 :|: ((f0 :&: e ) :|: r1)) (l2 :|: ((f0 :&: e') :|: r1))
bonrll :: b e e' -> b (l2 :|: ((e  :|: r0) :|: r1)) (l2 :|: ((e' :|: r0) :|: r1))
bonrlr :: b e e' -> b (l2 :|: ((l0 :|: e ) :|: r1)) (l2 :|: ((l0 :|: e') :|: r1))
bonrrf :: b e e' -> b (l2 :|: (l1 :|: (e  :&: s0))) (l2 :|: (l1 :|: (e' :&: s0)))
bonrrs :: b e e' -> b (l2 :|: (l1 :|: (f0 :&: e ))) (l2 :|: (l1 :|: (f0 :&: e')))
bonrrl :: b e e' -> b (l2 :|: (l1 :|: (e  :|: r0))) (l2 :|: (l1 :|: (e' :|: r0)))
bonrrr :: b e e' -> b (l2 :|: (l1 :|: (l0 :|: e ))) (l2 :|: (l1 :|: (l0 :|: e')))
bonffff :: b e e' -> b ((((e  :&: s0) :&: s1) :&: s2) :&: s3) ((((e' :&: s0) :&: s1) :&: s2) :&: s3)
bonfffs :: b e e' -> b ((((f0 :&: e ) :&: s1) :&: s2) :&: s3) ((((f0 :&: e') :&: s1) :&: s2) :&: s3)
bonfffl :: b e e' -> b ((((e  :|: r0) :&: s1) :&: s2) :&: s3) ((((e' :|: r0) :&: s1) :&: s2) :&: s3)
bonfffr :: b e e' -> b ((((l0 :|: e ) :&: s1) :&: s2) :&: s3) ((((l0 :|: e') :&: s1) :&: s2) :&: s3)
bonffsf :: b e e' -> b (((f1 :&: (e  :&: s0)) :&: s2) :&: s3) (((f1 :&: (e' :&: s0)) :&: s2) :&: s3)
bonffss :: b e e' -> b (((f1 :&: (f0 :&: e )) :&: s2) :&: s3) (((f1 :&: (f0 :&: e')) :&: s2) :&: s3)
bonffsl :: b e e' -> b (((f1 :&: (e  :|: r0)) :&: s2) :&: s3) (((f1 :&: (e' :|: r0)) :&: s2) :&: s3)
bonffsr :: b e e' -> b (((f1 :&: (l0 :|: e )) :&: s2) :&: s3) (((f1 :&: (l0 :|: e')) :&: s2) :&: s3)
bonfflf :: b e e' -> b ((((e  :&: s0) :|: r1) :&: s2) :&: s3) ((((e' :&: s0) :|: r1) :&: s2) :&: s3)
bonffls :: b e e' -> b ((((f0 :&: e ) :|: r1) :&: s2) :&: s3) ((((f0 :&: e') :|: r1) :&: s2) :&: s3)
bonffll :: b e e' -> b ((((e  :|: r0) :|: r1) :&: s2) :&: s3) ((((e' :|: r0) :|: r1) :&: s2) :&: s3)
bonfflr :: b e e' -> b ((((l0 :|: e ) :|: r1) :&: s2) :&: s3) ((((l0 :|: e') :|: r1) :&: s2) :&: s3)
bonffrf :: b e e' -> b (((l1 :|: (e  :&: s0)) :&: s2) :&: s3) (((l1 :|: (e' :&: s0)) :&: s2) :&: s3)
bonffrs :: b e e' -> b (((l1 :|: (f0 :&: e )) :&: s2) :&: s3) (((l1 :|: (f0 :&: e')) :&: s2) :&: s3)
bonffrl :: b e e' -> b (((l1 :|: (e  :|: r0)) :&: s2) :&: s3) (((l1 :|: (e' :|: r0)) :&: s2) :&: s3)
bonffrr :: b e e' -> b (((l1 :|: (l0 :|: e )) :&: s2) :&: s3) (((l1 :|: (l0 :|: e')) :&: s2) :&: s3)
bonfsff :: b e e' -> b ((f2 :&: ((e  :&: s0) :&: s1)) :&: s3) ((f2 :&: ((e' :&: s0) :&: s1)) :&: s3)
bonfsfs :: b e e' -> b ((f2 :&: ((f0 :&: e ) :&: s1)) :&: s3) ((f2 :&: ((f0 :&: e') :&: s1)) :&: s3)
bonfsfl :: b e e' -> b ((f2 :&: ((e  :|: r0) :&: s1)) :&: s3) ((f2 :&: ((e' :|: r0) :&: s1)) :&: s3)
bonfsfr :: b e e' -> b ((f2 :&: ((l0 :|: e ) :&: s1)) :&: s3) ((f2 :&: ((l0 :|: e') :&: s1)) :&: s3)
bonfssf :: b e e' -> b ((f2 :&: (f1 :&: (e  :&: s0))) :&: s3) ((f2 :&: (f1 :&: (e' :&: s0))) :&: s3)
bonfsss :: b e e' -> b ((f2 :&: (f1 :&: (f0 :&: e ))) :&: s3) ((f2 :&: (f1 :&: (f0 :&: e'))) :&: s3)
bonfssl :: b e e' -> b ((f2 :&: (f1 :&: (e  :|: r0))) :&: s3) ((f2 :&: (f1 :&: (e' :|: r0))) :&: s3)
bonfssr :: b e e' -> b ((f2 :&: (f1 :&: (l0 :|: e ))) :&: s3) ((f2 :&: (f1 :&: (l0 :|: e'))) :&: s3)
bonfslf :: b e e' -> b ((f2 :&: ((e  :&: s0) :|: r1)) :&: s3) ((f2 :&: ((e' :&: s0) :|: r1)) :&: s3)
bonfsls :: b e e' -> b ((f2 :&: ((f0 :&: e ) :|: r1)) :&: s3) ((f2 :&: ((f0 :&: e') :|: r1)) :&: s3)
bonfsll :: b e e' -> b ((f2 :&: ((e  :|: r0) :|: r1)) :&: s3) ((f2 :&: ((e' :|: r0) :|: r1)) :&: s3)
bonfslr :: b e e' -> b ((f2 :&: ((l0 :|: e ) :|: r1)) :&: s3) ((f2 :&: ((l0 :|: e') :|: r1)) :&: s3)
bonfsrf :: b e e' -> b ((f2 :&: (l1 :|: (e  :&: s0))) :&: s3) ((f2 :&: (l1 :|: (e' :&: s0))) :&: s3)
bonfsrs :: b e e' -> b ((f2 :&: (l1 :|: (f0 :&: e ))) :&: s3) ((f2 :&: (l1 :|: (f0 :&: e'))) :&: s3)
bonfsrl :: b e e' -> b ((f2 :&: (l1 :|: (e  :|: r0))) :&: s3) ((f2 :&: (l1 :|: (e' :|: r0))) :&: s3)
bonfsrr :: b e e' -> b ((f2 :&: (l1 :|: (l0 :|: e ))) :&: s3) ((f2 :&: (l1 :|: (l0 :|: e'))) :&: s3)
bonflff :: b e e' -> b ((((e  :&: s0) :&: s1) :|: r2) :&: s3) ((((e' :&: s0) :&: s1) :|: r2) :&: s3)
bonflfs :: b e e' -> b ((((f0 :&: e ) :&: s1) :|: r2) :&: s3) ((((f0 :&: e') :&: s1) :|: r2) :&: s3)
bonflfl :: b e e' -> b ((((e  :|: r0) :&: s1) :|: r2) :&: s3) ((((e' :|: r0) :&: s1) :|: r2) :&: s3)
bonflfr :: b e e' -> b ((((l0 :|: e ) :&: s1) :|: r2) :&: s3) ((((l0 :|: e') :&: s1) :|: r2) :&: s3)
bonflsf :: b e e' -> b (((f1 :&: (e  :&: s0)) :|: r2) :&: s3) (((f1 :&: (e' :&: s0)) :|: r2) :&: s3)
bonflss :: b e e' -> b (((f1 :&: (f0 :&: e )) :|: r2) :&: s3) (((f1 :&: (f0 :&: e')) :|: r2) :&: s3)
bonflsl :: b e e' -> b (((f1 :&: (e  :|: r0)) :|: r2) :&: s3) (((f1 :&: (e' :|: r0)) :|: r2) :&: s3)
bonflsr :: b e e' -> b (((f1 :&: (l0 :|: e )) :|: r2) :&: s3) (((f1 :&: (l0 :|: e')) :|: r2) :&: s3)
bonfllf :: b e e' -> b ((((e  :&: s0) :|: r1) :|: r2) :&: s3) ((((e' :&: s0) :|: r1) :|: r2) :&: s3)
bonflls :: b e e' -> b ((((f0 :&: e ) :|: r1) :|: r2) :&: s3) ((((f0 :&: e') :|: r1) :|: r2) :&: s3)
bonflll :: b e e' -> b ((((e  :|: r0) :|: r1) :|: r2) :&: s3) ((((e' :|: r0) :|: r1) :|: r2) :&: s3)
bonfllr :: b e e' -> b ((((l0 :|: e ) :|: r1) :|: r2) :&: s3) ((((l0 :|: e') :|: r1) :|: r2) :&: s3)
bonflrf :: b e e' -> b (((l1 :|: (e  :&: s0)) :|: r2) :&: s3) (((l1 :|: (e' :&: s0)) :|: r2) :&: s3)
bonflrs :: b e e' -> b (((l1 :|: (f0 :&: e )) :|: r2) :&: s3) (((l1 :|: (f0 :&: e')) :|: r2) :&: s3)
bonflrl :: b e e' -> b (((l1 :|: (e  :|: r0)) :|: r2) :&: s3) (((l1 :|: (e' :|: r0)) :|: r2) :&: s3)
bonflrr :: b e e' -> b (((l1 :|: (l0 :|: e )) :|: r2) :&: s3) (((l1 :|: (l0 :|: e')) :|: r2) :&: s3)
bonfrff :: b e e' -> b ((l2 :|: ((e  :&: s0) :&: s1)) :&: s3) ((l2 :|: ((e' :&: s0) :&: s1)) :&: s3)
bonfrfs :: b e e' -> b ((l2 :|: ((f0 :&: e ) :&: s1)) :&: s3) ((l2 :|: ((f0 :&: e') :&: s1)) :&: s3)
bonfrfl :: b e e' -> b ((l2 :|: ((e  :|: r0) :&: s1)) :&: s3) ((l2 :|: ((e' :|: r0) :&: s1)) :&: s3)
bonfrfr :: b e e' -> b ((l2 :|: ((l0 :|: e ) :&: s1)) :&: s3) ((l2 :|: ((l0 :|: e') :&: s1)) :&: s3)
bonfrsf :: b e e' -> b ((l2 :|: (f1 :&: (e  :&: s0))) :&: s3) ((l2 :|: (f1 :&: (e' :&: s0))) :&: s3)
bonfrss :: b e e' -> b ((l2 :|: (f1 :&: (f0 :&: e ))) :&: s3) ((l2 :|: (f1 :&: (f0 :&: e'))) :&: s3)
bonfrsl :: b e e' -> b ((l2 :|: (f1 :&: (e  :|: r0))) :&: s3) ((l2 :|: (f1 :&: (e' :|: r0))) :&: s3)
bonfrsr :: b e e' -> b ((l2 :|: (f1 :&: (l0 :|: e ))) :&: s3) ((l2 :|: (f1 :&: (l0 :|: e'))) :&: s3)
bonfrlf :: b e e' -> b ((l2 :|: ((e  :&: s0) :|: r1)) :&: s3) ((l2 :|: ((e' :&: s0) :|: r1)) :&: s3)
bonfrls :: b e e' -> b ((l2 :|: ((f0 :&: e ) :|: r1)) :&: s3) ((l2 :|: ((f0 :&: e') :|: r1)) :&: s3)
bonfrll :: b e e' -> b ((l2 :|: ((e  :|: r0) :|: r1)) :&: s3) ((l2 :|: ((e' :|: r0) :|: r1)) :&: s3)
bonfrlr :: b e e' -> b ((l2 :|: ((l0 :|: e ) :|: r1)) :&: s3) ((l2 :|: ((l0 :|: e') :|: r1)) :&: s3)
bonfrrf :: b e e' -> b ((l2 :|: (l1 :|: (e  :&: s0))) :&: s3) ((l2 :|: (l1 :|: (e' :&: s0))) :&: s3)
bonfrrs :: b e e' -> b ((l2 :|: (l1 :|: (f0 :&: e ))) :&: s3) ((l2 :|: (l1 :|: (f0 :&: e'))) :&: s3)
bonfrrl :: b e e' -> b ((l2 :|: (l1 :|: (e  :|: r0))) :&: s3) ((l2 :|: (l1 :|: (e' :|: r0))) :&: s3)
bonfrrr :: b e e' -> b ((l2 :|: (l1 :|: (l0 :|: e ))) :&: s3) ((l2 :|: (l1 :|: (l0 :|: e'))) :&: s3)
bonsfff :: b e e' -> b (f3 :&: (((e  :&: s0) :&: s1) :&: s2)) (f3 :&: (((e' :&: s0) :&: s1) :&: s2))
bonsffs :: b e e' -> b (f3 :&: (((f0 :&: e ) :&: s1) :&: s2)) (f3 :&: (((f0 :&: e') :&: s1) :&: s2))
bonsffl :: b e e' -> b (f3 :&: (((e  :|: r0) :&: s1) :&: s2)) (f3 :&: (((e' :|: r0) :&: s1) :&: s2))
bonsffr :: b e e' -> b (f3 :&: (((l0 :|: e ) :&: s1) :&: s2)) (f3 :&: (((l0 :|: e') :&: s1) :&: s2))
bonsfsf :: b e e' -> b (f3 :&: ((f1 :&: (e  :&: s0)) :&: s2)) (f3 :&: ((f1 :&: (e' :&: s0)) :&: s2))
bonsfss :: b e e' -> b (f3 :&: ((f1 :&: (f0 :&: e )) :&: s2)) (f3 :&: ((f1 :&: (f0 :&: e')) :&: s2))
bonsfsl :: b e e' -> b (f3 :&: ((f1 :&: (e  :|: r0)) :&: s2)) (f3 :&: ((f1 :&: (e' :|: r0)) :&: s2))
bonsfsr :: b e e' -> b (f3 :&: ((f1 :&: (l0 :|: e )) :&: s2)) (f3 :&: ((f1 :&: (l0 :|: e')) :&: s2))
bonsflf :: b e e' -> b (f3 :&: (((e  :&: s0) :|: r1) :&: s2)) (f3 :&: (((e' :&: s0) :|: r1) :&: s2))
bonsfls :: b e e' -> b (f3 :&: (((f0 :&: e ) :|: r1) :&: s2)) (f3 :&: (((f0 :&: e') :|: r1) :&: s2))
bonsfll :: b e e' -> b (f3 :&: (((e  :|: r0) :|: r1) :&: s2)) (f3 :&: (((e' :|: r0) :|: r1) :&: s2))
bonsflr :: b e e' -> b (f3 :&: (((l0 :|: e ) :|: r1) :&: s2)) (f3 :&: (((l0 :|: e') :|: r1) :&: s2))
bonsfrf :: b e e' -> b (f3 :&: ((l1 :|: (e  :&: s0)) :&: s2)) (f3 :&: ((l1 :|: (e' :&: s0)) :&: s2))
bonsfrs :: b e e' -> b (f3 :&: ((l1 :|: (f0 :&: e )) :&: s2)) (f3 :&: ((l1 :|: (f0 :&: e')) :&: s2))
bonsfrl :: b e e' -> b (f3 :&: ((l1 :|: (e  :|: r0)) :&: s2)) (f3 :&: ((l1 :|: (e' :|: r0)) :&: s2))
bonsfrr :: b e e' -> b (f3 :&: ((l1 :|: (l0 :|: e )) :&: s2)) (f3 :&: ((l1 :|: (l0 :|: e')) :&: s2))
bonssff :: b e e' -> b (f3 :&: (f2 :&: ((e  :&: s0) :&: s1))) (f3 :&: (f2 :&: ((e' :&: s0) :&: s1)))
bonssfs :: b e e' -> b (f3 :&: (f2 :&: ((f0 :&: e ) :&: s1))) (f3 :&: (f2 :&: ((f0 :&: e') :&: s1)))
bonssfl :: b e e' -> b (f3 :&: (f2 :&: ((e  :|: r0) :&: s1))) (f3 :&: (f2 :&: ((e' :|: r0) :&: s1)))
bonssfr :: b e e' -> b (f3 :&: (f2 :&: ((l0 :|: e ) :&: s1))) (f3 :&: (f2 :&: ((l0 :|: e') :&: s1)))
bonsssf :: b e e' -> b (f3 :&: (f2 :&: (f1 :&: (e  :&: s0)))) (f3 :&: (f2 :&: (f1 :&: (e' :&: s0))))
bonssss :: b e e' -> b (f3 :&: (f2 :&: (f1 :&: (f0 :&: e )))) (f3 :&: (f2 :&: (f1 :&: (f0 :&: e'))))
bonsssl :: b e e' -> b (f3 :&: (f2 :&: (f1 :&: (e  :|: r0)))) (f3 :&: (f2 :&: (f1 :&: (e' :|: r0))))
bonsssr :: b e e' -> b (f3 :&: (f2 :&: (f1 :&: (l0 :|: e )))) (f3 :&: (f2 :&: (f1 :&: (l0 :|: e'))))
bonsslf :: b e e' -> b (f3 :&: (f2 :&: ((e  :&: s0) :|: r1))) (f3 :&: (f2 :&: ((e' :&: s0) :|: r1)))
bonssls :: b e e' -> b (f3 :&: (f2 :&: ((f0 :&: e ) :|: r1))) (f3 :&: (f2 :&: ((f0 :&: e') :|: r1)))
bonssll :: b e e' -> b (f3 :&: (f2 :&: ((e  :|: r0) :|: r1))) (f3 :&: (f2 :&: ((e' :|: r0) :|: r1)))
bonsslr :: b e e' -> b (f3 :&: (f2 :&: ((l0 :|: e ) :|: r1))) (f3 :&: (f2 :&: ((l0 :|: e') :|: r1)))
bonssrf :: b e e' -> b (f3 :&: (f2 :&: (l1 :|: (e  :&: s0)))) (f3 :&: (f2 :&: (l1 :|: (e' :&: s0))))
bonssrs :: b e e' -> b (f3 :&: (f2 :&: (l1 :|: (f0 :&: e )))) (f3 :&: (f2 :&: (l1 :|: (f0 :&: e'))))
bonssrl :: b e e' -> b (f3 :&: (f2 :&: (l1 :|: (e  :|: r0)))) (f3 :&: (f2 :&: (l1 :|: (e' :|: r0))))
bonssrr :: b e e' -> b (f3 :&: (f2 :&: (l1 :|: (l0 :|: e )))) (f3 :&: (f2 :&: (l1 :|: (l0 :|: e'))))
bonslff :: b e e' -> b (f3 :&: (((e  :&: s0) :&: s1) :|: r2)) (f3 :&: (((e' :&: s0) :&: s1) :|: r2))
bonslfs :: b e e' -> b (f3 :&: (((f0 :&: e ) :&: s1) :|: r2)) (f3 :&: (((f0 :&: e') :&: s1) :|: r2))
bonslfl :: b e e' -> b (f3 :&: (((e  :|: r0) :&: s1) :|: r2)) (f3 :&: (((e' :|: r0) :&: s1) :|: r2))
bonslfr :: b e e' -> b (f3 :&: (((l0 :|: e ) :&: s1) :|: r2)) (f3 :&: (((l0 :|: e') :&: s1) :|: r2))
bonslsf :: b e e' -> b (f3 :&: ((f1 :&: (e  :&: s0)) :|: r2)) (f3 :&: ((f1 :&: (e' :&: s0)) :|: r2))
bonslss :: b e e' -> b (f3 :&: ((f1 :&: (f0 :&: e )) :|: r2)) (f3 :&: ((f1 :&: (f0 :&: e')) :|: r2))
bonslsl :: b e e' -> b (f3 :&: ((f1 :&: (e  :|: r0)) :|: r2)) (f3 :&: ((f1 :&: (e' :|: r0)) :|: r2))
bonslsr :: b e e' -> b (f3 :&: ((f1 :&: (l0 :|: e )) :|: r2)) (f3 :&: ((f1 :&: (l0 :|: e')) :|: r2))
bonsllf :: b e e' -> b (f3 :&: (((e  :&: s0) :|: r1) :|: r2)) (f3 :&: (((e' :&: s0) :|: r1) :|: r2))
bonslls :: b e e' -> b (f3 :&: (((f0 :&: e ) :|: r1) :|: r2)) (f3 :&: (((f0 :&: e') :|: r1) :|: r2))
bonslll :: b e e' -> b (f3 :&: (((e  :|: r0) :|: r1) :|: r2)) (f3 :&: (((e' :|: r0) :|: r1) :|: r2))
bonsllr :: b e e' -> b (f3 :&: (((l0 :|: e ) :|: r1) :|: r2)) (f3 :&: (((l0 :|: e') :|: r1) :|: r2))
bonslrf :: b e e' -> b (f3 :&: ((l1 :|: (e  :&: s0)) :|: r2)) (f3 :&: ((l1 :|: (e' :&: s0)) :|: r2))
bonslrs :: b e e' -> b (f3 :&: ((l1 :|: (f0 :&: e )) :|: r2)) (f3 :&: ((l1 :|: (f0 :&: e')) :|: r2))
bonslrl :: b e e' -> b (f3 :&: ((l1 :|: (e  :|: r0)) :|: r2)) (f3 :&: ((l1 :|: (e' :|: r0)) :|: r2))
bonslrr :: b e e' -> b (f3 :&: ((l1 :|: (l0 :|: e )) :|: r2)) (f3 :&: ((l1 :|: (l0 :|: e')) :|: r2))
bonsrff :: b e e' -> b (f3 :&: (l2 :|: ((e  :&: s0) :&: s1))) (f3 :&: (l2 :|: ((e' :&: s0) :&: s1)))
bonsrfs :: b e e' -> b (f3 :&: (l2 :|: ((f0 :&: e ) :&: s1))) (f3 :&: (l2 :|: ((f0 :&: e') :&: s1)))
bonsrfl :: b e e' -> b (f3 :&: (l2 :|: ((e  :|: r0) :&: s1))) (f3 :&: (l2 :|: ((e' :|: r0) :&: s1)))
bonsrfr :: b e e' -> b (f3 :&: (l2 :|: ((l0 :|: e ) :&: s1))) (f3 :&: (l2 :|: ((l0 :|: e') :&: s1)))
bonsrsf :: b e e' -> b (f3 :&: (l2 :|: (f1 :&: (e  :&: s0)))) (f3 :&: (l2 :|: (f1 :&: (e' :&: s0))))
bonsrss :: b e e' -> b (f3 :&: (l2 :|: (f1 :&: (f0 :&: e )))) (f3 :&: (l2 :|: (f1 :&: (f0 :&: e'))))
bonsrsl :: b e e' -> b (f3 :&: (l2 :|: (f1 :&: (e  :|: r0)))) (f3 :&: (l2 :|: (f1 :&: (e' :|: r0))))
bonsrsr :: b e e' -> b (f3 :&: (l2 :|: (f1 :&: (l0 :|: e )))) (f3 :&: (l2 :|: (f1 :&: (l0 :|: e'))))
bonsrlf :: b e e' -> b (f3 :&: (l2 :|: ((e  :&: s0) :|: r1))) (f3 :&: (l2 :|: ((e' :&: s0) :|: r1)))
bonsrls :: b e e' -> b (f3 :&: (l2 :|: ((f0 :&: e ) :|: r1))) (f3 :&: (l2 :|: ((f0 :&: e') :|: r1)))
bonsrll :: b e e' -> b (f3 :&: (l2 :|: ((e  :|: r0) :|: r1))) (f3 :&: (l2 :|: ((e' :|: r0) :|: r1)))
bonsrlr :: b e e' -> b (f3 :&: (l2 :|: ((l0 :|: e ) :|: r1))) (f3 :&: (l2 :|: ((l0 :|: e') :|: r1)))
bonsrrf :: b e e' -> b (f3 :&: (l2 :|: (l1 :|: (e  :&: s0)))) (f3 :&: (l2 :|: (l1 :|: (e' :&: s0))))
bonsrrs :: b e e' -> b (f3 :&: (l2 :|: (l1 :|: (f0 :&: e )))) (f3 :&: (l2 :|: (l1 :|: (f0 :&: e'))))
bonsrrl :: b e e' -> b (f3 :&: (l2 :|: (l1 :|: (e  :|: r0)))) (f3 :&: (l2 :|: (l1 :|: (e' :|: r0))))
bonsrrr :: b e e' -> b (f3 :&: (l2 :|: (l1 :|: (l0 :|: e )))) (f3 :&: (l2 :|: (l1 :|: (l0 :|: e'))))
bonlfff :: b e e' -> b ((((e  :&: s0) :&: s1) :&: s2) :|: r3) ((((e' :&: s0) :&: s1) :&: s2) :|: r3)
bonlffs :: b e e' -> b ((((f0 :&: e ) :&: s1) :&: s2) :|: r3) ((((f0 :&: e') :&: s1) :&: s2) :|: r3)
bonlffl :: b e e' -> b ((((e  :|: r0) :&: s1) :&: s2) :|: r3) ((((e' :|: r0) :&: s1) :&: s2) :|: r3)
bonlffr :: b e e' -> b ((((l0 :|: e ) :&: s1) :&: s2) :|: r3) ((((l0 :|: e') :&: s1) :&: s2) :|: r3)
bonlfsf :: b e e' -> b (((f1 :&: (e  :&: s0)) :&: s2) :|: r3) (((f1 :&: (e' :&: s0)) :&: s2) :|: r3)
bonlfss :: b e e' -> b (((f1 :&: (f0 :&: e )) :&: s2) :|: r3) (((f1 :&: (f0 :&: e')) :&: s2) :|: r3)
bonlfsl :: b e e' -> b (((f1 :&: (e  :|: r0)) :&: s2) :|: r3) (((f1 :&: (e' :|: r0)) :&: s2) :|: r3)
bonlfsr :: b e e' -> b (((f1 :&: (l0 :|: e )) :&: s2) :|: r3) (((f1 :&: (l0 :|: e')) :&: s2) :|: r3)
bonlflf :: b e e' -> b ((((e  :&: s0) :|: r1) :&: s2) :|: r3) ((((e' :&: s0) :|: r1) :&: s2) :|: r3)
bonlfls :: b e e' -> b ((((f0 :&: e ) :|: r1) :&: s2) :|: r3) ((((f0 :&: e') :|: r1) :&: s2) :|: r3)
bonlfll :: b e e' -> b ((((e  :|: r0) :|: r1) :&: s2) :|: r3) ((((e' :|: r0) :|: r1) :&: s2) :|: r3)
bonlflr :: b e e' -> b ((((l0 :|: e ) :|: r1) :&: s2) :|: r3) ((((l0 :|: e') :|: r1) :&: s2) :|: r3)
bonlfrf :: b e e' -> b (((l1 :|: (e  :&: s0)) :&: s2) :|: r3) (((l1 :|: (e' :&: s0)) :&: s2) :|: r3)
bonlfrs :: b e e' -> b (((l1 :|: (f0 :&: e )) :&: s2) :|: r3) (((l1 :|: (f0 :&: e')) :&: s2) :|: r3)
bonlfrl :: b e e' -> b (((l1 :|: (e  :|: r0)) :&: s2) :|: r3) (((l1 :|: (e' :|: r0)) :&: s2) :|: r3)
bonlfrr :: b e e' -> b (((l1 :|: (l0 :|: e )) :&: s2) :|: r3) (((l1 :|: (l0 :|: e')) :&: s2) :|: r3)
bonlsff :: b e e' -> b ((f2 :&: ((e  :&: s0) :&: s1)) :|: r3) ((f2 :&: ((e' :&: s0) :&: s1)) :|: r3)
bonlsfs :: b e e' -> b ((f2 :&: ((f0 :&: e ) :&: s1)) :|: r3) ((f2 :&: ((f0 :&: e') :&: s1)) :|: r3)
bonlsfl :: b e e' -> b ((f2 :&: ((e  :|: r0) :&: s1)) :|: r3) ((f2 :&: ((e' :|: r0) :&: s1)) :|: r3)
bonlsfr :: b e e' -> b ((f2 :&: ((l0 :|: e ) :&: s1)) :|: r3) ((f2 :&: ((l0 :|: e') :&: s1)) :|: r3)
bonlssf :: b e e' -> b ((f2 :&: (f1 :&: (e  :&: s0))) :|: r3) ((f2 :&: (f1 :&: (e' :&: s0))) :|: r3)
bonlsss :: b e e' -> b ((f2 :&: (f1 :&: (f0 :&: e ))) :|: r3) ((f2 :&: (f1 :&: (f0 :&: e'))) :|: r3)
bonlssl :: b e e' -> b ((f2 :&: (f1 :&: (e  :|: r0))) :|: r3) ((f2 :&: (f1 :&: (e' :|: r0))) :|: r3)
bonlssr :: b e e' -> b ((f2 :&: (f1 :&: (l0 :|: e ))) :|: r3) ((f2 :&: (f1 :&: (l0 :|: e'))) :|: r3)
bonlslf :: b e e' -> b ((f2 :&: ((e  :&: s0) :|: r1)) :|: r3) ((f2 :&: ((e' :&: s0) :|: r1)) :|: r3)
bonlsls :: b e e' -> b ((f2 :&: ((f0 :&: e ) :|: r1)) :|: r3) ((f2 :&: ((f0 :&: e') :|: r1)) :|: r3)
bonlsll :: b e e' -> b ((f2 :&: ((e  :|: r0) :|: r1)) :|: r3) ((f2 :&: ((e' :|: r0) :|: r1)) :|: r3)
bonlslr :: b e e' -> b ((f2 :&: ((l0 :|: e ) :|: r1)) :|: r3) ((f2 :&: ((l0 :|: e') :|: r1)) :|: r3)
bonlsrf :: b e e' -> b ((f2 :&: (l1 :|: (e  :&: s0))) :|: r3) ((f2 :&: (l1 :|: (e' :&: s0))) :|: r3)
bonlsrs :: b e e' -> b ((f2 :&: (l1 :|: (f0 :&: e ))) :|: r3) ((f2 :&: (l1 :|: (f0 :&: e'))) :|: r3)
bonlsrl :: b e e' -> b ((f2 :&: (l1 :|: (e  :|: r0))) :|: r3) ((f2 :&: (l1 :|: (e' :|: r0))) :|: r3)
bonlsrr :: b e e' -> b ((f2 :&: (l1 :|: (l0 :|: e ))) :|: r3) ((f2 :&: (l1 :|: (l0 :|: e'))) :|: r3)
bonllff :: b e e' -> b ((((e  :&: s0) :&: s1) :|: r2) :|: r3) ((((e' :&: s0) :&: s1) :|: r2) :|: r3)
bonllfs :: b e e' -> b ((((f0 :&: e ) :&: s1) :|: r2) :|: r3) ((((f0 :&: e') :&: s1) :|: r2) :|: r3)
bonllfl :: b e e' -> b ((((e  :|: r0) :&: s1) :|: r2) :|: r3) ((((e' :|: r0) :&: s1) :|: r2) :|: r3)
bonllfr :: b e e' -> b ((((l0 :|: e ) :&: s1) :|: r2) :|: r3) ((((l0 :|: e') :&: s1) :|: r2) :|: r3)
bonllsf :: b e e' -> b (((f1 :&: (e  :&: s0)) :|: r2) :|: r3) (((f1 :&: (e' :&: s0)) :|: r2) :|: r3)
bonllss :: b e e' -> b (((f1 :&: (f0 :&: e )) :|: r2) :|: r3) (((f1 :&: (f0 :&: e')) :|: r2) :|: r3)
bonllsl :: b e e' -> b (((f1 :&: (e  :|: r0)) :|: r2) :|: r3) (((f1 :&: (e' :|: r0)) :|: r2) :|: r3)
bonllsr :: b e e' -> b (((f1 :&: (l0 :|: e )) :|: r2) :|: r3) (((f1 :&: (l0 :|: e')) :|: r2) :|: r3)
bonlllf :: b e e' -> b ((((e  :&: s0) :|: r1) :|: r2) :|: r3) ((((e' :&: s0) :|: r1) :|: r2) :|: r3)
bonllls :: b e e' -> b ((((f0 :&: e ) :|: r1) :|: r2) :|: r3) ((((f0 :&: e') :|: r1) :|: r2) :|: r3)
bonllll :: b e e' -> b ((((e  :|: r0) :|: r1) :|: r2) :|: r3) ((((e' :|: r0) :|: r1) :|: r2) :|: r3)
bonlllr :: b e e' -> b ((((l0 :|: e ) :|: r1) :|: r2) :|: r3) ((((l0 :|: e') :|: r1) :|: r2) :|: r3)
bonllrf :: b e e' -> b (((l1 :|: (e  :&: s0)) :|: r2) :|: r3) (((l1 :|: (e' :&: s0)) :|: r2) :|: r3)
bonllrs :: b e e' -> b (((l1 :|: (f0 :&: e )) :|: r2) :|: r3) (((l1 :|: (f0 :&: e')) :|: r2) :|: r3)
bonllrl :: b e e' -> b (((l1 :|: (e  :|: r0)) :|: r2) :|: r3) (((l1 :|: (e' :|: r0)) :|: r2) :|: r3)
bonllrr :: b e e' -> b (((l1 :|: (l0 :|: e )) :|: r2) :|: r3) (((l1 :|: (l0 :|: e')) :|: r2) :|: r3)
bonlrff :: b e e' -> b ((l2 :|: ((e  :&: s0) :&: s1)) :|: r3) ((l2 :|: ((e' :&: s0) :&: s1)) :|: r3)
bonlrfs :: b e e' -> b ((l2 :|: ((f0 :&: e ) :&: s1)) :|: r3) ((l2 :|: ((f0 :&: e') :&: s1)) :|: r3)
bonlrfl :: b e e' -> b ((l2 :|: ((e  :|: r0) :&: s1)) :|: r3) ((l2 :|: ((e' :|: r0) :&: s1)) :|: r3)
bonlrfr :: b e e' -> b ((l2 :|: ((l0 :|: e ) :&: s1)) :|: r3) ((l2 :|: ((l0 :|: e') :&: s1)) :|: r3)
bonlrsf :: b e e' -> b ((l2 :|: (f1 :&: (e  :&: s0))) :|: r3) ((l2 :|: (f1 :&: (e' :&: s0))) :|: r3)
bonlrss :: b e e' -> b ((l2 :|: (f1 :&: (f0 :&: e ))) :|: r3) ((l2 :|: (f1 :&: (f0 :&: e'))) :|: r3)
bonlrsl :: b e e' -> b ((l2 :|: (f1 :&: (e  :|: r0))) :|: r3) ((l2 :|: (f1 :&: (e' :|: r0))) :|: r3)
bonlrsr :: b e e' -> b ((l2 :|: (f1 :&: (l0 :|: e ))) :|: r3) ((l2 :|: (f1 :&: (l0 :|: e'))) :|: r3)
bonlrlf :: b e e' -> b ((l2 :|: ((e  :&: s0) :|: r1)) :|: r3) ((l2 :|: ((e' :&: s0) :|: r1)) :|: r3)
bonlrls :: b e e' -> b ((l2 :|: ((f0 :&: e ) :|: r1)) :|: r3) ((l2 :|: ((f0 :&: e') :|: r1)) :|: r3)
bonlrll :: b e e' -> b ((l2 :|: ((e  :|: r0) :|: r1)) :|: r3) ((l2 :|: ((e' :|: r0) :|: r1)) :|: r3)
bonlrlr :: b e e' -> b ((l2 :|: ((l0 :|: e ) :|: r1)) :|: r3) ((l2 :|: ((l0 :|: e') :|: r1)) :|: r3)
bonlrrf :: b e e' -> b ((l2 :|: (l1 :|: (e  :&: s0))) :|: r3) ((l2 :|: (l1 :|: (e' :&: s0))) :|: r3)
bonlrrs :: b e e' -> b ((l2 :|: (l1 :|: (f0 :&: e ))) :|: r3) ((l2 :|: (l1 :|: (f0 :&: e'))) :|: r3)
bonlrrl :: b e e' -> b ((l2 :|: (l1 :|: (e  :|: r0))) :|: r3) ((l2 :|: (l1 :|: (e' :|: r0))) :|: r3)
bonlrrr :: b e e' -> b ((l2 :|: (l1 :|: (l0 :|: e ))) :|: r3) ((l2 :|: (l1 :|: (l0 :|: e'))) :|: r3)
bonrfff :: b e e' -> b (l3 :|: (((e  :&: s0) :&: s1) :&: s2)) (l3 :|: (((e' :&: s0) :&: s1) :&: s2))
bonrffs :: b e e' -> b (l3 :|: (((f0 :&: e ) :&: s1) :&: s2)) (l3 :|: (((f0 :&: e') :&: s1) :&: s2))
bonrffl :: b e e' -> b (l3 :|: (((e  :|: r0) :&: s1) :&: s2)) (l3 :|: (((e' :|: r0) :&: s1) :&: s2))
bonrffr :: b e e' -> b (l3 :|: (((l0 :|: e ) :&: s1) :&: s2)) (l3 :|: (((l0 :|: e') :&: s1) :&: s2))
bonrfsf :: b e e' -> b (l3 :|: ((f1 :&: (e  :&: s0)) :&: s2)) (l3 :|: ((f1 :&: (e' :&: s0)) :&: s2))
bonrfss :: b e e' -> b (l3 :|: ((f1 :&: (f0 :&: e )) :&: s2)) (l3 :|: ((f1 :&: (f0 :&: e')) :&: s2))
bonrfsl :: b e e' -> b (l3 :|: ((f1 :&: (e  :|: r0)) :&: s2)) (l3 :|: ((f1 :&: (e' :|: r0)) :&: s2))
bonrfsr :: b e e' -> b (l3 :|: ((f1 :&: (l0 :|: e )) :&: s2)) (l3 :|: ((f1 :&: (l0 :|: e')) :&: s2))
bonrflf :: b e e' -> b (l3 :|: (((e  :&: s0) :|: r1) :&: s2)) (l3 :|: (((e' :&: s0) :|: r1) :&: s2))
bonrfls :: b e e' -> b (l3 :|: (((f0 :&: e ) :|: r1) :&: s2)) (l3 :|: (((f0 :&: e') :|: r1) :&: s2))
bonrfll :: b e e' -> b (l3 :|: (((e  :|: r0) :|: r1) :&: s2)) (l3 :|: (((e' :|: r0) :|: r1) :&: s2))
bonrflr :: b e e' -> b (l3 :|: (((l0 :|: e ) :|: r1) :&: s2)) (l3 :|: (((l0 :|: e') :|: r1) :&: s2))
bonrfrf :: b e e' -> b (l3 :|: ((l1 :|: (e  :&: s0)) :&: s2)) (l3 :|: ((l1 :|: (e' :&: s0)) :&: s2))
bonrfrs :: b e e' -> b (l3 :|: ((l1 :|: (f0 :&: e )) :&: s2)) (l3 :|: ((l1 :|: (f0 :&: e')) :&: s2))
bonrfrl :: b e e' -> b (l3 :|: ((l1 :|: (e  :|: r0)) :&: s2)) (l3 :|: ((l1 :|: (e' :|: r0)) :&: s2))
bonrfrr :: b e e' -> b (l3 :|: ((l1 :|: (l0 :|: e )) :&: s2)) (l3 :|: ((l1 :|: (l0 :|: e')) :&: s2))
bonrsff :: b e e' -> b (l3 :|: (f2 :&: ((e  :&: s0) :&: s1))) (l3 :|: (f2 :&: ((e' :&: s0) :&: s1)))
bonrsfs :: b e e' -> b (l3 :|: (f2 :&: ((f0 :&: e ) :&: s1))) (l3 :|: (f2 :&: ((f0 :&: e') :&: s1)))
bonrsfl :: b e e' -> b (l3 :|: (f2 :&: ((e  :|: r0) :&: s1))) (l3 :|: (f2 :&: ((e' :|: r0) :&: s1)))
bonrsfr :: b e e' -> b (l3 :|: (f2 :&: ((l0 :|: e ) :&: s1))) (l3 :|: (f2 :&: ((l0 :|: e') :&: s1)))
bonrssf :: b e e' -> b (l3 :|: (f2 :&: (f1 :&: (e  :&: s0)))) (l3 :|: (f2 :&: (f1 :&: (e' :&: s0))))
bonrsss :: b e e' -> b (l3 :|: (f2 :&: (f1 :&: (f0 :&: e )))) (l3 :|: (f2 :&: (f1 :&: (f0 :&: e'))))
bonrssl :: b e e' -> b (l3 :|: (f2 :&: (f1 :&: (e  :|: r0)))) (l3 :|: (f2 :&: (f1 :&: (e' :|: r0))))
bonrssr :: b e e' -> b (l3 :|: (f2 :&: (f1 :&: (l0 :|: e )))) (l3 :|: (f2 :&: (f1 :&: (l0 :|: e'))))
bonrslf :: b e e' -> b (l3 :|: (f2 :&: ((e  :&: s0) :|: r1))) (l3 :|: (f2 :&: ((e' :&: s0) :|: r1)))
bonrsls :: b e e' -> b (l3 :|: (f2 :&: ((f0 :&: e ) :|: r1))) (l3 :|: (f2 :&: ((f0 :&: e') :|: r1)))
bonrsll :: b e e' -> b (l3 :|: (f2 :&: ((e  :|: r0) :|: r1))) (l3 :|: (f2 :&: ((e' :|: r0) :|: r1)))
bonrslr :: b e e' -> b (l3 :|: (f2 :&: ((l0 :|: e ) :|: r1))) (l3 :|: (f2 :&: ((l0 :|: e') :|: r1)))
bonrsrf :: b e e' -> b (l3 :|: (f2 :&: (l1 :|: (e  :&: s0)))) (l3 :|: (f2 :&: (l1 :|: (e' :&: s0))))
bonrsrs :: b e e' -> b (l3 :|: (f2 :&: (l1 :|: (f0 :&: e )))) (l3 :|: (f2 :&: (l1 :|: (f0 :&: e'))))
bonrsrl :: b e e' -> b (l3 :|: (f2 :&: (l1 :|: (e  :|: r0)))) (l3 :|: (f2 :&: (l1 :|: (e' :|: r0))))
bonrsrr :: b e e' -> b (l3 :|: (f2 :&: (l1 :|: (l0 :|: e )))) (l3 :|: (f2 :&: (l1 :|: (l0 :|: e'))))
bonrlff :: b e e' -> b (l3 :|: (((e  :&: s0) :&: s1) :|: r2)) (l3 :|: (((e' :&: s0) :&: s1) :|: r2))
bonrlfs :: b e e' -> b (l3 :|: (((f0 :&: e ) :&: s1) :|: r2)) (l3 :|: (((f0 :&: e') :&: s1) :|: r2))
bonrlfl :: b e e' -> b (l3 :|: (((e  :|: r0) :&: s1) :|: r2)) (l3 :|: (((e' :|: r0) :&: s1) :|: r2))
bonrlfr :: b e e' -> b (l3 :|: (((l0 :|: e ) :&: s1) :|: r2)) (l3 :|: (((l0 :|: e') :&: s1) :|: r2))
bonrlsf :: b e e' -> b (l3 :|: ((f1 :&: (e  :&: s0)) :|: r2)) (l3 :|: ((f1 :&: (e' :&: s0)) :|: r2))
bonrlss :: b e e' -> b (l3 :|: ((f1 :&: (f0 :&: e )) :|: r2)) (l3 :|: ((f1 :&: (f0 :&: e')) :|: r2))
bonrlsl :: b e e' -> b (l3 :|: ((f1 :&: (e  :|: r0)) :|: r2)) (l3 :|: ((f1 :&: (e' :|: r0)) :|: r2))
bonrlsr :: b e e' -> b (l3 :|: ((f1 :&: (l0 :|: e )) :|: r2)) (l3 :|: ((f1 :&: (l0 :|: e')) :|: r2))
bonrllf :: b e e' -> b (l3 :|: (((e  :&: s0) :|: r1) :|: r2)) (l3 :|: (((e' :&: s0) :|: r1) :|: r2))
bonrlls :: b e e' -> b (l3 :|: (((f0 :&: e ) :|: r1) :|: r2)) (l3 :|: (((f0 :&: e') :|: r1) :|: r2))
bonrlll :: b e e' -> b (l3 :|: (((e  :|: r0) :|: r1) :|: r2)) (l3 :|: (((e' :|: r0) :|: r1) :|: r2))
bonrllr :: b e e' -> b (l3 :|: (((l0 :|: e ) :|: r1) :|: r2)) (l3 :|: (((l0 :|: e') :|: r1) :|: r2))
bonrlrf :: b e e' -> b (l3 :|: ((l1 :|: (e  :&: s0)) :|: r2)) (l3 :|: ((l1 :|: (e' :&: s0)) :|: r2))
bonrlrs :: b e e' -> b (l3 :|: ((l1 :|: (f0 :&: e )) :|: r2)) (l3 :|: ((l1 :|: (f0 :&: e')) :|: r2))
bonrlrl :: b e e' -> b (l3 :|: ((l1 :|: (e  :|: r0)) :|: r2)) (l3 :|: ((l1 :|: (e' :|: r0)) :|: r2))
bonrlrr :: b e e' -> b (l3 :|: ((l1 :|: (l0 :|: e )) :|: r2)) (l3 :|: ((l1 :|: (l0 :|: e')) :|: r2))
bonrrff :: b e e' -> b (l3 :|: (l2 :|: ((e  :&: s0) :&: s1))) (l3 :|: (l2 :|: ((e' :&: s0) :&: s1)))
bonrrfs :: b e e' -> b (l3 :|: (l2 :|: ((f0 :&: e ) :&: s1))) (l3 :|: (l2 :|: ((f0 :&: e') :&: s1)))
bonrrfl :: b e e' -> b (l3 :|: (l2 :|: ((e  :|: r0) :&: s1))) (l3 :|: (l2 :|: ((e' :|: r0) :&: s1)))
bonrrfr :: b e e' -> b (l3 :|: (l2 :|: ((l0 :|: e ) :&: s1))) (l3 :|: (l2 :|: ((l0 :|: e') :&: s1)))
bonrrsf :: b e e' -> b (l3 :|: (l2 :|: (f1 :&: (e  :&: s0)))) (l3 :|: (l2 :|: (f1 :&: (e' :&: s0))))
bonrrss :: b e e' -> b (l3 :|: (l2 :|: (f1 :&: (f0 :&: e )))) (l3 :|: (l2 :|: (f1 :&: (f0 :&: e'))))
bonrrsl :: b e e' -> b (l3 :|: (l2 :|: (f1 :&: (e  :|: r0)))) (l3 :|: (l2 :|: (f1 :&: (e' :|: r0))))
bonrrsr :: b e e' -> b (l3 :|: (l2 :|: (f1 :&: (l0 :|: e )))) (l3 :|: (l2 :|: (f1 :&: (l0 :|: e'))))
bonrrlf :: b e e' -> b (l3 :|: (l2 :|: ((e  :&: s0) :|: r1))) (l3 :|: (l2 :|: ((e' :&: s0) :|: r1)))
bonrrls :: b e e' -> b (l3 :|: (l2 :|: ((f0 :&: e ) :|: r1))) (l3 :|: (l2 :|: ((f0 :&: e') :|: r1)))
bonrrll :: b e e' -> b (l3 :|: (l2 :|: ((e  :|: r0) :|: r1))) (l3 :|: (l2 :|: ((e' :|: r0) :|: r1)))
bonrrlr :: b e e' -> b (l3 :|: (l2 :|: ((l0 :|: e ) :|: r1))) (l3 :|: (l2 :|: ((l0 :|: e') :|: r1)))
bonrrrf :: b e e' -> b (l3 :|: (l2 :|: (l1 :|: (e  :&: s0)))) (l3 :|: (l2 :|: (l1 :|: (e' :&: s0))))
bonrrrs :: b e e' -> b (l3 :|: (l2 :|: (l1 :|: (f0 :&: e )))) (l3 :|: (l2 :|: (l1 :|: (f0 :&: e'))))
bonrrrl :: b e e' -> b (l3 :|: (l2 :|: (l1 :|: (e  :|: r0)))) (l3 :|: (l2 :|: (l1 :|: (e' :|: r0))))
bonrrrr :: b e e' -> b (l3 :|: (l2 :|: (l1 :|: (l0 :|: e )))) (l3 :|: (l2 :|: (l1 :|: (l0 :|: e'))))
bonfffff :: b e e' -> b (((((e  :&: s0) :&: s1) :&: s2) :&: s3) :&: s4) (((((e' :&: s0) :&: s1) :&: s2) :&: s3) :&: s4)
bonffffs :: b e e' -> b (((((f0 :&: e ) :&: s1) :&: s2) :&: s3) :&: s4) (((((f0 :&: e') :&: s1) :&: s2) :&: s3) :&: s4)
bonffffl :: b e e' -> b (((((e  :|: r0) :&: s1) :&: s2) :&: s3) :&: s4) (((((e' :|: r0) :&: s1) :&: s2) :&: s3) :&: s4)
bonffffr :: b e e' -> b (((((l0 :|: e ) :&: s1) :&: s2) :&: s3) :&: s4) (((((l0 :|: e') :&: s1) :&: s2) :&: s3) :&: s4)
bonfffsf :: b e e' -> b ((((f1 :&: (e  :&: s0)) :&: s2) :&: s3) :&: s4) ((((f1 :&: (e' :&: s0)) :&: s2) :&: s3) :&: s4)
bonfffss :: b e e' -> b ((((f1 :&: (f0 :&: e )) :&: s2) :&: s3) :&: s4) ((((f1 :&: (f0 :&: e')) :&: s2) :&: s3) :&: s4)
bonfffsl :: b e e' -> b ((((f1 :&: (e  :|: r0)) :&: s2) :&: s3) :&: s4) ((((f1 :&: (e' :|: r0)) :&: s2) :&: s3) :&: s4)
bonfffsr :: b e e' -> b ((((f1 :&: (l0 :|: e )) :&: s2) :&: s3) :&: s4) ((((f1 :&: (l0 :|: e')) :&: s2) :&: s3) :&: s4)
bonffflf :: b e e' -> b (((((e  :&: s0) :|: r1) :&: s2) :&: s3) :&: s4) (((((e' :&: s0) :|: r1) :&: s2) :&: s3) :&: s4)
bonfffls :: b e e' -> b (((((f0 :&: e ) :|: r1) :&: s2) :&: s3) :&: s4) (((((f0 :&: e') :|: r1) :&: s2) :&: s3) :&: s4)
bonfffll :: b e e' -> b (((((e  :|: r0) :|: r1) :&: s2) :&: s3) :&: s4) (((((e' :|: r0) :|: r1) :&: s2) :&: s3) :&: s4)
bonffflr :: b e e' -> b (((((l0 :|: e ) :|: r1) :&: s2) :&: s3) :&: s4) (((((l0 :|: e') :|: r1) :&: s2) :&: s3) :&: s4)
bonfffrf :: b e e' -> b ((((l1 :|: (e  :&: s0)) :&: s2) :&: s3) :&: s4) ((((l1 :|: (e' :&: s0)) :&: s2) :&: s3) :&: s4)
bonfffrs :: b e e' -> b ((((l1 :|: (f0 :&: e )) :&: s2) :&: s3) :&: s4) ((((l1 :|: (f0 :&: e')) :&: s2) :&: s3) :&: s4)
bonfffrl :: b e e' -> b ((((l1 :|: (e  :|: r0)) :&: s2) :&: s3) :&: s4) ((((l1 :|: (e' :|: r0)) :&: s2) :&: s3) :&: s4)
bonfffrr :: b e e' -> b ((((l1 :|: (l0 :|: e )) :&: s2) :&: s3) :&: s4) ((((l1 :|: (l0 :|: e')) :&: s2) :&: s3) :&: s4)
bonffsff :: b e e' -> b (((f2 :&: ((e  :&: s0) :&: s1)) :&: s3) :&: s4) (((f2 :&: ((e' :&: s0) :&: s1)) :&: s3) :&: s4)
bonffsfs :: b e e' -> b (((f2 :&: ((f0 :&: e ) :&: s1)) :&: s3) :&: s4) (((f2 :&: ((f0 :&: e') :&: s1)) :&: s3) :&: s4)
bonffsfl :: b e e' -> b (((f2 :&: ((e  :|: r0) :&: s1)) :&: s3) :&: s4) (((f2 :&: ((e' :|: r0) :&: s1)) :&: s3) :&: s4)
bonffsfr :: b e e' -> b (((f2 :&: ((l0 :|: e ) :&: s1)) :&: s3) :&: s4) (((f2 :&: ((l0 :|: e') :&: s1)) :&: s3) :&: s4)
bonffssf :: b e e' -> b (((f2 :&: (f1 :&: (e  :&: s0))) :&: s3) :&: s4) (((f2 :&: (f1 :&: (e' :&: s0))) :&: s3) :&: s4)
bonffsss :: b e e' -> b (((f2 :&: (f1 :&: (f0 :&: e ))) :&: s3) :&: s4) (((f2 :&: (f1 :&: (f0 :&: e'))) :&: s3) :&: s4)
bonffssl :: b e e' -> b (((f2 :&: (f1 :&: (e  :|: r0))) :&: s3) :&: s4) (((f2 :&: (f1 :&: (e' :|: r0))) :&: s3) :&: s4)
bonffssr :: b e e' -> b (((f2 :&: (f1 :&: (l0 :|: e ))) :&: s3) :&: s4) (((f2 :&: (f1 :&: (l0 :|: e'))) :&: s3) :&: s4)
bonffslf :: b e e' -> b (((f2 :&: ((e  :&: s0) :|: r1)) :&: s3) :&: s4) (((f2 :&: ((e' :&: s0) :|: r1)) :&: s3) :&: s4)
bonffsls :: b e e' -> b (((f2 :&: ((f0 :&: e ) :|: r1)) :&: s3) :&: s4) (((f2 :&: ((f0 :&: e') :|: r1)) :&: s3) :&: s4)
bonffsll :: b e e' -> b (((f2 :&: ((e  :|: r0) :|: r1)) :&: s3) :&: s4) (((f2 :&: ((e' :|: r0) :|: r1)) :&: s3) :&: s4)
bonffslr :: b e e' -> b (((f2 :&: ((l0 :|: e ) :|: r1)) :&: s3) :&: s4) (((f2 :&: ((l0 :|: e') :|: r1)) :&: s3) :&: s4)
bonffsrf :: b e e' -> b (((f2 :&: (l1 :|: (e  :&: s0))) :&: s3) :&: s4) (((f2 :&: (l1 :|: (e' :&: s0))) :&: s3) :&: s4)
bonffsrs :: b e e' -> b (((f2 :&: (l1 :|: (f0 :&: e ))) :&: s3) :&: s4) (((f2 :&: (l1 :|: (f0 :&: e'))) :&: s3) :&: s4)
bonffsrl :: b e e' -> b (((f2 :&: (l1 :|: (e  :|: r0))) :&: s3) :&: s4) (((f2 :&: (l1 :|: (e' :|: r0))) :&: s3) :&: s4)
bonffsrr :: b e e' -> b (((f2 :&: (l1 :|: (l0 :|: e ))) :&: s3) :&: s4) (((f2 :&: (l1 :|: (l0 :|: e'))) :&: s3) :&: s4)
bonfflff :: b e e' -> b (((((e  :&: s0) :&: s1) :|: r2) :&: s3) :&: s4) (((((e' :&: s0) :&: s1) :|: r2) :&: s3) :&: s4)
bonfflfs :: b e e' -> b (((((f0 :&: e ) :&: s1) :|: r2) :&: s3) :&: s4) (((((f0 :&: e') :&: s1) :|: r2) :&: s3) :&: s4)
bonfflfl :: b e e' -> b (((((e  :|: r0) :&: s1) :|: r2) :&: s3) :&: s4) (((((e' :|: r0) :&: s1) :|: r2) :&: s3) :&: s4)
bonfflfr :: b e e' -> b (((((l0 :|: e ) :&: s1) :|: r2) :&: s3) :&: s4) (((((l0 :|: e') :&: s1) :|: r2) :&: s3) :&: s4)
bonfflsf :: b e e' -> b ((((f1 :&: (e  :&: s0)) :|: r2) :&: s3) :&: s4) ((((f1 :&: (e' :&: s0)) :|: r2) :&: s3) :&: s4)
bonfflss :: b e e' -> b ((((f1 :&: (f0 :&: e )) :|: r2) :&: s3) :&: s4) ((((f1 :&: (f0 :&: e')) :|: r2) :&: s3) :&: s4)
bonfflsl :: b e e' -> b ((((f1 :&: (e  :|: r0)) :|: r2) :&: s3) :&: s4) ((((f1 :&: (e' :|: r0)) :|: r2) :&: s3) :&: s4)
bonfflsr :: b e e' -> b ((((f1 :&: (l0 :|: e )) :|: r2) :&: s3) :&: s4) ((((f1 :&: (l0 :|: e')) :|: r2) :&: s3) :&: s4)
bonffllf :: b e e' -> b (((((e  :&: s0) :|: r1) :|: r2) :&: s3) :&: s4) (((((e' :&: s0) :|: r1) :|: r2) :&: s3) :&: s4)
bonfflls :: b e e' -> b (((((f0 :&: e ) :|: r1) :|: r2) :&: s3) :&: s4) (((((f0 :&: e') :|: r1) :|: r2) :&: s3) :&: s4)
bonfflll :: b e e' -> b (((((e  :|: r0) :|: r1) :|: r2) :&: s3) :&: s4) (((((e' :|: r0) :|: r1) :|: r2) :&: s3) :&: s4)
bonffllr :: b e e' -> b (((((l0 :|: e ) :|: r1) :|: r2) :&: s3) :&: s4) (((((l0 :|: e') :|: r1) :|: r2) :&: s3) :&: s4)
bonfflrf :: b e e' -> b ((((l1 :|: (e  :&: s0)) :|: r2) :&: s3) :&: s4) ((((l1 :|: (e' :&: s0)) :|: r2) :&: s3) :&: s4)
bonfflrs :: b e e' -> b ((((l1 :|: (f0 :&: e )) :|: r2) :&: s3) :&: s4) ((((l1 :|: (f0 :&: e')) :|: r2) :&: s3) :&: s4)
bonfflrl :: b e e' -> b ((((l1 :|: (e  :|: r0)) :|: r2) :&: s3) :&: s4) ((((l1 :|: (e' :|: r0)) :|: r2) :&: s3) :&: s4)
bonfflrr :: b e e' -> b ((((l1 :|: (l0 :|: e )) :|: r2) :&: s3) :&: s4) ((((l1 :|: (l0 :|: e')) :|: r2) :&: s3) :&: s4)
bonffrff :: b e e' -> b (((l2 :|: ((e  :&: s0) :&: s1)) :&: s3) :&: s4) (((l2 :|: ((e' :&: s0) :&: s1)) :&: s3) :&: s4)
bonffrfs :: b e e' -> b (((l2 :|: ((f0 :&: e ) :&: s1)) :&: s3) :&: s4) (((l2 :|: ((f0 :&: e') :&: s1)) :&: s3) :&: s4)
bonffrfl :: b e e' -> b (((l2 :|: ((e  :|: r0) :&: s1)) :&: s3) :&: s4) (((l2 :|: ((e' :|: r0) :&: s1)) :&: s3) :&: s4)
bonffrfr :: b e e' -> b (((l2 :|: ((l0 :|: e ) :&: s1)) :&: s3) :&: s4) (((l2 :|: ((l0 :|: e') :&: s1)) :&: s3) :&: s4)
bonffrsf :: b e e' -> b (((l2 :|: (f1 :&: (e  :&: s0))) :&: s3) :&: s4) (((l2 :|: (f1 :&: (e' :&: s0))) :&: s3) :&: s4)
bonffrss :: b e e' -> b (((l2 :|: (f1 :&: (f0 :&: e ))) :&: s3) :&: s4) (((l2 :|: (f1 :&: (f0 :&: e'))) :&: s3) :&: s4)
bonffrsl :: b e e' -> b (((l2 :|: (f1 :&: (e  :|: r0))) :&: s3) :&: s4) (((l2 :|: (f1 :&: (e' :|: r0))) :&: s3) :&: s4)
bonffrsr :: b e e' -> b (((l2 :|: (f1 :&: (l0 :|: e ))) :&: s3) :&: s4) (((l2 :|: (f1 :&: (l0 :|: e'))) :&: s3) :&: s4)
bonffrlf :: b e e' -> b (((l2 :|: ((e  :&: s0) :|: r1)) :&: s3) :&: s4) (((l2 :|: ((e' :&: s0) :|: r1)) :&: s3) :&: s4)
bonffrls :: b e e' -> b (((l2 :|: ((f0 :&: e ) :|: r1)) :&: s3) :&: s4) (((l2 :|: ((f0 :&: e') :|: r1)) :&: s3) :&: s4)
bonffrll :: b e e' -> b (((l2 :|: ((e  :|: r0) :|: r1)) :&: s3) :&: s4) (((l2 :|: ((e' :|: r0) :|: r1)) :&: s3) :&: s4)
bonffrlr :: b e e' -> b (((l2 :|: ((l0 :|: e ) :|: r1)) :&: s3) :&: s4) (((l2 :|: ((l0 :|: e') :|: r1)) :&: s3) :&: s4)
bonffrrf :: b e e' -> b (((l2 :|: (l1 :|: (e  :&: s0))) :&: s3) :&: s4) (((l2 :|: (l1 :|: (e' :&: s0))) :&: s3) :&: s4)
bonffrrs :: b e e' -> b (((l2 :|: (l1 :|: (f0 :&: e ))) :&: s3) :&: s4) (((l2 :|: (l1 :|: (f0 :&: e'))) :&: s3) :&: s4)
bonffrrl :: b e e' -> b (((l2 :|: (l1 :|: (e  :|: r0))) :&: s3) :&: s4) (((l2 :|: (l1 :|: (e' :|: r0))) :&: s3) :&: s4)
bonffrrr :: b e e' -> b (((l2 :|: (l1 :|: (l0 :|: e ))) :&: s3) :&: s4) (((l2 :|: (l1 :|: (l0 :|: e'))) :&: s3) :&: s4)
bonfsfff :: b e e' -> b ((f3 :&: (((e  :&: s0) :&: s1) :&: s2)) :&: s4) ((f3 :&: (((e' :&: s0) :&: s1) :&: s2)) :&: s4)
bonfsffs :: b e e' -> b ((f3 :&: (((f0 :&: e ) :&: s1) :&: s2)) :&: s4) ((f3 :&: (((f0 :&: e') :&: s1) :&: s2)) :&: s4)
bonfsffl :: b e e' -> b ((f3 :&: (((e  :|: r0) :&: s1) :&: s2)) :&: s4) ((f3 :&: (((e' :|: r0) :&: s1) :&: s2)) :&: s4)
bonfsffr :: b e e' -> b ((f3 :&: (((l0 :|: e ) :&: s1) :&: s2)) :&: s4) ((f3 :&: (((l0 :|: e') :&: s1) :&: s2)) :&: s4)
bonfsfsf :: b e e' -> b ((f3 :&: ((f1 :&: (e  :&: s0)) :&: s2)) :&: s4) ((f3 :&: ((f1 :&: (e' :&: s0)) :&: s2)) :&: s4)
bonfsfss :: b e e' -> b ((f3 :&: ((f1 :&: (f0 :&: e )) :&: s2)) :&: s4) ((f3 :&: ((f1 :&: (f0 :&: e')) :&: s2)) :&: s4)
bonfsfsl :: b e e' -> b ((f3 :&: ((f1 :&: (e  :|: r0)) :&: s2)) :&: s4) ((f3 :&: ((f1 :&: (e' :|: r0)) :&: s2)) :&: s4)
bonfsfsr :: b e e' -> b ((f3 :&: ((f1 :&: (l0 :|: e )) :&: s2)) :&: s4) ((f3 :&: ((f1 :&: (l0 :|: e')) :&: s2)) :&: s4)
bonfsflf :: b e e' -> b ((f3 :&: (((e  :&: s0) :|: r1) :&: s2)) :&: s4) ((f3 :&: (((e' :&: s0) :|: r1) :&: s2)) :&: s4)
bonfsfls :: b e e' -> b ((f3 :&: (((f0 :&: e ) :|: r1) :&: s2)) :&: s4) ((f3 :&: (((f0 :&: e') :|: r1) :&: s2)) :&: s4)
bonfsfll :: b e e' -> b ((f3 :&: (((e  :|: r0) :|: r1) :&: s2)) :&: s4) ((f3 :&: (((e' :|: r0) :|: r1) :&: s2)) :&: s4)
bonfsflr :: b e e' -> b ((f3 :&: (((l0 :|: e ) :|: r1) :&: s2)) :&: s4) ((f3 :&: (((l0 :|: e') :|: r1) :&: s2)) :&: s4)
bonfsfrf :: b e e' -> b ((f3 :&: ((l1 :|: (e  :&: s0)) :&: s2)) :&: s4) ((f3 :&: ((l1 :|: (e' :&: s0)) :&: s2)) :&: s4)
bonfsfrs :: b e e' -> b ((f3 :&: ((l1 :|: (f0 :&: e )) :&: s2)) :&: s4) ((f3 :&: ((l1 :|: (f0 :&: e')) :&: s2)) :&: s4)
bonfsfrl :: b e e' -> b ((f3 :&: ((l1 :|: (e  :|: r0)) :&: s2)) :&: s4) ((f3 :&: ((l1 :|: (e' :|: r0)) :&: s2)) :&: s4)
bonfsfrr :: b e e' -> b ((f3 :&: ((l1 :|: (l0 :|: e )) :&: s2)) :&: s4) ((f3 :&: ((l1 :|: (l0 :|: e')) :&: s2)) :&: s4)
bonfssff :: b e e' -> b ((f3 :&: (f2 :&: ((e  :&: s0) :&: s1))) :&: s4) ((f3 :&: (f2 :&: ((e' :&: s0) :&: s1))) :&: s4)
bonfssfs :: b e e' -> b ((f3 :&: (f2 :&: ((f0 :&: e ) :&: s1))) :&: s4) ((f3 :&: (f2 :&: ((f0 :&: e') :&: s1))) :&: s4)
bonfssfl :: b e e' -> b ((f3 :&: (f2 :&: ((e  :|: r0) :&: s1))) :&: s4) ((f3 :&: (f2 :&: ((e' :|: r0) :&: s1))) :&: s4)
bonfssfr :: b e e' -> b ((f3 :&: (f2 :&: ((l0 :|: e ) :&: s1))) :&: s4) ((f3 :&: (f2 :&: ((l0 :|: e') :&: s1))) :&: s4)
bonfsssf :: b e e' -> b ((f3 :&: (f2 :&: (f1 :&: (e  :&: s0)))) :&: s4) ((f3 :&: (f2 :&: (f1 :&: (e' :&: s0)))) :&: s4)
bonfssss :: b e e' -> b ((f3 :&: (f2 :&: (f1 :&: (f0 :&: e )))) :&: s4) ((f3 :&: (f2 :&: (f1 :&: (f0 :&: e')))) :&: s4)
bonfsssl :: b e e' -> b ((f3 :&: (f2 :&: (f1 :&: (e  :|: r0)))) :&: s4) ((f3 :&: (f2 :&: (f1 :&: (e' :|: r0)))) :&: s4)
bonfsssr :: b e e' -> b ((f3 :&: (f2 :&: (f1 :&: (l0 :|: e )))) :&: s4) ((f3 :&: (f2 :&: (f1 :&: (l0 :|: e')))) :&: s4)
bonfsslf :: b e e' -> b ((f3 :&: (f2 :&: ((e  :&: s0) :|: r1))) :&: s4) ((f3 :&: (f2 :&: ((e' :&: s0) :|: r1))) :&: s4)
bonfssls :: b e e' -> b ((f3 :&: (f2 :&: ((f0 :&: e ) :|: r1))) :&: s4) ((f3 :&: (f2 :&: ((f0 :&: e') :|: r1))) :&: s4)
bonfssll :: b e e' -> b ((f3 :&: (f2 :&: ((e  :|: r0) :|: r1))) :&: s4) ((f3 :&: (f2 :&: ((e' :|: r0) :|: r1))) :&: s4)
bonfsslr :: b e e' -> b ((f3 :&: (f2 :&: ((l0 :|: e ) :|: r1))) :&: s4) ((f3 :&: (f2 :&: ((l0 :|: e') :|: r1))) :&: s4)
bonfssrf :: b e e' -> b ((f3 :&: (f2 :&: (l1 :|: (e  :&: s0)))) :&: s4) ((f3 :&: (f2 :&: (l1 :|: (e' :&: s0)))) :&: s4)
bonfssrs :: b e e' -> b ((f3 :&: (f2 :&: (l1 :|: (f0 :&: e )))) :&: s4) ((f3 :&: (f2 :&: (l1 :|: (f0 :&: e')))) :&: s4)
bonfssrl :: b e e' -> b ((f3 :&: (f2 :&: (l1 :|: (e  :|: r0)))) :&: s4) ((f3 :&: (f2 :&: (l1 :|: (e' :|: r0)))) :&: s4)
bonfssrr :: b e e' -> b ((f3 :&: (f2 :&: (l1 :|: (l0 :|: e )))) :&: s4) ((f3 :&: (f2 :&: (l1 :|: (l0 :|: e')))) :&: s4)
bonfslff :: b e e' -> b ((f3 :&: (((e  :&: s0) :&: s1) :|: r2)) :&: s4) ((f3 :&: (((e' :&: s0) :&: s1) :|: r2)) :&: s4)
bonfslfs :: b e e' -> b ((f3 :&: (((f0 :&: e ) :&: s1) :|: r2)) :&: s4) ((f3 :&: (((f0 :&: e') :&: s1) :|: r2)) :&: s4)
bonfslfl :: b e e' -> b ((f3 :&: (((e  :|: r0) :&: s1) :|: r2)) :&: s4) ((f3 :&: (((e' :|: r0) :&: s1) :|: r2)) :&: s4)
bonfslfr :: b e e' -> b ((f3 :&: (((l0 :|: e ) :&: s1) :|: r2)) :&: s4) ((f3 :&: (((l0 :|: e') :&: s1) :|: r2)) :&: s4)
bonfslsf :: b e e' -> b ((f3 :&: ((f1 :&: (e  :&: s0)) :|: r2)) :&: s4) ((f3 :&: ((f1 :&: (e' :&: s0)) :|: r2)) :&: s4)
bonfslss :: b e e' -> b ((f3 :&: ((f1 :&: (f0 :&: e )) :|: r2)) :&: s4) ((f3 :&: ((f1 :&: (f0 :&: e')) :|: r2)) :&: s4)
bonfslsl :: b e e' -> b ((f3 :&: ((f1 :&: (e  :|: r0)) :|: r2)) :&: s4) ((f3 :&: ((f1 :&: (e' :|: r0)) :|: r2)) :&: s4)
bonfslsr :: b e e' -> b ((f3 :&: ((f1 :&: (l0 :|: e )) :|: r2)) :&: s4) ((f3 :&: ((f1 :&: (l0 :|: e')) :|: r2)) :&: s4)
bonfsllf :: b e e' -> b ((f3 :&: (((e  :&: s0) :|: r1) :|: r2)) :&: s4) ((f3 :&: (((e' :&: s0) :|: r1) :|: r2)) :&: s4)
bonfslls :: b e e' -> b ((f3 :&: (((f0 :&: e ) :|: r1) :|: r2)) :&: s4) ((f3 :&: (((f0 :&: e') :|: r1) :|: r2)) :&: s4)
bonfslll :: b e e' -> b ((f3 :&: (((e  :|: r0) :|: r1) :|: r2)) :&: s4) ((f3 :&: (((e' :|: r0) :|: r1) :|: r2)) :&: s4)
bonfsllr :: b e e' -> b ((f3 :&: (((l0 :|: e ) :|: r1) :|: r2)) :&: s4) ((f3 :&: (((l0 :|: e') :|: r1) :|: r2)) :&: s4)
bonfslrf :: b e e' -> b ((f3 :&: ((l1 :|: (e  :&: s0)) :|: r2)) :&: s4) ((f3 :&: ((l1 :|: (e' :&: s0)) :|: r2)) :&: s4)
bonfslrs :: b e e' -> b ((f3 :&: ((l1 :|: (f0 :&: e )) :|: r2)) :&: s4) ((f3 :&: ((l1 :|: (f0 :&: e')) :|: r2)) :&: s4)
bonfslrl :: b e e' -> b ((f3 :&: ((l1 :|: (e  :|: r0)) :|: r2)) :&: s4) ((f3 :&: ((l1 :|: (e' :|: r0)) :|: r2)) :&: s4)
bonfslrr :: b e e' -> b ((f3 :&: ((l1 :|: (l0 :|: e )) :|: r2)) :&: s4) ((f3 :&: ((l1 :|: (l0 :|: e')) :|: r2)) :&: s4)
bonfsrff :: b e e' -> b ((f3 :&: (l2 :|: ((e  :&: s0) :&: s1))) :&: s4) ((f3 :&: (l2 :|: ((e' :&: s0) :&: s1))) :&: s4)
bonfsrfs :: b e e' -> b ((f3 :&: (l2 :|: ((f0 :&: e ) :&: s1))) :&: s4) ((f3 :&: (l2 :|: ((f0 :&: e') :&: s1))) :&: s4)
bonfsrfl :: b e e' -> b ((f3 :&: (l2 :|: ((e  :|: r0) :&: s1))) :&: s4) ((f3 :&: (l2 :|: ((e' :|: r0) :&: s1))) :&: s4)
bonfsrfr :: b e e' -> b ((f3 :&: (l2 :|: ((l0 :|: e ) :&: s1))) :&: s4) ((f3 :&: (l2 :|: ((l0 :|: e') :&: s1))) :&: s4)
bonfsrsf :: b e e' -> b ((f3 :&: (l2 :|: (f1 :&: (e  :&: s0)))) :&: s4) ((f3 :&: (l2 :|: (f1 :&: (e' :&: s0)))) :&: s4)
bonfsrss :: b e e' -> b ((f3 :&: (l2 :|: (f1 :&: (f0 :&: e )))) :&: s4) ((f3 :&: (l2 :|: (f1 :&: (f0 :&: e')))) :&: s4)
bonfsrsl :: b e e' -> b ((f3 :&: (l2 :|: (f1 :&: (e  :|: r0)))) :&: s4) ((f3 :&: (l2 :|: (f1 :&: (e' :|: r0)))) :&: s4)
bonfsrsr :: b e e' -> b ((f3 :&: (l2 :|: (f1 :&: (l0 :|: e )))) :&: s4) ((f3 :&: (l2 :|: (f1 :&: (l0 :|: e')))) :&: s4)
bonfsrlf :: b e e' -> b ((f3 :&: (l2 :|: ((e  :&: s0) :|: r1))) :&: s4) ((f3 :&: (l2 :|: ((e' :&: s0) :|: r1))) :&: s4)
bonfsrls :: b e e' -> b ((f3 :&: (l2 :|: ((f0 :&: e ) :|: r1))) :&: s4) ((f3 :&: (l2 :|: ((f0 :&: e') :|: r1))) :&: s4)
bonfsrll :: b e e' -> b ((f3 :&: (l2 :|: ((e  :|: r0) :|: r1))) :&: s4) ((f3 :&: (l2 :|: ((e' :|: r0) :|: r1))) :&: s4)
bonfsrlr :: b e e' -> b ((f3 :&: (l2 :|: ((l0 :|: e ) :|: r1))) :&: s4) ((f3 :&: (l2 :|: ((l0 :|: e') :|: r1))) :&: s4)
bonfsrrf :: b e e' -> b ((f3 :&: (l2 :|: (l1 :|: (e  :&: s0)))) :&: s4) ((f3 :&: (l2 :|: (l1 :|: (e' :&: s0)))) :&: s4)
bonfsrrs :: b e e' -> b ((f3 :&: (l2 :|: (l1 :|: (f0 :&: e )))) :&: s4) ((f3 :&: (l2 :|: (l1 :|: (f0 :&: e')))) :&: s4)
bonfsrrl :: b e e' -> b ((f3 :&: (l2 :|: (l1 :|: (e  :|: r0)))) :&: s4) ((f3 :&: (l2 :|: (l1 :|: (e' :|: r0)))) :&: s4)
bonfsrrr :: b e e' -> b ((f3 :&: (l2 :|: (l1 :|: (l0 :|: e )))) :&: s4) ((f3 :&: (l2 :|: (l1 :|: (l0 :|: e')))) :&: s4)
bonflfff :: b e e' -> b (((((e  :&: s0) :&: s1) :&: s2) :|: r3) :&: s4) (((((e' :&: s0) :&: s1) :&: s2) :|: r3) :&: s4)
bonflffs :: b e e' -> b (((((f0 :&: e ) :&: s1) :&: s2) :|: r3) :&: s4) (((((f0 :&: e') :&: s1) :&: s2) :|: r3) :&: s4)
bonflffl :: b e e' -> b (((((e  :|: r0) :&: s1) :&: s2) :|: r3) :&: s4) (((((e' :|: r0) :&: s1) :&: s2) :|: r3) :&: s4)
bonflffr :: b e e' -> b (((((l0 :|: e ) :&: s1) :&: s2) :|: r3) :&: s4) (((((l0 :|: e') :&: s1) :&: s2) :|: r3) :&: s4)
bonflfsf :: b e e' -> b ((((f1 :&: (e  :&: s0)) :&: s2) :|: r3) :&: s4) ((((f1 :&: (e' :&: s0)) :&: s2) :|: r3) :&: s4)
bonflfss :: b e e' -> b ((((f1 :&: (f0 :&: e )) :&: s2) :|: r3) :&: s4) ((((f1 :&: (f0 :&: e')) :&: s2) :|: r3) :&: s4)
bonflfsl :: b e e' -> b ((((f1 :&: (e  :|: r0)) :&: s2) :|: r3) :&: s4) ((((f1 :&: (e' :|: r0)) :&: s2) :|: r3) :&: s4)
bonflfsr :: b e e' -> b ((((f1 :&: (l0 :|: e )) :&: s2) :|: r3) :&: s4) ((((f1 :&: (l0 :|: e')) :&: s2) :|: r3) :&: s4)
bonflflf :: b e e' -> b (((((e  :&: s0) :|: r1) :&: s2) :|: r3) :&: s4) (((((e' :&: s0) :|: r1) :&: s2) :|: r3) :&: s4)
bonflfls :: b e e' -> b (((((f0 :&: e ) :|: r1) :&: s2) :|: r3) :&: s4) (((((f0 :&: e') :|: r1) :&: s2) :|: r3) :&: s4)
bonflfll :: b e e' -> b (((((e  :|: r0) :|: r1) :&: s2) :|: r3) :&: s4) (((((e' :|: r0) :|: r1) :&: s2) :|: r3) :&: s4)
bonflflr :: b e e' -> b (((((l0 :|: e ) :|: r1) :&: s2) :|: r3) :&: s4) (((((l0 :|: e') :|: r1) :&: s2) :|: r3) :&: s4)
bonflfrf :: b e e' -> b ((((l1 :|: (e  :&: s0)) :&: s2) :|: r3) :&: s4) ((((l1 :|: (e' :&: s0)) :&: s2) :|: r3) :&: s4)
bonflfrs :: b e e' -> b ((((l1 :|: (f0 :&: e )) :&: s2) :|: r3) :&: s4) ((((l1 :|: (f0 :&: e')) :&: s2) :|: r3) :&: s4)
bonflfrl :: b e e' -> b ((((l1 :|: (e  :|: r0)) :&: s2) :|: r3) :&: s4) ((((l1 :|: (e' :|: r0)) :&: s2) :|: r3) :&: s4)
bonflfrr :: b e e' -> b ((((l1 :|: (l0 :|: e )) :&: s2) :|: r3) :&: s4) ((((l1 :|: (l0 :|: e')) :&: s2) :|: r3) :&: s4)
bonflsff :: b e e' -> b (((f2 :&: ((e  :&: s0) :&: s1)) :|: r3) :&: s4) (((f2 :&: ((e' :&: s0) :&: s1)) :|: r3) :&: s4)
bonflsfs :: b e e' -> b (((f2 :&: ((f0 :&: e ) :&: s1)) :|: r3) :&: s4) (((f2 :&: ((f0 :&: e') :&: s1)) :|: r3) :&: s4)
bonflsfl :: b e e' -> b (((f2 :&: ((e  :|: r0) :&: s1)) :|: r3) :&: s4) (((f2 :&: ((e' :|: r0) :&: s1)) :|: r3) :&: s4)
bonflsfr :: b e e' -> b (((f2 :&: ((l0 :|: e ) :&: s1)) :|: r3) :&: s4) (((f2 :&: ((l0 :|: e') :&: s1)) :|: r3) :&: s4)
bonflssf :: b e e' -> b (((f2 :&: (f1 :&: (e  :&: s0))) :|: r3) :&: s4) (((f2 :&: (f1 :&: (e' :&: s0))) :|: r3) :&: s4)
bonflsss :: b e e' -> b (((f2 :&: (f1 :&: (f0 :&: e ))) :|: r3) :&: s4) (((f2 :&: (f1 :&: (f0 :&: e'))) :|: r3) :&: s4)
bonflssl :: b e e' -> b (((f2 :&: (f1 :&: (e  :|: r0))) :|: r3) :&: s4) (((f2 :&: (f1 :&: (e' :|: r0))) :|: r3) :&: s4)
bonflssr :: b e e' -> b (((f2 :&: (f1 :&: (l0 :|: e ))) :|: r3) :&: s4) (((f2 :&: (f1 :&: (l0 :|: e'))) :|: r3) :&: s4)
bonflslf :: b e e' -> b (((f2 :&: ((e  :&: s0) :|: r1)) :|: r3) :&: s4) (((f2 :&: ((e' :&: s0) :|: r1)) :|: r3) :&: s4)
bonflsls :: b e e' -> b (((f2 :&: ((f0 :&: e ) :|: r1)) :|: r3) :&: s4) (((f2 :&: ((f0 :&: e') :|: r1)) :|: r3) :&: s4)
bonflsll :: b e e' -> b (((f2 :&: ((e  :|: r0) :|: r1)) :|: r3) :&: s4) (((f2 :&: ((e' :|: r0) :|: r1)) :|: r3) :&: s4)
bonflslr :: b e e' -> b (((f2 :&: ((l0 :|: e ) :|: r1)) :|: r3) :&: s4) (((f2 :&: ((l0 :|: e') :|: r1)) :|: r3) :&: s4)
bonflsrf :: b e e' -> b (((f2 :&: (l1 :|: (e  :&: s0))) :|: r3) :&: s4) (((f2 :&: (l1 :|: (e' :&: s0))) :|: r3) :&: s4)
bonflsrs :: b e e' -> b (((f2 :&: (l1 :|: (f0 :&: e ))) :|: r3) :&: s4) (((f2 :&: (l1 :|: (f0 :&: e'))) :|: r3) :&: s4)
bonflsrl :: b e e' -> b (((f2 :&: (l1 :|: (e  :|: r0))) :|: r3) :&: s4) (((f2 :&: (l1 :|: (e' :|: r0))) :|: r3) :&: s4)
bonflsrr :: b e e' -> b (((f2 :&: (l1 :|: (l0 :|: e ))) :|: r3) :&: s4) (((f2 :&: (l1 :|: (l0 :|: e'))) :|: r3) :&: s4)
bonfllff :: b e e' -> b (((((e  :&: s0) :&: s1) :|: r2) :|: r3) :&: s4) (((((e' :&: s0) :&: s1) :|: r2) :|: r3) :&: s4)
bonfllfs :: b e e' -> b (((((f0 :&: e ) :&: s1) :|: r2) :|: r3) :&: s4) (((((f0 :&: e') :&: s1) :|: r2) :|: r3) :&: s4)
bonfllfl :: b e e' -> b (((((e  :|: r0) :&: s1) :|: r2) :|: r3) :&: s4) (((((e' :|: r0) :&: s1) :|: r2) :|: r3) :&: s4)
bonfllfr :: b e e' -> b (((((l0 :|: e ) :&: s1) :|: r2) :|: r3) :&: s4) (((((l0 :|: e') :&: s1) :|: r2) :|: r3) :&: s4)
bonfllsf :: b e e' -> b ((((f1 :&: (e  :&: s0)) :|: r2) :|: r3) :&: s4) ((((f1 :&: (e' :&: s0)) :|: r2) :|: r3) :&: s4)
bonfllss :: b e e' -> b ((((f1 :&: (f0 :&: e )) :|: r2) :|: r3) :&: s4) ((((f1 :&: (f0 :&: e')) :|: r2) :|: r3) :&: s4)
bonfllsl :: b e e' -> b ((((f1 :&: (e  :|: r0)) :|: r2) :|: r3) :&: s4) ((((f1 :&: (e' :|: r0)) :|: r2) :|: r3) :&: s4)
bonfllsr :: b e e' -> b ((((f1 :&: (l0 :|: e )) :|: r2) :|: r3) :&: s4) ((((f1 :&: (l0 :|: e')) :|: r2) :|: r3) :&: s4)
bonflllf :: b e e' -> b (((((e  :&: s0) :|: r1) :|: r2) :|: r3) :&: s4) (((((e' :&: s0) :|: r1) :|: r2) :|: r3) :&: s4)
bonfllls :: b e e' -> b (((((f0 :&: e ) :|: r1) :|: r2) :|: r3) :&: s4) (((((f0 :&: e') :|: r1) :|: r2) :|: r3) :&: s4)
bonfllll :: b e e' -> b (((((e  :|: r0) :|: r1) :|: r2) :|: r3) :&: s4) (((((e' :|: r0) :|: r1) :|: r2) :|: r3) :&: s4)
bonflllr :: b e e' -> b (((((l0 :|: e ) :|: r1) :|: r2) :|: r3) :&: s4) (((((l0 :|: e') :|: r1) :|: r2) :|: r3) :&: s4)
bonfllrf :: b e e' -> b ((((l1 :|: (e  :&: s0)) :|: r2) :|: r3) :&: s4) ((((l1 :|: (e' :&: s0)) :|: r2) :|: r3) :&: s4)
bonfllrs :: b e e' -> b ((((l1 :|: (f0 :&: e )) :|: r2) :|: r3) :&: s4) ((((l1 :|: (f0 :&: e')) :|: r2) :|: r3) :&: s4)
bonfllrl :: b e e' -> b ((((l1 :|: (e  :|: r0)) :|: r2) :|: r3) :&: s4) ((((l1 :|: (e' :|: r0)) :|: r2) :|: r3) :&: s4)
bonfllrr :: b e e' -> b ((((l1 :|: (l0 :|: e )) :|: r2) :|: r3) :&: s4) ((((l1 :|: (l0 :|: e')) :|: r2) :|: r3) :&: s4)
bonflrff :: b e e' -> b (((l2 :|: ((e  :&: s0) :&: s1)) :|: r3) :&: s4) (((l2 :|: ((e' :&: s0) :&: s1)) :|: r3) :&: s4)
bonflrfs :: b e e' -> b (((l2 :|: ((f0 :&: e ) :&: s1)) :|: r3) :&: s4) (((l2 :|: ((f0 :&: e') :&: s1)) :|: r3) :&: s4)
bonflrfl :: b e e' -> b (((l2 :|: ((e  :|: r0) :&: s1)) :|: r3) :&: s4) (((l2 :|: ((e' :|: r0) :&: s1)) :|: r3) :&: s4)
bonflrfr :: b e e' -> b (((l2 :|: ((l0 :|: e ) :&: s1)) :|: r3) :&: s4) (((l2 :|: ((l0 :|: e') :&: s1)) :|: r3) :&: s4)
bonflrsf :: b e e' -> b (((l2 :|: (f1 :&: (e  :&: s0))) :|: r3) :&: s4) (((l2 :|: (f1 :&: (e' :&: s0))) :|: r3) :&: s4)
bonflrss :: b e e' -> b (((l2 :|: (f1 :&: (f0 :&: e ))) :|: r3) :&: s4) (((l2 :|: (f1 :&: (f0 :&: e'))) :|: r3) :&: s4)
bonflrsl :: b e e' -> b (((l2 :|: (f1 :&: (e  :|: r0))) :|: r3) :&: s4) (((l2 :|: (f1 :&: (e' :|: r0))) :|: r3) :&: s4)
bonflrsr :: b e e' -> b (((l2 :|: (f1 :&: (l0 :|: e ))) :|: r3) :&: s4) (((l2 :|: (f1 :&: (l0 :|: e'))) :|: r3) :&: s4)
bonflrlf :: b e e' -> b (((l2 :|: ((e  :&: s0) :|: r1)) :|: r3) :&: s4) (((l2 :|: ((e' :&: s0) :|: r1)) :|: r3) :&: s4)
bonflrls :: b e e' -> b (((l2 :|: ((f0 :&: e ) :|: r1)) :|: r3) :&: s4) (((l2 :|: ((f0 :&: e') :|: r1)) :|: r3) :&: s4)
bonflrll :: b e e' -> b (((l2 :|: ((e  :|: r0) :|: r1)) :|: r3) :&: s4) (((l2 :|: ((e' :|: r0) :|: r1)) :|: r3) :&: s4)
bonflrlr :: b e e' -> b (((l2 :|: ((l0 :|: e ) :|: r1)) :|: r3) :&: s4) (((l2 :|: ((l0 :|: e') :|: r1)) :|: r3) :&: s4)
bonflrrf :: b e e' -> b (((l2 :|: (l1 :|: (e  :&: s0))) :|: r3) :&: s4) (((l2 :|: (l1 :|: (e' :&: s0))) :|: r3) :&: s4)
bonflrrs :: b e e' -> b (((l2 :|: (l1 :|: (f0 :&: e ))) :|: r3) :&: s4) (((l2 :|: (l1 :|: (f0 :&: e'))) :|: r3) :&: s4)
bonflrrl :: b e e' -> b (((l2 :|: (l1 :|: (e  :|: r0))) :|: r3) :&: s4) (((l2 :|: (l1 :|: (e' :|: r0))) :|: r3) :&: s4)
bonflrrr :: b e e' -> b (((l2 :|: (l1 :|: (l0 :|: e ))) :|: r3) :&: s4) (((l2 :|: (l1 :|: (l0 :|: e'))) :|: r3) :&: s4)
bonfrfff :: b e e' -> b ((l3 :|: (((e  :&: s0) :&: s1) :&: s2)) :&: s4) ((l3 :|: (((e' :&: s0) :&: s1) :&: s2)) :&: s4)
bonfrffs :: b e e' -> b ((l3 :|: (((f0 :&: e ) :&: s1) :&: s2)) :&: s4) ((l3 :|: (((f0 :&: e') :&: s1) :&: s2)) :&: s4)
bonfrffl :: b e e' -> b ((l3 :|: (((e  :|: r0) :&: s1) :&: s2)) :&: s4) ((l3 :|: (((e' :|: r0) :&: s1) :&: s2)) :&: s4)
bonfrffr :: b e e' -> b ((l3 :|: (((l0 :|: e ) :&: s1) :&: s2)) :&: s4) ((l3 :|: (((l0 :|: e') :&: s1) :&: s2)) :&: s4)
bonfrfsf :: b e e' -> b ((l3 :|: ((f1 :&: (e  :&: s0)) :&: s2)) :&: s4) ((l3 :|: ((f1 :&: (e' :&: s0)) :&: s2)) :&: s4)
bonfrfss :: b e e' -> b ((l3 :|: ((f1 :&: (f0 :&: e )) :&: s2)) :&: s4) ((l3 :|: ((f1 :&: (f0 :&: e')) :&: s2)) :&: s4)
bonfrfsl :: b e e' -> b ((l3 :|: ((f1 :&: (e  :|: r0)) :&: s2)) :&: s4) ((l3 :|: ((f1 :&: (e' :|: r0)) :&: s2)) :&: s4)
bonfrfsr :: b e e' -> b ((l3 :|: ((f1 :&: (l0 :|: e )) :&: s2)) :&: s4) ((l3 :|: ((f1 :&: (l0 :|: e')) :&: s2)) :&: s4)
bonfrflf :: b e e' -> b ((l3 :|: (((e  :&: s0) :|: r1) :&: s2)) :&: s4) ((l3 :|: (((e' :&: s0) :|: r1) :&: s2)) :&: s4)
bonfrfls :: b e e' -> b ((l3 :|: (((f0 :&: e ) :|: r1) :&: s2)) :&: s4) ((l3 :|: (((f0 :&: e') :|: r1) :&: s2)) :&: s4)
bonfrfll :: b e e' -> b ((l3 :|: (((e  :|: r0) :|: r1) :&: s2)) :&: s4) ((l3 :|: (((e' :|: r0) :|: r1) :&: s2)) :&: s4)
bonfrflr :: b e e' -> b ((l3 :|: (((l0 :|: e ) :|: r1) :&: s2)) :&: s4) ((l3 :|: (((l0 :|: e') :|: r1) :&: s2)) :&: s4)
bonfrfrf :: b e e' -> b ((l3 :|: ((l1 :|: (e  :&: s0)) :&: s2)) :&: s4) ((l3 :|: ((l1 :|: (e' :&: s0)) :&: s2)) :&: s4)
bonfrfrs :: b e e' -> b ((l3 :|: ((l1 :|: (f0 :&: e )) :&: s2)) :&: s4) ((l3 :|: ((l1 :|: (f0 :&: e')) :&: s2)) :&: s4)
bonfrfrl :: b e e' -> b ((l3 :|: ((l1 :|: (e  :|: r0)) :&: s2)) :&: s4) ((l3 :|: ((l1 :|: (e' :|: r0)) :&: s2)) :&: s4)
bonfrfrr :: b e e' -> b ((l3 :|: ((l1 :|: (l0 :|: e )) :&: s2)) :&: s4) ((l3 :|: ((l1 :|: (l0 :|: e')) :&: s2)) :&: s4)
bonfrsff :: b e e' -> b ((l3 :|: (f2 :&: ((e  :&: s0) :&: s1))) :&: s4) ((l3 :|: (f2 :&: ((e' :&: s0) :&: s1))) :&: s4)
bonfrsfs :: b e e' -> b ((l3 :|: (f2 :&: ((f0 :&: e ) :&: s1))) :&: s4) ((l3 :|: (f2 :&: ((f0 :&: e') :&: s1))) :&: s4)
bonfrsfl :: b e e' -> b ((l3 :|: (f2 :&: ((e  :|: r0) :&: s1))) :&: s4) ((l3 :|: (f2 :&: ((e' :|: r0) :&: s1))) :&: s4)
bonfrsfr :: b e e' -> b ((l3 :|: (f2 :&: ((l0 :|: e ) :&: s1))) :&: s4) ((l3 :|: (f2 :&: ((l0 :|: e') :&: s1))) :&: s4)
bonfrssf :: b e e' -> b ((l3 :|: (f2 :&: (f1 :&: (e  :&: s0)))) :&: s4) ((l3 :|: (f2 :&: (f1 :&: (e' :&: s0)))) :&: s4)
bonfrsss :: b e e' -> b ((l3 :|: (f2 :&: (f1 :&: (f0 :&: e )))) :&: s4) ((l3 :|: (f2 :&: (f1 :&: (f0 :&: e')))) :&: s4)
bonfrssl :: b e e' -> b ((l3 :|: (f2 :&: (f1 :&: (e  :|: r0)))) :&: s4) ((l3 :|: (f2 :&: (f1 :&: (e' :|: r0)))) :&: s4)
bonfrssr :: b e e' -> b ((l3 :|: (f2 :&: (f1 :&: (l0 :|: e )))) :&: s4) ((l3 :|: (f2 :&: (f1 :&: (l0 :|: e')))) :&: s4)
bonfrslf :: b e e' -> b ((l3 :|: (f2 :&: ((e  :&: s0) :|: r1))) :&: s4) ((l3 :|: (f2 :&: ((e' :&: s0) :|: r1))) :&: s4)
bonfrsls :: b e e' -> b ((l3 :|: (f2 :&: ((f0 :&: e ) :|: r1))) :&: s4) ((l3 :|: (f2 :&: ((f0 :&: e') :|: r1))) :&: s4)
bonfrsll :: b e e' -> b ((l3 :|: (f2 :&: ((e  :|: r0) :|: r1))) :&: s4) ((l3 :|: (f2 :&: ((e' :|: r0) :|: r1))) :&: s4)
bonfrslr :: b e e' -> b ((l3 :|: (f2 :&: ((l0 :|: e ) :|: r1))) :&: s4) ((l3 :|: (f2 :&: ((l0 :|: e') :|: r1))) :&: s4)
bonfrsrf :: b e e' -> b ((l3 :|: (f2 :&: (l1 :|: (e  :&: s0)))) :&: s4) ((l3 :|: (f2 :&: (l1 :|: (e' :&: s0)))) :&: s4)
bonfrsrs :: b e e' -> b ((l3 :|: (f2 :&: (l1 :|: (f0 :&: e )))) :&: s4) ((l3 :|: (f2 :&: (l1 :|: (f0 :&: e')))) :&: s4)
bonfrsrl :: b e e' -> b ((l3 :|: (f2 :&: (l1 :|: (e  :|: r0)))) :&: s4) ((l3 :|: (f2 :&: (l1 :|: (e' :|: r0)))) :&: s4)
bonfrsrr :: b e e' -> b ((l3 :|: (f2 :&: (l1 :|: (l0 :|: e )))) :&: s4) ((l3 :|: (f2 :&: (l1 :|: (l0 :|: e')))) :&: s4)
bonfrlff :: b e e' -> b ((l3 :|: (((e  :&: s0) :&: s1) :|: r2)) :&: s4) ((l3 :|: (((e' :&: s0) :&: s1) :|: r2)) :&: s4)
bonfrlfs :: b e e' -> b ((l3 :|: (((f0 :&: e ) :&: s1) :|: r2)) :&: s4) ((l3 :|: (((f0 :&: e') :&: s1) :|: r2)) :&: s4)
bonfrlfl :: b e e' -> b ((l3 :|: (((e  :|: r0) :&: s1) :|: r2)) :&: s4) ((l3 :|: (((e' :|: r0) :&: s1) :|: r2)) :&: s4)
bonfrlfr :: b e e' -> b ((l3 :|: (((l0 :|: e ) :&: s1) :|: r2)) :&: s4) ((l3 :|: (((l0 :|: e') :&: s1) :|: r2)) :&: s4)
bonfrlsf :: b e e' -> b ((l3 :|: ((f1 :&: (e  :&: s0)) :|: r2)) :&: s4) ((l3 :|: ((f1 :&: (e' :&: s0)) :|: r2)) :&: s4)
bonfrlss :: b e e' -> b ((l3 :|: ((f1 :&: (f0 :&: e )) :|: r2)) :&: s4) ((l3 :|: ((f1 :&: (f0 :&: e')) :|: r2)) :&: s4)
bonfrlsl :: b e e' -> b ((l3 :|: ((f1 :&: (e  :|: r0)) :|: r2)) :&: s4) ((l3 :|: ((f1 :&: (e' :|: r0)) :|: r2)) :&: s4)
bonfrlsr :: b e e' -> b ((l3 :|: ((f1 :&: (l0 :|: e )) :|: r2)) :&: s4) ((l3 :|: ((f1 :&: (l0 :|: e')) :|: r2)) :&: s4)
bonfrllf :: b e e' -> b ((l3 :|: (((e  :&: s0) :|: r1) :|: r2)) :&: s4) ((l3 :|: (((e' :&: s0) :|: r1) :|: r2)) :&: s4)
bonfrlls :: b e e' -> b ((l3 :|: (((f0 :&: e ) :|: r1) :|: r2)) :&: s4) ((l3 :|: (((f0 :&: e') :|: r1) :|: r2)) :&: s4)
bonfrlll :: b e e' -> b ((l3 :|: (((e  :|: r0) :|: r1) :|: r2)) :&: s4) ((l3 :|: (((e' :|: r0) :|: r1) :|: r2)) :&: s4)
bonfrllr :: b e e' -> b ((l3 :|: (((l0 :|: e ) :|: r1) :|: r2)) :&: s4) ((l3 :|: (((l0 :|: e') :|: r1) :|: r2)) :&: s4)
bonfrlrf :: b e e' -> b ((l3 :|: ((l1 :|: (e  :&: s0)) :|: r2)) :&: s4) ((l3 :|: ((l1 :|: (e' :&: s0)) :|: r2)) :&: s4)
bonfrlrs :: b e e' -> b ((l3 :|: ((l1 :|: (f0 :&: e )) :|: r2)) :&: s4) ((l3 :|: ((l1 :|: (f0 :&: e')) :|: r2)) :&: s4)
bonfrlrl :: b e e' -> b ((l3 :|: ((l1 :|: (e  :|: r0)) :|: r2)) :&: s4) ((l3 :|: ((l1 :|: (e' :|: r0)) :|: r2)) :&: s4)
bonfrlrr :: b e e' -> b ((l3 :|: ((l1 :|: (l0 :|: e )) :|: r2)) :&: s4) ((l3 :|: ((l1 :|: (l0 :|: e')) :|: r2)) :&: s4)
bonfrrff :: b e e' -> b ((l3 :|: (l2 :|: ((e  :&: s0) :&: s1))) :&: s4) ((l3 :|: (l2 :|: ((e' :&: s0) :&: s1))) :&: s4)
bonfrrfs :: b e e' -> b ((l3 :|: (l2 :|: ((f0 :&: e ) :&: s1))) :&: s4) ((l3 :|: (l2 :|: ((f0 :&: e') :&: s1))) :&: s4)
bonfrrfl :: b e e' -> b ((l3 :|: (l2 :|: ((e  :|: r0) :&: s1))) :&: s4) ((l3 :|: (l2 :|: ((e' :|: r0) :&: s1))) :&: s4)
bonfrrfr :: b e e' -> b ((l3 :|: (l2 :|: ((l0 :|: e ) :&: s1))) :&: s4) ((l3 :|: (l2 :|: ((l0 :|: e') :&: s1))) :&: s4)
bonfrrsf :: b e e' -> b ((l3 :|: (l2 :|: (f1 :&: (e  :&: s0)))) :&: s4) ((l3 :|: (l2 :|: (f1 :&: (e' :&: s0)))) :&: s4)
bonfrrss :: b e e' -> b ((l3 :|: (l2 :|: (f1 :&: (f0 :&: e )))) :&: s4) ((l3 :|: (l2 :|: (f1 :&: (f0 :&: e')))) :&: s4)
bonfrrsl :: b e e' -> b ((l3 :|: (l2 :|: (f1 :&: (e  :|: r0)))) :&: s4) ((l3 :|: (l2 :|: (f1 :&: (e' :|: r0)))) :&: s4)
bonfrrsr :: b e e' -> b ((l3 :|: (l2 :|: (f1 :&: (l0 :|: e )))) :&: s4) ((l3 :|: (l2 :|: (f1 :&: (l0 :|: e')))) :&: s4)
bonfrrlf :: b e e' -> b ((l3 :|: (l2 :|: ((e  :&: s0) :|: r1))) :&: s4) ((l3 :|: (l2 :|: ((e' :&: s0) :|: r1))) :&: s4)
bonfrrls :: b e e' -> b ((l3 :|: (l2 :|: ((f0 :&: e ) :|: r1))) :&: s4) ((l3 :|: (l2 :|: ((f0 :&: e') :|: r1))) :&: s4)
bonfrrll :: b e e' -> b ((l3 :|: (l2 :|: ((e  :|: r0) :|: r1))) :&: s4) ((l3 :|: (l2 :|: ((e' :|: r0) :|: r1))) :&: s4)
bonfrrlr :: b e e' -> b ((l3 :|: (l2 :|: ((l0 :|: e ) :|: r1))) :&: s4) ((l3 :|: (l2 :|: ((l0 :|: e') :|: r1))) :&: s4)
bonfrrrf :: b e e' -> b ((l3 :|: (l2 :|: (l1 :|: (e  :&: s0)))) :&: s4) ((l3 :|: (l2 :|: (l1 :|: (e' :&: s0)))) :&: s4)
bonfrrrs :: b e e' -> b ((l3 :|: (l2 :|: (l1 :|: (f0 :&: e )))) :&: s4) ((l3 :|: (l2 :|: (l1 :|: (f0 :&: e')))) :&: s4)
bonfrrrl :: b e e' -> b ((l3 :|: (l2 :|: (l1 :|: (e  :|: r0)))) :&: s4) ((l3 :|: (l2 :|: (l1 :|: (e' :|: r0)))) :&: s4)
bonfrrrr :: b e e' -> b ((l3 :|: (l2 :|: (l1 :|: (l0 :|: e )))) :&: s4) ((l3 :|: (l2 :|: (l1 :|: (l0 :|: e')))) :&: s4)
bonsffff :: b e e' -> b (f4 :&: ((((e  :&: s0) :&: s1) :&: s2) :&: s3)) (f4 :&: ((((e' :&: s0) :&: s1) :&: s2) :&: s3))
bonsfffs :: b e e' -> b (f4 :&: ((((f0 :&: e ) :&: s1) :&: s2) :&: s3)) (f4 :&: ((((f0 :&: e') :&: s1) :&: s2) :&: s3))
bonsfffl :: b e e' -> b (f4 :&: ((((e  :|: r0) :&: s1) :&: s2) :&: s3)) (f4 :&: ((((e' :|: r0) :&: s1) :&: s2) :&: s3))
bonsfffr :: b e e' -> b (f4 :&: ((((l0 :|: e ) :&: s1) :&: s2) :&: s3)) (f4 :&: ((((l0 :|: e') :&: s1) :&: s2) :&: s3))
bonsffsf :: b e e' -> b (f4 :&: (((f1 :&: (e  :&: s0)) :&: s2) :&: s3)) (f4 :&: (((f1 :&: (e' :&: s0)) :&: s2) :&: s3))
bonsffss :: b e e' -> b (f4 :&: (((f1 :&: (f0 :&: e )) :&: s2) :&: s3)) (f4 :&: (((f1 :&: (f0 :&: e')) :&: s2) :&: s3))
bonsffsl :: b e e' -> b (f4 :&: (((f1 :&: (e  :|: r0)) :&: s2) :&: s3)) (f4 :&: (((f1 :&: (e' :|: r0)) :&: s2) :&: s3))
bonsffsr :: b e e' -> b (f4 :&: (((f1 :&: (l0 :|: e )) :&: s2) :&: s3)) (f4 :&: (((f1 :&: (l0 :|: e')) :&: s2) :&: s3))
bonsfflf :: b e e' -> b (f4 :&: ((((e  :&: s0) :|: r1) :&: s2) :&: s3)) (f4 :&: ((((e' :&: s0) :|: r1) :&: s2) :&: s3))
bonsffls :: b e e' -> b (f4 :&: ((((f0 :&: e ) :|: r1) :&: s2) :&: s3)) (f4 :&: ((((f0 :&: e') :|: r1) :&: s2) :&: s3))
bonsffll :: b e e' -> b (f4 :&: ((((e  :|: r0) :|: r1) :&: s2) :&: s3)) (f4 :&: ((((e' :|: r0) :|: r1) :&: s2) :&: s3))
bonsfflr :: b e e' -> b (f4 :&: ((((l0 :|: e ) :|: r1) :&: s2) :&: s3)) (f4 :&: ((((l0 :|: e') :|: r1) :&: s2) :&: s3))
bonsffrf :: b e e' -> b (f4 :&: (((l1 :|: (e  :&: s0)) :&: s2) :&: s3)) (f4 :&: (((l1 :|: (e' :&: s0)) :&: s2) :&: s3))
bonsffrs :: b e e' -> b (f4 :&: (((l1 :|: (f0 :&: e )) :&: s2) :&: s3)) (f4 :&: (((l1 :|: (f0 :&: e')) :&: s2) :&: s3))
bonsffrl :: b e e' -> b (f4 :&: (((l1 :|: (e  :|: r0)) :&: s2) :&: s3)) (f4 :&: (((l1 :|: (e' :|: r0)) :&: s2) :&: s3))
bonsffrr :: b e e' -> b (f4 :&: (((l1 :|: (l0 :|: e )) :&: s2) :&: s3)) (f4 :&: (((l1 :|: (l0 :|: e')) :&: s2) :&: s3))
bonsfsff :: b e e' -> b (f4 :&: ((f2 :&: ((e  :&: s0) :&: s1)) :&: s3)) (f4 :&: ((f2 :&: ((e' :&: s0) :&: s1)) :&: s3))
bonsfsfs :: b e e' -> b (f4 :&: ((f2 :&: ((f0 :&: e ) :&: s1)) :&: s3)) (f4 :&: ((f2 :&: ((f0 :&: e') :&: s1)) :&: s3))
bonsfsfl :: b e e' -> b (f4 :&: ((f2 :&: ((e  :|: r0) :&: s1)) :&: s3)) (f4 :&: ((f2 :&: ((e' :|: r0) :&: s1)) :&: s3))
bonsfsfr :: b e e' -> b (f4 :&: ((f2 :&: ((l0 :|: e ) :&: s1)) :&: s3)) (f4 :&: ((f2 :&: ((l0 :|: e') :&: s1)) :&: s3))
bonsfssf :: b e e' -> b (f4 :&: ((f2 :&: (f1 :&: (e  :&: s0))) :&: s3)) (f4 :&: ((f2 :&: (f1 :&: (e' :&: s0))) :&: s3))
bonsfsss :: b e e' -> b (f4 :&: ((f2 :&: (f1 :&: (f0 :&: e ))) :&: s3)) (f4 :&: ((f2 :&: (f1 :&: (f0 :&: e'))) :&: s3))
bonsfssl :: b e e' -> b (f4 :&: ((f2 :&: (f1 :&: (e  :|: r0))) :&: s3)) (f4 :&: ((f2 :&: (f1 :&: (e' :|: r0))) :&: s3))
bonsfssr :: b e e' -> b (f4 :&: ((f2 :&: (f1 :&: (l0 :|: e ))) :&: s3)) (f4 :&: ((f2 :&: (f1 :&: (l0 :|: e'))) :&: s3))
bonsfslf :: b e e' -> b (f4 :&: ((f2 :&: ((e  :&: s0) :|: r1)) :&: s3)) (f4 :&: ((f2 :&: ((e' :&: s0) :|: r1)) :&: s3))
bonsfsls :: b e e' -> b (f4 :&: ((f2 :&: ((f0 :&: e ) :|: r1)) :&: s3)) (f4 :&: ((f2 :&: ((f0 :&: e') :|: r1)) :&: s3))
bonsfsll :: b e e' -> b (f4 :&: ((f2 :&: ((e  :|: r0) :|: r1)) :&: s3)) (f4 :&: ((f2 :&: ((e' :|: r0) :|: r1)) :&: s3))
bonsfslr :: b e e' -> b (f4 :&: ((f2 :&: ((l0 :|: e ) :|: r1)) :&: s3)) (f4 :&: ((f2 :&: ((l0 :|: e') :|: r1)) :&: s3))
bonsfsrf :: b e e' -> b (f4 :&: ((f2 :&: (l1 :|: (e  :&: s0))) :&: s3)) (f4 :&: ((f2 :&: (l1 :|: (e' :&: s0))) :&: s3))
bonsfsrs :: b e e' -> b (f4 :&: ((f2 :&: (l1 :|: (f0 :&: e ))) :&: s3)) (f4 :&: ((f2 :&: (l1 :|: (f0 :&: e'))) :&: s3))
bonsfsrl :: b e e' -> b (f4 :&: ((f2 :&: (l1 :|: (e  :|: r0))) :&: s3)) (f4 :&: ((f2 :&: (l1 :|: (e' :|: r0))) :&: s3))
bonsfsrr :: b e e' -> b (f4 :&: ((f2 :&: (l1 :|: (l0 :|: e ))) :&: s3)) (f4 :&: ((f2 :&: (l1 :|: (l0 :|: e'))) :&: s3))
bonsflff :: b e e' -> b (f4 :&: ((((e  :&: s0) :&: s1) :|: r2) :&: s3)) (f4 :&: ((((e' :&: s0) :&: s1) :|: r2) :&: s3))
bonsflfs :: b e e' -> b (f4 :&: ((((f0 :&: e ) :&: s1) :|: r2) :&: s3)) (f4 :&: ((((f0 :&: e') :&: s1) :|: r2) :&: s3))
bonsflfl :: b e e' -> b (f4 :&: ((((e  :|: r0) :&: s1) :|: r2) :&: s3)) (f4 :&: ((((e' :|: r0) :&: s1) :|: r2) :&: s3))
bonsflfr :: b e e' -> b (f4 :&: ((((l0 :|: e ) :&: s1) :|: r2) :&: s3)) (f4 :&: ((((l0 :|: e') :&: s1) :|: r2) :&: s3))
bonsflsf :: b e e' -> b (f4 :&: (((f1 :&: (e  :&: s0)) :|: r2) :&: s3)) (f4 :&: (((f1 :&: (e' :&: s0)) :|: r2) :&: s3))
bonsflss :: b e e' -> b (f4 :&: (((f1 :&: (f0 :&: e )) :|: r2) :&: s3)) (f4 :&: (((f1 :&: (f0 :&: e')) :|: r2) :&: s3))
bonsflsl :: b e e' -> b (f4 :&: (((f1 :&: (e  :|: r0)) :|: r2) :&: s3)) (f4 :&: (((f1 :&: (e' :|: r0)) :|: r2) :&: s3))
bonsflsr :: b e e' -> b (f4 :&: (((f1 :&: (l0 :|: e )) :|: r2) :&: s3)) (f4 :&: (((f1 :&: (l0 :|: e')) :|: r2) :&: s3))
bonsfllf :: b e e' -> b (f4 :&: ((((e  :&: s0) :|: r1) :|: r2) :&: s3)) (f4 :&: ((((e' :&: s0) :|: r1) :|: r2) :&: s3))
bonsflls :: b e e' -> b (f4 :&: ((((f0 :&: e ) :|: r1) :|: r2) :&: s3)) (f4 :&: ((((f0 :&: e') :|: r1) :|: r2) :&: s3))
bonsflll :: b e e' -> b (f4 :&: ((((e  :|: r0) :|: r1) :|: r2) :&: s3)) (f4 :&: ((((e' :|: r0) :|: r1) :|: r2) :&: s3))
bonsfllr :: b e e' -> b (f4 :&: ((((l0 :|: e ) :|: r1) :|: r2) :&: s3)) (f4 :&: ((((l0 :|: e') :|: r1) :|: r2) :&: s3))
bonsflrf :: b e e' -> b (f4 :&: (((l1 :|: (e  :&: s0)) :|: r2) :&: s3)) (f4 :&: (((l1 :|: (e' :&: s0)) :|: r2) :&: s3))
bonsflrs :: b e e' -> b (f4 :&: (((l1 :|: (f0 :&: e )) :|: r2) :&: s3)) (f4 :&: (((l1 :|: (f0 :&: e')) :|: r2) :&: s3))
bonsflrl :: b e e' -> b (f4 :&: (((l1 :|: (e  :|: r0)) :|: r2) :&: s3)) (f4 :&: (((l1 :|: (e' :|: r0)) :|: r2) :&: s3))
bonsflrr :: b e e' -> b (f4 :&: (((l1 :|: (l0 :|: e )) :|: r2) :&: s3)) (f4 :&: (((l1 :|: (l0 :|: e')) :|: r2) :&: s3))
bonsfrff :: b e e' -> b (f4 :&: ((l2 :|: ((e  :&: s0) :&: s1)) :&: s3)) (f4 :&: ((l2 :|: ((e' :&: s0) :&: s1)) :&: s3))
bonsfrfs :: b e e' -> b (f4 :&: ((l2 :|: ((f0 :&: e ) :&: s1)) :&: s3)) (f4 :&: ((l2 :|: ((f0 :&: e') :&: s1)) :&: s3))
bonsfrfl :: b e e' -> b (f4 :&: ((l2 :|: ((e  :|: r0) :&: s1)) :&: s3)) (f4 :&: ((l2 :|: ((e' :|: r0) :&: s1)) :&: s3))
bonsfrfr :: b e e' -> b (f4 :&: ((l2 :|: ((l0 :|: e ) :&: s1)) :&: s3)) (f4 :&: ((l2 :|: ((l0 :|: e') :&: s1)) :&: s3))
bonsfrsf :: b e e' -> b (f4 :&: ((l2 :|: (f1 :&: (e  :&: s0))) :&: s3)) (f4 :&: ((l2 :|: (f1 :&: (e' :&: s0))) :&: s3))
bonsfrss :: b e e' -> b (f4 :&: ((l2 :|: (f1 :&: (f0 :&: e ))) :&: s3)) (f4 :&: ((l2 :|: (f1 :&: (f0 :&: e'))) :&: s3))
bonsfrsl :: b e e' -> b (f4 :&: ((l2 :|: (f1 :&: (e  :|: r0))) :&: s3)) (f4 :&: ((l2 :|: (f1 :&: (e' :|: r0))) :&: s3))
bonsfrsr :: b e e' -> b (f4 :&: ((l2 :|: (f1 :&: (l0 :|: e ))) :&: s3)) (f4 :&: ((l2 :|: (f1 :&: (l0 :|: e'))) :&: s3))
bonsfrlf :: b e e' -> b (f4 :&: ((l2 :|: ((e  :&: s0) :|: r1)) :&: s3)) (f4 :&: ((l2 :|: ((e' :&: s0) :|: r1)) :&: s3))
bonsfrls :: b e e' -> b (f4 :&: ((l2 :|: ((f0 :&: e ) :|: r1)) :&: s3)) (f4 :&: ((l2 :|: ((f0 :&: e') :|: r1)) :&: s3))
bonsfrll :: b e e' -> b (f4 :&: ((l2 :|: ((e  :|: r0) :|: r1)) :&: s3)) (f4 :&: ((l2 :|: ((e' :|: r0) :|: r1)) :&: s3))
bonsfrlr :: b e e' -> b (f4 :&: ((l2 :|: ((l0 :|: e ) :|: r1)) :&: s3)) (f4 :&: ((l2 :|: ((l0 :|: e') :|: r1)) :&: s3))
bonsfrrf :: b e e' -> b (f4 :&: ((l2 :|: (l1 :|: (e  :&: s0))) :&: s3)) (f4 :&: ((l2 :|: (l1 :|: (e' :&: s0))) :&: s3))
bonsfrrs :: b e e' -> b (f4 :&: ((l2 :|: (l1 :|: (f0 :&: e ))) :&: s3)) (f4 :&: ((l2 :|: (l1 :|: (f0 :&: e'))) :&: s3))
bonsfrrl :: b e e' -> b (f4 :&: ((l2 :|: (l1 :|: (e  :|: r0))) :&: s3)) (f4 :&: ((l2 :|: (l1 :|: (e' :|: r0))) :&: s3))
bonsfrrr :: b e e' -> b (f4 :&: ((l2 :|: (l1 :|: (l0 :|: e ))) :&: s3)) (f4 :&: ((l2 :|: (l1 :|: (l0 :|: e'))) :&: s3))
bonssfff :: b e e' -> b (f4 :&: (f3 :&: (((e  :&: s0) :&: s1) :&: s2))) (f4 :&: (f3 :&: (((e' :&: s0) :&: s1) :&: s2)))
bonssffs :: b e e' -> b (f4 :&: (f3 :&: (((f0 :&: e ) :&: s1) :&: s2))) (f4 :&: (f3 :&: (((f0 :&: e') :&: s1) :&: s2)))
bonssffl :: b e e' -> b (f4 :&: (f3 :&: (((e  :|: r0) :&: s1) :&: s2))) (f4 :&: (f3 :&: (((e' :|: r0) :&: s1) :&: s2)))
bonssffr :: b e e' -> b (f4 :&: (f3 :&: (((l0 :|: e ) :&: s1) :&: s2))) (f4 :&: (f3 :&: (((l0 :|: e') :&: s1) :&: s2)))
bonssfsf :: b e e' -> b (f4 :&: (f3 :&: ((f1 :&: (e  :&: s0)) :&: s2))) (f4 :&: (f3 :&: ((f1 :&: (e' :&: s0)) :&: s2)))
bonssfss :: b e e' -> b (f4 :&: (f3 :&: ((f1 :&: (f0 :&: e )) :&: s2))) (f4 :&: (f3 :&: ((f1 :&: (f0 :&: e')) :&: s2)))
bonssfsl :: b e e' -> b (f4 :&: (f3 :&: ((f1 :&: (e  :|: r0)) :&: s2))) (f4 :&: (f3 :&: ((f1 :&: (e' :|: r0)) :&: s2)))
bonssfsr :: b e e' -> b (f4 :&: (f3 :&: ((f1 :&: (l0 :|: e )) :&: s2))) (f4 :&: (f3 :&: ((f1 :&: (l0 :|: e')) :&: s2)))
bonssflf :: b e e' -> b (f4 :&: (f3 :&: (((e  :&: s0) :|: r1) :&: s2))) (f4 :&: (f3 :&: (((e' :&: s0) :|: r1) :&: s2)))
bonssfls :: b e e' -> b (f4 :&: (f3 :&: (((f0 :&: e ) :|: r1) :&: s2))) (f4 :&: (f3 :&: (((f0 :&: e') :|: r1) :&: s2)))
bonssfll :: b e e' -> b (f4 :&: (f3 :&: (((e  :|: r0) :|: r1) :&: s2))) (f4 :&: (f3 :&: (((e' :|: r0) :|: r1) :&: s2)))
bonssflr :: b e e' -> b (f4 :&: (f3 :&: (((l0 :|: e ) :|: r1) :&: s2))) (f4 :&: (f3 :&: (((l0 :|: e') :|: r1) :&: s2)))
bonssfrf :: b e e' -> b (f4 :&: (f3 :&: ((l1 :|: (e  :&: s0)) :&: s2))) (f4 :&: (f3 :&: ((l1 :|: (e' :&: s0)) :&: s2)))
bonssfrs :: b e e' -> b (f4 :&: (f3 :&: ((l1 :|: (f0 :&: e )) :&: s2))) (f4 :&: (f3 :&: ((l1 :|: (f0 :&: e')) :&: s2)))
bonssfrl :: b e e' -> b (f4 :&: (f3 :&: ((l1 :|: (e  :|: r0)) :&: s2))) (f4 :&: (f3 :&: ((l1 :|: (e' :|: r0)) :&: s2)))
bonssfrr :: b e e' -> b (f4 :&: (f3 :&: ((l1 :|: (l0 :|: e )) :&: s2))) (f4 :&: (f3 :&: ((l1 :|: (l0 :|: e')) :&: s2)))
bonsssff :: b e e' -> b (f4 :&: (f3 :&: (f2 :&: ((e  :&: s0) :&: s1)))) (f4 :&: (f3 :&: (f2 :&: ((e' :&: s0) :&: s1))))
bonsssfs :: b e e' -> b (f4 :&: (f3 :&: (f2 :&: ((f0 :&: e ) :&: s1)))) (f4 :&: (f3 :&: (f2 :&: ((f0 :&: e') :&: s1))))
bonsssfl :: b e e' -> b (f4 :&: (f3 :&: (f2 :&: ((e  :|: r0) :&: s1)))) (f4 :&: (f3 :&: (f2 :&: ((e' :|: r0) :&: s1))))
bonsssfr :: b e e' -> b (f4 :&: (f3 :&: (f2 :&: ((l0 :|: e ) :&: s1)))) (f4 :&: (f3 :&: (f2 :&: ((l0 :|: e') :&: s1))))
bonssssf :: b e e' -> b (f4 :&: (f3 :&: (f2 :&: (f1 :&: (e  :&: s0))))) (f4 :&: (f3 :&: (f2 :&: (f1 :&: (e' :&: s0)))))
bonsssss :: b e e' -> b (f4 :&: (f3 :&: (f2 :&: (f1 :&: (f0 :&: e ))))) (f4 :&: (f3 :&: (f2 :&: (f1 :&: (f0 :&: e')))))
bonssssl :: b e e' -> b (f4 :&: (f3 :&: (f2 :&: (f1 :&: (e  :|: r0))))) (f4 :&: (f3 :&: (f2 :&: (f1 :&: (e' :|: r0)))))
bonssssr :: b e e' -> b (f4 :&: (f3 :&: (f2 :&: (f1 :&: (l0 :|: e ))))) (f4 :&: (f3 :&: (f2 :&: (f1 :&: (l0 :|: e')))))
bonssslf :: b e e' -> b (f4 :&: (f3 :&: (f2 :&: ((e  :&: s0) :|: r1)))) (f4 :&: (f3 :&: (f2 :&: ((e' :&: s0) :|: r1))))
bonsssls :: b e e' -> b (f4 :&: (f3 :&: (f2 :&: ((f0 :&: e ) :|: r1)))) (f4 :&: (f3 :&: (f2 :&: ((f0 :&: e') :|: r1))))
bonsssll :: b e e' -> b (f4 :&: (f3 :&: (f2 :&: ((e  :|: r0) :|: r1)))) (f4 :&: (f3 :&: (f2 :&: ((e' :|: r0) :|: r1))))
bonssslr :: b e e' -> b (f4 :&: (f3 :&: (f2 :&: ((l0 :|: e ) :|: r1)))) (f4 :&: (f3 :&: (f2 :&: ((l0 :|: e') :|: r1))))
bonsssrf :: b e e' -> b (f4 :&: (f3 :&: (f2 :&: (l1 :|: (e  :&: s0))))) (f4 :&: (f3 :&: (f2 :&: (l1 :|: (e' :&: s0)))))
bonsssrs :: b e e' -> b (f4 :&: (f3 :&: (f2 :&: (l1 :|: (f0 :&: e ))))) (f4 :&: (f3 :&: (f2 :&: (l1 :|: (f0 :&: e')))))
bonsssrl :: b e e' -> b (f4 :&: (f3 :&: (f2 :&: (l1 :|: (e  :|: r0))))) (f4 :&: (f3 :&: (f2 :&: (l1 :|: (e' :|: r0)))))
bonsssrr :: b e e' -> b (f4 :&: (f3 :&: (f2 :&: (l1 :|: (l0 :|: e ))))) (f4 :&: (f3 :&: (f2 :&: (l1 :|: (l0 :|: e')))))
bonsslff :: b e e' -> b (f4 :&: (f3 :&: (((e  :&: s0) :&: s1) :|: r2))) (f4 :&: (f3 :&: (((e' :&: s0) :&: s1) :|: r2)))
bonsslfs :: b e e' -> b (f4 :&: (f3 :&: (((f0 :&: e ) :&: s1) :|: r2))) (f4 :&: (f3 :&: (((f0 :&: e') :&: s1) :|: r2)))
bonsslfl :: b e e' -> b (f4 :&: (f3 :&: (((e  :|: r0) :&: s1) :|: r2))) (f4 :&: (f3 :&: (((e' :|: r0) :&: s1) :|: r2)))
bonsslfr :: b e e' -> b (f4 :&: (f3 :&: (((l0 :|: e ) :&: s1) :|: r2))) (f4 :&: (f3 :&: (((l0 :|: e') :&: s1) :|: r2)))
bonsslsf :: b e e' -> b (f4 :&: (f3 :&: ((f1 :&: (e  :&: s0)) :|: r2))) (f4 :&: (f3 :&: ((f1 :&: (e' :&: s0)) :|: r2)))
bonsslss :: b e e' -> b (f4 :&: (f3 :&: ((f1 :&: (f0 :&: e )) :|: r2))) (f4 :&: (f3 :&: ((f1 :&: (f0 :&: e')) :|: r2)))
bonsslsl :: b e e' -> b (f4 :&: (f3 :&: ((f1 :&: (e  :|: r0)) :|: r2))) (f4 :&: (f3 :&: ((f1 :&: (e' :|: r0)) :|: r2)))
bonsslsr :: b e e' -> b (f4 :&: (f3 :&: ((f1 :&: (l0 :|: e )) :|: r2))) (f4 :&: (f3 :&: ((f1 :&: (l0 :|: e')) :|: r2)))
bonssllf :: b e e' -> b (f4 :&: (f3 :&: (((e  :&: s0) :|: r1) :|: r2))) (f4 :&: (f3 :&: (((e' :&: s0) :|: r1) :|: r2)))
bonsslls :: b e e' -> b (f4 :&: (f3 :&: (((f0 :&: e ) :|: r1) :|: r2))) (f4 :&: (f3 :&: (((f0 :&: e') :|: r1) :|: r2)))
bonsslll :: b e e' -> b (f4 :&: (f3 :&: (((e  :|: r0) :|: r1) :|: r2))) (f4 :&: (f3 :&: (((e' :|: r0) :|: r1) :|: r2)))
bonssllr :: b e e' -> b (f4 :&: (f3 :&: (((l0 :|: e ) :|: r1) :|: r2))) (f4 :&: (f3 :&: (((l0 :|: e') :|: r1) :|: r2)))
bonsslrf :: b e e' -> b (f4 :&: (f3 :&: ((l1 :|: (e  :&: s0)) :|: r2))) (f4 :&: (f3 :&: ((l1 :|: (e' :&: s0)) :|: r2)))
bonsslrs :: b e e' -> b (f4 :&: (f3 :&: ((l1 :|: (f0 :&: e )) :|: r2))) (f4 :&: (f3 :&: ((l1 :|: (f0 :&: e')) :|: r2)))
bonsslrl :: b e e' -> b (f4 :&: (f3 :&: ((l1 :|: (e  :|: r0)) :|: r2))) (f4 :&: (f3 :&: ((l1 :|: (e' :|: r0)) :|: r2)))
bonsslrr :: b e e' -> b (f4 :&: (f3 :&: ((l1 :|: (l0 :|: e )) :|: r2))) (f4 :&: (f3 :&: ((l1 :|: (l0 :|: e')) :|: r2)))
bonssrff :: b e e' -> b (f4 :&: (f3 :&: (l2 :|: ((e  :&: s0) :&: s1)))) (f4 :&: (f3 :&: (l2 :|: ((e' :&: s0) :&: s1))))
bonssrfs :: b e e' -> b (f4 :&: (f3 :&: (l2 :|: ((f0 :&: e ) :&: s1)))) (f4 :&: (f3 :&: (l2 :|: ((f0 :&: e') :&: s1))))
bonssrfl :: b e e' -> b (f4 :&: (f3 :&: (l2 :|: ((e  :|: r0) :&: s1)))) (f4 :&: (f3 :&: (l2 :|: ((e' :|: r0) :&: s1))))
bonssrfr :: b e e' -> b (f4 :&: (f3 :&: (l2 :|: ((l0 :|: e ) :&: s1)))) (f4 :&: (f3 :&: (l2 :|: ((l0 :|: e') :&: s1))))
bonssrsf :: b e e' -> b (f4 :&: (f3 :&: (l2 :|: (f1 :&: (e  :&: s0))))) (f4 :&: (f3 :&: (l2 :|: (f1 :&: (e' :&: s0)))))
bonssrss :: b e e' -> b (f4 :&: (f3 :&: (l2 :|: (f1 :&: (f0 :&: e ))))) (f4 :&: (f3 :&: (l2 :|: (f1 :&: (f0 :&: e')))))
bonssrsl :: b e e' -> b (f4 :&: (f3 :&: (l2 :|: (f1 :&: (e  :|: r0))))) (f4 :&: (f3 :&: (l2 :|: (f1 :&: (e' :|: r0)))))
bonssrsr :: b e e' -> b (f4 :&: (f3 :&: (l2 :|: (f1 :&: (l0 :|: e ))))) (f4 :&: (f3 :&: (l2 :|: (f1 :&: (l0 :|: e')))))
bonssrlf :: b e e' -> b (f4 :&: (f3 :&: (l2 :|: ((e  :&: s0) :|: r1)))) (f4 :&: (f3 :&: (l2 :|: ((e' :&: s0) :|: r1))))
bonssrls :: b e e' -> b (f4 :&: (f3 :&: (l2 :|: ((f0 :&: e ) :|: r1)))) (f4 :&: (f3 :&: (l2 :|: ((f0 :&: e') :|: r1))))
bonssrll :: b e e' -> b (f4 :&: (f3 :&: (l2 :|: ((e  :|: r0) :|: r1)))) (f4 :&: (f3 :&: (l2 :|: ((e' :|: r0) :|: r1))))
bonssrlr :: b e e' -> b (f4 :&: (f3 :&: (l2 :|: ((l0 :|: e ) :|: r1)))) (f4 :&: (f3 :&: (l2 :|: ((l0 :|: e') :|: r1))))
bonssrrf :: b e e' -> b (f4 :&: (f3 :&: (l2 :|: (l1 :|: (e  :&: s0))))) (f4 :&: (f3 :&: (l2 :|: (l1 :|: (e' :&: s0)))))
bonssrrs :: b e e' -> b (f4 :&: (f3 :&: (l2 :|: (l1 :|: (f0 :&: e ))))) (f4 :&: (f3 :&: (l2 :|: (l1 :|: (f0 :&: e')))))
bonssrrl :: b e e' -> b (f4 :&: (f3 :&: (l2 :|: (l1 :|: (e  :|: r0))))) (f4 :&: (f3 :&: (l2 :|: (l1 :|: (e' :|: r0)))))
bonssrrr :: b e e' -> b (f4 :&: (f3 :&: (l2 :|: (l1 :|: (l0 :|: e ))))) (f4 :&: (f3 :&: (l2 :|: (l1 :|: (l0 :|: e')))))
bonslfff :: b e e' -> b (f4 :&: ((((e  :&: s0) :&: s1) :&: s2) :|: r3)) (f4 :&: ((((e' :&: s0) :&: s1) :&: s2) :|: r3))
bonslffs :: b e e' -> b (f4 :&: ((((f0 :&: e ) :&: s1) :&: s2) :|: r3)) (f4 :&: ((((f0 :&: e') :&: s1) :&: s2) :|: r3))
bonslffl :: b e e' -> b (f4 :&: ((((e  :|: r0) :&: s1) :&: s2) :|: r3)) (f4 :&: ((((e' :|: r0) :&: s1) :&: s2) :|: r3))
bonslffr :: b e e' -> b (f4 :&: ((((l0 :|: e ) :&: s1) :&: s2) :|: r3)) (f4 :&: ((((l0 :|: e') :&: s1) :&: s2) :|: r3))
bonslfsf :: b e e' -> b (f4 :&: (((f1 :&: (e  :&: s0)) :&: s2) :|: r3)) (f4 :&: (((f1 :&: (e' :&: s0)) :&: s2) :|: r3))
bonslfss :: b e e' -> b (f4 :&: (((f1 :&: (f0 :&: e )) :&: s2) :|: r3)) (f4 :&: (((f1 :&: (f0 :&: e')) :&: s2) :|: r3))
bonslfsl :: b e e' -> b (f4 :&: (((f1 :&: (e  :|: r0)) :&: s2) :|: r3)) (f4 :&: (((f1 :&: (e' :|: r0)) :&: s2) :|: r3))
bonslfsr :: b e e' -> b (f4 :&: (((f1 :&: (l0 :|: e )) :&: s2) :|: r3)) (f4 :&: (((f1 :&: (l0 :|: e')) :&: s2) :|: r3))
bonslflf :: b e e' -> b (f4 :&: ((((e  :&: s0) :|: r1) :&: s2) :|: r3)) (f4 :&: ((((e' :&: s0) :|: r1) :&: s2) :|: r3))
bonslfls :: b e e' -> b (f4 :&: ((((f0 :&: e ) :|: r1) :&: s2) :|: r3)) (f4 :&: ((((f0 :&: e') :|: r1) :&: s2) :|: r3))
bonslfll :: b e e' -> b (f4 :&: ((((e  :|: r0) :|: r1) :&: s2) :|: r3)) (f4 :&: ((((e' :|: r0) :|: r1) :&: s2) :|: r3))
bonslflr :: b e e' -> b (f4 :&: ((((l0 :|: e ) :|: r1) :&: s2) :|: r3)) (f4 :&: ((((l0 :|: e') :|: r1) :&: s2) :|: r3))
bonslfrf :: b e e' -> b (f4 :&: (((l1 :|: (e  :&: s0)) :&: s2) :|: r3)) (f4 :&: (((l1 :|: (e' :&: s0)) :&: s2) :|: r3))
bonslfrs :: b e e' -> b (f4 :&: (((l1 :|: (f0 :&: e )) :&: s2) :|: r3)) (f4 :&: (((l1 :|: (f0 :&: e')) :&: s2) :|: r3))
bonslfrl :: b e e' -> b (f4 :&: (((l1 :|: (e  :|: r0)) :&: s2) :|: r3)) (f4 :&: (((l1 :|: (e' :|: r0)) :&: s2) :|: r3))
bonslfrr :: b e e' -> b (f4 :&: (((l1 :|: (l0 :|: e )) :&: s2) :|: r3)) (f4 :&: (((l1 :|: (l0 :|: e')) :&: s2) :|: r3))
bonslsff :: b e e' -> b (f4 :&: ((f2 :&: ((e  :&: s0) :&: s1)) :|: r3)) (f4 :&: ((f2 :&: ((e' :&: s0) :&: s1)) :|: r3))
bonslsfs :: b e e' -> b (f4 :&: ((f2 :&: ((f0 :&: e ) :&: s1)) :|: r3)) (f4 :&: ((f2 :&: ((f0 :&: e') :&: s1)) :|: r3))
bonslsfl :: b e e' -> b (f4 :&: ((f2 :&: ((e  :|: r0) :&: s1)) :|: r3)) (f4 :&: ((f2 :&: ((e' :|: r0) :&: s1)) :|: r3))
bonslsfr :: b e e' -> b (f4 :&: ((f2 :&: ((l0 :|: e ) :&: s1)) :|: r3)) (f4 :&: ((f2 :&: ((l0 :|: e') :&: s1)) :|: r3))
bonslssf :: b e e' -> b (f4 :&: ((f2 :&: (f1 :&: (e  :&: s0))) :|: r3)) (f4 :&: ((f2 :&: (f1 :&: (e' :&: s0))) :|: r3))
bonslsss :: b e e' -> b (f4 :&: ((f2 :&: (f1 :&: (f0 :&: e ))) :|: r3)) (f4 :&: ((f2 :&: (f1 :&: (f0 :&: e'))) :|: r3))
bonslssl :: b e e' -> b (f4 :&: ((f2 :&: (f1 :&: (e  :|: r0))) :|: r3)) (f4 :&: ((f2 :&: (f1 :&: (e' :|: r0))) :|: r3))
bonslssr :: b e e' -> b (f4 :&: ((f2 :&: (f1 :&: (l0 :|: e ))) :|: r3)) (f4 :&: ((f2 :&: (f1 :&: (l0 :|: e'))) :|: r3))
bonslslf :: b e e' -> b (f4 :&: ((f2 :&: ((e  :&: s0) :|: r1)) :|: r3)) (f4 :&: ((f2 :&: ((e' :&: s0) :|: r1)) :|: r3))
bonslsls :: b e e' -> b (f4 :&: ((f2 :&: ((f0 :&: e ) :|: r1)) :|: r3)) (f4 :&: ((f2 :&: ((f0 :&: e') :|: r1)) :|: r3))
bonslsll :: b e e' -> b (f4 :&: ((f2 :&: ((e  :|: r0) :|: r1)) :|: r3)) (f4 :&: ((f2 :&: ((e' :|: r0) :|: r1)) :|: r3))
bonslslr :: b e e' -> b (f4 :&: ((f2 :&: ((l0 :|: e ) :|: r1)) :|: r3)) (f4 :&: ((f2 :&: ((l0 :|: e') :|: r1)) :|: r3))
bonslsrf :: b e e' -> b (f4 :&: ((f2 :&: (l1 :|: (e  :&: s0))) :|: r3)) (f4 :&: ((f2 :&: (l1 :|: (e' :&: s0))) :|: r3))
bonslsrs :: b e e' -> b (f4 :&: ((f2 :&: (l1 :|: (f0 :&: e ))) :|: r3)) (f4 :&: ((f2 :&: (l1 :|: (f0 :&: e'))) :|: r3))
bonslsrl :: b e e' -> b (f4 :&: ((f2 :&: (l1 :|: (e  :|: r0))) :|: r3)) (f4 :&: ((f2 :&: (l1 :|: (e' :|: r0))) :|: r3))
bonslsrr :: b e e' -> b (f4 :&: ((f2 :&: (l1 :|: (l0 :|: e ))) :|: r3)) (f4 :&: ((f2 :&: (l1 :|: (l0 :|: e'))) :|: r3))
bonsllff :: b e e' -> b (f4 :&: ((((e  :&: s0) :&: s1) :|: r2) :|: r3)) (f4 :&: ((((e' :&: s0) :&: s1) :|: r2) :|: r3))
bonsllfs :: b e e' -> b (f4 :&: ((((f0 :&: e ) :&: s1) :|: r2) :|: r3)) (f4 :&: ((((f0 :&: e') :&: s1) :|: r2) :|: r3))
bonsllfl :: b e e' -> b (f4 :&: ((((e  :|: r0) :&: s1) :|: r2) :|: r3)) (f4 :&: ((((e' :|: r0) :&: s1) :|: r2) :|: r3))
bonsllfr :: b e e' -> b (f4 :&: ((((l0 :|: e ) :&: s1) :|: r2) :|: r3)) (f4 :&: ((((l0 :|: e') :&: s1) :|: r2) :|: r3))
bonsllsf :: b e e' -> b (f4 :&: (((f1 :&: (e  :&: s0)) :|: r2) :|: r3)) (f4 :&: (((f1 :&: (e' :&: s0)) :|: r2) :|: r3))
bonsllss :: b e e' -> b (f4 :&: (((f1 :&: (f0 :&: e )) :|: r2) :|: r3)) (f4 :&: (((f1 :&: (f0 :&: e')) :|: r2) :|: r3))
bonsllsl :: b e e' -> b (f4 :&: (((f1 :&: (e  :|: r0)) :|: r2) :|: r3)) (f4 :&: (((f1 :&: (e' :|: r0)) :|: r2) :|: r3))
bonsllsr :: b e e' -> b (f4 :&: (((f1 :&: (l0 :|: e )) :|: r2) :|: r3)) (f4 :&: (((f1 :&: (l0 :|: e')) :|: r2) :|: r3))
bonslllf :: b e e' -> b (f4 :&: ((((e  :&: s0) :|: r1) :|: r2) :|: r3)) (f4 :&: ((((e' :&: s0) :|: r1) :|: r2) :|: r3))
bonsllls :: b e e' -> b (f4 :&: ((((f0 :&: e ) :|: r1) :|: r2) :|: r3)) (f4 :&: ((((f0 :&: e') :|: r1) :|: r2) :|: r3))
bonsllll :: b e e' -> b (f4 :&: ((((e  :|: r0) :|: r1) :|: r2) :|: r3)) (f4 :&: ((((e' :|: r0) :|: r1) :|: r2) :|: r3))
bonslllr :: b e e' -> b (f4 :&: ((((l0 :|: e ) :|: r1) :|: r2) :|: r3)) (f4 :&: ((((l0 :|: e') :|: r1) :|: r2) :|: r3))
bonsllrf :: b e e' -> b (f4 :&: (((l1 :|: (e  :&: s0)) :|: r2) :|: r3)) (f4 :&: (((l1 :|: (e' :&: s0)) :|: r2) :|: r3))
bonsllrs :: b e e' -> b (f4 :&: (((l1 :|: (f0 :&: e )) :|: r2) :|: r3)) (f4 :&: (((l1 :|: (f0 :&: e')) :|: r2) :|: r3))
bonsllrl :: b e e' -> b (f4 :&: (((l1 :|: (e  :|: r0)) :|: r2) :|: r3)) (f4 :&: (((l1 :|: (e' :|: r0)) :|: r2) :|: r3))
bonsllrr :: b e e' -> b (f4 :&: (((l1 :|: (l0 :|: e )) :|: r2) :|: r3)) (f4 :&: (((l1 :|: (l0 :|: e')) :|: r2) :|: r3))
bonslrff :: b e e' -> b (f4 :&: ((l2 :|: ((e  :&: s0) :&: s1)) :|: r3)) (f4 :&: ((l2 :|: ((e' :&: s0) :&: s1)) :|: r3))
bonslrfs :: b e e' -> b (f4 :&: ((l2 :|: ((f0 :&: e ) :&: s1)) :|: r3)) (f4 :&: ((l2 :|: ((f0 :&: e') :&: s1)) :|: r3))
bonslrfl :: b e e' -> b (f4 :&: ((l2 :|: ((e  :|: r0) :&: s1)) :|: r3)) (f4 :&: ((l2 :|: ((e' :|: r0) :&: s1)) :|: r3))
bonslrfr :: b e e' -> b (f4 :&: ((l2 :|: ((l0 :|: e ) :&: s1)) :|: r3)) (f4 :&: ((l2 :|: ((l0 :|: e') :&: s1)) :|: r3))
bonslrsf :: b e e' -> b (f4 :&: ((l2 :|: (f1 :&: (e  :&: s0))) :|: r3)) (f4 :&: ((l2 :|: (f1 :&: (e' :&: s0))) :|: r3))
bonslrss :: b e e' -> b (f4 :&: ((l2 :|: (f1 :&: (f0 :&: e ))) :|: r3)) (f4 :&: ((l2 :|: (f1 :&: (f0 :&: e'))) :|: r3))
bonslrsl :: b e e' -> b (f4 :&: ((l2 :|: (f1 :&: (e  :|: r0))) :|: r3)) (f4 :&: ((l2 :|: (f1 :&: (e' :|: r0))) :|: r3))
bonslrsr :: b e e' -> b (f4 :&: ((l2 :|: (f1 :&: (l0 :|: e ))) :|: r3)) (f4 :&: ((l2 :|: (f1 :&: (l0 :|: e'))) :|: r3))
bonslrlf :: b e e' -> b (f4 :&: ((l2 :|: ((e  :&: s0) :|: r1)) :|: r3)) (f4 :&: ((l2 :|: ((e' :&: s0) :|: r1)) :|: r3))
bonslrls :: b e e' -> b (f4 :&: ((l2 :|: ((f0 :&: e ) :|: r1)) :|: r3)) (f4 :&: ((l2 :|: ((f0 :&: e') :|: r1)) :|: r3))
bonslrll :: b e e' -> b (f4 :&: ((l2 :|: ((e  :|: r0) :|: r1)) :|: r3)) (f4 :&: ((l2 :|: ((e' :|: r0) :|: r1)) :|: r3))
bonslrlr :: b e e' -> b (f4 :&: ((l2 :|: ((l0 :|: e ) :|: r1)) :|: r3)) (f4 :&: ((l2 :|: ((l0 :|: e') :|: r1)) :|: r3))
bonslrrf :: b e e' -> b (f4 :&: ((l2 :|: (l1 :|: (e  :&: s0))) :|: r3)) (f4 :&: ((l2 :|: (l1 :|: (e' :&: s0))) :|: r3))
bonslrrs :: b e e' -> b (f4 :&: ((l2 :|: (l1 :|: (f0 :&: e ))) :|: r3)) (f4 :&: ((l2 :|: (l1 :|: (f0 :&: e'))) :|: r3))
bonslrrl :: b e e' -> b (f4 :&: ((l2 :|: (l1 :|: (e  :|: r0))) :|: r3)) (f4 :&: ((l2 :|: (l1 :|: (e' :|: r0))) :|: r3))
bonslrrr :: b e e' -> b (f4 :&: ((l2 :|: (l1 :|: (l0 :|: e ))) :|: r3)) (f4 :&: ((l2 :|: (l1 :|: (l0 :|: e'))) :|: r3))
bonsrfff :: b e e' -> b (f4 :&: (l3 :|: (((e  :&: s0) :&: s1) :&: s2))) (f4 :&: (l3 :|: (((e' :&: s0) :&: s1) :&: s2)))
bonsrffs :: b e e' -> b (f4 :&: (l3 :|: (((f0 :&: e ) :&: s1) :&: s2))) (f4 :&: (l3 :|: (((f0 :&: e') :&: s1) :&: s2)))
bonsrffl :: b e e' -> b (f4 :&: (l3 :|: (((e  :|: r0) :&: s1) :&: s2))) (f4 :&: (l3 :|: (((e' :|: r0) :&: s1) :&: s2)))
bonsrffr :: b e e' -> b (f4 :&: (l3 :|: (((l0 :|: e ) :&: s1) :&: s2))) (f4 :&: (l3 :|: (((l0 :|: e') :&: s1) :&: s2)))
bonsrfsf :: b e e' -> b (f4 :&: (l3 :|: ((f1 :&: (e  :&: s0)) :&: s2))) (f4 :&: (l3 :|: ((f1 :&: (e' :&: s0)) :&: s2)))
bonsrfss :: b e e' -> b (f4 :&: (l3 :|: ((f1 :&: (f0 :&: e )) :&: s2))) (f4 :&: (l3 :|: ((f1 :&: (f0 :&: e')) :&: s2)))
bonsrfsl :: b e e' -> b (f4 :&: (l3 :|: ((f1 :&: (e  :|: r0)) :&: s2))) (f4 :&: (l3 :|: ((f1 :&: (e' :|: r0)) :&: s2)))
bonsrfsr :: b e e' -> b (f4 :&: (l3 :|: ((f1 :&: (l0 :|: e )) :&: s2))) (f4 :&: (l3 :|: ((f1 :&: (l0 :|: e')) :&: s2)))
bonsrflf :: b e e' -> b (f4 :&: (l3 :|: (((e  :&: s0) :|: r1) :&: s2))) (f4 :&: (l3 :|: (((e' :&: s0) :|: r1) :&: s2)))
bonsrfls :: b e e' -> b (f4 :&: (l3 :|: (((f0 :&: e ) :|: r1) :&: s2))) (f4 :&: (l3 :|: (((f0 :&: e') :|: r1) :&: s2)))
bonsrfll :: b e e' -> b (f4 :&: (l3 :|: (((e  :|: r0) :|: r1) :&: s2))) (f4 :&: (l3 :|: (((e' :|: r0) :|: r1) :&: s2)))
bonsrflr :: b e e' -> b (f4 :&: (l3 :|: (((l0 :|: e ) :|: r1) :&: s2))) (f4 :&: (l3 :|: (((l0 :|: e') :|: r1) :&: s2)))
bonsrfrf :: b e e' -> b (f4 :&: (l3 :|: ((l1 :|: (e  :&: s0)) :&: s2))) (f4 :&: (l3 :|: ((l1 :|: (e' :&: s0)) :&: s2)))
bonsrfrs :: b e e' -> b (f4 :&: (l3 :|: ((l1 :|: (f0 :&: e )) :&: s2))) (f4 :&: (l3 :|: ((l1 :|: (f0 :&: e')) :&: s2)))
bonsrfrl :: b e e' -> b (f4 :&: (l3 :|: ((l1 :|: (e  :|: r0)) :&: s2))) (f4 :&: (l3 :|: ((l1 :|: (e' :|: r0)) :&: s2)))
bonsrfrr :: b e e' -> b (f4 :&: (l3 :|: ((l1 :|: (l0 :|: e )) :&: s2))) (f4 :&: (l3 :|: ((l1 :|: (l0 :|: e')) :&: s2)))
bonsrsff :: b e e' -> b (f4 :&: (l3 :|: (f2 :&: ((e  :&: s0) :&: s1)))) (f4 :&: (l3 :|: (f2 :&: ((e' :&: s0) :&: s1))))
bonsrsfs :: b e e' -> b (f4 :&: (l3 :|: (f2 :&: ((f0 :&: e ) :&: s1)))) (f4 :&: (l3 :|: (f2 :&: ((f0 :&: e') :&: s1))))
bonsrsfl :: b e e' -> b (f4 :&: (l3 :|: (f2 :&: ((e  :|: r0) :&: s1)))) (f4 :&: (l3 :|: (f2 :&: ((e' :|: r0) :&: s1))))
bonsrsfr :: b e e' -> b (f4 :&: (l3 :|: (f2 :&: ((l0 :|: e ) :&: s1)))) (f4 :&: (l3 :|: (f2 :&: ((l0 :|: e') :&: s1))))
bonsrssf :: b e e' -> b (f4 :&: (l3 :|: (f2 :&: (f1 :&: (e  :&: s0))))) (f4 :&: (l3 :|: (f2 :&: (f1 :&: (e' :&: s0)))))
bonsrsss :: b e e' -> b (f4 :&: (l3 :|: (f2 :&: (f1 :&: (f0 :&: e ))))) (f4 :&: (l3 :|: (f2 :&: (f1 :&: (f0 :&: e')))))
bonsrssl :: b e e' -> b (f4 :&: (l3 :|: (f2 :&: (f1 :&: (e  :|: r0))))) (f4 :&: (l3 :|: (f2 :&: (f1 :&: (e' :|: r0)))))
bonsrssr :: b e e' -> b (f4 :&: (l3 :|: (f2 :&: (f1 :&: (l0 :|: e ))))) (f4 :&: (l3 :|: (f2 :&: (f1 :&: (l0 :|: e')))))
bonsrslf :: b e e' -> b (f4 :&: (l3 :|: (f2 :&: ((e  :&: s0) :|: r1)))) (f4 :&: (l3 :|: (f2 :&: ((e' :&: s0) :|: r1))))
bonsrsls :: b e e' -> b (f4 :&: (l3 :|: (f2 :&: ((f0 :&: e ) :|: r1)))) (f4 :&: (l3 :|: (f2 :&: ((f0 :&: e') :|: r1))))
bonsrsll :: b e e' -> b (f4 :&: (l3 :|: (f2 :&: ((e  :|: r0) :|: r1)))) (f4 :&: (l3 :|: (f2 :&: ((e' :|: r0) :|: r1))))
bonsrslr :: b e e' -> b (f4 :&: (l3 :|: (f2 :&: ((l0 :|: e ) :|: r1)))) (f4 :&: (l3 :|: (f2 :&: ((l0 :|: e') :|: r1))))
bonsrsrf :: b e e' -> b (f4 :&: (l3 :|: (f2 :&: (l1 :|: (e  :&: s0))))) (f4 :&: (l3 :|: (f2 :&: (l1 :|: (e' :&: s0)))))
bonsrsrs :: b e e' -> b (f4 :&: (l3 :|: (f2 :&: (l1 :|: (f0 :&: e ))))) (f4 :&: (l3 :|: (f2 :&: (l1 :|: (f0 :&: e')))))
bonsrsrl :: b e e' -> b (f4 :&: (l3 :|: (f2 :&: (l1 :|: (e  :|: r0))))) (f4 :&: (l3 :|: (f2 :&: (l1 :|: (e' :|: r0)))))
bonsrsrr :: b e e' -> b (f4 :&: (l3 :|: (f2 :&: (l1 :|: (l0 :|: e ))))) (f4 :&: (l3 :|: (f2 :&: (l1 :|: (l0 :|: e')))))
bonsrlff :: b e e' -> b (f4 :&: (l3 :|: (((e  :&: s0) :&: s1) :|: r2))) (f4 :&: (l3 :|: (((e' :&: s0) :&: s1) :|: r2)))
bonsrlfs :: b e e' -> b (f4 :&: (l3 :|: (((f0 :&: e ) :&: s1) :|: r2))) (f4 :&: (l3 :|: (((f0 :&: e') :&: s1) :|: r2)))
bonsrlfl :: b e e' -> b (f4 :&: (l3 :|: (((e  :|: r0) :&: s1) :|: r2))) (f4 :&: (l3 :|: (((e' :|: r0) :&: s1) :|: r2)))
bonsrlfr :: b e e' -> b (f4 :&: (l3 :|: (((l0 :|: e ) :&: s1) :|: r2))) (f4 :&: (l3 :|: (((l0 :|: e') :&: s1) :|: r2)))
bonsrlsf :: b e e' -> b (f4 :&: (l3 :|: ((f1 :&: (e  :&: s0)) :|: r2))) (f4 :&: (l3 :|: ((f1 :&: (e' :&: s0)) :|: r2)))
bonsrlss :: b e e' -> b (f4 :&: (l3 :|: ((f1 :&: (f0 :&: e )) :|: r2))) (f4 :&: (l3 :|: ((f1 :&: (f0 :&: e')) :|: r2)))
bonsrlsl :: b e e' -> b (f4 :&: (l3 :|: ((f1 :&: (e  :|: r0)) :|: r2))) (f4 :&: (l3 :|: ((f1 :&: (e' :|: r0)) :|: r2)))
bonsrlsr :: b e e' -> b (f4 :&: (l3 :|: ((f1 :&: (l0 :|: e )) :|: r2))) (f4 :&: (l3 :|: ((f1 :&: (l0 :|: e')) :|: r2)))
bonsrllf :: b e e' -> b (f4 :&: (l3 :|: (((e  :&: s0) :|: r1) :|: r2))) (f4 :&: (l3 :|: (((e' :&: s0) :|: r1) :|: r2)))
bonsrlls :: b e e' -> b (f4 :&: (l3 :|: (((f0 :&: e ) :|: r1) :|: r2))) (f4 :&: (l3 :|: (((f0 :&: e') :|: r1) :|: r2)))
bonsrlll :: b e e' -> b (f4 :&: (l3 :|: (((e  :|: r0) :|: r1) :|: r2))) (f4 :&: (l3 :|: (((e' :|: r0) :|: r1) :|: r2)))
bonsrllr :: b e e' -> b (f4 :&: (l3 :|: (((l0 :|: e ) :|: r1) :|: r2))) (f4 :&: (l3 :|: (((l0 :|: e') :|: r1) :|: r2)))
bonsrlrf :: b e e' -> b (f4 :&: (l3 :|: ((l1 :|: (e  :&: s0)) :|: r2))) (f4 :&: (l3 :|: ((l1 :|: (e' :&: s0)) :|: r2)))
bonsrlrs :: b e e' -> b (f4 :&: (l3 :|: ((l1 :|: (f0 :&: e )) :|: r2))) (f4 :&: (l3 :|: ((l1 :|: (f0 :&: e')) :|: r2)))
bonsrlrl :: b e e' -> b (f4 :&: (l3 :|: ((l1 :|: (e  :|: r0)) :|: r2))) (f4 :&: (l3 :|: ((l1 :|: (e' :|: r0)) :|: r2)))
bonsrlrr :: b e e' -> b (f4 :&: (l3 :|: ((l1 :|: (l0 :|: e )) :|: r2))) (f4 :&: (l3 :|: ((l1 :|: (l0 :|: e')) :|: r2)))
bonsrrff :: b e e' -> b (f4 :&: (l3 :|: (l2 :|: ((e  :&: s0) :&: s1)))) (f4 :&: (l3 :|: (l2 :|: ((e' :&: s0) :&: s1))))
bonsrrfs :: b e e' -> b (f4 :&: (l3 :|: (l2 :|: ((f0 :&: e ) :&: s1)))) (f4 :&: (l3 :|: (l2 :|: ((f0 :&: e') :&: s1))))
bonsrrfl :: b e e' -> b (f4 :&: (l3 :|: (l2 :|: ((e  :|: r0) :&: s1)))) (f4 :&: (l3 :|: (l2 :|: ((e' :|: r0) :&: s1))))
bonsrrfr :: b e e' -> b (f4 :&: (l3 :|: (l2 :|: ((l0 :|: e ) :&: s1)))) (f4 :&: (l3 :|: (l2 :|: ((l0 :|: e') :&: s1))))
bonsrrsf :: b e e' -> b (f4 :&: (l3 :|: (l2 :|: (f1 :&: (e  :&: s0))))) (f4 :&: (l3 :|: (l2 :|: (f1 :&: (e' :&: s0)))))
bonsrrss :: b e e' -> b (f4 :&: (l3 :|: (l2 :|: (f1 :&: (f0 :&: e ))))) (f4 :&: (l3 :|: (l2 :|: (f1 :&: (f0 :&: e')))))
bonsrrsl :: b e e' -> b (f4 :&: (l3 :|: (l2 :|: (f1 :&: (e  :|: r0))))) (f4 :&: (l3 :|: (l2 :|: (f1 :&: (e' :|: r0)))))
bonsrrsr :: b e e' -> b (f4 :&: (l3 :|: (l2 :|: (f1 :&: (l0 :|: e ))))) (f4 :&: (l3 :|: (l2 :|: (f1 :&: (l0 :|: e')))))
bonsrrlf :: b e e' -> b (f4 :&: (l3 :|: (l2 :|: ((e  :&: s0) :|: r1)))) (f4 :&: (l3 :|: (l2 :|: ((e' :&: s0) :|: r1))))
bonsrrls :: b e e' -> b (f4 :&: (l3 :|: (l2 :|: ((f0 :&: e ) :|: r1)))) (f4 :&: (l3 :|: (l2 :|: ((f0 :&: e') :|: r1))))
bonsrrll :: b e e' -> b (f4 :&: (l3 :|: (l2 :|: ((e  :|: r0) :|: r1)))) (f4 :&: (l3 :|: (l2 :|: ((e' :|: r0) :|: r1))))
bonsrrlr :: b e e' -> b (f4 :&: (l3 :|: (l2 :|: ((l0 :|: e ) :|: r1)))) (f4 :&: (l3 :|: (l2 :|: ((l0 :|: e') :|: r1))))
bonsrrrf :: b e e' -> b (f4 :&: (l3 :|: (l2 :|: (l1 :|: (e  :&: s0))))) (f4 :&: (l3 :|: (l2 :|: (l1 :|: (e' :&: s0)))))
bonsrrrs :: b e e' -> b (f4 :&: (l3 :|: (l2 :|: (l1 :|: (f0 :&: e ))))) (f4 :&: (l3 :|: (l2 :|: (l1 :|: (f0 :&: e')))))
bonsrrrl :: b e e' -> b (f4 :&: (l3 :|: (l2 :|: (l1 :|: (e  :|: r0))))) (f4 :&: (l3 :|: (l2 :|: (l1 :|: (e' :|: r0)))))
bonsrrrr :: b e e' -> b (f4 :&: (l3 :|: (l2 :|: (l1 :|: (l0 :|: e ))))) (f4 :&: (l3 :|: (l2 :|: (l1 :|: (l0 :|: e')))))
bonlffff :: b e e' -> b (((((e  :&: s0) :&: s1) :&: s2) :&: s3) :|: r4) (((((e' :&: s0) :&: s1) :&: s2) :&: s3) :|: r4)
bonlfffs :: b e e' -> b (((((f0 :&: e ) :&: s1) :&: s2) :&: s3) :|: r4) (((((f0 :&: e') :&: s1) :&: s2) :&: s3) :|: r4)
bonlfffl :: b e e' -> b (((((e  :|: r0) :&: s1) :&: s2) :&: s3) :|: r4) (((((e' :|: r0) :&: s1) :&: s2) :&: s3) :|: r4)
bonlfffr :: b e e' -> b (((((l0 :|: e ) :&: s1) :&: s2) :&: s3) :|: r4) (((((l0 :|: e') :&: s1) :&: s2) :&: s3) :|: r4)
bonlffsf :: b e e' -> b ((((f1 :&: (e  :&: s0)) :&: s2) :&: s3) :|: r4) ((((f1 :&: (e' :&: s0)) :&: s2) :&: s3) :|: r4)
bonlffss :: b e e' -> b ((((f1 :&: (f0 :&: e )) :&: s2) :&: s3) :|: r4) ((((f1 :&: (f0 :&: e')) :&: s2) :&: s3) :|: r4)
bonlffsl :: b e e' -> b ((((f1 :&: (e  :|: r0)) :&: s2) :&: s3) :|: r4) ((((f1 :&: (e' :|: r0)) :&: s2) :&: s3) :|: r4)
bonlffsr :: b e e' -> b ((((f1 :&: (l0 :|: e )) :&: s2) :&: s3) :|: r4) ((((f1 :&: (l0 :|: e')) :&: s2) :&: s3) :|: r4)
bonlfflf :: b e e' -> b (((((e  :&: s0) :|: r1) :&: s2) :&: s3) :|: r4) (((((e' :&: s0) :|: r1) :&: s2) :&: s3) :|: r4)
bonlffls :: b e e' -> b (((((f0 :&: e ) :|: r1) :&: s2) :&: s3) :|: r4) (((((f0 :&: e') :|: r1) :&: s2) :&: s3) :|: r4)
bonlffll :: b e e' -> b (((((e  :|: r0) :|: r1) :&: s2) :&: s3) :|: r4) (((((e' :|: r0) :|: r1) :&: s2) :&: s3) :|: r4)
bonlfflr :: b e e' -> b (((((l0 :|: e ) :|: r1) :&: s2) :&: s3) :|: r4) (((((l0 :|: e') :|: r1) :&: s2) :&: s3) :|: r4)
bonlffrf :: b e e' -> b ((((l1 :|: (e  :&: s0)) :&: s2) :&: s3) :|: r4) ((((l1 :|: (e' :&: s0)) :&: s2) :&: s3) :|: r4)
bonlffrs :: b e e' -> b ((((l1 :|: (f0 :&: e )) :&: s2) :&: s3) :|: r4) ((((l1 :|: (f0 :&: e')) :&: s2) :&: s3) :|: r4)
bonlffrl :: b e e' -> b ((((l1 :|: (e  :|: r0)) :&: s2) :&: s3) :|: r4) ((((l1 :|: (e' :|: r0)) :&: s2) :&: s3) :|: r4)
bonlffrr :: b e e' -> b ((((l1 :|: (l0 :|: e )) :&: s2) :&: s3) :|: r4) ((((l1 :|: (l0 :|: e')) :&: s2) :&: s3) :|: r4)
bonlfsff :: b e e' -> b (((f2 :&: ((e  :&: s0) :&: s1)) :&: s3) :|: r4) (((f2 :&: ((e' :&: s0) :&: s1)) :&: s3) :|: r4)
bonlfsfs :: b e e' -> b (((f2 :&: ((f0 :&: e ) :&: s1)) :&: s3) :|: r4) (((f2 :&: ((f0 :&: e') :&: s1)) :&: s3) :|: r4)
bonlfsfl :: b e e' -> b (((f2 :&: ((e  :|: r0) :&: s1)) :&: s3) :|: r4) (((f2 :&: ((e' :|: r0) :&: s1)) :&: s3) :|: r4)
bonlfsfr :: b e e' -> b (((f2 :&: ((l0 :|: e ) :&: s1)) :&: s3) :|: r4) (((f2 :&: ((l0 :|: e') :&: s1)) :&: s3) :|: r4)
bonlfssf :: b e e' -> b (((f2 :&: (f1 :&: (e  :&: s0))) :&: s3) :|: r4) (((f2 :&: (f1 :&: (e' :&: s0))) :&: s3) :|: r4)
bonlfsss :: b e e' -> b (((f2 :&: (f1 :&: (f0 :&: e ))) :&: s3) :|: r4) (((f2 :&: (f1 :&: (f0 :&: e'))) :&: s3) :|: r4)
bonlfssl :: b e e' -> b (((f2 :&: (f1 :&: (e  :|: r0))) :&: s3) :|: r4) (((f2 :&: (f1 :&: (e' :|: r0))) :&: s3) :|: r4)
bonlfssr :: b e e' -> b (((f2 :&: (f1 :&: (l0 :|: e ))) :&: s3) :|: r4) (((f2 :&: (f1 :&: (l0 :|: e'))) :&: s3) :|: r4)
bonlfslf :: b e e' -> b (((f2 :&: ((e  :&: s0) :|: r1)) :&: s3) :|: r4) (((f2 :&: ((e' :&: s0) :|: r1)) :&: s3) :|: r4)
bonlfsls :: b e e' -> b (((f2 :&: ((f0 :&: e ) :|: r1)) :&: s3) :|: r4) (((f2 :&: ((f0 :&: e') :|: r1)) :&: s3) :|: r4)
bonlfsll :: b e e' -> b (((f2 :&: ((e  :|: r0) :|: r1)) :&: s3) :|: r4) (((f2 :&: ((e' :|: r0) :|: r1)) :&: s3) :|: r4)
bonlfslr :: b e e' -> b (((f2 :&: ((l0 :|: e ) :|: r1)) :&: s3) :|: r4) (((f2 :&: ((l0 :|: e') :|: r1)) :&: s3) :|: r4)
bonlfsrf :: b e e' -> b (((f2 :&: (l1 :|: (e  :&: s0))) :&: s3) :|: r4) (((f2 :&: (l1 :|: (e' :&: s0))) :&: s3) :|: r4)
bonlfsrs :: b e e' -> b (((f2 :&: (l1 :|: (f0 :&: e ))) :&: s3) :|: r4) (((f2 :&: (l1 :|: (f0 :&: e'))) :&: s3) :|: r4)
bonlfsrl :: b e e' -> b (((f2 :&: (l1 :|: (e  :|: r0))) :&: s3) :|: r4) (((f2 :&: (l1 :|: (e' :|: r0))) :&: s3) :|: r4)
bonlfsrr :: b e e' -> b (((f2 :&: (l1 :|: (l0 :|: e ))) :&: s3) :|: r4) (((f2 :&: (l1 :|: (l0 :|: e'))) :&: s3) :|: r4)
bonlflff :: b e e' -> b (((((e  :&: s0) :&: s1) :|: r2) :&: s3) :|: r4) (((((e' :&: s0) :&: s1) :|: r2) :&: s3) :|: r4)
bonlflfs :: b e e' -> b (((((f0 :&: e ) :&: s1) :|: r2) :&: s3) :|: r4) (((((f0 :&: e') :&: s1) :|: r2) :&: s3) :|: r4)
bonlflfl :: b e e' -> b (((((e  :|: r0) :&: s1) :|: r2) :&: s3) :|: r4) (((((e' :|: r0) :&: s1) :|: r2) :&: s3) :|: r4)
bonlflfr :: b e e' -> b (((((l0 :|: e ) :&: s1) :|: r2) :&: s3) :|: r4) (((((l0 :|: e') :&: s1) :|: r2) :&: s3) :|: r4)
bonlflsf :: b e e' -> b ((((f1 :&: (e  :&: s0)) :|: r2) :&: s3) :|: r4) ((((f1 :&: (e' :&: s0)) :|: r2) :&: s3) :|: r4)
bonlflss :: b e e' -> b ((((f1 :&: (f0 :&: e )) :|: r2) :&: s3) :|: r4) ((((f1 :&: (f0 :&: e')) :|: r2) :&: s3) :|: r4)
bonlflsl :: b e e' -> b ((((f1 :&: (e  :|: r0)) :|: r2) :&: s3) :|: r4) ((((f1 :&: (e' :|: r0)) :|: r2) :&: s3) :|: r4)
bonlflsr :: b e e' -> b ((((f1 :&: (l0 :|: e )) :|: r2) :&: s3) :|: r4) ((((f1 :&: (l0 :|: e')) :|: r2) :&: s3) :|: r4)
bonlfllf :: b e e' -> b (((((e  :&: s0) :|: r1) :|: r2) :&: s3) :|: r4) (((((e' :&: s0) :|: r1) :|: r2) :&: s3) :|: r4)
bonlflls :: b e e' -> b (((((f0 :&: e ) :|: r1) :|: r2) :&: s3) :|: r4) (((((f0 :&: e') :|: r1) :|: r2) :&: s3) :|: r4)
bonlflll :: b e e' -> b (((((e  :|: r0) :|: r1) :|: r2) :&: s3) :|: r4) (((((e' :|: r0) :|: r1) :|: r2) :&: s3) :|: r4)
bonlfllr :: b e e' -> b (((((l0 :|: e ) :|: r1) :|: r2) :&: s3) :|: r4) (((((l0 :|: e') :|: r1) :|: r2) :&: s3) :|: r4)
bonlflrf :: b e e' -> b ((((l1 :|: (e  :&: s0)) :|: r2) :&: s3) :|: r4) ((((l1 :|: (e' :&: s0)) :|: r2) :&: s3) :|: r4)
bonlflrs :: b e e' -> b ((((l1 :|: (f0 :&: e )) :|: r2) :&: s3) :|: r4) ((((l1 :|: (f0 :&: e')) :|: r2) :&: s3) :|: r4)
bonlflrl :: b e e' -> b ((((l1 :|: (e  :|: r0)) :|: r2) :&: s3) :|: r4) ((((l1 :|: (e' :|: r0)) :|: r2) :&: s3) :|: r4)
bonlflrr :: b e e' -> b ((((l1 :|: (l0 :|: e )) :|: r2) :&: s3) :|: r4) ((((l1 :|: (l0 :|: e')) :|: r2) :&: s3) :|: r4)
bonlfrff :: b e e' -> b (((l2 :|: ((e  :&: s0) :&: s1)) :&: s3) :|: r4) (((l2 :|: ((e' :&: s0) :&: s1)) :&: s3) :|: r4)
bonlfrfs :: b e e' -> b (((l2 :|: ((f0 :&: e ) :&: s1)) :&: s3) :|: r4) (((l2 :|: ((f0 :&: e') :&: s1)) :&: s3) :|: r4)
bonlfrfl :: b e e' -> b (((l2 :|: ((e  :|: r0) :&: s1)) :&: s3) :|: r4) (((l2 :|: ((e' :|: r0) :&: s1)) :&: s3) :|: r4)
bonlfrfr :: b e e' -> b (((l2 :|: ((l0 :|: e ) :&: s1)) :&: s3) :|: r4) (((l2 :|: ((l0 :|: e') :&: s1)) :&: s3) :|: r4)
bonlfrsf :: b e e' -> b (((l2 :|: (f1 :&: (e  :&: s0))) :&: s3) :|: r4) (((l2 :|: (f1 :&: (e' :&: s0))) :&: s3) :|: r4)
bonlfrss :: b e e' -> b (((l2 :|: (f1 :&: (f0 :&: e ))) :&: s3) :|: r4) (((l2 :|: (f1 :&: (f0 :&: e'))) :&: s3) :|: r4)
bonlfrsl :: b e e' -> b (((l2 :|: (f1 :&: (e  :|: r0))) :&: s3) :|: r4) (((l2 :|: (f1 :&: (e' :|: r0))) :&: s3) :|: r4)
bonlfrsr :: b e e' -> b (((l2 :|: (f1 :&: (l0 :|: e ))) :&: s3) :|: r4) (((l2 :|: (f1 :&: (l0 :|: e'))) :&: s3) :|: r4)
bonlfrlf :: b e e' -> b (((l2 :|: ((e  :&: s0) :|: r1)) :&: s3) :|: r4) (((l2 :|: ((e' :&: s0) :|: r1)) :&: s3) :|: r4)
bonlfrls :: b e e' -> b (((l2 :|: ((f0 :&: e ) :|: r1)) :&: s3) :|: r4) (((l2 :|: ((f0 :&: e') :|: r1)) :&: s3) :|: r4)
bonlfrll :: b e e' -> b (((l2 :|: ((e  :|: r0) :|: r1)) :&: s3) :|: r4) (((l2 :|: ((e' :|: r0) :|: r1)) :&: s3) :|: r4)
bonlfrlr :: b e e' -> b (((l2 :|: ((l0 :|: e ) :|: r1)) :&: s3) :|: r4) (((l2 :|: ((l0 :|: e') :|: r1)) :&: s3) :|: r4)
bonlfrrf :: b e e' -> b (((l2 :|: (l1 :|: (e  :&: s0))) :&: s3) :|: r4) (((l2 :|: (l1 :|: (e' :&: s0))) :&: s3) :|: r4)
bonlfrrs :: b e e' -> b (((l2 :|: (l1 :|: (f0 :&: e ))) :&: s3) :|: r4) (((l2 :|: (l1 :|: (f0 :&: e'))) :&: s3) :|: r4)
bonlfrrl :: b e e' -> b (((l2 :|: (l1 :|: (e  :|: r0))) :&: s3) :|: r4) (((l2 :|: (l1 :|: (e' :|: r0))) :&: s3) :|: r4)
bonlfrrr :: b e e' -> b (((l2 :|: (l1 :|: (l0 :|: e ))) :&: s3) :|: r4) (((l2 :|: (l1 :|: (l0 :|: e'))) :&: s3) :|: r4)
bonlsfff :: b e e' -> b ((f3 :&: (((e  :&: s0) :&: s1) :&: s2)) :|: r4) ((f3 :&: (((e' :&: s0) :&: s1) :&: s2)) :|: r4)
bonlsffs :: b e e' -> b ((f3 :&: (((f0 :&: e ) :&: s1) :&: s2)) :|: r4) ((f3 :&: (((f0 :&: e') :&: s1) :&: s2)) :|: r4)
bonlsffl :: b e e' -> b ((f3 :&: (((e  :|: r0) :&: s1) :&: s2)) :|: r4) ((f3 :&: (((e' :|: r0) :&: s1) :&: s2)) :|: r4)
bonlsffr :: b e e' -> b ((f3 :&: (((l0 :|: e ) :&: s1) :&: s2)) :|: r4) ((f3 :&: (((l0 :|: e') :&: s1) :&: s2)) :|: r4)
bonlsfsf :: b e e' -> b ((f3 :&: ((f1 :&: (e  :&: s0)) :&: s2)) :|: r4) ((f3 :&: ((f1 :&: (e' :&: s0)) :&: s2)) :|: r4)
bonlsfss :: b e e' -> b ((f3 :&: ((f1 :&: (f0 :&: e )) :&: s2)) :|: r4) ((f3 :&: ((f1 :&: (f0 :&: e')) :&: s2)) :|: r4)
bonlsfsl :: b e e' -> b ((f3 :&: ((f1 :&: (e  :|: r0)) :&: s2)) :|: r4) ((f3 :&: ((f1 :&: (e' :|: r0)) :&: s2)) :|: r4)
bonlsfsr :: b e e' -> b ((f3 :&: ((f1 :&: (l0 :|: e )) :&: s2)) :|: r4) ((f3 :&: ((f1 :&: (l0 :|: e')) :&: s2)) :|: r4)
bonlsflf :: b e e' -> b ((f3 :&: (((e  :&: s0) :|: r1) :&: s2)) :|: r4) ((f3 :&: (((e' :&: s0) :|: r1) :&: s2)) :|: r4)
bonlsfls :: b e e' -> b ((f3 :&: (((f0 :&: e ) :|: r1) :&: s2)) :|: r4) ((f3 :&: (((f0 :&: e') :|: r1) :&: s2)) :|: r4)
bonlsfll :: b e e' -> b ((f3 :&: (((e  :|: r0) :|: r1) :&: s2)) :|: r4) ((f3 :&: (((e' :|: r0) :|: r1) :&: s2)) :|: r4)
bonlsflr :: b e e' -> b ((f3 :&: (((l0 :|: e ) :|: r1) :&: s2)) :|: r4) ((f3 :&: (((l0 :|: e') :|: r1) :&: s2)) :|: r4)
bonlsfrf :: b e e' -> b ((f3 :&: ((l1 :|: (e  :&: s0)) :&: s2)) :|: r4) ((f3 :&: ((l1 :|: (e' :&: s0)) :&: s2)) :|: r4)
bonlsfrs :: b e e' -> b ((f3 :&: ((l1 :|: (f0 :&: e )) :&: s2)) :|: r4) ((f3 :&: ((l1 :|: (f0 :&: e')) :&: s2)) :|: r4)
bonlsfrl :: b e e' -> b ((f3 :&: ((l1 :|: (e  :|: r0)) :&: s2)) :|: r4) ((f3 :&: ((l1 :|: (e' :|: r0)) :&: s2)) :|: r4)
bonlsfrr :: b e e' -> b ((f3 :&: ((l1 :|: (l0 :|: e )) :&: s2)) :|: r4) ((f3 :&: ((l1 :|: (l0 :|: e')) :&: s2)) :|: r4)
bonlssff :: b e e' -> b ((f3 :&: (f2 :&: ((e  :&: s0) :&: s1))) :|: r4) ((f3 :&: (f2 :&: ((e' :&: s0) :&: s1))) :|: r4)
bonlssfs :: b e e' -> b ((f3 :&: (f2 :&: ((f0 :&: e ) :&: s1))) :|: r4) ((f3 :&: (f2 :&: ((f0 :&: e') :&: s1))) :|: r4)
bonlssfl :: b e e' -> b ((f3 :&: (f2 :&: ((e  :|: r0) :&: s1))) :|: r4) ((f3 :&: (f2 :&: ((e' :|: r0) :&: s1))) :|: r4)
bonlssfr :: b e e' -> b ((f3 :&: (f2 :&: ((l0 :|: e ) :&: s1))) :|: r4) ((f3 :&: (f2 :&: ((l0 :|: e') :&: s1))) :|: r4)
bonlsssf :: b e e' -> b ((f3 :&: (f2 :&: (f1 :&: (e  :&: s0)))) :|: r4) ((f3 :&: (f2 :&: (f1 :&: (e' :&: s0)))) :|: r4)
bonlssss :: b e e' -> b ((f3 :&: (f2 :&: (f1 :&: (f0 :&: e )))) :|: r4) ((f3 :&: (f2 :&: (f1 :&: (f0 :&: e')))) :|: r4)
bonlsssl :: b e e' -> b ((f3 :&: (f2 :&: (f1 :&: (e  :|: r0)))) :|: r4) ((f3 :&: (f2 :&: (f1 :&: (e' :|: r0)))) :|: r4)
bonlsssr :: b e e' -> b ((f3 :&: (f2 :&: (f1 :&: (l0 :|: e )))) :|: r4) ((f3 :&: (f2 :&: (f1 :&: (l0 :|: e')))) :|: r4)
bonlsslf :: b e e' -> b ((f3 :&: (f2 :&: ((e  :&: s0) :|: r1))) :|: r4) ((f3 :&: (f2 :&: ((e' :&: s0) :|: r1))) :|: r4)
bonlssls :: b e e' -> b ((f3 :&: (f2 :&: ((f0 :&: e ) :|: r1))) :|: r4) ((f3 :&: (f2 :&: ((f0 :&: e') :|: r1))) :|: r4)
bonlssll :: b e e' -> b ((f3 :&: (f2 :&: ((e  :|: r0) :|: r1))) :|: r4) ((f3 :&: (f2 :&: ((e' :|: r0) :|: r1))) :|: r4)
bonlsslr :: b e e' -> b ((f3 :&: (f2 :&: ((l0 :|: e ) :|: r1))) :|: r4) ((f3 :&: (f2 :&: ((l0 :|: e') :|: r1))) :|: r4)
bonlssrf :: b e e' -> b ((f3 :&: (f2 :&: (l1 :|: (e  :&: s0)))) :|: r4) ((f3 :&: (f2 :&: (l1 :|: (e' :&: s0)))) :|: r4)
bonlssrs :: b e e' -> b ((f3 :&: (f2 :&: (l1 :|: (f0 :&: e )))) :|: r4) ((f3 :&: (f2 :&: (l1 :|: (f0 :&: e')))) :|: r4)
bonlssrl :: b e e' -> b ((f3 :&: (f2 :&: (l1 :|: (e  :|: r0)))) :|: r4) ((f3 :&: (f2 :&: (l1 :|: (e' :|: r0)))) :|: r4)
bonlssrr :: b e e' -> b ((f3 :&: (f2 :&: (l1 :|: (l0 :|: e )))) :|: r4) ((f3 :&: (f2 :&: (l1 :|: (l0 :|: e')))) :|: r4)
bonlslff :: b e e' -> b ((f3 :&: (((e  :&: s0) :&: s1) :|: r2)) :|: r4) ((f3 :&: (((e' :&: s0) :&: s1) :|: r2)) :|: r4)
bonlslfs :: b e e' -> b ((f3 :&: (((f0 :&: e ) :&: s1) :|: r2)) :|: r4) ((f3 :&: (((f0 :&: e') :&: s1) :|: r2)) :|: r4)
bonlslfl :: b e e' -> b ((f3 :&: (((e  :|: r0) :&: s1) :|: r2)) :|: r4) ((f3 :&: (((e' :|: r0) :&: s1) :|: r2)) :|: r4)
bonlslfr :: b e e' -> b ((f3 :&: (((l0 :|: e ) :&: s1) :|: r2)) :|: r4) ((f3 :&: (((l0 :|: e') :&: s1) :|: r2)) :|: r4)
bonlslsf :: b e e' -> b ((f3 :&: ((f1 :&: (e  :&: s0)) :|: r2)) :|: r4) ((f3 :&: ((f1 :&: (e' :&: s0)) :|: r2)) :|: r4)
bonlslss :: b e e' -> b ((f3 :&: ((f1 :&: (f0 :&: e )) :|: r2)) :|: r4) ((f3 :&: ((f1 :&: (f0 :&: e')) :|: r2)) :|: r4)
bonlslsl :: b e e' -> b ((f3 :&: ((f1 :&: (e  :|: r0)) :|: r2)) :|: r4) ((f3 :&: ((f1 :&: (e' :|: r0)) :|: r2)) :|: r4)
bonlslsr :: b e e' -> b ((f3 :&: ((f1 :&: (l0 :|: e )) :|: r2)) :|: r4) ((f3 :&: ((f1 :&: (l0 :|: e')) :|: r2)) :|: r4)
bonlsllf :: b e e' -> b ((f3 :&: (((e  :&: s0) :|: r1) :|: r2)) :|: r4) ((f3 :&: (((e' :&: s0) :|: r1) :|: r2)) :|: r4)
bonlslls :: b e e' -> b ((f3 :&: (((f0 :&: e ) :|: r1) :|: r2)) :|: r4) ((f3 :&: (((f0 :&: e') :|: r1) :|: r2)) :|: r4)
bonlslll :: b e e' -> b ((f3 :&: (((e  :|: r0) :|: r1) :|: r2)) :|: r4) ((f3 :&: (((e' :|: r0) :|: r1) :|: r2)) :|: r4)
bonlsllr :: b e e' -> b ((f3 :&: (((l0 :|: e ) :|: r1) :|: r2)) :|: r4) ((f3 :&: (((l0 :|: e') :|: r1) :|: r2)) :|: r4)
bonlslrf :: b e e' -> b ((f3 :&: ((l1 :|: (e  :&: s0)) :|: r2)) :|: r4) ((f3 :&: ((l1 :|: (e' :&: s0)) :|: r2)) :|: r4)
bonlslrs :: b e e' -> b ((f3 :&: ((l1 :|: (f0 :&: e )) :|: r2)) :|: r4) ((f3 :&: ((l1 :|: (f0 :&: e')) :|: r2)) :|: r4)
bonlslrl :: b e e' -> b ((f3 :&: ((l1 :|: (e  :|: r0)) :|: r2)) :|: r4) ((f3 :&: ((l1 :|: (e' :|: r0)) :|: r2)) :|: r4)
bonlslrr :: b e e' -> b ((f3 :&: ((l1 :|: (l0 :|: e )) :|: r2)) :|: r4) ((f3 :&: ((l1 :|: (l0 :|: e')) :|: r2)) :|: r4)
bonlsrff :: b e e' -> b ((f3 :&: (l2 :|: ((e  :&: s0) :&: s1))) :|: r4) ((f3 :&: (l2 :|: ((e' :&: s0) :&: s1))) :|: r4)
bonlsrfs :: b e e' -> b ((f3 :&: (l2 :|: ((f0 :&: e ) :&: s1))) :|: r4) ((f3 :&: (l2 :|: ((f0 :&: e') :&: s1))) :|: r4)
bonlsrfl :: b e e' -> b ((f3 :&: (l2 :|: ((e  :|: r0) :&: s1))) :|: r4) ((f3 :&: (l2 :|: ((e' :|: r0) :&: s1))) :|: r4)
bonlsrfr :: b e e' -> b ((f3 :&: (l2 :|: ((l0 :|: e ) :&: s1))) :|: r4) ((f3 :&: (l2 :|: ((l0 :|: e') :&: s1))) :|: r4)
bonlsrsf :: b e e' -> b ((f3 :&: (l2 :|: (f1 :&: (e  :&: s0)))) :|: r4) ((f3 :&: (l2 :|: (f1 :&: (e' :&: s0)))) :|: r4)
bonlsrss :: b e e' -> b ((f3 :&: (l2 :|: (f1 :&: (f0 :&: e )))) :|: r4) ((f3 :&: (l2 :|: (f1 :&: (f0 :&: e')))) :|: r4)
bonlsrsl :: b e e' -> b ((f3 :&: (l2 :|: (f1 :&: (e  :|: r0)))) :|: r4) ((f3 :&: (l2 :|: (f1 :&: (e' :|: r0)))) :|: r4)
bonlsrsr :: b e e' -> b ((f3 :&: (l2 :|: (f1 :&: (l0 :|: e )))) :|: r4) ((f3 :&: (l2 :|: (f1 :&: (l0 :|: e')))) :|: r4)
bonlsrlf :: b e e' -> b ((f3 :&: (l2 :|: ((e  :&: s0) :|: r1))) :|: r4) ((f3 :&: (l2 :|: ((e' :&: s0) :|: r1))) :|: r4)
bonlsrls :: b e e' -> b ((f3 :&: (l2 :|: ((f0 :&: e ) :|: r1))) :|: r4) ((f3 :&: (l2 :|: ((f0 :&: e') :|: r1))) :|: r4)
bonlsrll :: b e e' -> b ((f3 :&: (l2 :|: ((e  :|: r0) :|: r1))) :|: r4) ((f3 :&: (l2 :|: ((e' :|: r0) :|: r1))) :|: r4)
bonlsrlr :: b e e' -> b ((f3 :&: (l2 :|: ((l0 :|: e ) :|: r1))) :|: r4) ((f3 :&: (l2 :|: ((l0 :|: e') :|: r1))) :|: r4)
bonlsrrf :: b e e' -> b ((f3 :&: (l2 :|: (l1 :|: (e  :&: s0)))) :|: r4) ((f3 :&: (l2 :|: (l1 :|: (e' :&: s0)))) :|: r4)
bonlsrrs :: b e e' -> b ((f3 :&: (l2 :|: (l1 :|: (f0 :&: e )))) :|: r4) ((f3 :&: (l2 :|: (l1 :|: (f0 :&: e')))) :|: r4)
bonlsrrl :: b e e' -> b ((f3 :&: (l2 :|: (l1 :|: (e  :|: r0)))) :|: r4) ((f3 :&: (l2 :|: (l1 :|: (e' :|: r0)))) :|: r4)
bonlsrrr :: b e e' -> b ((f3 :&: (l2 :|: (l1 :|: (l0 :|: e )))) :|: r4) ((f3 :&: (l2 :|: (l1 :|: (l0 :|: e')))) :|: r4)
bonllfff :: b e e' -> b (((((e  :&: s0) :&: s1) :&: s2) :|: r3) :|: r4) (((((e' :&: s0) :&: s1) :&: s2) :|: r3) :|: r4)
bonllffs :: b e e' -> b (((((f0 :&: e ) :&: s1) :&: s2) :|: r3) :|: r4) (((((f0 :&: e') :&: s1) :&: s2) :|: r3) :|: r4)
bonllffl :: b e e' -> b (((((e  :|: r0) :&: s1) :&: s2) :|: r3) :|: r4) (((((e' :|: r0) :&: s1) :&: s2) :|: r3) :|: r4)
bonllffr :: b e e' -> b (((((l0 :|: e ) :&: s1) :&: s2) :|: r3) :|: r4) (((((l0 :|: e') :&: s1) :&: s2) :|: r3) :|: r4)
bonllfsf :: b e e' -> b ((((f1 :&: (e  :&: s0)) :&: s2) :|: r3) :|: r4) ((((f1 :&: (e' :&: s0)) :&: s2) :|: r3) :|: r4)
bonllfss :: b e e' -> b ((((f1 :&: (f0 :&: e )) :&: s2) :|: r3) :|: r4) ((((f1 :&: (f0 :&: e')) :&: s2) :|: r3) :|: r4)
bonllfsl :: b e e' -> b ((((f1 :&: (e  :|: r0)) :&: s2) :|: r3) :|: r4) ((((f1 :&: (e' :|: r0)) :&: s2) :|: r3) :|: r4)
bonllfsr :: b e e' -> b ((((f1 :&: (l0 :|: e )) :&: s2) :|: r3) :|: r4) ((((f1 :&: (l0 :|: e')) :&: s2) :|: r3) :|: r4)
bonllflf :: b e e' -> b (((((e  :&: s0) :|: r1) :&: s2) :|: r3) :|: r4) (((((e' :&: s0) :|: r1) :&: s2) :|: r3) :|: r4)
bonllfls :: b e e' -> b (((((f0 :&: e ) :|: r1) :&: s2) :|: r3) :|: r4) (((((f0 :&: e') :|: r1) :&: s2) :|: r3) :|: r4)
bonllfll :: b e e' -> b (((((e  :|: r0) :|: r1) :&: s2) :|: r3) :|: r4) (((((e' :|: r0) :|: r1) :&: s2) :|: r3) :|: r4)
bonllflr :: b e e' -> b (((((l0 :|: e ) :|: r1) :&: s2) :|: r3) :|: r4) (((((l0 :|: e') :|: r1) :&: s2) :|: r3) :|: r4)
bonllfrf :: b e e' -> b ((((l1 :|: (e  :&: s0)) :&: s2) :|: r3) :|: r4) ((((l1 :|: (e' :&: s0)) :&: s2) :|: r3) :|: r4)
bonllfrs :: b e e' -> b ((((l1 :|: (f0 :&: e )) :&: s2) :|: r3) :|: r4) ((((l1 :|: (f0 :&: e')) :&: s2) :|: r3) :|: r4)
bonllfrl :: b e e' -> b ((((l1 :|: (e  :|: r0)) :&: s2) :|: r3) :|: r4) ((((l1 :|: (e' :|: r0)) :&: s2) :|: r3) :|: r4)
bonllfrr :: b e e' -> b ((((l1 :|: (l0 :|: e )) :&: s2) :|: r3) :|: r4) ((((l1 :|: (l0 :|: e')) :&: s2) :|: r3) :|: r4)
bonllsff :: b e e' -> b (((f2 :&: ((e  :&: s0) :&: s1)) :|: r3) :|: r4) (((f2 :&: ((e' :&: s0) :&: s1)) :|: r3) :|: r4)
bonllsfs :: b e e' -> b (((f2 :&: ((f0 :&: e ) :&: s1)) :|: r3) :|: r4) (((f2 :&: ((f0 :&: e') :&: s1)) :|: r3) :|: r4)
bonllsfl :: b e e' -> b (((f2 :&: ((e  :|: r0) :&: s1)) :|: r3) :|: r4) (((f2 :&: ((e' :|: r0) :&: s1)) :|: r3) :|: r4)
bonllsfr :: b e e' -> b (((f2 :&: ((l0 :|: e ) :&: s1)) :|: r3) :|: r4) (((f2 :&: ((l0 :|: e') :&: s1)) :|: r3) :|: r4)
bonllssf :: b e e' -> b (((f2 :&: (f1 :&: (e  :&: s0))) :|: r3) :|: r4) (((f2 :&: (f1 :&: (e' :&: s0))) :|: r3) :|: r4)
bonllsss :: b e e' -> b (((f2 :&: (f1 :&: (f0 :&: e ))) :|: r3) :|: r4) (((f2 :&: (f1 :&: (f0 :&: e'))) :|: r3) :|: r4)
bonllssl :: b e e' -> b (((f2 :&: (f1 :&: (e  :|: r0))) :|: r3) :|: r4) (((f2 :&: (f1 :&: (e' :|: r0))) :|: r3) :|: r4)
bonllssr :: b e e' -> b (((f2 :&: (f1 :&: (l0 :|: e ))) :|: r3) :|: r4) (((f2 :&: (f1 :&: (l0 :|: e'))) :|: r3) :|: r4)
bonllslf :: b e e' -> b (((f2 :&: ((e  :&: s0) :|: r1)) :|: r3) :|: r4) (((f2 :&: ((e' :&: s0) :|: r1)) :|: r3) :|: r4)
bonllsls :: b e e' -> b (((f2 :&: ((f0 :&: e ) :|: r1)) :|: r3) :|: r4) (((f2 :&: ((f0 :&: e') :|: r1)) :|: r3) :|: r4)
bonllsll :: b e e' -> b (((f2 :&: ((e  :|: r0) :|: r1)) :|: r3) :|: r4) (((f2 :&: ((e' :|: r0) :|: r1)) :|: r3) :|: r4)
bonllslr :: b e e' -> b (((f2 :&: ((l0 :|: e ) :|: r1)) :|: r3) :|: r4) (((f2 :&: ((l0 :|: e') :|: r1)) :|: r3) :|: r4)
bonllsrf :: b e e' -> b (((f2 :&: (l1 :|: (e  :&: s0))) :|: r3) :|: r4) (((f2 :&: (l1 :|: (e' :&: s0))) :|: r3) :|: r4)
bonllsrs :: b e e' -> b (((f2 :&: (l1 :|: (f0 :&: e ))) :|: r3) :|: r4) (((f2 :&: (l1 :|: (f0 :&: e'))) :|: r3) :|: r4)
bonllsrl :: b e e' -> b (((f2 :&: (l1 :|: (e  :|: r0))) :|: r3) :|: r4) (((f2 :&: (l1 :|: (e' :|: r0))) :|: r3) :|: r4)
bonllsrr :: b e e' -> b (((f2 :&: (l1 :|: (l0 :|: e ))) :|: r3) :|: r4) (((f2 :&: (l1 :|: (l0 :|: e'))) :|: r3) :|: r4)
bonlllff :: b e e' -> b (((((e  :&: s0) :&: s1) :|: r2) :|: r3) :|: r4) (((((e' :&: s0) :&: s1) :|: r2) :|: r3) :|: r4)
bonlllfs :: b e e' -> b (((((f0 :&: e ) :&: s1) :|: r2) :|: r3) :|: r4) (((((f0 :&: e') :&: s1) :|: r2) :|: r3) :|: r4)
bonlllfl :: b e e' -> b (((((e  :|: r0) :&: s1) :|: r2) :|: r3) :|: r4) (((((e' :|: r0) :&: s1) :|: r2) :|: r3) :|: r4)
bonlllfr :: b e e' -> b (((((l0 :|: e ) :&: s1) :|: r2) :|: r3) :|: r4) (((((l0 :|: e') :&: s1) :|: r2) :|: r3) :|: r4)
bonlllsf :: b e e' -> b ((((f1 :&: (e  :&: s0)) :|: r2) :|: r3) :|: r4) ((((f1 :&: (e' :&: s0)) :|: r2) :|: r3) :|: r4)
bonlllss :: b e e' -> b ((((f1 :&: (f0 :&: e )) :|: r2) :|: r3) :|: r4) ((((f1 :&: (f0 :&: e')) :|: r2) :|: r3) :|: r4)
bonlllsl :: b e e' -> b ((((f1 :&: (e  :|: r0)) :|: r2) :|: r3) :|: r4) ((((f1 :&: (e' :|: r0)) :|: r2) :|: r3) :|: r4)
bonlllsr :: b e e' -> b ((((f1 :&: (l0 :|: e )) :|: r2) :|: r3) :|: r4) ((((f1 :&: (l0 :|: e')) :|: r2) :|: r3) :|: r4)
bonllllf :: b e e' -> b (((((e  :&: s0) :|: r1) :|: r2) :|: r3) :|: r4) (((((e' :&: s0) :|: r1) :|: r2) :|: r3) :|: r4)
bonlllls :: b e e' -> b (((((f0 :&: e ) :|: r1) :|: r2) :|: r3) :|: r4) (((((f0 :&: e') :|: r1) :|: r2) :|: r3) :|: r4)
bonlllll :: b e e' -> b (((((e  :|: r0) :|: r1) :|: r2) :|: r3) :|: r4) (((((e' :|: r0) :|: r1) :|: r2) :|: r3) :|: r4)
bonllllr :: b e e' -> b (((((l0 :|: e ) :|: r1) :|: r2) :|: r3) :|: r4) (((((l0 :|: e') :|: r1) :|: r2) :|: r3) :|: r4)
bonlllrf :: b e e' -> b ((((l1 :|: (e  :&: s0)) :|: r2) :|: r3) :|: r4) ((((l1 :|: (e' :&: s0)) :|: r2) :|: r3) :|: r4)
bonlllrs :: b e e' -> b ((((l1 :|: (f0 :&: e )) :|: r2) :|: r3) :|: r4) ((((l1 :|: (f0 :&: e')) :|: r2) :|: r3) :|: r4)
bonlllrl :: b e e' -> b ((((l1 :|: (e  :|: r0)) :|: r2) :|: r3) :|: r4) ((((l1 :|: (e' :|: r0)) :|: r2) :|: r3) :|: r4)
bonlllrr :: b e e' -> b ((((l1 :|: (l0 :|: e )) :|: r2) :|: r3) :|: r4) ((((l1 :|: (l0 :|: e')) :|: r2) :|: r3) :|: r4)
bonllrff :: b e e' -> b (((l2 :|: ((e  :&: s0) :&: s1)) :|: r3) :|: r4) (((l2 :|: ((e' :&: s0) :&: s1)) :|: r3) :|: r4)
bonllrfs :: b e e' -> b (((l2 :|: ((f0 :&: e ) :&: s1)) :|: r3) :|: r4) (((l2 :|: ((f0 :&: e') :&: s1)) :|: r3) :|: r4)
bonllrfl :: b e e' -> b (((l2 :|: ((e  :|: r0) :&: s1)) :|: r3) :|: r4) (((l2 :|: ((e' :|: r0) :&: s1)) :|: r3) :|: r4)
bonllrfr :: b e e' -> b (((l2 :|: ((l0 :|: e ) :&: s1)) :|: r3) :|: r4) (((l2 :|: ((l0 :|: e') :&: s1)) :|: r3) :|: r4)
bonllrsf :: b e e' -> b (((l2 :|: (f1 :&: (e  :&: s0))) :|: r3) :|: r4) (((l2 :|: (f1 :&: (e' :&: s0))) :|: r3) :|: r4)
bonllrss :: b e e' -> b (((l2 :|: (f1 :&: (f0 :&: e ))) :|: r3) :|: r4) (((l2 :|: (f1 :&: (f0 :&: e'))) :|: r3) :|: r4)
bonllrsl :: b e e' -> b (((l2 :|: (f1 :&: (e  :|: r0))) :|: r3) :|: r4) (((l2 :|: (f1 :&: (e' :|: r0))) :|: r3) :|: r4)
bonllrsr :: b e e' -> b (((l2 :|: (f1 :&: (l0 :|: e ))) :|: r3) :|: r4) (((l2 :|: (f1 :&: (l0 :|: e'))) :|: r3) :|: r4)
bonllrlf :: b e e' -> b (((l2 :|: ((e  :&: s0) :|: r1)) :|: r3) :|: r4) (((l2 :|: ((e' :&: s0) :|: r1)) :|: r3) :|: r4)
bonllrls :: b e e' -> b (((l2 :|: ((f0 :&: e ) :|: r1)) :|: r3) :|: r4) (((l2 :|: ((f0 :&: e') :|: r1)) :|: r3) :|: r4)
bonllrll :: b e e' -> b (((l2 :|: ((e  :|: r0) :|: r1)) :|: r3) :|: r4) (((l2 :|: ((e' :|: r0) :|: r1)) :|: r3) :|: r4)
bonllrlr :: b e e' -> b (((l2 :|: ((l0 :|: e ) :|: r1)) :|: r3) :|: r4) (((l2 :|: ((l0 :|: e') :|: r1)) :|: r3) :|: r4)
bonllrrf :: b e e' -> b (((l2 :|: (l1 :|: (e  :&: s0))) :|: r3) :|: r4) (((l2 :|: (l1 :|: (e' :&: s0))) :|: r3) :|: r4)
bonllrrs :: b e e' -> b (((l2 :|: (l1 :|: (f0 :&: e ))) :|: r3) :|: r4) (((l2 :|: (l1 :|: (f0 :&: e'))) :|: r3) :|: r4)
bonllrrl :: b e e' -> b (((l2 :|: (l1 :|: (e  :|: r0))) :|: r3) :|: r4) (((l2 :|: (l1 :|: (e' :|: r0))) :|: r3) :|: r4)
bonllrrr :: b e e' -> b (((l2 :|: (l1 :|: (l0 :|: e ))) :|: r3) :|: r4) (((l2 :|: (l1 :|: (l0 :|: e'))) :|: r3) :|: r4)
bonlrfff :: b e e' -> b ((l3 :|: (((e  :&: s0) :&: s1) :&: s2)) :|: r4) ((l3 :|: (((e' :&: s0) :&: s1) :&: s2)) :|: r4)
bonlrffs :: b e e' -> b ((l3 :|: (((f0 :&: e ) :&: s1) :&: s2)) :|: r4) ((l3 :|: (((f0 :&: e') :&: s1) :&: s2)) :|: r4)
bonlrffl :: b e e' -> b ((l3 :|: (((e  :|: r0) :&: s1) :&: s2)) :|: r4) ((l3 :|: (((e' :|: r0) :&: s1) :&: s2)) :|: r4)
bonlrffr :: b e e' -> b ((l3 :|: (((l0 :|: e ) :&: s1) :&: s2)) :|: r4) ((l3 :|: (((l0 :|: e') :&: s1) :&: s2)) :|: r4)
bonlrfsf :: b e e' -> b ((l3 :|: ((f1 :&: (e  :&: s0)) :&: s2)) :|: r4) ((l3 :|: ((f1 :&: (e' :&: s0)) :&: s2)) :|: r4)
bonlrfss :: b e e' -> b ((l3 :|: ((f1 :&: (f0 :&: e )) :&: s2)) :|: r4) ((l3 :|: ((f1 :&: (f0 :&: e')) :&: s2)) :|: r4)
bonlrfsl :: b e e' -> b ((l3 :|: ((f1 :&: (e  :|: r0)) :&: s2)) :|: r4) ((l3 :|: ((f1 :&: (e' :|: r0)) :&: s2)) :|: r4)
bonlrfsr :: b e e' -> b ((l3 :|: ((f1 :&: (l0 :|: e )) :&: s2)) :|: r4) ((l3 :|: ((f1 :&: (l0 :|: e')) :&: s2)) :|: r4)
bonlrflf :: b e e' -> b ((l3 :|: (((e  :&: s0) :|: r1) :&: s2)) :|: r4) ((l3 :|: (((e' :&: s0) :|: r1) :&: s2)) :|: r4)
bonlrfls :: b e e' -> b ((l3 :|: (((f0 :&: e ) :|: r1) :&: s2)) :|: r4) ((l3 :|: (((f0 :&: e') :|: r1) :&: s2)) :|: r4)
bonlrfll :: b e e' -> b ((l3 :|: (((e  :|: r0) :|: r1) :&: s2)) :|: r4) ((l3 :|: (((e' :|: r0) :|: r1) :&: s2)) :|: r4)
bonlrflr :: b e e' -> b ((l3 :|: (((l0 :|: e ) :|: r1) :&: s2)) :|: r4) ((l3 :|: (((l0 :|: e') :|: r1) :&: s2)) :|: r4)
bonlrfrf :: b e e' -> b ((l3 :|: ((l1 :|: (e  :&: s0)) :&: s2)) :|: r4) ((l3 :|: ((l1 :|: (e' :&: s0)) :&: s2)) :|: r4)
bonlrfrs :: b e e' -> b ((l3 :|: ((l1 :|: (f0 :&: e )) :&: s2)) :|: r4) ((l3 :|: ((l1 :|: (f0 :&: e')) :&: s2)) :|: r4)
bonlrfrl :: b e e' -> b ((l3 :|: ((l1 :|: (e  :|: r0)) :&: s2)) :|: r4) ((l3 :|: ((l1 :|: (e' :|: r0)) :&: s2)) :|: r4)
bonlrfrr :: b e e' -> b ((l3 :|: ((l1 :|: (l0 :|: e )) :&: s2)) :|: r4) ((l3 :|: ((l1 :|: (l0 :|: e')) :&: s2)) :|: r4)
bonlrsff :: b e e' -> b ((l3 :|: (f2 :&: ((e  :&: s0) :&: s1))) :|: r4) ((l3 :|: (f2 :&: ((e' :&: s0) :&: s1))) :|: r4)
bonlrsfs :: b e e' -> b ((l3 :|: (f2 :&: ((f0 :&: e ) :&: s1))) :|: r4) ((l3 :|: (f2 :&: ((f0 :&: e') :&: s1))) :|: r4)
bonlrsfl :: b e e' -> b ((l3 :|: (f2 :&: ((e  :|: r0) :&: s1))) :|: r4) ((l3 :|: (f2 :&: ((e' :|: r0) :&: s1))) :|: r4)
bonlrsfr :: b e e' -> b ((l3 :|: (f2 :&: ((l0 :|: e ) :&: s1))) :|: r4) ((l3 :|: (f2 :&: ((l0 :|: e') :&: s1))) :|: r4)
bonlrssf :: b e e' -> b ((l3 :|: (f2 :&: (f1 :&: (e  :&: s0)))) :|: r4) ((l3 :|: (f2 :&: (f1 :&: (e' :&: s0)))) :|: r4)
bonlrsss :: b e e' -> b ((l3 :|: (f2 :&: (f1 :&: (f0 :&: e )))) :|: r4) ((l3 :|: (f2 :&: (f1 :&: (f0 :&: e')))) :|: r4)
bonlrssl :: b e e' -> b ((l3 :|: (f2 :&: (f1 :&: (e  :|: r0)))) :|: r4) ((l3 :|: (f2 :&: (f1 :&: (e' :|: r0)))) :|: r4)
bonlrssr :: b e e' -> b ((l3 :|: (f2 :&: (f1 :&: (l0 :|: e )))) :|: r4) ((l3 :|: (f2 :&: (f1 :&: (l0 :|: e')))) :|: r4)
bonlrslf :: b e e' -> b ((l3 :|: (f2 :&: ((e  :&: s0) :|: r1))) :|: r4) ((l3 :|: (f2 :&: ((e' :&: s0) :|: r1))) :|: r4)
bonlrsls :: b e e' -> b ((l3 :|: (f2 :&: ((f0 :&: e ) :|: r1))) :|: r4) ((l3 :|: (f2 :&: ((f0 :&: e') :|: r1))) :|: r4)
bonlrsll :: b e e' -> b ((l3 :|: (f2 :&: ((e  :|: r0) :|: r1))) :|: r4) ((l3 :|: (f2 :&: ((e' :|: r0) :|: r1))) :|: r4)
bonlrslr :: b e e' -> b ((l3 :|: (f2 :&: ((l0 :|: e ) :|: r1))) :|: r4) ((l3 :|: (f2 :&: ((l0 :|: e') :|: r1))) :|: r4)
bonlrsrf :: b e e' -> b ((l3 :|: (f2 :&: (l1 :|: (e  :&: s0)))) :|: r4) ((l3 :|: (f2 :&: (l1 :|: (e' :&: s0)))) :|: r4)
bonlrsrs :: b e e' -> b ((l3 :|: (f2 :&: (l1 :|: (f0 :&: e )))) :|: r4) ((l3 :|: (f2 :&: (l1 :|: (f0 :&: e')))) :|: r4)
bonlrsrl :: b e e' -> b ((l3 :|: (f2 :&: (l1 :|: (e  :|: r0)))) :|: r4) ((l3 :|: (f2 :&: (l1 :|: (e' :|: r0)))) :|: r4)
bonlrsrr :: b e e' -> b ((l3 :|: (f2 :&: (l1 :|: (l0 :|: e )))) :|: r4) ((l3 :|: (f2 :&: (l1 :|: (l0 :|: e')))) :|: r4)
bonlrlff :: b e e' -> b ((l3 :|: (((e  :&: s0) :&: s1) :|: r2)) :|: r4) ((l3 :|: (((e' :&: s0) :&: s1) :|: r2)) :|: r4)
bonlrlfs :: b e e' -> b ((l3 :|: (((f0 :&: e ) :&: s1) :|: r2)) :|: r4) ((l3 :|: (((f0 :&: e') :&: s1) :|: r2)) :|: r4)
bonlrlfl :: b e e' -> b ((l3 :|: (((e  :|: r0) :&: s1) :|: r2)) :|: r4) ((l3 :|: (((e' :|: r0) :&: s1) :|: r2)) :|: r4)
bonlrlfr :: b e e' -> b ((l3 :|: (((l0 :|: e ) :&: s1) :|: r2)) :|: r4) ((l3 :|: (((l0 :|: e') :&: s1) :|: r2)) :|: r4)
bonlrlsf :: b e e' -> b ((l3 :|: ((f1 :&: (e  :&: s0)) :|: r2)) :|: r4) ((l3 :|: ((f1 :&: (e' :&: s0)) :|: r2)) :|: r4)
bonlrlss :: b e e' -> b ((l3 :|: ((f1 :&: (f0 :&: e )) :|: r2)) :|: r4) ((l3 :|: ((f1 :&: (f0 :&: e')) :|: r2)) :|: r4)
bonlrlsl :: b e e' -> b ((l3 :|: ((f1 :&: (e  :|: r0)) :|: r2)) :|: r4) ((l3 :|: ((f1 :&: (e' :|: r0)) :|: r2)) :|: r4)
bonlrlsr :: b e e' -> b ((l3 :|: ((f1 :&: (l0 :|: e )) :|: r2)) :|: r4) ((l3 :|: ((f1 :&: (l0 :|: e')) :|: r2)) :|: r4)
bonlrllf :: b e e' -> b ((l3 :|: (((e  :&: s0) :|: r1) :|: r2)) :|: r4) ((l3 :|: (((e' :&: s0) :|: r1) :|: r2)) :|: r4)
bonlrlls :: b e e' -> b ((l3 :|: (((f0 :&: e ) :|: r1) :|: r2)) :|: r4) ((l3 :|: (((f0 :&: e') :|: r1) :|: r2)) :|: r4)
bonlrlll :: b e e' -> b ((l3 :|: (((e  :|: r0) :|: r1) :|: r2)) :|: r4) ((l3 :|: (((e' :|: r0) :|: r1) :|: r2)) :|: r4)
bonlrllr :: b e e' -> b ((l3 :|: (((l0 :|: e ) :|: r1) :|: r2)) :|: r4) ((l3 :|: (((l0 :|: e') :|: r1) :|: r2)) :|: r4)
bonlrlrf :: b e e' -> b ((l3 :|: ((l1 :|: (e  :&: s0)) :|: r2)) :|: r4) ((l3 :|: ((l1 :|: (e' :&: s0)) :|: r2)) :|: r4)
bonlrlrs :: b e e' -> b ((l3 :|: ((l1 :|: (f0 :&: e )) :|: r2)) :|: r4) ((l3 :|: ((l1 :|: (f0 :&: e')) :|: r2)) :|: r4)
bonlrlrl :: b e e' -> b ((l3 :|: ((l1 :|: (e  :|: r0)) :|: r2)) :|: r4) ((l3 :|: ((l1 :|: (e' :|: r0)) :|: r2)) :|: r4)
bonlrlrr :: b e e' -> b ((l3 :|: ((l1 :|: (l0 :|: e )) :|: r2)) :|: r4) ((l3 :|: ((l1 :|: (l0 :|: e')) :|: r2)) :|: r4)
bonlrrff :: b e e' -> b ((l3 :|: (l2 :|: ((e  :&: s0) :&: s1))) :|: r4) ((l3 :|: (l2 :|: ((e' :&: s0) :&: s1))) :|: r4)
bonlrrfs :: b e e' -> b ((l3 :|: (l2 :|: ((f0 :&: e ) :&: s1))) :|: r4) ((l3 :|: (l2 :|: ((f0 :&: e') :&: s1))) :|: r4)
bonlrrfl :: b e e' -> b ((l3 :|: (l2 :|: ((e  :|: r0) :&: s1))) :|: r4) ((l3 :|: (l2 :|: ((e' :|: r0) :&: s1))) :|: r4)
bonlrrfr :: b e e' -> b ((l3 :|: (l2 :|: ((l0 :|: e ) :&: s1))) :|: r4) ((l3 :|: (l2 :|: ((l0 :|: e') :&: s1))) :|: r4)
bonlrrsf :: b e e' -> b ((l3 :|: (l2 :|: (f1 :&: (e  :&: s0)))) :|: r4) ((l3 :|: (l2 :|: (f1 :&: (e' :&: s0)))) :|: r4)
bonlrrss :: b e e' -> b ((l3 :|: (l2 :|: (f1 :&: (f0 :&: e )))) :|: r4) ((l3 :|: (l2 :|: (f1 :&: (f0 :&: e')))) :|: r4)
bonlrrsl :: b e e' -> b ((l3 :|: (l2 :|: (f1 :&: (e  :|: r0)))) :|: r4) ((l3 :|: (l2 :|: (f1 :&: (e' :|: r0)))) :|: r4)
bonlrrsr :: b e e' -> b ((l3 :|: (l2 :|: (f1 :&: (l0 :|: e )))) :|: r4) ((l3 :|: (l2 :|: (f1 :&: (l0 :|: e')))) :|: r4)
bonlrrlf :: b e e' -> b ((l3 :|: (l2 :|: ((e  :&: s0) :|: r1))) :|: r4) ((l3 :|: (l2 :|: ((e' :&: s0) :|: r1))) :|: r4)
bonlrrls :: b e e' -> b ((l3 :|: (l2 :|: ((f0 :&: e ) :|: r1))) :|: r4) ((l3 :|: (l2 :|: ((f0 :&: e') :|: r1))) :|: r4)
bonlrrll :: b e e' -> b ((l3 :|: (l2 :|: ((e  :|: r0) :|: r1))) :|: r4) ((l3 :|: (l2 :|: ((e' :|: r0) :|: r1))) :|: r4)
bonlrrlr :: b e e' -> b ((l3 :|: (l2 :|: ((l0 :|: e ) :|: r1))) :|: r4) ((l3 :|: (l2 :|: ((l0 :|: e') :|: r1))) :|: r4)
bonlrrrf :: b e e' -> b ((l3 :|: (l2 :|: (l1 :|: (e  :&: s0)))) :|: r4) ((l3 :|: (l2 :|: (l1 :|: (e' :&: s0)))) :|: r4)
bonlrrrs :: b e e' -> b ((l3 :|: (l2 :|: (l1 :|: (f0 :&: e )))) :|: r4) ((l3 :|: (l2 :|: (l1 :|: (f0 :&: e')))) :|: r4)
bonlrrrl :: b e e' -> b ((l3 :|: (l2 :|: (l1 :|: (e  :|: r0)))) :|: r4) ((l3 :|: (l2 :|: (l1 :|: (e' :|: r0)))) :|: r4)
bonlrrrr :: b e e' -> b ((l3 :|: (l2 :|: (l1 :|: (l0 :|: e )))) :|: r4) ((l3 :|: (l2 :|: (l1 :|: (l0 :|: e')))) :|: r4)
bonrffff :: b e e' -> b (l4 :|: ((((e  :&: s0) :&: s1) :&: s2) :&: s3)) (l4 :|: ((((e' :&: s0) :&: s1) :&: s2) :&: s3))
bonrfffs :: b e e' -> b (l4 :|: ((((f0 :&: e ) :&: s1) :&: s2) :&: s3)) (l4 :|: ((((f0 :&: e') :&: s1) :&: s2) :&: s3))
bonrfffl :: b e e' -> b (l4 :|: ((((e  :|: r0) :&: s1) :&: s2) :&: s3)) (l4 :|: ((((e' :|: r0) :&: s1) :&: s2) :&: s3))
bonrfffr :: b e e' -> b (l4 :|: ((((l0 :|: e ) :&: s1) :&: s2) :&: s3)) (l4 :|: ((((l0 :|: e') :&: s1) :&: s2) :&: s3))
bonrffsf :: b e e' -> b (l4 :|: (((f1 :&: (e  :&: s0)) :&: s2) :&: s3)) (l4 :|: (((f1 :&: (e' :&: s0)) :&: s2) :&: s3))
bonrffss :: b e e' -> b (l4 :|: (((f1 :&: (f0 :&: e )) :&: s2) :&: s3)) (l4 :|: (((f1 :&: (f0 :&: e')) :&: s2) :&: s3))
bonrffsl :: b e e' -> b (l4 :|: (((f1 :&: (e  :|: r0)) :&: s2) :&: s3)) (l4 :|: (((f1 :&: (e' :|: r0)) :&: s2) :&: s3))
bonrffsr :: b e e' -> b (l4 :|: (((f1 :&: (l0 :|: e )) :&: s2) :&: s3)) (l4 :|: (((f1 :&: (l0 :|: e')) :&: s2) :&: s3))
bonrfflf :: b e e' -> b (l4 :|: ((((e  :&: s0) :|: r1) :&: s2) :&: s3)) (l4 :|: ((((e' :&: s0) :|: r1) :&: s2) :&: s3))
bonrffls :: b e e' -> b (l4 :|: ((((f0 :&: e ) :|: r1) :&: s2) :&: s3)) (l4 :|: ((((f0 :&: e') :|: r1) :&: s2) :&: s3))
bonrffll :: b e e' -> b (l4 :|: ((((e  :|: r0) :|: r1) :&: s2) :&: s3)) (l4 :|: ((((e' :|: r0) :|: r1) :&: s2) :&: s3))
bonrfflr :: b e e' -> b (l4 :|: ((((l0 :|: e ) :|: r1) :&: s2) :&: s3)) (l4 :|: ((((l0 :|: e') :|: r1) :&: s2) :&: s3))
bonrffrf :: b e e' -> b (l4 :|: (((l1 :|: (e  :&: s0)) :&: s2) :&: s3)) (l4 :|: (((l1 :|: (e' :&: s0)) :&: s2) :&: s3))
bonrffrs :: b e e' -> b (l4 :|: (((l1 :|: (f0 :&: e )) :&: s2) :&: s3)) (l4 :|: (((l1 :|: (f0 :&: e')) :&: s2) :&: s3))
bonrffrl :: b e e' -> b (l4 :|: (((l1 :|: (e  :|: r0)) :&: s2) :&: s3)) (l4 :|: (((l1 :|: (e' :|: r0)) :&: s2) :&: s3))
bonrffrr :: b e e' -> b (l4 :|: (((l1 :|: (l0 :|: e )) :&: s2) :&: s3)) (l4 :|: (((l1 :|: (l0 :|: e')) :&: s2) :&: s3))
bonrfsff :: b e e' -> b (l4 :|: ((f2 :&: ((e  :&: s0) :&: s1)) :&: s3)) (l4 :|: ((f2 :&: ((e' :&: s0) :&: s1)) :&: s3))
bonrfsfs :: b e e' -> b (l4 :|: ((f2 :&: ((f0 :&: e ) :&: s1)) :&: s3)) (l4 :|: ((f2 :&: ((f0 :&: e') :&: s1)) :&: s3))
bonrfsfl :: b e e' -> b (l4 :|: ((f2 :&: ((e  :|: r0) :&: s1)) :&: s3)) (l4 :|: ((f2 :&: ((e' :|: r0) :&: s1)) :&: s3))
bonrfsfr :: b e e' -> b (l4 :|: ((f2 :&: ((l0 :|: e ) :&: s1)) :&: s3)) (l4 :|: ((f2 :&: ((l0 :|: e') :&: s1)) :&: s3))
bonrfssf :: b e e' -> b (l4 :|: ((f2 :&: (f1 :&: (e  :&: s0))) :&: s3)) (l4 :|: ((f2 :&: (f1 :&: (e' :&: s0))) :&: s3))
bonrfsss :: b e e' -> b (l4 :|: ((f2 :&: (f1 :&: (f0 :&: e ))) :&: s3)) (l4 :|: ((f2 :&: (f1 :&: (f0 :&: e'))) :&: s3))
bonrfssl :: b e e' -> b (l4 :|: ((f2 :&: (f1 :&: (e  :|: r0))) :&: s3)) (l4 :|: ((f2 :&: (f1 :&: (e' :|: r0))) :&: s3))
bonrfssr :: b e e' -> b (l4 :|: ((f2 :&: (f1 :&: (l0 :|: e ))) :&: s3)) (l4 :|: ((f2 :&: (f1 :&: (l0 :|: e'))) :&: s3))
bonrfslf :: b e e' -> b (l4 :|: ((f2 :&: ((e  :&: s0) :|: r1)) :&: s3)) (l4 :|: ((f2 :&: ((e' :&: s0) :|: r1)) :&: s3))
bonrfsls :: b e e' -> b (l4 :|: ((f2 :&: ((f0 :&: e ) :|: r1)) :&: s3)) (l4 :|: ((f2 :&: ((f0 :&: e') :|: r1)) :&: s3))
bonrfsll :: b e e' -> b (l4 :|: ((f2 :&: ((e  :|: r0) :|: r1)) :&: s3)) (l4 :|: ((f2 :&: ((e' :|: r0) :|: r1)) :&: s3))
bonrfslr :: b e e' -> b (l4 :|: ((f2 :&: ((l0 :|: e ) :|: r1)) :&: s3)) (l4 :|: ((f2 :&: ((l0 :|: e') :|: r1)) :&: s3))
bonrfsrf :: b e e' -> b (l4 :|: ((f2 :&: (l1 :|: (e  :&: s0))) :&: s3)) (l4 :|: ((f2 :&: (l1 :|: (e' :&: s0))) :&: s3))
bonrfsrs :: b e e' -> b (l4 :|: ((f2 :&: (l1 :|: (f0 :&: e ))) :&: s3)) (l4 :|: ((f2 :&: (l1 :|: (f0 :&: e'))) :&: s3))
bonrfsrl :: b e e' -> b (l4 :|: ((f2 :&: (l1 :|: (e  :|: r0))) :&: s3)) (l4 :|: ((f2 :&: (l1 :|: (e' :|: r0))) :&: s3))
bonrfsrr :: b e e' -> b (l4 :|: ((f2 :&: (l1 :|: (l0 :|: e ))) :&: s3)) (l4 :|: ((f2 :&: (l1 :|: (l0 :|: e'))) :&: s3))
bonrflff :: b e e' -> b (l4 :|: ((((e  :&: s0) :&: s1) :|: r2) :&: s3)) (l4 :|: ((((e' :&: s0) :&: s1) :|: r2) :&: s3))
bonrflfs :: b e e' -> b (l4 :|: ((((f0 :&: e ) :&: s1) :|: r2) :&: s3)) (l4 :|: ((((f0 :&: e') :&: s1) :|: r2) :&: s3))
bonrflfl :: b e e' -> b (l4 :|: ((((e  :|: r0) :&: s1) :|: r2) :&: s3)) (l4 :|: ((((e' :|: r0) :&: s1) :|: r2) :&: s3))
bonrflfr :: b e e' -> b (l4 :|: ((((l0 :|: e ) :&: s1) :|: r2) :&: s3)) (l4 :|: ((((l0 :|: e') :&: s1) :|: r2) :&: s3))
bonrflsf :: b e e' -> b (l4 :|: (((f1 :&: (e  :&: s0)) :|: r2) :&: s3)) (l4 :|: (((f1 :&: (e' :&: s0)) :|: r2) :&: s3))
bonrflss :: b e e' -> b (l4 :|: (((f1 :&: (f0 :&: e )) :|: r2) :&: s3)) (l4 :|: (((f1 :&: (f0 :&: e')) :|: r2) :&: s3))
bonrflsl :: b e e' -> b (l4 :|: (((f1 :&: (e  :|: r0)) :|: r2) :&: s3)) (l4 :|: (((f1 :&: (e' :|: r0)) :|: r2) :&: s3))
bonrflsr :: b e e' -> b (l4 :|: (((f1 :&: (l0 :|: e )) :|: r2) :&: s3)) (l4 :|: (((f1 :&: (l0 :|: e')) :|: r2) :&: s3))
bonrfllf :: b e e' -> b (l4 :|: ((((e  :&: s0) :|: r1) :|: r2) :&: s3)) (l4 :|: ((((e' :&: s0) :|: r1) :|: r2) :&: s3))
bonrflls :: b e e' -> b (l4 :|: ((((f0 :&: e ) :|: r1) :|: r2) :&: s3)) (l4 :|: ((((f0 :&: e') :|: r1) :|: r2) :&: s3))
bonrflll :: b e e' -> b (l4 :|: ((((e  :|: r0) :|: r1) :|: r2) :&: s3)) (l4 :|: ((((e' :|: r0) :|: r1) :|: r2) :&: s3))
bonrfllr :: b e e' -> b (l4 :|: ((((l0 :|: e ) :|: r1) :|: r2) :&: s3)) (l4 :|: ((((l0 :|: e') :|: r1) :|: r2) :&: s3))
bonrflrf :: b e e' -> b (l4 :|: (((l1 :|: (e  :&: s0)) :|: r2) :&: s3)) (l4 :|: (((l1 :|: (e' :&: s0)) :|: r2) :&: s3))
bonrflrs :: b e e' -> b (l4 :|: (((l1 :|: (f0 :&: e )) :|: r2) :&: s3)) (l4 :|: (((l1 :|: (f0 :&: e')) :|: r2) :&: s3))
bonrflrl :: b e e' -> b (l4 :|: (((l1 :|: (e  :|: r0)) :|: r2) :&: s3)) (l4 :|: (((l1 :|: (e' :|: r0)) :|: r2) :&: s3))
bonrflrr :: b e e' -> b (l4 :|: (((l1 :|: (l0 :|: e )) :|: r2) :&: s3)) (l4 :|: (((l1 :|: (l0 :|: e')) :|: r2) :&: s3))
bonrfrff :: b e e' -> b (l4 :|: ((l2 :|: ((e  :&: s0) :&: s1)) :&: s3)) (l4 :|: ((l2 :|: ((e' :&: s0) :&: s1)) :&: s3))
bonrfrfs :: b e e' -> b (l4 :|: ((l2 :|: ((f0 :&: e ) :&: s1)) :&: s3)) (l4 :|: ((l2 :|: ((f0 :&: e') :&: s1)) :&: s3))
bonrfrfl :: b e e' -> b (l4 :|: ((l2 :|: ((e  :|: r0) :&: s1)) :&: s3)) (l4 :|: ((l2 :|: ((e' :|: r0) :&: s1)) :&: s3))
bonrfrfr :: b e e' -> b (l4 :|: ((l2 :|: ((l0 :|: e ) :&: s1)) :&: s3)) (l4 :|: ((l2 :|: ((l0 :|: e') :&: s1)) :&: s3))
bonrfrsf :: b e e' -> b (l4 :|: ((l2 :|: (f1 :&: (e  :&: s0))) :&: s3)) (l4 :|: ((l2 :|: (f1 :&: (e' :&: s0))) :&: s3))
bonrfrss :: b e e' -> b (l4 :|: ((l2 :|: (f1 :&: (f0 :&: e ))) :&: s3)) (l4 :|: ((l2 :|: (f1 :&: (f0 :&: e'))) :&: s3))
bonrfrsl :: b e e' -> b (l4 :|: ((l2 :|: (f1 :&: (e  :|: r0))) :&: s3)) (l4 :|: ((l2 :|: (f1 :&: (e' :|: r0))) :&: s3))
bonrfrsr :: b e e' -> b (l4 :|: ((l2 :|: (f1 :&: (l0 :|: e ))) :&: s3)) (l4 :|: ((l2 :|: (f1 :&: (l0 :|: e'))) :&: s3))
bonrfrlf :: b e e' -> b (l4 :|: ((l2 :|: ((e  :&: s0) :|: r1)) :&: s3)) (l4 :|: ((l2 :|: ((e' :&: s0) :|: r1)) :&: s3))
bonrfrls :: b e e' -> b (l4 :|: ((l2 :|: ((f0 :&: e ) :|: r1)) :&: s3)) (l4 :|: ((l2 :|: ((f0 :&: e') :|: r1)) :&: s3))
bonrfrll :: b e e' -> b (l4 :|: ((l2 :|: ((e  :|: r0) :|: r1)) :&: s3)) (l4 :|: ((l2 :|: ((e' :|: r0) :|: r1)) :&: s3))
bonrfrlr :: b e e' -> b (l4 :|: ((l2 :|: ((l0 :|: e ) :|: r1)) :&: s3)) (l4 :|: ((l2 :|: ((l0 :|: e') :|: r1)) :&: s3))
bonrfrrf :: b e e' -> b (l4 :|: ((l2 :|: (l1 :|: (e  :&: s0))) :&: s3)) (l4 :|: ((l2 :|: (l1 :|: (e' :&: s0))) :&: s3))
bonrfrrs :: b e e' -> b (l4 :|: ((l2 :|: (l1 :|: (f0 :&: e ))) :&: s3)) (l4 :|: ((l2 :|: (l1 :|: (f0 :&: e'))) :&: s3))
bonrfrrl :: b e e' -> b (l4 :|: ((l2 :|: (l1 :|: (e  :|: r0))) :&: s3)) (l4 :|: ((l2 :|: (l1 :|: (e' :|: r0))) :&: s3))
bonrfrrr :: b e e' -> b (l4 :|: ((l2 :|: (l1 :|: (l0 :|: e ))) :&: s3)) (l4 :|: ((l2 :|: (l1 :|: (l0 :|: e'))) :&: s3))
bonrsfff :: b e e' -> b (l4 :|: (f3 :&: (((e  :&: s0) :&: s1) :&: s2))) (l4 :|: (f3 :&: (((e' :&: s0) :&: s1) :&: s2)))
bonrsffs :: b e e' -> b (l4 :|: (f3 :&: (((f0 :&: e ) :&: s1) :&: s2))) (l4 :|: (f3 :&: (((f0 :&: e') :&: s1) :&: s2)))
bonrsffl :: b e e' -> b (l4 :|: (f3 :&: (((e  :|: r0) :&: s1) :&: s2))) (l4 :|: (f3 :&: (((e' :|: r0) :&: s1) :&: s2)))
bonrsffr :: b e e' -> b (l4 :|: (f3 :&: (((l0 :|: e ) :&: s1) :&: s2))) (l4 :|: (f3 :&: (((l0 :|: e') :&: s1) :&: s2)))
bonrsfsf :: b e e' -> b (l4 :|: (f3 :&: ((f1 :&: (e  :&: s0)) :&: s2))) (l4 :|: (f3 :&: ((f1 :&: (e' :&: s0)) :&: s2)))
bonrsfss :: b e e' -> b (l4 :|: (f3 :&: ((f1 :&: (f0 :&: e )) :&: s2))) (l4 :|: (f3 :&: ((f1 :&: (f0 :&: e')) :&: s2)))
bonrsfsl :: b e e' -> b (l4 :|: (f3 :&: ((f1 :&: (e  :|: r0)) :&: s2))) (l4 :|: (f3 :&: ((f1 :&: (e' :|: r0)) :&: s2)))
bonrsfsr :: b e e' -> b (l4 :|: (f3 :&: ((f1 :&: (l0 :|: e )) :&: s2))) (l4 :|: (f3 :&: ((f1 :&: (l0 :|: e')) :&: s2)))
bonrsflf :: b e e' -> b (l4 :|: (f3 :&: (((e  :&: s0) :|: r1) :&: s2))) (l4 :|: (f3 :&: (((e' :&: s0) :|: r1) :&: s2)))
bonrsfls :: b e e' -> b (l4 :|: (f3 :&: (((f0 :&: e ) :|: r1) :&: s2))) (l4 :|: (f3 :&: (((f0 :&: e') :|: r1) :&: s2)))
bonrsfll :: b e e' -> b (l4 :|: (f3 :&: (((e  :|: r0) :|: r1) :&: s2))) (l4 :|: (f3 :&: (((e' :|: r0) :|: r1) :&: s2)))
bonrsflr :: b e e' -> b (l4 :|: (f3 :&: (((l0 :|: e ) :|: r1) :&: s2))) (l4 :|: (f3 :&: (((l0 :|: e') :|: r1) :&: s2)))
bonrsfrf :: b e e' -> b (l4 :|: (f3 :&: ((l1 :|: (e  :&: s0)) :&: s2))) (l4 :|: (f3 :&: ((l1 :|: (e' :&: s0)) :&: s2)))
bonrsfrs :: b e e' -> b (l4 :|: (f3 :&: ((l1 :|: (f0 :&: e )) :&: s2))) (l4 :|: (f3 :&: ((l1 :|: (f0 :&: e')) :&: s2)))
bonrsfrl :: b e e' -> b (l4 :|: (f3 :&: ((l1 :|: (e  :|: r0)) :&: s2))) (l4 :|: (f3 :&: ((l1 :|: (e' :|: r0)) :&: s2)))
bonrsfrr :: b e e' -> b (l4 :|: (f3 :&: ((l1 :|: (l0 :|: e )) :&: s2))) (l4 :|: (f3 :&: ((l1 :|: (l0 :|: e')) :&: s2)))
bonrssff :: b e e' -> b (l4 :|: (f3 :&: (f2 :&: ((e  :&: s0) :&: s1)))) (l4 :|: (f3 :&: (f2 :&: ((e' :&: s0) :&: s1))))
bonrssfs :: b e e' -> b (l4 :|: (f3 :&: (f2 :&: ((f0 :&: e ) :&: s1)))) (l4 :|: (f3 :&: (f2 :&: ((f0 :&: e') :&: s1))))
bonrssfl :: b e e' -> b (l4 :|: (f3 :&: (f2 :&: ((e  :|: r0) :&: s1)))) (l4 :|: (f3 :&: (f2 :&: ((e' :|: r0) :&: s1))))
bonrssfr :: b e e' -> b (l4 :|: (f3 :&: (f2 :&: ((l0 :|: e ) :&: s1)))) (l4 :|: (f3 :&: (f2 :&: ((l0 :|: e') :&: s1))))
bonrsssf :: b e e' -> b (l4 :|: (f3 :&: (f2 :&: (f1 :&: (e  :&: s0))))) (l4 :|: (f3 :&: (f2 :&: (f1 :&: (e' :&: s0)))))
bonrssss :: b e e' -> b (l4 :|: (f3 :&: (f2 :&: (f1 :&: (f0 :&: e ))))) (l4 :|: (f3 :&: (f2 :&: (f1 :&: (f0 :&: e')))))
bonrsssl :: b e e' -> b (l4 :|: (f3 :&: (f2 :&: (f1 :&: (e  :|: r0))))) (l4 :|: (f3 :&: (f2 :&: (f1 :&: (e' :|: r0)))))
bonrsssr :: b e e' -> b (l4 :|: (f3 :&: (f2 :&: (f1 :&: (l0 :|: e ))))) (l4 :|: (f3 :&: (f2 :&: (f1 :&: (l0 :|: e')))))
bonrsslf :: b e e' -> b (l4 :|: (f3 :&: (f2 :&: ((e  :&: s0) :|: r1)))) (l4 :|: (f3 :&: (f2 :&: ((e' :&: s0) :|: r1))))
bonrssls :: b e e' -> b (l4 :|: (f3 :&: (f2 :&: ((f0 :&: e ) :|: r1)))) (l4 :|: (f3 :&: (f2 :&: ((f0 :&: e') :|: r1))))
bonrssll :: b e e' -> b (l4 :|: (f3 :&: (f2 :&: ((e  :|: r0) :|: r1)))) (l4 :|: (f3 :&: (f2 :&: ((e' :|: r0) :|: r1))))
bonrsslr :: b e e' -> b (l4 :|: (f3 :&: (f2 :&: ((l0 :|: e ) :|: r1)))) (l4 :|: (f3 :&: (f2 :&: ((l0 :|: e') :|: r1))))
bonrssrf :: b e e' -> b (l4 :|: (f3 :&: (f2 :&: (l1 :|: (e  :&: s0))))) (l4 :|: (f3 :&: (f2 :&: (l1 :|: (e' :&: s0)))))
bonrssrs :: b e e' -> b (l4 :|: (f3 :&: (f2 :&: (l1 :|: (f0 :&: e ))))) (l4 :|: (f3 :&: (f2 :&: (l1 :|: (f0 :&: e')))))
bonrssrl :: b e e' -> b (l4 :|: (f3 :&: (f2 :&: (l1 :|: (e  :|: r0))))) (l4 :|: (f3 :&: (f2 :&: (l1 :|: (e' :|: r0)))))
bonrssrr :: b e e' -> b (l4 :|: (f3 :&: (f2 :&: (l1 :|: (l0 :|: e ))))) (l4 :|: (f3 :&: (f2 :&: (l1 :|: (l0 :|: e')))))
bonrslff :: b e e' -> b (l4 :|: (f3 :&: (((e  :&: s0) :&: s1) :|: r2))) (l4 :|: (f3 :&: (((e' :&: s0) :&: s1) :|: r2)))
bonrslfs :: b e e' -> b (l4 :|: (f3 :&: (((f0 :&: e ) :&: s1) :|: r2))) (l4 :|: (f3 :&: (((f0 :&: e') :&: s1) :|: r2)))
bonrslfl :: b e e' -> b (l4 :|: (f3 :&: (((e  :|: r0) :&: s1) :|: r2))) (l4 :|: (f3 :&: (((e' :|: r0) :&: s1) :|: r2)))
bonrslfr :: b e e' -> b (l4 :|: (f3 :&: (((l0 :|: e ) :&: s1) :|: r2))) (l4 :|: (f3 :&: (((l0 :|: e') :&: s1) :|: r2)))
bonrslsf :: b e e' -> b (l4 :|: (f3 :&: ((f1 :&: (e  :&: s0)) :|: r2))) (l4 :|: (f3 :&: ((f1 :&: (e' :&: s0)) :|: r2)))
bonrslss :: b e e' -> b (l4 :|: (f3 :&: ((f1 :&: (f0 :&: e )) :|: r2))) (l4 :|: (f3 :&: ((f1 :&: (f0 :&: e')) :|: r2)))
bonrslsl :: b e e' -> b (l4 :|: (f3 :&: ((f1 :&: (e  :|: r0)) :|: r2))) (l4 :|: (f3 :&: ((f1 :&: (e' :|: r0)) :|: r2)))
bonrslsr :: b e e' -> b (l4 :|: (f3 :&: ((f1 :&: (l0 :|: e )) :|: r2))) (l4 :|: (f3 :&: ((f1 :&: (l0 :|: e')) :|: r2)))
bonrsllf :: b e e' -> b (l4 :|: (f3 :&: (((e  :&: s0) :|: r1) :|: r2))) (l4 :|: (f3 :&: (((e' :&: s0) :|: r1) :|: r2)))
bonrslls :: b e e' -> b (l4 :|: (f3 :&: (((f0 :&: e ) :|: r1) :|: r2))) (l4 :|: (f3 :&: (((f0 :&: e') :|: r1) :|: r2)))
bonrslll :: b e e' -> b (l4 :|: (f3 :&: (((e  :|: r0) :|: r1) :|: r2))) (l4 :|: (f3 :&: (((e' :|: r0) :|: r1) :|: r2)))
bonrsllr :: b e e' -> b (l4 :|: (f3 :&: (((l0 :|: e ) :|: r1) :|: r2))) (l4 :|: (f3 :&: (((l0 :|: e') :|: r1) :|: r2)))
bonrslrf :: b e e' -> b (l4 :|: (f3 :&: ((l1 :|: (e  :&: s0)) :|: r2))) (l4 :|: (f3 :&: ((l1 :|: (e' :&: s0)) :|: r2)))
bonrslrs :: b e e' -> b (l4 :|: (f3 :&: ((l1 :|: (f0 :&: e )) :|: r2))) (l4 :|: (f3 :&: ((l1 :|: (f0 :&: e')) :|: r2)))
bonrslrl :: b e e' -> b (l4 :|: (f3 :&: ((l1 :|: (e  :|: r0)) :|: r2))) (l4 :|: (f3 :&: ((l1 :|: (e' :|: r0)) :|: r2)))
bonrslrr :: b e e' -> b (l4 :|: (f3 :&: ((l1 :|: (l0 :|: e )) :|: r2))) (l4 :|: (f3 :&: ((l1 :|: (l0 :|: e')) :|: r2)))
bonrsrff :: b e e' -> b (l4 :|: (f3 :&: (l2 :|: ((e  :&: s0) :&: s1)))) (l4 :|: (f3 :&: (l2 :|: ((e' :&: s0) :&: s1))))
bonrsrfs :: b e e' -> b (l4 :|: (f3 :&: (l2 :|: ((f0 :&: e ) :&: s1)))) (l4 :|: (f3 :&: (l2 :|: ((f0 :&: e') :&: s1))))
bonrsrfl :: b e e' -> b (l4 :|: (f3 :&: (l2 :|: ((e  :|: r0) :&: s1)))) (l4 :|: (f3 :&: (l2 :|: ((e' :|: r0) :&: s1))))
bonrsrfr :: b e e' -> b (l4 :|: (f3 :&: (l2 :|: ((l0 :|: e ) :&: s1)))) (l4 :|: (f3 :&: (l2 :|: ((l0 :|: e') :&: s1))))
bonrsrsf :: b e e' -> b (l4 :|: (f3 :&: (l2 :|: (f1 :&: (e  :&: s0))))) (l4 :|: (f3 :&: (l2 :|: (f1 :&: (e' :&: s0)))))
bonrsrss :: b e e' -> b (l4 :|: (f3 :&: (l2 :|: (f1 :&: (f0 :&: e ))))) (l4 :|: (f3 :&: (l2 :|: (f1 :&: (f0 :&: e')))))
bonrsrsl :: b e e' -> b (l4 :|: (f3 :&: (l2 :|: (f1 :&: (e  :|: r0))))) (l4 :|: (f3 :&: (l2 :|: (f1 :&: (e' :|: r0)))))
bonrsrsr :: b e e' -> b (l4 :|: (f3 :&: (l2 :|: (f1 :&: (l0 :|: e ))))) (l4 :|: (f3 :&: (l2 :|: (f1 :&: (l0 :|: e')))))
bonrsrlf :: b e e' -> b (l4 :|: (f3 :&: (l2 :|: ((e  :&: s0) :|: r1)))) (l4 :|: (f3 :&: (l2 :|: ((e' :&: s0) :|: r1))))
bonrsrls :: b e e' -> b (l4 :|: (f3 :&: (l2 :|: ((f0 :&: e ) :|: r1)))) (l4 :|: (f3 :&: (l2 :|: ((f0 :&: e') :|: r1))))
bonrsrll :: b e e' -> b (l4 :|: (f3 :&: (l2 :|: ((e  :|: r0) :|: r1)))) (l4 :|: (f3 :&: (l2 :|: ((e' :|: r0) :|: r1))))
bonrsrlr :: b e e' -> b (l4 :|: (f3 :&: (l2 :|: ((l0 :|: e ) :|: r1)))) (l4 :|: (f3 :&: (l2 :|: ((l0 :|: e') :|: r1))))
bonrsrrf :: b e e' -> b (l4 :|: (f3 :&: (l2 :|: (l1 :|: (e  :&: s0))))) (l4 :|: (f3 :&: (l2 :|: (l1 :|: (e' :&: s0)))))
bonrsrrs :: b e e' -> b (l4 :|: (f3 :&: (l2 :|: (l1 :|: (f0 :&: e ))))) (l4 :|: (f3 :&: (l2 :|: (l1 :|: (f0 :&: e')))))
bonrsrrl :: b e e' -> b (l4 :|: (f3 :&: (l2 :|: (l1 :|: (e  :|: r0))))) (l4 :|: (f3 :&: (l2 :|: (l1 :|: (e' :|: r0)))))
bonrsrrr :: b e e' -> b (l4 :|: (f3 :&: (l2 :|: (l1 :|: (l0 :|: e ))))) (l4 :|: (f3 :&: (l2 :|: (l1 :|: (l0 :|: e')))))
bonrlfff :: b e e' -> b (l4 :|: ((((e  :&: s0) :&: s1) :&: s2) :|: r3)) (l4 :|: ((((e' :&: s0) :&: s1) :&: s2) :|: r3))
bonrlffs :: b e e' -> b (l4 :|: ((((f0 :&: e ) :&: s1) :&: s2) :|: r3)) (l4 :|: ((((f0 :&: e') :&: s1) :&: s2) :|: r3))
bonrlffl :: b e e' -> b (l4 :|: ((((e  :|: r0) :&: s1) :&: s2) :|: r3)) (l4 :|: ((((e' :|: r0) :&: s1) :&: s2) :|: r3))
bonrlffr :: b e e' -> b (l4 :|: ((((l0 :|: e ) :&: s1) :&: s2) :|: r3)) (l4 :|: ((((l0 :|: e') :&: s1) :&: s2) :|: r3))
bonrlfsf :: b e e' -> b (l4 :|: (((f1 :&: (e  :&: s0)) :&: s2) :|: r3)) (l4 :|: (((f1 :&: (e' :&: s0)) :&: s2) :|: r3))
bonrlfss :: b e e' -> b (l4 :|: (((f1 :&: (f0 :&: e )) :&: s2) :|: r3)) (l4 :|: (((f1 :&: (f0 :&: e')) :&: s2) :|: r3))
bonrlfsl :: b e e' -> b (l4 :|: (((f1 :&: (e  :|: r0)) :&: s2) :|: r3)) (l4 :|: (((f1 :&: (e' :|: r0)) :&: s2) :|: r3))
bonrlfsr :: b e e' -> b (l4 :|: (((f1 :&: (l0 :|: e )) :&: s2) :|: r3)) (l4 :|: (((f1 :&: (l0 :|: e')) :&: s2) :|: r3))
bonrlflf :: b e e' -> b (l4 :|: ((((e  :&: s0) :|: r1) :&: s2) :|: r3)) (l4 :|: ((((e' :&: s0) :|: r1) :&: s2) :|: r3))
bonrlfls :: b e e' -> b (l4 :|: ((((f0 :&: e ) :|: r1) :&: s2) :|: r3)) (l4 :|: ((((f0 :&: e') :|: r1) :&: s2) :|: r3))
bonrlfll :: b e e' -> b (l4 :|: ((((e  :|: r0) :|: r1) :&: s2) :|: r3)) (l4 :|: ((((e' :|: r0) :|: r1) :&: s2) :|: r3))
bonrlflr :: b e e' -> b (l4 :|: ((((l0 :|: e ) :|: r1) :&: s2) :|: r3)) (l4 :|: ((((l0 :|: e') :|: r1) :&: s2) :|: r3))
bonrlfrf :: b e e' -> b (l4 :|: (((l1 :|: (e  :&: s0)) :&: s2) :|: r3)) (l4 :|: (((l1 :|: (e' :&: s0)) :&: s2) :|: r3))
bonrlfrs :: b e e' -> b (l4 :|: (((l1 :|: (f0 :&: e )) :&: s2) :|: r3)) (l4 :|: (((l1 :|: (f0 :&: e')) :&: s2) :|: r3))
bonrlfrl :: b e e' -> b (l4 :|: (((l1 :|: (e  :|: r0)) :&: s2) :|: r3)) (l4 :|: (((l1 :|: (e' :|: r0)) :&: s2) :|: r3))
bonrlfrr :: b e e' -> b (l4 :|: (((l1 :|: (l0 :|: e )) :&: s2) :|: r3)) (l4 :|: (((l1 :|: (l0 :|: e')) :&: s2) :|: r3))
bonrlsff :: b e e' -> b (l4 :|: ((f2 :&: ((e  :&: s0) :&: s1)) :|: r3)) (l4 :|: ((f2 :&: ((e' :&: s0) :&: s1)) :|: r3))
bonrlsfs :: b e e' -> b (l4 :|: ((f2 :&: ((f0 :&: e ) :&: s1)) :|: r3)) (l4 :|: ((f2 :&: ((f0 :&: e') :&: s1)) :|: r3))
bonrlsfl :: b e e' -> b (l4 :|: ((f2 :&: ((e  :|: r0) :&: s1)) :|: r3)) (l4 :|: ((f2 :&: ((e' :|: r0) :&: s1)) :|: r3))
bonrlsfr :: b e e' -> b (l4 :|: ((f2 :&: ((l0 :|: e ) :&: s1)) :|: r3)) (l4 :|: ((f2 :&: ((l0 :|: e') :&: s1)) :|: r3))
bonrlssf :: b e e' -> b (l4 :|: ((f2 :&: (f1 :&: (e  :&: s0))) :|: r3)) (l4 :|: ((f2 :&: (f1 :&: (e' :&: s0))) :|: r3))
bonrlsss :: b e e' -> b (l4 :|: ((f2 :&: (f1 :&: (f0 :&: e ))) :|: r3)) (l4 :|: ((f2 :&: (f1 :&: (f0 :&: e'))) :|: r3))
bonrlssl :: b e e' -> b (l4 :|: ((f2 :&: (f1 :&: (e  :|: r0))) :|: r3)) (l4 :|: ((f2 :&: (f1 :&: (e' :|: r0))) :|: r3))
bonrlssr :: b e e' -> b (l4 :|: ((f2 :&: (f1 :&: (l0 :|: e ))) :|: r3)) (l4 :|: ((f2 :&: (f1 :&: (l0 :|: e'))) :|: r3))
bonrlslf :: b e e' -> b (l4 :|: ((f2 :&: ((e  :&: s0) :|: r1)) :|: r3)) (l4 :|: ((f2 :&: ((e' :&: s0) :|: r1)) :|: r3))
bonrlsls :: b e e' -> b (l4 :|: ((f2 :&: ((f0 :&: e ) :|: r1)) :|: r3)) (l4 :|: ((f2 :&: ((f0 :&: e') :|: r1)) :|: r3))
bonrlsll :: b e e' -> b (l4 :|: ((f2 :&: ((e  :|: r0) :|: r1)) :|: r3)) (l4 :|: ((f2 :&: ((e' :|: r0) :|: r1)) :|: r3))
bonrlslr :: b e e' -> b (l4 :|: ((f2 :&: ((l0 :|: e ) :|: r1)) :|: r3)) (l4 :|: ((f2 :&: ((l0 :|: e') :|: r1)) :|: r3))
bonrlsrf :: b e e' -> b (l4 :|: ((f2 :&: (l1 :|: (e  :&: s0))) :|: r3)) (l4 :|: ((f2 :&: (l1 :|: (e' :&: s0))) :|: r3))
bonrlsrs :: b e e' -> b (l4 :|: ((f2 :&: (l1 :|: (f0 :&: e ))) :|: r3)) (l4 :|: ((f2 :&: (l1 :|: (f0 :&: e'))) :|: r3))
bonrlsrl :: b e e' -> b (l4 :|: ((f2 :&: (l1 :|: (e  :|: r0))) :|: r3)) (l4 :|: ((f2 :&: (l1 :|: (e' :|: r0))) :|: r3))
bonrlsrr :: b e e' -> b (l4 :|: ((f2 :&: (l1 :|: (l0 :|: e ))) :|: r3)) (l4 :|: ((f2 :&: (l1 :|: (l0 :|: e'))) :|: r3))
bonrllff :: b e e' -> b (l4 :|: ((((e  :&: s0) :&: s1) :|: r2) :|: r3)) (l4 :|: ((((e' :&: s0) :&: s1) :|: r2) :|: r3))
bonrllfs :: b e e' -> b (l4 :|: ((((f0 :&: e ) :&: s1) :|: r2) :|: r3)) (l4 :|: ((((f0 :&: e') :&: s1) :|: r2) :|: r3))
bonrllfl :: b e e' -> b (l4 :|: ((((e  :|: r0) :&: s1) :|: r2) :|: r3)) (l4 :|: ((((e' :|: r0) :&: s1) :|: r2) :|: r3))
bonrllfr :: b e e' -> b (l4 :|: ((((l0 :|: e ) :&: s1) :|: r2) :|: r3)) (l4 :|: ((((l0 :|: e') :&: s1) :|: r2) :|: r3))
bonrllsf :: b e e' -> b (l4 :|: (((f1 :&: (e  :&: s0)) :|: r2) :|: r3)) (l4 :|: (((f1 :&: (e' :&: s0)) :|: r2) :|: r3))
bonrllss :: b e e' -> b (l4 :|: (((f1 :&: (f0 :&: e )) :|: r2) :|: r3)) (l4 :|: (((f1 :&: (f0 :&: e')) :|: r2) :|: r3))
bonrllsl :: b e e' -> b (l4 :|: (((f1 :&: (e  :|: r0)) :|: r2) :|: r3)) (l4 :|: (((f1 :&: (e' :|: r0)) :|: r2) :|: r3))
bonrllsr :: b e e' -> b (l4 :|: (((f1 :&: (l0 :|: e )) :|: r2) :|: r3)) (l4 :|: (((f1 :&: (l0 :|: e')) :|: r2) :|: r3))
bonrlllf :: b e e' -> b (l4 :|: ((((e  :&: s0) :|: r1) :|: r2) :|: r3)) (l4 :|: ((((e' :&: s0) :|: r1) :|: r2) :|: r3))
bonrllls :: b e e' -> b (l4 :|: ((((f0 :&: e ) :|: r1) :|: r2) :|: r3)) (l4 :|: ((((f0 :&: e') :|: r1) :|: r2) :|: r3))
bonrllll :: b e e' -> b (l4 :|: ((((e  :|: r0) :|: r1) :|: r2) :|: r3)) (l4 :|: ((((e' :|: r0) :|: r1) :|: r2) :|: r3))
bonrlllr :: b e e' -> b (l4 :|: ((((l0 :|: e ) :|: r1) :|: r2) :|: r3)) (l4 :|: ((((l0 :|: e') :|: r1) :|: r2) :|: r3))
bonrllrf :: b e e' -> b (l4 :|: (((l1 :|: (e  :&: s0)) :|: r2) :|: r3)) (l4 :|: (((l1 :|: (e' :&: s0)) :|: r2) :|: r3))
bonrllrs :: b e e' -> b (l4 :|: (((l1 :|: (f0 :&: e )) :|: r2) :|: r3)) (l4 :|: (((l1 :|: (f0 :&: e')) :|: r2) :|: r3))
bonrllrl :: b e e' -> b (l4 :|: (((l1 :|: (e  :|: r0)) :|: r2) :|: r3)) (l4 :|: (((l1 :|: (e' :|: r0)) :|: r2) :|: r3))
bonrllrr :: b e e' -> b (l4 :|: (((l1 :|: (l0 :|: e )) :|: r2) :|: r3)) (l4 :|: (((l1 :|: (l0 :|: e')) :|: r2) :|: r3))
bonrlrff :: b e e' -> b (l4 :|: ((l2 :|: ((e  :&: s0) :&: s1)) :|: r3)) (l4 :|: ((l2 :|: ((e' :&: s0) :&: s1)) :|: r3))
bonrlrfs :: b e e' -> b (l4 :|: ((l2 :|: ((f0 :&: e ) :&: s1)) :|: r3)) (l4 :|: ((l2 :|: ((f0 :&: e') :&: s1)) :|: r3))
bonrlrfl :: b e e' -> b (l4 :|: ((l2 :|: ((e  :|: r0) :&: s1)) :|: r3)) (l4 :|: ((l2 :|: ((e' :|: r0) :&: s1)) :|: r3))
bonrlrfr :: b e e' -> b (l4 :|: ((l2 :|: ((l0 :|: e ) :&: s1)) :|: r3)) (l4 :|: ((l2 :|: ((l0 :|: e') :&: s1)) :|: r3))
bonrlrsf :: b e e' -> b (l4 :|: ((l2 :|: (f1 :&: (e  :&: s0))) :|: r3)) (l4 :|: ((l2 :|: (f1 :&: (e' :&: s0))) :|: r3))
bonrlrss :: b e e' -> b (l4 :|: ((l2 :|: (f1 :&: (f0 :&: e ))) :|: r3)) (l4 :|: ((l2 :|: (f1 :&: (f0 :&: e'))) :|: r3))
bonrlrsl :: b e e' -> b (l4 :|: ((l2 :|: (f1 :&: (e  :|: r0))) :|: r3)) (l4 :|: ((l2 :|: (f1 :&: (e' :|: r0))) :|: r3))
bonrlrsr :: b e e' -> b (l4 :|: ((l2 :|: (f1 :&: (l0 :|: e ))) :|: r3)) (l4 :|: ((l2 :|: (f1 :&: (l0 :|: e'))) :|: r3))
bonrlrlf :: b e e' -> b (l4 :|: ((l2 :|: ((e  :&: s0) :|: r1)) :|: r3)) (l4 :|: ((l2 :|: ((e' :&: s0) :|: r1)) :|: r3))
bonrlrls :: b e e' -> b (l4 :|: ((l2 :|: ((f0 :&: e ) :|: r1)) :|: r3)) (l4 :|: ((l2 :|: ((f0 :&: e') :|: r1)) :|: r3))
bonrlrll :: b e e' -> b (l4 :|: ((l2 :|: ((e  :|: r0) :|: r1)) :|: r3)) (l4 :|: ((l2 :|: ((e' :|: r0) :|: r1)) :|: r3))
bonrlrlr :: b e e' -> b (l4 :|: ((l2 :|: ((l0 :|: e ) :|: r1)) :|: r3)) (l4 :|: ((l2 :|: ((l0 :|: e') :|: r1)) :|: r3))
bonrlrrf :: b e e' -> b (l4 :|: ((l2 :|: (l1 :|: (e  :&: s0))) :|: r3)) (l4 :|: ((l2 :|: (l1 :|: (e' :&: s0))) :|: r3))
bonrlrrs :: b e e' -> b (l4 :|: ((l2 :|: (l1 :|: (f0 :&: e ))) :|: r3)) (l4 :|: ((l2 :|: (l1 :|: (f0 :&: e'))) :|: r3))
bonrlrrl :: b e e' -> b (l4 :|: ((l2 :|: (l1 :|: (e  :|: r0))) :|: r3)) (l4 :|: ((l2 :|: (l1 :|: (e' :|: r0))) :|: r3))
bonrlrrr :: b e e' -> b (l4 :|: ((l2 :|: (l1 :|: (l0 :|: e ))) :|: r3)) (l4 :|: ((l2 :|: (l1 :|: (l0 :|: e'))) :|: r3))
bonrrfff :: b e e' -> b (l4 :|: (l3 :|: (((e  :&: s0) :&: s1) :&: s2))) (l4 :|: (l3 :|: (((e' :&: s0) :&: s1) :&: s2)))
bonrrffs :: b e e' -> b (l4 :|: (l3 :|: (((f0 :&: e ) :&: s1) :&: s2))) (l4 :|: (l3 :|: (((f0 :&: e') :&: s1) :&: s2)))
bonrrffl :: b e e' -> b (l4 :|: (l3 :|: (((e  :|: r0) :&: s1) :&: s2))) (l4 :|: (l3 :|: (((e' :|: r0) :&: s1) :&: s2)))
bonrrffr :: b e e' -> b (l4 :|: (l3 :|: (((l0 :|: e ) :&: s1) :&: s2))) (l4 :|: (l3 :|: (((l0 :|: e') :&: s1) :&: s2)))
bonrrfsf :: b e e' -> b (l4 :|: (l3 :|: ((f1 :&: (e  :&: s0)) :&: s2))) (l4 :|: (l3 :|: ((f1 :&: (e' :&: s0)) :&: s2)))
bonrrfss :: b e e' -> b (l4 :|: (l3 :|: ((f1 :&: (f0 :&: e )) :&: s2))) (l4 :|: (l3 :|: ((f1 :&: (f0 :&: e')) :&: s2)))
bonrrfsl :: b e e' -> b (l4 :|: (l3 :|: ((f1 :&: (e  :|: r0)) :&: s2))) (l4 :|: (l3 :|: ((f1 :&: (e' :|: r0)) :&: s2)))
bonrrfsr :: b e e' -> b (l4 :|: (l3 :|: ((f1 :&: (l0 :|: e )) :&: s2))) (l4 :|: (l3 :|: ((f1 :&: (l0 :|: e')) :&: s2)))
bonrrflf :: b e e' -> b (l4 :|: (l3 :|: (((e  :&: s0) :|: r1) :&: s2))) (l4 :|: (l3 :|: (((e' :&: s0) :|: r1) :&: s2)))
bonrrfls :: b e e' -> b (l4 :|: (l3 :|: (((f0 :&: e ) :|: r1) :&: s2))) (l4 :|: (l3 :|: (((f0 :&: e') :|: r1) :&: s2)))
bonrrfll :: b e e' -> b (l4 :|: (l3 :|: (((e  :|: r0) :|: r1) :&: s2))) (l4 :|: (l3 :|: (((e' :|: r0) :|: r1) :&: s2)))
bonrrflr :: b e e' -> b (l4 :|: (l3 :|: (((l0 :|: e ) :|: r1) :&: s2))) (l4 :|: (l3 :|: (((l0 :|: e') :|: r1) :&: s2)))
bonrrfrf :: b e e' -> b (l4 :|: (l3 :|: ((l1 :|: (e  :&: s0)) :&: s2))) (l4 :|: (l3 :|: ((l1 :|: (e' :&: s0)) :&: s2)))
bonrrfrs :: b e e' -> b (l4 :|: (l3 :|: ((l1 :|: (f0 :&: e )) :&: s2))) (l4 :|: (l3 :|: ((l1 :|: (f0 :&: e')) :&: s2)))
bonrrfrl :: b e e' -> b (l4 :|: (l3 :|: ((l1 :|: (e  :|: r0)) :&: s2))) (l4 :|: (l3 :|: ((l1 :|: (e' :|: r0)) :&: s2)))
bonrrfrr :: b e e' -> b (l4 :|: (l3 :|: ((l1 :|: (l0 :|: e )) :&: s2))) (l4 :|: (l3 :|: ((l1 :|: (l0 :|: e')) :&: s2)))
bonrrsff :: b e e' -> b (l4 :|: (l3 :|: (f2 :&: ((e  :&: s0) :&: s1)))) (l4 :|: (l3 :|: (f2 :&: ((e' :&: s0) :&: s1))))
bonrrsfs :: b e e' -> b (l4 :|: (l3 :|: (f2 :&: ((f0 :&: e ) :&: s1)))) (l4 :|: (l3 :|: (f2 :&: ((f0 :&: e') :&: s1))))
bonrrsfl :: b e e' -> b (l4 :|: (l3 :|: (f2 :&: ((e  :|: r0) :&: s1)))) (l4 :|: (l3 :|: (f2 :&: ((e' :|: r0) :&: s1))))
bonrrsfr :: b e e' -> b (l4 :|: (l3 :|: (f2 :&: ((l0 :|: e ) :&: s1)))) (l4 :|: (l3 :|: (f2 :&: ((l0 :|: e') :&: s1))))
bonrrssf :: b e e' -> b (l4 :|: (l3 :|: (f2 :&: (f1 :&: (e  :&: s0))))) (l4 :|: (l3 :|: (f2 :&: (f1 :&: (e' :&: s0)))))
bonrrsss :: b e e' -> b (l4 :|: (l3 :|: (f2 :&: (f1 :&: (f0 :&: e ))))) (l4 :|: (l3 :|: (f2 :&: (f1 :&: (f0 :&: e')))))
bonrrssl :: b e e' -> b (l4 :|: (l3 :|: (f2 :&: (f1 :&: (e  :|: r0))))) (l4 :|: (l3 :|: (f2 :&: (f1 :&: (e' :|: r0)))))
bonrrssr :: b e e' -> b (l4 :|: (l3 :|: (f2 :&: (f1 :&: (l0 :|: e ))))) (l4 :|: (l3 :|: (f2 :&: (f1 :&: (l0 :|: e')))))
bonrrslf :: b e e' -> b (l4 :|: (l3 :|: (f2 :&: ((e  :&: s0) :|: r1)))) (l4 :|: (l3 :|: (f2 :&: ((e' :&: s0) :|: r1))))
bonrrsls :: b e e' -> b (l4 :|: (l3 :|: (f2 :&: ((f0 :&: e ) :|: r1)))) (l4 :|: (l3 :|: (f2 :&: ((f0 :&: e') :|: r1))))
bonrrsll :: b e e' -> b (l4 :|: (l3 :|: (f2 :&: ((e  :|: r0) :|: r1)))) (l4 :|: (l3 :|: (f2 :&: ((e' :|: r0) :|: r1))))
bonrrslr :: b e e' -> b (l4 :|: (l3 :|: (f2 :&: ((l0 :|: e ) :|: r1)))) (l4 :|: (l3 :|: (f2 :&: ((l0 :|: e') :|: r1))))
bonrrsrf :: b e e' -> b (l4 :|: (l3 :|: (f2 :&: (l1 :|: (e  :&: s0))))) (l4 :|: (l3 :|: (f2 :&: (l1 :|: (e' :&: s0)))))
bonrrsrs :: b e e' -> b (l4 :|: (l3 :|: (f2 :&: (l1 :|: (f0 :&: e ))))) (l4 :|: (l3 :|: (f2 :&: (l1 :|: (f0 :&: e')))))
bonrrsrl :: b e e' -> b (l4 :|: (l3 :|: (f2 :&: (l1 :|: (e  :|: r0))))) (l4 :|: (l3 :|: (f2 :&: (l1 :|: (e' :|: r0)))))
bonrrsrr :: b e e' -> b (l4 :|: (l3 :|: (f2 :&: (l1 :|: (l0 :|: e ))))) (l4 :|: (l3 :|: (f2 :&: (l1 :|: (l0 :|: e')))))
bonrrlff :: b e e' -> b (l4 :|: (l3 :|: (((e  :&: s0) :&: s1) :|: r2))) (l4 :|: (l3 :|: (((e' :&: s0) :&: s1) :|: r2)))
bonrrlfs :: b e e' -> b (l4 :|: (l3 :|: (((f0 :&: e ) :&: s1) :|: r2))) (l4 :|: (l3 :|: (((f0 :&: e') :&: s1) :|: r2)))
bonrrlfl :: b e e' -> b (l4 :|: (l3 :|: (((e  :|: r0) :&: s1) :|: r2))) (l4 :|: (l3 :|: (((e' :|: r0) :&: s1) :|: r2)))
bonrrlfr :: b e e' -> b (l4 :|: (l3 :|: (((l0 :|: e ) :&: s1) :|: r2))) (l4 :|: (l3 :|: (((l0 :|: e') :&: s1) :|: r2)))
bonrrlsf :: b e e' -> b (l4 :|: (l3 :|: ((f1 :&: (e  :&: s0)) :|: r2))) (l4 :|: (l3 :|: ((f1 :&: (e' :&: s0)) :|: r2)))
bonrrlss :: b e e' -> b (l4 :|: (l3 :|: ((f1 :&: (f0 :&: e )) :|: r2))) (l4 :|: (l3 :|: ((f1 :&: (f0 :&: e')) :|: r2)))
bonrrlsl :: b e e' -> b (l4 :|: (l3 :|: ((f1 :&: (e  :|: r0)) :|: r2))) (l4 :|: (l3 :|: ((f1 :&: (e' :|: r0)) :|: r2)))
bonrrlsr :: b e e' -> b (l4 :|: (l3 :|: ((f1 :&: (l0 :|: e )) :|: r2))) (l4 :|: (l3 :|: ((f1 :&: (l0 :|: e')) :|: r2)))
bonrrllf :: b e e' -> b (l4 :|: (l3 :|: (((e  :&: s0) :|: r1) :|: r2))) (l4 :|: (l3 :|: (((e' :&: s0) :|: r1) :|: r2)))
bonrrlls :: b e e' -> b (l4 :|: (l3 :|: (((f0 :&: e ) :|: r1) :|: r2))) (l4 :|: (l3 :|: (((f0 :&: e') :|: r1) :|: r2)))
bonrrlll :: b e e' -> b (l4 :|: (l3 :|: (((e  :|: r0) :|: r1) :|: r2))) (l4 :|: (l3 :|: (((e' :|: r0) :|: r1) :|: r2)))
bonrrllr :: b e e' -> b (l4 :|: (l3 :|: (((l0 :|: e ) :|: r1) :|: r2))) (l4 :|: (l3 :|: (((l0 :|: e') :|: r1) :|: r2)))
bonrrlrf :: b e e' -> b (l4 :|: (l3 :|: ((l1 :|: (e  :&: s0)) :|: r2))) (l4 :|: (l3 :|: ((l1 :|: (e' :&: s0)) :|: r2)))
bonrrlrs :: b e e' -> b (l4 :|: (l3 :|: ((l1 :|: (f0 :&: e )) :|: r2))) (l4 :|: (l3 :|: ((l1 :|: (f0 :&: e')) :|: r2)))
bonrrlrl :: b e e' -> b (l4 :|: (l3 :|: ((l1 :|: (e  :|: r0)) :|: r2))) (l4 :|: (l3 :|: ((l1 :|: (e' :|: r0)) :|: r2)))
bonrrlrr :: b e e' -> b (l4 :|: (l3 :|: ((l1 :|: (l0 :|: e )) :|: r2))) (l4 :|: (l3 :|: ((l1 :|: (l0 :|: e')) :|: r2)))
bonrrrff :: b e e' -> b (l4 :|: (l3 :|: (l2 :|: ((e  :&: s0) :&: s1)))) (l4 :|: (l3 :|: (l2 :|: ((e' :&: s0) :&: s1))))
bonrrrfs :: b e e' -> b (l4 :|: (l3 :|: (l2 :|: ((f0 :&: e ) :&: s1)))) (l4 :|: (l3 :|: (l2 :|: ((f0 :&: e') :&: s1))))
bonrrrfl :: b e e' -> b (l4 :|: (l3 :|: (l2 :|: ((e  :|: r0) :&: s1)))) (l4 :|: (l3 :|: (l2 :|: ((e' :|: r0) :&: s1))))
bonrrrfr :: b e e' -> b (l4 :|: (l3 :|: (l2 :|: ((l0 :|: e ) :&: s1)))) (l4 :|: (l3 :|: (l2 :|: ((l0 :|: e') :&: s1))))
bonrrrsf :: b e e' -> b (l4 :|: (l3 :|: (l2 :|: (f1 :&: (e  :&: s0))))) (l4 :|: (l3 :|: (l2 :|: (f1 :&: (e' :&: s0)))))
bonrrrss :: b e e' -> b (l4 :|: (l3 :|: (l2 :|: (f1 :&: (f0 :&: e ))))) (l4 :|: (l3 :|: (l2 :|: (f1 :&: (f0 :&: e')))))
bonrrrsl :: b e e' -> b (l4 :|: (l3 :|: (l2 :|: (f1 :&: (e  :|: r0))))) (l4 :|: (l3 :|: (l2 :|: (f1 :&: (e' :|: r0)))))
bonrrrsr :: b e e' -> b (l4 :|: (l3 :|: (l2 :|: (f1 :&: (l0 :|: e ))))) (l4 :|: (l3 :|: (l2 :|: (f1 :&: (l0 :|: e')))))
bonrrrlf :: b e e' -> b (l4 :|: (l3 :|: (l2 :|: ((e  :&: s0) :|: r1)))) (l4 :|: (l3 :|: (l2 :|: ((e' :&: s0) :|: r1))))
bonrrrls :: b e e' -> b (l4 :|: (l3 :|: (l2 :|: ((f0 :&: e ) :|: r1)))) (l4 :|: (l3 :|: (l2 :|: ((f0 :&: e') :|: r1))))
bonrrrll :: b e e' -> b (l4 :|: (l3 :|: (l2 :|: ((e  :|: r0) :|: r1)))) (l4 :|: (l3 :|: (l2 :|: ((e' :|: r0) :|: r1))))
bonrrrlr :: b e e' -> b (l4 :|: (l3 :|: (l2 :|: ((l0 :|: e ) :|: r1)))) (l4 :|: (l3 :|: (l2 :|: ((l0 :|: e') :|: r1))))
bonrrrrf :: b e e' -> b (l4 :|: (l3 :|: (l2 :|: (l1 :|: (e  :&: s0))))) (l4 :|: (l3 :|: (l2 :|: (l1 :|: (e' :&: s0)))))
bonrrrrs :: b e e' -> b (l4 :|: (l3 :|: (l2 :|: (l1 :|: (f0 :&: e ))))) (l4 :|: (l3 :|: (l2 :|: (l1 :|: (f0 :&: e')))))
bonrrrrl :: b e e' -> b (l4 :|: (l3 :|: (l2 :|: (l1 :|: (e  :|: r0))))) (l4 :|: (l3 :|: (l2 :|: (l1 :|: (e' :|: r0)))))
bonrrrrr :: b e e' -> b (l4 :|: (l3 :|: (l2 :|: (l1 :|: (l0 :|: e ))))) (l4 :|: (l3 :|: (l2 :|: (l1 :|: (l0 :|: e')))))

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
bxfffff = bxf >>> bxffff
bxffffs = bxf >>> bxfffs
bxfffsf = bxf >>> bxffsf
bxfffss = bxf >>> bxffss
bxffsff = bxf >>> bxfsff
bxffsfs = bxf >>> bxfsfs
bxffssf = bxf >>> bxfssf
bxffsss = bxf >>> bxfsss
bxfsfff = bxf >>> bxsfff
bxfsffs = bxf >>> bxsffs
bxfsfsf = bxf >>> bxsfsf
bxfsfss = bxf >>> bxsfss
bxfssff = bxf >>> bxssff
bxfssfs = bxf >>> bxssfs
bxfsssf = bxf >>> bxsssf
bxfssss = bxf >>> bxssss
bxsffff = bxs >>> bxffff
bxsfffs = bxs >>> bxfffs
bxsffsf = bxs >>> bxffsf
bxsffss = bxs >>> bxffss
bxsfsff = bxs >>> bxfsff
bxsfsfs = bxs >>> bxfsfs
bxsfssf = bxs >>> bxfssf
bxsfsss = bxs >>> bxfsss
bxssfff = bxs >>> bxsfff
bxssffs = bxs >>> bxsffs
bxssfsf = bxs >>> bxsfsf
bxssfss = bxs >>> bxsfss
bxsssff = bxs >>> bxssff
bxsssfs = bxs >>> bxssfs
bxssssf = bxs >>> bxsssf
bxsssss = bxs >>> bxssss
-- binl already defined in FRP.Sirea.Behavior.
-- binr already defined in FRP.Sirea.Behavior.
binll = binl >>> binl
binlr = binl >>> binr
binrl = binr >>> binl
binrr = binr >>> binr
binlll = binl >>> binll
binllr = binl >>> binlr
binlrl = binl >>> binrl
binlrr = binl >>> binrr
binrll = binr >>> binll
binrlr = binr >>> binlr
binrrl = binr >>> binrl
binrrr = binr >>> binrr
binllll = binl >>> binlll
binlllr = binl >>> binllr
binllrl = binl >>> binlrl
binllrr = binl >>> binlrr
binlrll = binl >>> binrll
binlrlr = binl >>> binrlr
binlrrl = binl >>> binrrl
binlrrr = binl >>> binrrr
binrlll = binr >>> binlll
binrllr = binr >>> binllr
binrlrl = binr >>> binlrl
binrlrr = binr >>> binlrr
binrrll = binr >>> binrll
binrrlr = binr >>> binrlr
binrrrl = binr >>> binrrl
binrrrr = binr >>> binrrr
binlllll = binl >>> binllll
binllllr = binl >>> binlllr
binlllrl = binl >>> binllrl
binlllrr = binl >>> binllrr
binllrll = binl >>> binlrll
binllrlr = binl >>> binlrlr
binllrrl = binl >>> binlrrl
binllrrr = binl >>> binlrrr
binlrlll = binl >>> binrlll
binlrllr = binl >>> binrllr
binlrlrl = binl >>> binrlrl
binlrlrr = binl >>> binrlrr
binlrrll = binl >>> binrrll
binlrrlr = binl >>> binrrlr
binlrrrl = binl >>> binrrrl
binlrrrr = binl >>> binrrrr
binrllll = binr >>> binllll
binrlllr = binr >>> binlllr
binrllrl = binr >>> binllrl
binrllrr = binr >>> binllrr
binrlrll = binr >>> binlrll
binrlrlr = binr >>> binlrlr
binrlrrl = binr >>> binlrrl
binrlrrr = binr >>> binlrrr
binrrlll = binr >>> binrlll
binrrllr = binr >>> binrllr
binrrlrl = binr >>> binrlrl
binrrlrr = binr >>> binrlrr
binrrrll = binr >>> binrrll
binrrrlr = binr >>> binrrlr
binrrrrl = binr >>> binrrrl
binrrrrr = binr >>> binrrrr
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
bonfffff = bonf . bonffff
bonffffs = bonf . bonfffs
bonffffl = bonf . bonfffl
bonffffr = bonf . bonfffr
bonfffsf = bonf . bonffsf
bonfffss = bonf . bonffss
bonfffsl = bonf . bonffsl
bonfffsr = bonf . bonffsr
bonffflf = bonf . bonfflf
bonfffls = bonf . bonffls
bonfffll = bonf . bonffll
bonffflr = bonf . bonfflr
bonfffrf = bonf . bonffrf
bonfffrs = bonf . bonffrs
bonfffrl = bonf . bonffrl
bonfffrr = bonf . bonffrr
bonffsff = bonf . bonfsff
bonffsfs = bonf . bonfsfs
bonffsfl = bonf . bonfsfl
bonffsfr = bonf . bonfsfr
bonffssf = bonf . bonfssf
bonffsss = bonf . bonfsss
bonffssl = bonf . bonfssl
bonffssr = bonf . bonfssr
bonffslf = bonf . bonfslf
bonffsls = bonf . bonfsls
bonffsll = bonf . bonfsll
bonffslr = bonf . bonfslr
bonffsrf = bonf . bonfsrf
bonffsrs = bonf . bonfsrs
bonffsrl = bonf . bonfsrl
bonffsrr = bonf . bonfsrr
bonfflff = bonf . bonflff
bonfflfs = bonf . bonflfs
bonfflfl = bonf . bonflfl
bonfflfr = bonf . bonflfr
bonfflsf = bonf . bonflsf
bonfflss = bonf . bonflss
bonfflsl = bonf . bonflsl
bonfflsr = bonf . bonflsr
bonffllf = bonf . bonfllf
bonfflls = bonf . bonflls
bonfflll = bonf . bonflll
bonffllr = bonf . bonfllr
bonfflrf = bonf . bonflrf
bonfflrs = bonf . bonflrs
bonfflrl = bonf . bonflrl
bonfflrr = bonf . bonflrr
bonffrff = bonf . bonfrff
bonffrfs = bonf . bonfrfs
bonffrfl = bonf . bonfrfl
bonffrfr = bonf . bonfrfr
bonffrsf = bonf . bonfrsf
bonffrss = bonf . bonfrss
bonffrsl = bonf . bonfrsl
bonffrsr = bonf . bonfrsr
bonffrlf = bonf . bonfrlf
bonffrls = bonf . bonfrls
bonffrll = bonf . bonfrll
bonffrlr = bonf . bonfrlr
bonffrrf = bonf . bonfrrf
bonffrrs = bonf . bonfrrs
bonffrrl = bonf . bonfrrl
bonffrrr = bonf . bonfrrr
bonfsfff = bonf . bonsfff
bonfsffs = bonf . bonsffs
bonfsffl = bonf . bonsffl
bonfsffr = bonf . bonsffr
bonfsfsf = bonf . bonsfsf
bonfsfss = bonf . bonsfss
bonfsfsl = bonf . bonsfsl
bonfsfsr = bonf . bonsfsr
bonfsflf = bonf . bonsflf
bonfsfls = bonf . bonsfls
bonfsfll = bonf . bonsfll
bonfsflr = bonf . bonsflr
bonfsfrf = bonf . bonsfrf
bonfsfrs = bonf . bonsfrs
bonfsfrl = bonf . bonsfrl
bonfsfrr = bonf . bonsfrr
bonfssff = bonf . bonssff
bonfssfs = bonf . bonssfs
bonfssfl = bonf . bonssfl
bonfssfr = bonf . bonssfr
bonfsssf = bonf . bonsssf
bonfssss = bonf . bonssss
bonfsssl = bonf . bonsssl
bonfsssr = bonf . bonsssr
bonfsslf = bonf . bonsslf
bonfssls = bonf . bonssls
bonfssll = bonf . bonssll
bonfsslr = bonf . bonsslr
bonfssrf = bonf . bonssrf
bonfssrs = bonf . bonssrs
bonfssrl = bonf . bonssrl
bonfssrr = bonf . bonssrr
bonfslff = bonf . bonslff
bonfslfs = bonf . bonslfs
bonfslfl = bonf . bonslfl
bonfslfr = bonf . bonslfr
bonfslsf = bonf . bonslsf
bonfslss = bonf . bonslss
bonfslsl = bonf . bonslsl
bonfslsr = bonf . bonslsr
bonfsllf = bonf . bonsllf
bonfslls = bonf . bonslls
bonfslll = bonf . bonslll
bonfsllr = bonf . bonsllr
bonfslrf = bonf . bonslrf
bonfslrs = bonf . bonslrs
bonfslrl = bonf . bonslrl
bonfslrr = bonf . bonslrr
bonfsrff = bonf . bonsrff
bonfsrfs = bonf . bonsrfs
bonfsrfl = bonf . bonsrfl
bonfsrfr = bonf . bonsrfr
bonfsrsf = bonf . bonsrsf
bonfsrss = bonf . bonsrss
bonfsrsl = bonf . bonsrsl
bonfsrsr = bonf . bonsrsr
bonfsrlf = bonf . bonsrlf
bonfsrls = bonf . bonsrls
bonfsrll = bonf . bonsrll
bonfsrlr = bonf . bonsrlr
bonfsrrf = bonf . bonsrrf
bonfsrrs = bonf . bonsrrs
bonfsrrl = bonf . bonsrrl
bonfsrrr = bonf . bonsrrr
bonflfff = bonf . bonlfff
bonflffs = bonf . bonlffs
bonflffl = bonf . bonlffl
bonflffr = bonf . bonlffr
bonflfsf = bonf . bonlfsf
bonflfss = bonf . bonlfss
bonflfsl = bonf . bonlfsl
bonflfsr = bonf . bonlfsr
bonflflf = bonf . bonlflf
bonflfls = bonf . bonlfls
bonflfll = bonf . bonlfll
bonflflr = bonf . bonlflr
bonflfrf = bonf . bonlfrf
bonflfrs = bonf . bonlfrs
bonflfrl = bonf . bonlfrl
bonflfrr = bonf . bonlfrr
bonflsff = bonf . bonlsff
bonflsfs = bonf . bonlsfs
bonflsfl = bonf . bonlsfl
bonflsfr = bonf . bonlsfr
bonflssf = bonf . bonlssf
bonflsss = bonf . bonlsss
bonflssl = bonf . bonlssl
bonflssr = bonf . bonlssr
bonflslf = bonf . bonlslf
bonflsls = bonf . bonlsls
bonflsll = bonf . bonlsll
bonflslr = bonf . bonlslr
bonflsrf = bonf . bonlsrf
bonflsrs = bonf . bonlsrs
bonflsrl = bonf . bonlsrl
bonflsrr = bonf . bonlsrr
bonfllff = bonf . bonllff
bonfllfs = bonf . bonllfs
bonfllfl = bonf . bonllfl
bonfllfr = bonf . bonllfr
bonfllsf = bonf . bonllsf
bonfllss = bonf . bonllss
bonfllsl = bonf . bonllsl
bonfllsr = bonf . bonllsr
bonflllf = bonf . bonlllf
bonfllls = bonf . bonllls
bonfllll = bonf . bonllll
bonflllr = bonf . bonlllr
bonfllrf = bonf . bonllrf
bonfllrs = bonf . bonllrs
bonfllrl = bonf . bonllrl
bonfllrr = bonf . bonllrr
bonflrff = bonf . bonlrff
bonflrfs = bonf . bonlrfs
bonflrfl = bonf . bonlrfl
bonflrfr = bonf . bonlrfr
bonflrsf = bonf . bonlrsf
bonflrss = bonf . bonlrss
bonflrsl = bonf . bonlrsl
bonflrsr = bonf . bonlrsr
bonflrlf = bonf . bonlrlf
bonflrls = bonf . bonlrls
bonflrll = bonf . bonlrll
bonflrlr = bonf . bonlrlr
bonflrrf = bonf . bonlrrf
bonflrrs = bonf . bonlrrs
bonflrrl = bonf . bonlrrl
bonflrrr = bonf . bonlrrr
bonfrfff = bonf . bonrfff
bonfrffs = bonf . bonrffs
bonfrffl = bonf . bonrffl
bonfrffr = bonf . bonrffr
bonfrfsf = bonf . bonrfsf
bonfrfss = bonf . bonrfss
bonfrfsl = bonf . bonrfsl
bonfrfsr = bonf . bonrfsr
bonfrflf = bonf . bonrflf
bonfrfls = bonf . bonrfls
bonfrfll = bonf . bonrfll
bonfrflr = bonf . bonrflr
bonfrfrf = bonf . bonrfrf
bonfrfrs = bonf . bonrfrs
bonfrfrl = bonf . bonrfrl
bonfrfrr = bonf . bonrfrr
bonfrsff = bonf . bonrsff
bonfrsfs = bonf . bonrsfs
bonfrsfl = bonf . bonrsfl
bonfrsfr = bonf . bonrsfr
bonfrssf = bonf . bonrssf
bonfrsss = bonf . bonrsss
bonfrssl = bonf . bonrssl
bonfrssr = bonf . bonrssr
bonfrslf = bonf . bonrslf
bonfrsls = bonf . bonrsls
bonfrsll = bonf . bonrsll
bonfrslr = bonf . bonrslr
bonfrsrf = bonf . bonrsrf
bonfrsrs = bonf . bonrsrs
bonfrsrl = bonf . bonrsrl
bonfrsrr = bonf . bonrsrr
bonfrlff = bonf . bonrlff
bonfrlfs = bonf . bonrlfs
bonfrlfl = bonf . bonrlfl
bonfrlfr = bonf . bonrlfr
bonfrlsf = bonf . bonrlsf
bonfrlss = bonf . bonrlss
bonfrlsl = bonf . bonrlsl
bonfrlsr = bonf . bonrlsr
bonfrllf = bonf . bonrllf
bonfrlls = bonf . bonrlls
bonfrlll = bonf . bonrlll
bonfrllr = bonf . bonrllr
bonfrlrf = bonf . bonrlrf
bonfrlrs = bonf . bonrlrs
bonfrlrl = bonf . bonrlrl
bonfrlrr = bonf . bonrlrr
bonfrrff = bonf . bonrrff
bonfrrfs = bonf . bonrrfs
bonfrrfl = bonf . bonrrfl
bonfrrfr = bonf . bonrrfr
bonfrrsf = bonf . bonrrsf
bonfrrss = bonf . bonrrss
bonfrrsl = bonf . bonrrsl
bonfrrsr = bonf . bonrrsr
bonfrrlf = bonf . bonrrlf
bonfrrls = bonf . bonrrls
bonfrrll = bonf . bonrrll
bonfrrlr = bonf . bonrrlr
bonfrrrf = bonf . bonrrrf
bonfrrrs = bonf . bonrrrs
bonfrrrl = bonf . bonrrrl
bonfrrrr = bonf . bonrrrr
bonsffff = bons . bonffff
bonsfffs = bons . bonfffs
bonsfffl = bons . bonfffl
bonsfffr = bons . bonfffr
bonsffsf = bons . bonffsf
bonsffss = bons . bonffss
bonsffsl = bons . bonffsl
bonsffsr = bons . bonffsr
bonsfflf = bons . bonfflf
bonsffls = bons . bonffls
bonsffll = bons . bonffll
bonsfflr = bons . bonfflr
bonsffrf = bons . bonffrf
bonsffrs = bons . bonffrs
bonsffrl = bons . bonffrl
bonsffrr = bons . bonffrr
bonsfsff = bons . bonfsff
bonsfsfs = bons . bonfsfs
bonsfsfl = bons . bonfsfl
bonsfsfr = bons . bonfsfr
bonsfssf = bons . bonfssf
bonsfsss = bons . bonfsss
bonsfssl = bons . bonfssl
bonsfssr = bons . bonfssr
bonsfslf = bons . bonfslf
bonsfsls = bons . bonfsls
bonsfsll = bons . bonfsll
bonsfslr = bons . bonfslr
bonsfsrf = bons . bonfsrf
bonsfsrs = bons . bonfsrs
bonsfsrl = bons . bonfsrl
bonsfsrr = bons . bonfsrr
bonsflff = bons . bonflff
bonsflfs = bons . bonflfs
bonsflfl = bons . bonflfl
bonsflfr = bons . bonflfr
bonsflsf = bons . bonflsf
bonsflss = bons . bonflss
bonsflsl = bons . bonflsl
bonsflsr = bons . bonflsr
bonsfllf = bons . bonfllf
bonsflls = bons . bonflls
bonsflll = bons . bonflll
bonsfllr = bons . bonfllr
bonsflrf = bons . bonflrf
bonsflrs = bons . bonflrs
bonsflrl = bons . bonflrl
bonsflrr = bons . bonflrr
bonsfrff = bons . bonfrff
bonsfrfs = bons . bonfrfs
bonsfrfl = bons . bonfrfl
bonsfrfr = bons . bonfrfr
bonsfrsf = bons . bonfrsf
bonsfrss = bons . bonfrss
bonsfrsl = bons . bonfrsl
bonsfrsr = bons . bonfrsr
bonsfrlf = bons . bonfrlf
bonsfrls = bons . bonfrls
bonsfrll = bons . bonfrll
bonsfrlr = bons . bonfrlr
bonsfrrf = bons . bonfrrf
bonsfrrs = bons . bonfrrs
bonsfrrl = bons . bonfrrl
bonsfrrr = bons . bonfrrr
bonssfff = bons . bonsfff
bonssffs = bons . bonsffs
bonssffl = bons . bonsffl
bonssffr = bons . bonsffr
bonssfsf = bons . bonsfsf
bonssfss = bons . bonsfss
bonssfsl = bons . bonsfsl
bonssfsr = bons . bonsfsr
bonssflf = bons . bonsflf
bonssfls = bons . bonsfls
bonssfll = bons . bonsfll
bonssflr = bons . bonsflr
bonssfrf = bons . bonsfrf
bonssfrs = bons . bonsfrs
bonssfrl = bons . bonsfrl
bonssfrr = bons . bonsfrr
bonsssff = bons . bonssff
bonsssfs = bons . bonssfs
bonsssfl = bons . bonssfl
bonsssfr = bons . bonssfr
bonssssf = bons . bonsssf
bonsssss = bons . bonssss
bonssssl = bons . bonsssl
bonssssr = bons . bonsssr
bonssslf = bons . bonsslf
bonsssls = bons . bonssls
bonsssll = bons . bonssll
bonssslr = bons . bonsslr
bonsssrf = bons . bonssrf
bonsssrs = bons . bonssrs
bonsssrl = bons . bonssrl
bonsssrr = bons . bonssrr
bonsslff = bons . bonslff
bonsslfs = bons . bonslfs
bonsslfl = bons . bonslfl
bonsslfr = bons . bonslfr
bonsslsf = bons . bonslsf
bonsslss = bons . bonslss
bonsslsl = bons . bonslsl
bonsslsr = bons . bonslsr
bonssllf = bons . bonsllf
bonsslls = bons . bonslls
bonsslll = bons . bonslll
bonssllr = bons . bonsllr
bonsslrf = bons . bonslrf
bonsslrs = bons . bonslrs
bonsslrl = bons . bonslrl
bonsslrr = bons . bonslrr
bonssrff = bons . bonsrff
bonssrfs = bons . bonsrfs
bonssrfl = bons . bonsrfl
bonssrfr = bons . bonsrfr
bonssrsf = bons . bonsrsf
bonssrss = bons . bonsrss
bonssrsl = bons . bonsrsl
bonssrsr = bons . bonsrsr
bonssrlf = bons . bonsrlf
bonssrls = bons . bonsrls
bonssrll = bons . bonsrll
bonssrlr = bons . bonsrlr
bonssrrf = bons . bonsrrf
bonssrrs = bons . bonsrrs
bonssrrl = bons . bonsrrl
bonssrrr = bons . bonsrrr
bonslfff = bons . bonlfff
bonslffs = bons . bonlffs
bonslffl = bons . bonlffl
bonslffr = bons . bonlffr
bonslfsf = bons . bonlfsf
bonslfss = bons . bonlfss
bonslfsl = bons . bonlfsl
bonslfsr = bons . bonlfsr
bonslflf = bons . bonlflf
bonslfls = bons . bonlfls
bonslfll = bons . bonlfll
bonslflr = bons . bonlflr
bonslfrf = bons . bonlfrf
bonslfrs = bons . bonlfrs
bonslfrl = bons . bonlfrl
bonslfrr = bons . bonlfrr
bonslsff = bons . bonlsff
bonslsfs = bons . bonlsfs
bonslsfl = bons . bonlsfl
bonslsfr = bons . bonlsfr
bonslssf = bons . bonlssf
bonslsss = bons . bonlsss
bonslssl = bons . bonlssl
bonslssr = bons . bonlssr
bonslslf = bons . bonlslf
bonslsls = bons . bonlsls
bonslsll = bons . bonlsll
bonslslr = bons . bonlslr
bonslsrf = bons . bonlsrf
bonslsrs = bons . bonlsrs
bonslsrl = bons . bonlsrl
bonslsrr = bons . bonlsrr
bonsllff = bons . bonllff
bonsllfs = bons . bonllfs
bonsllfl = bons . bonllfl
bonsllfr = bons . bonllfr
bonsllsf = bons . bonllsf
bonsllss = bons . bonllss
bonsllsl = bons . bonllsl
bonsllsr = bons . bonllsr
bonslllf = bons . bonlllf
bonsllls = bons . bonllls
bonsllll = bons . bonllll
bonslllr = bons . bonlllr
bonsllrf = bons . bonllrf
bonsllrs = bons . bonllrs
bonsllrl = bons . bonllrl
bonsllrr = bons . bonllrr
bonslrff = bons . bonlrff
bonslrfs = bons . bonlrfs
bonslrfl = bons . bonlrfl
bonslrfr = bons . bonlrfr
bonslrsf = bons . bonlrsf
bonslrss = bons . bonlrss
bonslrsl = bons . bonlrsl
bonslrsr = bons . bonlrsr
bonslrlf = bons . bonlrlf
bonslrls = bons . bonlrls
bonslrll = bons . bonlrll
bonslrlr = bons . bonlrlr
bonslrrf = bons . bonlrrf
bonslrrs = bons . bonlrrs
bonslrrl = bons . bonlrrl
bonslrrr = bons . bonlrrr
bonsrfff = bons . bonrfff
bonsrffs = bons . bonrffs
bonsrffl = bons . bonrffl
bonsrffr = bons . bonrffr
bonsrfsf = bons . bonrfsf
bonsrfss = bons . bonrfss
bonsrfsl = bons . bonrfsl
bonsrfsr = bons . bonrfsr
bonsrflf = bons . bonrflf
bonsrfls = bons . bonrfls
bonsrfll = bons . bonrfll
bonsrflr = bons . bonrflr
bonsrfrf = bons . bonrfrf
bonsrfrs = bons . bonrfrs
bonsrfrl = bons . bonrfrl
bonsrfrr = bons . bonrfrr
bonsrsff = bons . bonrsff
bonsrsfs = bons . bonrsfs
bonsrsfl = bons . bonrsfl
bonsrsfr = bons . bonrsfr
bonsrssf = bons . bonrssf
bonsrsss = bons . bonrsss
bonsrssl = bons . bonrssl
bonsrssr = bons . bonrssr
bonsrslf = bons . bonrslf
bonsrsls = bons . bonrsls
bonsrsll = bons . bonrsll
bonsrslr = bons . bonrslr
bonsrsrf = bons . bonrsrf
bonsrsrs = bons . bonrsrs
bonsrsrl = bons . bonrsrl
bonsrsrr = bons . bonrsrr
bonsrlff = bons . bonrlff
bonsrlfs = bons . bonrlfs
bonsrlfl = bons . bonrlfl
bonsrlfr = bons . bonrlfr
bonsrlsf = bons . bonrlsf
bonsrlss = bons . bonrlss
bonsrlsl = bons . bonrlsl
bonsrlsr = bons . bonrlsr
bonsrllf = bons . bonrllf
bonsrlls = bons . bonrlls
bonsrlll = bons . bonrlll
bonsrllr = bons . bonrllr
bonsrlrf = bons . bonrlrf
bonsrlrs = bons . bonrlrs
bonsrlrl = bons . bonrlrl
bonsrlrr = bons . bonrlrr
bonsrrff = bons . bonrrff
bonsrrfs = bons . bonrrfs
bonsrrfl = bons . bonrrfl
bonsrrfr = bons . bonrrfr
bonsrrsf = bons . bonrrsf
bonsrrss = bons . bonrrss
bonsrrsl = bons . bonrrsl
bonsrrsr = bons . bonrrsr
bonsrrlf = bons . bonrrlf
bonsrrls = bons . bonrrls
bonsrrll = bons . bonrrll
bonsrrlr = bons . bonrrlr
bonsrrrf = bons . bonrrrf
bonsrrrs = bons . bonrrrs
bonsrrrl = bons . bonrrrl
bonsrrrr = bons . bonrrrr
bonlffff = bonl . bonffff
bonlfffs = bonl . bonfffs
bonlfffl = bonl . bonfffl
bonlfffr = bonl . bonfffr
bonlffsf = bonl . bonffsf
bonlffss = bonl . bonffss
bonlffsl = bonl . bonffsl
bonlffsr = bonl . bonffsr
bonlfflf = bonl . bonfflf
bonlffls = bonl . bonffls
bonlffll = bonl . bonffll
bonlfflr = bonl . bonfflr
bonlffrf = bonl . bonffrf
bonlffrs = bonl . bonffrs
bonlffrl = bonl . bonffrl
bonlffrr = bonl . bonffrr
bonlfsff = bonl . bonfsff
bonlfsfs = bonl . bonfsfs
bonlfsfl = bonl . bonfsfl
bonlfsfr = bonl . bonfsfr
bonlfssf = bonl . bonfssf
bonlfsss = bonl . bonfsss
bonlfssl = bonl . bonfssl
bonlfssr = bonl . bonfssr
bonlfslf = bonl . bonfslf
bonlfsls = bonl . bonfsls
bonlfsll = bonl . bonfsll
bonlfslr = bonl . bonfslr
bonlfsrf = bonl . bonfsrf
bonlfsrs = bonl . bonfsrs
bonlfsrl = bonl . bonfsrl
bonlfsrr = bonl . bonfsrr
bonlflff = bonl . bonflff
bonlflfs = bonl . bonflfs
bonlflfl = bonl . bonflfl
bonlflfr = bonl . bonflfr
bonlflsf = bonl . bonflsf
bonlflss = bonl . bonflss
bonlflsl = bonl . bonflsl
bonlflsr = bonl . bonflsr
bonlfllf = bonl . bonfllf
bonlflls = bonl . bonflls
bonlflll = bonl . bonflll
bonlfllr = bonl . bonfllr
bonlflrf = bonl . bonflrf
bonlflrs = bonl . bonflrs
bonlflrl = bonl . bonflrl
bonlflrr = bonl . bonflrr
bonlfrff = bonl . bonfrff
bonlfrfs = bonl . bonfrfs
bonlfrfl = bonl . bonfrfl
bonlfrfr = bonl . bonfrfr
bonlfrsf = bonl . bonfrsf
bonlfrss = bonl . bonfrss
bonlfrsl = bonl . bonfrsl
bonlfrsr = bonl . bonfrsr
bonlfrlf = bonl . bonfrlf
bonlfrls = bonl . bonfrls
bonlfrll = bonl . bonfrll
bonlfrlr = bonl . bonfrlr
bonlfrrf = bonl . bonfrrf
bonlfrrs = bonl . bonfrrs
bonlfrrl = bonl . bonfrrl
bonlfrrr = bonl . bonfrrr
bonlsfff = bonl . bonsfff
bonlsffs = bonl . bonsffs
bonlsffl = bonl . bonsffl
bonlsffr = bonl . bonsffr
bonlsfsf = bonl . bonsfsf
bonlsfss = bonl . bonsfss
bonlsfsl = bonl . bonsfsl
bonlsfsr = bonl . bonsfsr
bonlsflf = bonl . bonsflf
bonlsfls = bonl . bonsfls
bonlsfll = bonl . bonsfll
bonlsflr = bonl . bonsflr
bonlsfrf = bonl . bonsfrf
bonlsfrs = bonl . bonsfrs
bonlsfrl = bonl . bonsfrl
bonlsfrr = bonl . bonsfrr
bonlssff = bonl . bonssff
bonlssfs = bonl . bonssfs
bonlssfl = bonl . bonssfl
bonlssfr = bonl . bonssfr
bonlsssf = bonl . bonsssf
bonlssss = bonl . bonssss
bonlsssl = bonl . bonsssl
bonlsssr = bonl . bonsssr
bonlsslf = bonl . bonsslf
bonlssls = bonl . bonssls
bonlssll = bonl . bonssll
bonlsslr = bonl . bonsslr
bonlssrf = bonl . bonssrf
bonlssrs = bonl . bonssrs
bonlssrl = bonl . bonssrl
bonlssrr = bonl . bonssrr
bonlslff = bonl . bonslff
bonlslfs = bonl . bonslfs
bonlslfl = bonl . bonslfl
bonlslfr = bonl . bonslfr
bonlslsf = bonl . bonslsf
bonlslss = bonl . bonslss
bonlslsl = bonl . bonslsl
bonlslsr = bonl . bonslsr
bonlsllf = bonl . bonsllf
bonlslls = bonl . bonslls
bonlslll = bonl . bonslll
bonlsllr = bonl . bonsllr
bonlslrf = bonl . bonslrf
bonlslrs = bonl . bonslrs
bonlslrl = bonl . bonslrl
bonlslrr = bonl . bonslrr
bonlsrff = bonl . bonsrff
bonlsrfs = bonl . bonsrfs
bonlsrfl = bonl . bonsrfl
bonlsrfr = bonl . bonsrfr
bonlsrsf = bonl . bonsrsf
bonlsrss = bonl . bonsrss
bonlsrsl = bonl . bonsrsl
bonlsrsr = bonl . bonsrsr
bonlsrlf = bonl . bonsrlf
bonlsrls = bonl . bonsrls
bonlsrll = bonl . bonsrll
bonlsrlr = bonl . bonsrlr
bonlsrrf = bonl . bonsrrf
bonlsrrs = bonl . bonsrrs
bonlsrrl = bonl . bonsrrl
bonlsrrr = bonl . bonsrrr
bonllfff = bonl . bonlfff
bonllffs = bonl . bonlffs
bonllffl = bonl . bonlffl
bonllffr = bonl . bonlffr
bonllfsf = bonl . bonlfsf
bonllfss = bonl . bonlfss
bonllfsl = bonl . bonlfsl
bonllfsr = bonl . bonlfsr
bonllflf = bonl . bonlflf
bonllfls = bonl . bonlfls
bonllfll = bonl . bonlfll
bonllflr = bonl . bonlflr
bonllfrf = bonl . bonlfrf
bonllfrs = bonl . bonlfrs
bonllfrl = bonl . bonlfrl
bonllfrr = bonl . bonlfrr
bonllsff = bonl . bonlsff
bonllsfs = bonl . bonlsfs
bonllsfl = bonl . bonlsfl
bonllsfr = bonl . bonlsfr
bonllssf = bonl . bonlssf
bonllsss = bonl . bonlsss
bonllssl = bonl . bonlssl
bonllssr = bonl . bonlssr
bonllslf = bonl . bonlslf
bonllsls = bonl . bonlsls
bonllsll = bonl . bonlsll
bonllslr = bonl . bonlslr
bonllsrf = bonl . bonlsrf
bonllsrs = bonl . bonlsrs
bonllsrl = bonl . bonlsrl
bonllsrr = bonl . bonlsrr
bonlllff = bonl . bonllff
bonlllfs = bonl . bonllfs
bonlllfl = bonl . bonllfl
bonlllfr = bonl . bonllfr
bonlllsf = bonl . bonllsf
bonlllss = bonl . bonllss
bonlllsl = bonl . bonllsl
bonlllsr = bonl . bonllsr
bonllllf = bonl . bonlllf
bonlllls = bonl . bonllls
bonlllll = bonl . bonllll
bonllllr = bonl . bonlllr
bonlllrf = bonl . bonllrf
bonlllrs = bonl . bonllrs
bonlllrl = bonl . bonllrl
bonlllrr = bonl . bonllrr
bonllrff = bonl . bonlrff
bonllrfs = bonl . bonlrfs
bonllrfl = bonl . bonlrfl
bonllrfr = bonl . bonlrfr
bonllrsf = bonl . bonlrsf
bonllrss = bonl . bonlrss
bonllrsl = bonl . bonlrsl
bonllrsr = bonl . bonlrsr
bonllrlf = bonl . bonlrlf
bonllrls = bonl . bonlrls
bonllrll = bonl . bonlrll
bonllrlr = bonl . bonlrlr
bonllrrf = bonl . bonlrrf
bonllrrs = bonl . bonlrrs
bonllrrl = bonl . bonlrrl
bonllrrr = bonl . bonlrrr
bonlrfff = bonl . bonrfff
bonlrffs = bonl . bonrffs
bonlrffl = bonl . bonrffl
bonlrffr = bonl . bonrffr
bonlrfsf = bonl . bonrfsf
bonlrfss = bonl . bonrfss
bonlrfsl = bonl . bonrfsl
bonlrfsr = bonl . bonrfsr
bonlrflf = bonl . bonrflf
bonlrfls = bonl . bonrfls
bonlrfll = bonl . bonrfll
bonlrflr = bonl . bonrflr
bonlrfrf = bonl . bonrfrf
bonlrfrs = bonl . bonrfrs
bonlrfrl = bonl . bonrfrl
bonlrfrr = bonl . bonrfrr
bonlrsff = bonl . bonrsff
bonlrsfs = bonl . bonrsfs
bonlrsfl = bonl . bonrsfl
bonlrsfr = bonl . bonrsfr
bonlrssf = bonl . bonrssf
bonlrsss = bonl . bonrsss
bonlrssl = bonl . bonrssl
bonlrssr = bonl . bonrssr
bonlrslf = bonl . bonrslf
bonlrsls = bonl . bonrsls
bonlrsll = bonl . bonrsll
bonlrslr = bonl . bonrslr
bonlrsrf = bonl . bonrsrf
bonlrsrs = bonl . bonrsrs
bonlrsrl = bonl . bonrsrl
bonlrsrr = bonl . bonrsrr
bonlrlff = bonl . bonrlff
bonlrlfs = bonl . bonrlfs
bonlrlfl = bonl . bonrlfl
bonlrlfr = bonl . bonrlfr
bonlrlsf = bonl . bonrlsf
bonlrlss = bonl . bonrlss
bonlrlsl = bonl . bonrlsl
bonlrlsr = bonl . bonrlsr
bonlrllf = bonl . bonrllf
bonlrlls = bonl . bonrlls
bonlrlll = bonl . bonrlll
bonlrllr = bonl . bonrllr
bonlrlrf = bonl . bonrlrf
bonlrlrs = bonl . bonrlrs
bonlrlrl = bonl . bonrlrl
bonlrlrr = bonl . bonrlrr
bonlrrff = bonl . bonrrff
bonlrrfs = bonl . bonrrfs
bonlrrfl = bonl . bonrrfl
bonlrrfr = bonl . bonrrfr
bonlrrsf = bonl . bonrrsf
bonlrrss = bonl . bonrrss
bonlrrsl = bonl . bonrrsl
bonlrrsr = bonl . bonrrsr
bonlrrlf = bonl . bonrrlf
bonlrrls = bonl . bonrrls
bonlrrll = bonl . bonrrll
bonlrrlr = bonl . bonrrlr
bonlrrrf = bonl . bonrrrf
bonlrrrs = bonl . bonrrrs
bonlrrrl = bonl . bonrrrl
bonlrrrr = bonl . bonrrrr
bonrffff = bonr . bonffff
bonrfffs = bonr . bonfffs
bonrfffl = bonr . bonfffl
bonrfffr = bonr . bonfffr
bonrffsf = bonr . bonffsf
bonrffss = bonr . bonffss
bonrffsl = bonr . bonffsl
bonrffsr = bonr . bonffsr
bonrfflf = bonr . bonfflf
bonrffls = bonr . bonffls
bonrffll = bonr . bonffll
bonrfflr = bonr . bonfflr
bonrffrf = bonr . bonffrf
bonrffrs = bonr . bonffrs
bonrffrl = bonr . bonffrl
bonrffrr = bonr . bonffrr
bonrfsff = bonr . bonfsff
bonrfsfs = bonr . bonfsfs
bonrfsfl = bonr . bonfsfl
bonrfsfr = bonr . bonfsfr
bonrfssf = bonr . bonfssf
bonrfsss = bonr . bonfsss
bonrfssl = bonr . bonfssl
bonrfssr = bonr . bonfssr
bonrfslf = bonr . bonfslf
bonrfsls = bonr . bonfsls
bonrfsll = bonr . bonfsll
bonrfslr = bonr . bonfslr
bonrfsrf = bonr . bonfsrf
bonrfsrs = bonr . bonfsrs
bonrfsrl = bonr . bonfsrl
bonrfsrr = bonr . bonfsrr
bonrflff = bonr . bonflff
bonrflfs = bonr . bonflfs
bonrflfl = bonr . bonflfl
bonrflfr = bonr . bonflfr
bonrflsf = bonr . bonflsf
bonrflss = bonr . bonflss
bonrflsl = bonr . bonflsl
bonrflsr = bonr . bonflsr
bonrfllf = bonr . bonfllf
bonrflls = bonr . bonflls
bonrflll = bonr . bonflll
bonrfllr = bonr . bonfllr
bonrflrf = bonr . bonflrf
bonrflrs = bonr . bonflrs
bonrflrl = bonr . bonflrl
bonrflrr = bonr . bonflrr
bonrfrff = bonr . bonfrff
bonrfrfs = bonr . bonfrfs
bonrfrfl = bonr . bonfrfl
bonrfrfr = bonr . bonfrfr
bonrfrsf = bonr . bonfrsf
bonrfrss = bonr . bonfrss
bonrfrsl = bonr . bonfrsl
bonrfrsr = bonr . bonfrsr
bonrfrlf = bonr . bonfrlf
bonrfrls = bonr . bonfrls
bonrfrll = bonr . bonfrll
bonrfrlr = bonr . bonfrlr
bonrfrrf = bonr . bonfrrf
bonrfrrs = bonr . bonfrrs
bonrfrrl = bonr . bonfrrl
bonrfrrr = bonr . bonfrrr
bonrsfff = bonr . bonsfff
bonrsffs = bonr . bonsffs
bonrsffl = bonr . bonsffl
bonrsffr = bonr . bonsffr
bonrsfsf = bonr . bonsfsf
bonrsfss = bonr . bonsfss
bonrsfsl = bonr . bonsfsl
bonrsfsr = bonr . bonsfsr
bonrsflf = bonr . bonsflf
bonrsfls = bonr . bonsfls
bonrsfll = bonr . bonsfll
bonrsflr = bonr . bonsflr
bonrsfrf = bonr . bonsfrf
bonrsfrs = bonr . bonsfrs
bonrsfrl = bonr . bonsfrl
bonrsfrr = bonr . bonsfrr
bonrssff = bonr . bonssff
bonrssfs = bonr . bonssfs
bonrssfl = bonr . bonssfl
bonrssfr = bonr . bonssfr
bonrsssf = bonr . bonsssf
bonrssss = bonr . bonssss
bonrsssl = bonr . bonsssl
bonrsssr = bonr . bonsssr
bonrsslf = bonr . bonsslf
bonrssls = bonr . bonssls
bonrssll = bonr . bonssll
bonrsslr = bonr . bonsslr
bonrssrf = bonr . bonssrf
bonrssrs = bonr . bonssrs
bonrssrl = bonr . bonssrl
bonrssrr = bonr . bonssrr
bonrslff = bonr . bonslff
bonrslfs = bonr . bonslfs
bonrslfl = bonr . bonslfl
bonrslfr = bonr . bonslfr
bonrslsf = bonr . bonslsf
bonrslss = bonr . bonslss
bonrslsl = bonr . bonslsl
bonrslsr = bonr . bonslsr
bonrsllf = bonr . bonsllf
bonrslls = bonr . bonslls
bonrslll = bonr . bonslll
bonrsllr = bonr . bonsllr
bonrslrf = bonr . bonslrf
bonrslrs = bonr . bonslrs
bonrslrl = bonr . bonslrl
bonrslrr = bonr . bonslrr
bonrsrff = bonr . bonsrff
bonrsrfs = bonr . bonsrfs
bonrsrfl = bonr . bonsrfl
bonrsrfr = bonr . bonsrfr
bonrsrsf = bonr . bonsrsf
bonrsrss = bonr . bonsrss
bonrsrsl = bonr . bonsrsl
bonrsrsr = bonr . bonsrsr
bonrsrlf = bonr . bonsrlf
bonrsrls = bonr . bonsrls
bonrsrll = bonr . bonsrll
bonrsrlr = bonr . bonsrlr
bonrsrrf = bonr . bonsrrf
bonrsrrs = bonr . bonsrrs
bonrsrrl = bonr . bonsrrl
bonrsrrr = bonr . bonsrrr
bonrlfff = bonr . bonlfff
bonrlffs = bonr . bonlffs
bonrlffl = bonr . bonlffl
bonrlffr = bonr . bonlffr
bonrlfsf = bonr . bonlfsf
bonrlfss = bonr . bonlfss
bonrlfsl = bonr . bonlfsl
bonrlfsr = bonr . bonlfsr
bonrlflf = bonr . bonlflf
bonrlfls = bonr . bonlfls
bonrlfll = bonr . bonlfll
bonrlflr = bonr . bonlflr
bonrlfrf = bonr . bonlfrf
bonrlfrs = bonr . bonlfrs
bonrlfrl = bonr . bonlfrl
bonrlfrr = bonr . bonlfrr
bonrlsff = bonr . bonlsff
bonrlsfs = bonr . bonlsfs
bonrlsfl = bonr . bonlsfl
bonrlsfr = bonr . bonlsfr
bonrlssf = bonr . bonlssf
bonrlsss = bonr . bonlsss
bonrlssl = bonr . bonlssl
bonrlssr = bonr . bonlssr
bonrlslf = bonr . bonlslf
bonrlsls = bonr . bonlsls
bonrlsll = bonr . bonlsll
bonrlslr = bonr . bonlslr
bonrlsrf = bonr . bonlsrf
bonrlsrs = bonr . bonlsrs
bonrlsrl = bonr . bonlsrl
bonrlsrr = bonr . bonlsrr
bonrllff = bonr . bonllff
bonrllfs = bonr . bonllfs
bonrllfl = bonr . bonllfl
bonrllfr = bonr . bonllfr
bonrllsf = bonr . bonllsf
bonrllss = bonr . bonllss
bonrllsl = bonr . bonllsl
bonrllsr = bonr . bonllsr
bonrlllf = bonr . bonlllf
bonrllls = bonr . bonllls
bonrllll = bonr . bonllll
bonrlllr = bonr . bonlllr
bonrllrf = bonr . bonllrf
bonrllrs = bonr . bonllrs
bonrllrl = bonr . bonllrl
bonrllrr = bonr . bonllrr
bonrlrff = bonr . bonlrff
bonrlrfs = bonr . bonlrfs
bonrlrfl = bonr . bonlrfl
bonrlrfr = bonr . bonlrfr
bonrlrsf = bonr . bonlrsf
bonrlrss = bonr . bonlrss
bonrlrsl = bonr . bonlrsl
bonrlrsr = bonr . bonlrsr
bonrlrlf = bonr . bonlrlf
bonrlrls = bonr . bonlrls
bonrlrll = bonr . bonlrll
bonrlrlr = bonr . bonlrlr
bonrlrrf = bonr . bonlrrf
bonrlrrs = bonr . bonlrrs
bonrlrrl = bonr . bonlrrl
bonrlrrr = bonr . bonlrrr
bonrrfff = bonr . bonrfff
bonrrffs = bonr . bonrffs
bonrrffl = bonr . bonrffl
bonrrffr = bonr . bonrffr
bonrrfsf = bonr . bonrfsf
bonrrfss = bonr . bonrfss
bonrrfsl = bonr . bonrfsl
bonrrfsr = bonr . bonrfsr
bonrrflf = bonr . bonrflf
bonrrfls = bonr . bonrfls
bonrrfll = bonr . bonrfll
bonrrflr = bonr . bonrflr
bonrrfrf = bonr . bonrfrf
bonrrfrs = bonr . bonrfrs
bonrrfrl = bonr . bonrfrl
bonrrfrr = bonr . bonrfrr
bonrrsff = bonr . bonrsff
bonrrsfs = bonr . bonrsfs
bonrrsfl = bonr . bonrsfl
bonrrsfr = bonr . bonrsfr
bonrrssf = bonr . bonrssf
bonrrsss = bonr . bonrsss
bonrrssl = bonr . bonrssl
bonrrssr = bonr . bonrssr
bonrrslf = bonr . bonrslf
bonrrsls = bonr . bonrsls
bonrrsll = bonr . bonrsll
bonrrslr = bonr . bonrslr
bonrrsrf = bonr . bonrsrf
bonrrsrs = bonr . bonrsrs
bonrrsrl = bonr . bonrsrl
bonrrsrr = bonr . bonrsrr
bonrrlff = bonr . bonrlff
bonrrlfs = bonr . bonrlfs
bonrrlfl = bonr . bonrlfl
bonrrlfr = bonr . bonrlfr
bonrrlsf = bonr . bonrlsf
bonrrlss = bonr . bonrlss
bonrrlsl = bonr . bonrlsl
bonrrlsr = bonr . bonrlsr
bonrrllf = bonr . bonrllf
bonrrlls = bonr . bonrlls
bonrrlll = bonr . bonrlll
bonrrllr = bonr . bonrllr
bonrrlrf = bonr . bonrlrf
bonrrlrs = bonr . bonrlrs
bonrrlrl = bonr . bonrlrl
bonrrlrr = bonr . bonrlrr
bonrrrff = bonr . bonrrff
bonrrrfs = bonr . bonrrfs
bonrrrfl = bonr . bonrrfl
bonrrrfr = bonr . bonrrfr
bonrrrsf = bonr . bonrrsf
bonrrrss = bonr . bonrrss
bonrrrsl = bonr . bonrrsl
bonrrrsr = bonr . bonrrsr
bonrrrlf = bonr . bonrrlf
bonrrrls = bonr . bonrrls
bonrrrll = bonr . bonrrll
bonrrrlr = bonr . bonrrlr
bonrrrrf = bonr . bonrrrf
bonrrrrs = bonr . bonrrrs
bonrrrrl = bonr . bonrrrl
bonrrrrr = bonr . bonrrrr

