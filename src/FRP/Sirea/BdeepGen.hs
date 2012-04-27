
-- | BdeepGen is a utility program to generate Bdeep.hs
-- I don't try to be efficient here... just productive.
module Main where

import Control.Exception (assert)

infixr 4 +\
+\ :: String -> String -> String
hd +\ tl = hd ++ "\n-- " ++ tl

deepThoughts, deepHeader :: String

deepThoughts = 
    "                           " +\
    "| Bdeep provides convenient shorthand utility behaviors,  simple" +\
    "composites of RDP behaviors for: " +\
    "                           " +\
    "  * deep application of behaviors " +\
    "  * deep extraction of products " +\
    "                           " +\
    "For example, rather than:  " +\
    "                           " +\
    "> (bfirst . bsecond . bleft . bsecond) action " +\
    "                           " +\
    "Developers can simply write: " +\
    "                           " +\
    "> bonfsls action           " +\
    "                           " +\
    "This reads as `behavior on first second left second`. Note that" +\
    "the order of characters corresponds to the path of application." +\
    "These are defined up to 6 deep, for a total 5460 functions. The" +\
    "shallow elements bonf (= bfirst), bons, bonl, bonr are included" +\
    "so developers can have consistency. " +\
    "                           " +\
    "Similarly, rather than:    " +\
    "                           " +\
    "> bfst >>> bsnd >>> bfst >>> bfst >>> bsnd " +\
    "                           " +\
    "Developers can write:      " +\
    "                           " +\
    "> bxfsffs                  " +\
    "                           " +\
    "This reads as `behavior extract first second first first second." +\
    "Similarly to the above, the order of characters corresponds to " +\
    "the path to the element extracted. To extract multiple elements " +\
    "from deep positions, consider use of the (&&&) behavior: " +\
    "                           " +\
    " > bxfsffs &&& bxssffs     " +\
    "                           " +\
    "Note: extraction is NOT possible for left/right, due to duration " +\
    "coupling constraints. These are also defined up to 6 depth, for " +\
    "total 126 functions.       " +\
    "                           " +\
    "The dual to bx* is also provided, just for completeness. This is " +\
    "another 124 functions for injection - `binlr` is `binl >>> binr`." +\
    "These might be useful if a behavior is a big switch, but that is " +\
    "not a recommended pattern (better to use a lot of small behaviors)." +\
    "                           " +\
    "Since I'm not about to write 5710 functions by hand, Bdeep is " +\
    "generated programmatically. I should probably learn Template" +\
    "Haskell eventually, but for now it's just a plain old program." +\
    "                           " +\
    "See Also:                  " +\
    "  FRP.Sirea.Behavior       " +\
    "  FRP.Sirea.Bstack         " +\
    "                           "

allCombosOfSize :: Int -> [x] -> [[x]]
allCombosOfSize _ [] = []
allCombosOfSize n xs = 
    assert(n > 0) $
    if (n < 2) then map (:[]) xs else 
    xs `across` allCombosOfSize (n-1) xs
    where across :: [x] -> [[x]] -> [[x]]
          across [] _ = []
          across (x:xs) combos = map (x:) combos ++ xs `across` combos

chunksOf :: Int -> [x] -> [[x]]
chunksOf _ [] = []
chunksOf n xs = assert (n > 0) $ (take n xs):(chunksOf n $ drop n xs)

deepAppFunctions, deepExtractFunctions, deepInjectFunctions, allFunctions :: [String]
deepAppFunctions = map ("bon"++) $ concatMap (flip allCombosOfSize "fslr")  [1..6]
deepExtractFunctions = map ("bx"++) $ concatMap (flip allCombosOfSize "fs") [1..6]
deepInjectFunctions = map ("bin"++) $ concatMap (flip allCombosOfSize "lr") [2..6]
allFunctions = deepExtractFunctions ++ deepInjectFunctions ++ deepAppFunctions

deepHeader = 
    "module FRP.Sirea.Bdeep \n" ++
    "   (" ++ listFunctions ++ "\n" ++
    "   ) where \n" ++ 
    where separated sep xs = foldl (\ln s -> ln ++ sep ++ s) (head xs) (tail xs) 
          listFunctions = 
            let namesPerLine = chunksOf 8 (deepExtractFunctions ++ deepAppFunctions) in 
            let linesOfNames = map (separated ", ") namesPerLine in
            separated "\n    , " linesOfNames

buildFn :: String -> String
buildFn = id

main :: IO ()
main = 
    putStrLn deepThoughts >>
    putStrLn deepHeader >>
    putStrLn (unlines $ map ("import " ++) deepImports) >>
    putStrLn (unlines $ map buildFn allFunctions)
    return ()
    
    





