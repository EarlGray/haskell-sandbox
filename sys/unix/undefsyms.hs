--- This script takes output of nm(1) for an archive
-- and determines what symbols are not implemented 
-- within library

import qualified Data.Set as S
import Data.Char (isDigit, isHexDigit)

type SymSets = (S.Set String, S.Set String)

judge :: String -> SymSets -> SymSets
judge ln state@(undefset, defset) = 
    case words ln of
      [symtype, symname] ->
        if symtype == "U" then (S.insert symname undefset, defset)
        else if symtype `elem` ["D", "B", "C", "T", "W", "G", "R", "V"]
             then (undefset, S.insert symname defset)
             else state
      _ -> state

guessBitness :: [String] -> Int
guessBitness = length . takeWhile isHexDigit . head . filter (isDigit . head) . filter (not . null)

forM_ = flip mapM_

main = do
    inp <- filter (not.null) `fmap` lines `fmap` getContents
    let bitness = guessBitness inp
    let cutlns = filter (not . null) $ map (drop bitness) inp
    let (undefmap, defmap) = foldr judge (S.empty, S.empty) cutlns

    let extsyms = S.difference undefmap defmap
    forM_ (S.toAscList extsyms) putStrLn
