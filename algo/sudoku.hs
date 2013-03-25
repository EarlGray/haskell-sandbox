import qualified Data.Set as S 
import qualified Data.Map as M
import Data.Map ((!))
import Data.List hiding (map)
import Data.Maybe (fromJust, maybe)
import Data.Either (either)
import Control.Arrow (right)

type Figure = Int
type Position = (Int,Int)
type Possibilities = S.Set Int
type Cell = Either Figure Possibilities
type Sudoku = M.Map Position Cell
type Region = Sudoku

readSudoku :: String -> Sudoku
readSudoku = M.fromList . concat . zipWith readSudokuLine [0,1..] . filter (not.null) . lines
  where readSudokuLine i = zipWith makeCell [0,1..] . map read . words
          where makeCell j w | w `elem` [1..9] = ((j,i), Left w)
                makeCell j _ = ((j,i), Right (S.fromList [1..9]))

prettySudoku :: (Possibilities -> String) -> String -> Sudoku -> String
prettySudoku rt sep s = unlines [ prettyLine i | i <- [0..8] ]
  where prettyLine i = intercalate sep [ showCell (j,i) | j <- [0..8] ]
        showCell pos = maybe "?" (either show rt) (M.lookup pos s)
pretty' = putStrLn . prettySudoku (show . S.toList) "\t"
pretty = putStrLn . prettySudoku (const "0") " "
defaultSudoku = (readSudoku . unlines . drop 2 . lines) `fmap` readFile "default.sudoku"

------------------

onLine li (_,i) = i == li
onColumn lj (j,_) = j == lj
onSquare (lj,li) (j,i) = modEq 3 i li && modEq 3 j lj
  where modEq n a b = a`div`n == b`div`n

sudokuLine :: Int -> Sudoku -> Region
sudokuLine i       = M.filterWithKey (\k _ -> onLine i k)

sudokuColumn :: Int -> Sudoku -> Region
sudokuColumn j     = M.filterWithKey (\k _ -> onColumn j k)

sudokuSquare :: Position -> Sudoku -> Region
sudokuSquare (j,i) = M.filterWithKey (\k _ -> onSquare (j,i) k)

reduceByCellValue :: Figure -> Position -> Sudoku -> Sudoku
reduceByCellValue n (j,i) s = M.mapWithKey reduce s
  where reduce p | onLine i p || onColumn j p || onSquare (j,i) p = right (S.delete n)
        reduce p = id

reduceSudoku s = M.foldlWithKey reduce s s
  where reduce s k (Left n) = reduceByCellValue n k s
        reduce s k (Right _) = s

singleForCell s = M.mapWithKey intro $ reduceSudoku s
  where intro k (Right ps) | S.size ps == 1 = Left . head . S.elems $ ps
        intro k c = c

type RegionStat = M.Map Int (Int, Position) -- Map Possibility (Count FirstPosition)

statistics :: Region -> RegionStat
statistics s = M.foldlWithKey upd M.empty s
  where upd stat pos (Right ps) = S.fold eachPos stat ps
         where eachPos p stat = M.alter forStat p stat
                where forStat (Just (cnt, _)) = Just (cnt + 1, pos)
                      forStat Nothing = Just (1, pos)
        upd stat pos _ = stat

singlePossibilities :: Region -> Region
singlePossibilities s = M.foldWithKey forEach s stat
  where stat = M.filter ((== 1).fst) $ statistics s -- must be only one in the region
        forEach p (_, pos) s = M.insert pos (Left p) s

singlesForRegion :: (Position -> Bool) -> Sudoku -> Sudoku
singlesForRegion isreg s = M.union outer $ singlePossibilities inner
  where (inner, outer) = M.partitionWithKey (\k _ -> isreg k) s

regions = [ onLine i | i <- [0..8] ] ++ [ onColumn j | j <- [0..8] ] ++
          [ onSquare (3*i,3*j) | i <- [0..2], j <- [0..2] ]
allSingles s = foldr singlesForRegion s regions

allSolved :: Sudoku -> Bool
allSolved s = M.null $ snd $ M.partition (\a -> case a of { Left _ -> True; _ -> False; }) s

resolveReduces s = if s == s1 then s else resolveReduces s1
  where s1 = singleForCell $ reduceSudoku s

resolveSingles s = if s == s1 then s else resolveSingles s1
  where s1 = allSingles $ resolveReduces s
