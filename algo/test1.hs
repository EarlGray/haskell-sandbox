import Data.List
import Data.Ord (compare, comparing)
import qualified Data.Tree.AVL as T

type Point = Int
type Distance = Int
type Weight = Int
type IsLeftClosed = Bool
type LookupNode = (Point, Weight, IsLeftClosed)
type LookupTree = T.AVL LookupNode

rangeList :: [(Point, Point)] -> [LookupNode] -- complexity M * (3 + log M)
rangeList ps = rangeList' 0 (sort starts, sort ends)
    where (starts, ends) = unzip ps
          rangeList' w ([], end:[])     = [(end, w - 1, False)]
          rangeList' w ([], (b:bs))     = (b, w - 1, False) : rangeList' (w - 1) ([], bs)
          rangeList' w ((a:as), (b:bs)) | a < b     = (a, w + 1, True)  : rangeList' (w + 1) (as, (b:bs))
          rangeList' w ((a:as), (b:bs)) | b < a     = (b, w - 1, False) : rangeList' (w - 1) ((a:as), bs)
          rangeList' w ((a:as), (b:bs)) | a == b    = (a, w, False)     : rangeList' w (as, bs)

counter :: [(Point, Point)] -> [Point] -> [Weight]  -- complexity (N + M) * log M
counter ps ns = map (weight theLookupTree) ns       -- complexity N * log M
    where theLookupTree = T.asTreeL $ rangeList ps  -- complexity M * (4 + log M)

weight :: LookupTree -> Point -> Weight     -- complexity log M
weight t p = let mbNode = T.tryReadR $ T.takeLE (\(p1, _, _) -> p `compare` p1) t
        in case mbNode of
            Just (_, w, True) -> w
            Just (p1, w, False) -> if p == p1 then w + 1 else w
            Nothing -> 0

ps, ps1 :: [(Point, Point)]
ps = [(0, 10), (5, 20), (25, 30)]
ps1 = [(0, 5), (1, 7), (2, 4), (3, 6)] -- [(0, 1), (1, 2), (2, 3), (3, 4), (4, 3), (5, 2), (6, 1)]

ns :: [Point]
ns = [5, 20, 27, 100]

main :: IO ()
main = print $ counter ps ns  -- gives [2, 1, 1, 0]
