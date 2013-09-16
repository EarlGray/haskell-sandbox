{-
1. Given a list 'ps' of number pairs representing a closed range, and 
a list 'ns' of numbers, for each number n in 'ns', return the number 
of ranges from 'ps' that contain that number. A number 'n' is 
contained within the closed range [p1,p2] if p1 <= n <= p2.

For example, the function might have the type
    counter :: [(Rational, Rational)] -> [Rational] -> [Integer]
and
    counter [(0, 10), (5, 20), (25, 30)] [5, 20, 27, 100] gives [2, 1, 1, 0]

The first list may have millions of entries. The second, billions.
-}

{-
    This solution requires:
    $ cabal install AvlTree

    About solution:

    ...-----|--- area 1 ---|----- area 2 ------|--- area 3 --------...    - the number line
                 w = 1            w = 2            w = 0
            |----------- range 1 --------------|
                           |--- range 2 -------|
    
      An area is a piece of the number line that have the same number of ranges   
    intersecting it in all its points. Number of ranges that an area belongs to 
    is called area's weight.
      The solution is to make a binary search tree that will contain areas for 
    a given set of ranges. Determining how many ranges a point on the number line
    belongs to is just a lookup of an area for this point to output that area's 
    weight. This allows to make `counter` just a filter for arbitrary number of 
    inputs, the program is only constrained by size of the lookup tree.
-}

import Data.List
import Data.Ord (compare, comparing)
import Data.Function (on)
import qualified Data.Tree.AVL as T

type Point = Rational
type Weight = Int         -- how many ranges  some area of the number line belongs to
type IsEndClosed = Bool   -- does area include its start

--- Area describes a point that starts an area on the number line, 
--- possibly not closed at right
type Area = (Point, Weight, IsEndClosed)

--- LookupTree is a self-balancing binary search tree for areas 
--- sorted by their starting point
type LookupTree = T.AVL Area

areaList :: [(Point, Point)] -> [Area] -- complexity M * (3 + log M)
areaList ps = areaList' 0 (sort starts, sort ends)
    where (starts, ends) = unzip ps

          -- the last area, no ranges left
          areaList' w ([], end:[])     = [(end, w - 1, False)]
          -- no more ranges' startpoints, some endpoints left
          areaList' w ([], (b:bs))     = (b, w - 1, False) : areaList' (w - 1) ([], bs)
          -- there is a startpoint a before next endpoint b, increasing weight
          areaList' w ((a:as), (b:bs)) | a < b     = (a, w + 1, True)  : areaList' (w + 1) (as, (b:bs))
          -- there is an endpoint b before next startpoint a, decreasing weight
          areaList' w ((a:as), (b:bs)) | b < a     = (b, w - 1, False) : areaList' (w - 1) ((a:as), bs)
          -- an endpoint for a range is a startpoint for another range:
          areaList' w ((a:as), (b:bs)) | a == b    = (a, w, False)     : areaList' w (as, bs)

weightForPointIn :: LookupTree -> Point -> Weight     -- complexity log M
weightForPointIn t p = 
    -- get all nodes that are to the left of point p, take the last area there
    let (ltree, getree) = T.forkL (\(p1, _, _) -> p1 < p) t
        mbArea = case T.tryReadL getree of
                   Just area@(p1, _, _) | p1 == p -> Just area
                   _ -> T.tryReadR ltree
    in case mbArea of
         Just (p1, w, False) | p == p1 -> w + 1
         Just (_, w, _) -> w 
         Nothing -> 0

counter :: [(Point, Point)] -> [Point] -> [Weight]  -- complexity (N + M) * log M
counter ps ns = map (weightForPointIn theLookupTree) ns       -- complexity N * log M
    where theLookupTree = T.asTreeL $ areaList ps  -- complexity M * (4 + log M)

ps, ps1 :: [(Point, Point)]
ps = [(0, 10), (5, 20), (25, 30)]
ps1 = [(0, 5), (1, 7), (2, 4), (3, 6)] -- [(0, 1), (1, 2), (2, 3), (3, 4), (4, 3), (5, 2), (6, 1)]

ns :: [Point]
ns = [5, 20, 27, 100]

main :: IO ()
main = print $ counter ps ns  -- gives [2, 1, 1, 0]
