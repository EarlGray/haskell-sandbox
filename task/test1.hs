import AVL

data Range a = 
    LeftINF {
        next :: a 
    } | 
    RightINF {
        prev :: a
    } | 
    Range {
        this, next, prev :: a,
        weight :: Int
    }
    deriving (Show, Ord, Eq)

instance (Ord a) => Range a where
    a > b = (this a) > (this b)

rangesTree :: [(Rational, Rational)] -> AVLTree (Range Rational)
rangesTree rs = rangesTree' rs AVLEmpty
    where rangesTree' [] = id
          rangesTree' (r:rs) = rangesTree' rs $ insertRange r

insertRange :: (Ord a) => (a, a) -> AVLTree (Range a) -> AVLTree (Range a)
insertRange (rstart, rend) tree = -- undefined
    insertMutating (handleRangeInsert (rstart, rend)) newRange tree
    where handleRangeInsert (rstart,rend) aRange =
            undefined
            -- if (next aRange) < rstart || rend < (this aRange) 
            -- if breaksRange rstart aRange 
            -- then Range (val aRange) rstart (prev aRange)
            
lookupRange :: (Ord a) => AVLTree (Range a) -> a -> Range a

counter :: [(Rational, Rational)] -> [Rational] -> [Int]
counter rs = map rangesForNum 
    where rangesForNum = weight $ lookupRange theBST
          theBST = rangesTree rs

exampleRanges :: [(Rational, Rational)] = [(0, 10), (5, 20), (25, 30)]
examplePoints :: [Rational] = [5, 20, 27, 100]
            
main = do 
    print counter exampleRanges examplePoints  -- counter   gives [2, 1, 1, 0]
