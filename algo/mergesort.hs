import Data.List

import Control.Monad (replicateM, forM_)
import System.Random (randomRIO)
import System.Environment (getArgs)

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = (x: merge xs (y:ys))
merge xs (y:ys) = (y : merge xs ys)

mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort rxs) (mergesort lxs)
    where (rxs, lxs) = splitAt ((length xs)`div`2) xs

quicksort [] = []
quicksort (x:xs) = quicksort (filter (<x) xs) ++ [x] ++ quicksort (filter (>x) xs)

quicksort' [] = []
quicksort' (x:xs) = quicksort' lt ++ [x] ++ quicksort' gt
    where (lt, gt) = partition (<x) xs

testsort = mergesort

main = do
    args <- getArgs
    if args!!0 == "-"
    then do
        let n = (read (args!!1) :: Int)
        forM_ [1..10] $ \_ -> do
            replicateM n (randomRIO (1,1000000::Int)) >>= print
    else do
        let n = (read (args!!0) :: Int)
        forM_ [1..10] $ \_ -> do
            arr <- replicateM n (randomRIO (1,1000000::Int))
            print $ testsort arr

