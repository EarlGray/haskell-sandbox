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

 
part :: (a -> Bool) -> [a] -> ([a], [a])
part p xs = part' xs ([],[])
 where part' [] (lxs, rxs) = (lxs, rxs)
       part' (x:xs) (lxs,rxs) | p x = part' xs (x:lxs, rxs)
       part' (x:xs) (lxs,rxs) = part' xs (lxs, x:rxs)

quicksort' [] = []
quicksort' (x:xs) = quicksort' lt ++ [x] ++ quicksort' gt
    where (lt, gt) = part (<x) xs

qs2 [] = []
qs2 (x:xs) = case part (<x) xs of
                (lt, gt) -> qs2 lt ++ [x] ++ qs2 gt

rndAlloc n = replicateM n (randomRIO (1::Int, 1000000))

testsort = qs2

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

