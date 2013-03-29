module MergeSort (
  mergesort
) where

import Data.List (splitAt)

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = (x: merge xs (y:ys))
merge xs (y:ys) = (y : merge xs ys)

mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort rxs) (mergesort lxs)
    where (rxs, lxs) = splitAt ((length xs)`div`2) xs
