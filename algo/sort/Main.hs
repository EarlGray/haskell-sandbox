module Main where

import QuickSort
import BubbleSort
import MergeSort

import System.Random
import Control.Monad (replicateM)
import Data.Array 

randomList :: Int -> IO [Item]
randomList n = replicateM n (randomRIO(1,1000000))

randomArray :: Int -> IO (UArray Int Int)
randomArray n = mkArray `fmap` randomList n
  where mkArray xs = listArray (0, length xs - 1) xs


main :: IO ()
main = do
    putStrLn "List bubblesort:"
    list <- randomList 25
    print list
    print $ bubblesort list

    putStrLn "Array bubblesort:"
    arr <- randomArray 25
    print (elems arr)
    print $ elems $ bubblesortA arr
