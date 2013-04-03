{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import SortingCommon hiding (array)
import QuickSort
import BubbleSort
import SelectSort
import MergeSort

import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)

import System.Random
import System.Console.CmdArgs

data CommandOptions = CommandOptions {
        sort :: String,
        size :: Int,
        array, rand :: Bool
    } deriving (Show, Data, Typeable)
defaultOpts = CommandOptions "bubble" 25 False False

mkArray xs = listArray (0, length xs - 1) xs

randomList :: Int -> IO [Int]
randomList n = replicateM n (randomRIO(1,1000000))

randomArray :: Int -> IO (UArray Int Int)
randomArray n = mkArray `fmap` randomList n

sorts :: Ord e => [(String, [e] -> [e])]
sortAs :: [(String, UArray Int Int -> UArray Int Int)]
--sortAs :: (Ord e, Ix i) => M.Map String (UArray i e -> UArray i e)

sorts = [
    ("bubble", bubblesort),
    ("merge", mergesort),
    ("quick", quicksort),
    ("select", selectsort) ]

sortAs = [
    ("quick", quicksortA),
    ("bubble", bubblesortA) ]

readLst :: String -> [Int]
readLst = read

main :: IO ()
main = do
    opts <- cmdArgs defaultOpts
    let n = size opts

    if array opts 
    then do
        let sortf = fromMaybe bubblesortA (lookup (sort opts) sortAs)
        arr <- if rand opts 
                then do
                      a <- randomArray n
                      print $ elems a
                      return a
                else mkArray <$> readLst <$> getContents
        print $ elems $ sortf arr
    else do
        let sortf = fromMaybe bubblesort (lookup (sort opts) sorts)
        list <- if rand opts
                then do
                        list <- randomList n
                        print list
                        return list
                else readLst <$> getContents
        print $ sortf list

