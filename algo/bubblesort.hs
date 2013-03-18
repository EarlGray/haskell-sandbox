{-# LANGUAGE FlexibleContexts #-}

import Control.Arrow (first)
import Control.Monad
import Control.Monad.ST

import Data.Array.ST as STA
import Data.Array.Unboxed
import Data.Array.MArray as MA

import System.Random

type Item = Int

rndAlloc :: Int -> IO [Item]
rndAlloc n = replicateM n (randomRIO(1,1000000))

rndArray :: Int -> IO (Array Int Int)
rndArray n = mkArray `fmap` rndAlloc n
  where mkArray xs = listArray (0, length xs - 1) xs

{-| List sort |-}
bubblesort :: Ord a => [a] -> [a]
bubblesort [] = []
bubblesort xs = if (not swapped) then xs'
                else bubblesort xs'
  where (xs', swapped) = swapIter (xs, False)

swapIter :: Ord a => ([a],Bool) -> ([a], Bool)
swapIter ((x1:x2:xs), swapped) | x1 > x2 = first (x2:) $ swapIter ((x1:xs), True)
swapIter ((x1:x2:xs), swapped) = first (x1:) $ swapIter ((x2:xs), swapped)
swapIter (xs, swapped) = (xs, swapped)


{-| Array sort |-
bubblesortA :: Array Int Int -> Array Int Int
bubblesortA a = runST $ do
  ma <- thaw a :: ST s (STArray s Int Int)
  (sorted, False) <- bblsort' (ma, False)
  freeze sorted

bblsort' :: STArray s Int Int -> ST s (STArray s Int Int, Bool)
bblsort' (xs,swapped) = return xs
-- -}

main :: IO ()
main = do
    print =<< bubblesort `fmap` rndAlloc 25
