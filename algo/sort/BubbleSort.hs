{-# LANGUAGE FlexibleContexts #-}
module BubbleSort (
  bubblesort, bubblesortA
) where

import SortingCommon
import Control.Arrow (first)

type Item = Int

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


{-| Array sort |-}
bubblesortA :: UArray Int Int -> UArray Int Int
bubblesortA a = runST $ do
  ma <- thaw a :: ST s (STUArray s Int Int)
  bblsort' ma
  freeze ma

bbliter :: STUArray s Int Int -> ST s ()
bbliter a = do
  inds <- getBounds a
  forM_ (tail $ range inds) $ \i -> do
    x <- unsafeRead a (i - 1)
    y <- unsafeRead a i
    if x > y then swap a (i - 1) i
             else return ()

bblsort' :: STUArray s Int Int -> ST s ()
bblsort' a = do
  inds <- getBounds a
  forM_ (range inds) $ \_ -> bbliter a

