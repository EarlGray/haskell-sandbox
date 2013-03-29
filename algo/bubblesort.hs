{-# LANGUAGE FlexibleContexts #-}

import Control.Arrow (first)
import Control.Monad
import Control.Monad.ST

import Data.Array.ST as STA
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.Unboxed

import System.Random

type Item = Int

rndAlloc :: Int -> IO [Item]
rndAlloc n = replicateM n (randomRIO(1,1000000))

rndArray :: Int -> IO (UArray Int Int)
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


{-| Array sort |-}
bubblesortA :: UArray Int Int -> UArray Int Int
bubblesortA a = runST $ do
  ma <- thaw a :: ST s (STUArray s Int Int)
  bblsort' ma
  freeze ma

swap :: STUArray s Int Int -> Int -> Int -> ST s ()
swap a i j = do
  t <- unsafeRead a i
  unsafeRead a j >>= unsafeWrite a i
  unsafeWrite a j t

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

{-- test --}

main :: IO ()
main = do
    putStrLn "List bubblesort:"
    list <- rndAlloc 25
    print list
    print $ bubblesort list

    putStrLn "Array bubblesort:"
    arr <- rndArray 25
    print (elems arr)
    print $ elems $ bubblesortA arr
