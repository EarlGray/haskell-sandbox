module QuickSort (
  quicksortA,
  quicksort
) where

import SortingCommon
import Data.STRef
import Data.Array.Unsafe as AU

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

quicksortA a = runST $ do
    ma <- thaw a
    quicksortAU ma
    AU.unsafeFreeze ma

quicksortAU :: STUArray s Int Int -> ST s ()
quicksortAU a = do
    (from, to) <- getBounds a
    quicksortA' a from to

quicksortA' a l r =
    when (l < r) $ do
      p <- part a l r l
      quicksortA' a l (p-1)
      quicksortA' a (p+1) r
  where
    part a l r p = do
      pivot <- unsafeRead a p
      swap a p r
      sref <- newSTRef l
      forM_ (range (l, r - 1)) $ \i -> do
        ai <- unsafeRead a i
        when (ai <= pivot) $ do
          store <- readSTRef sref
          swap a i store
          writeSTRef sref (store + 1)
      store <- readSTRef sref
      swap a store r
      return store

{-
 -  http://www.mail-archive.com/haskell-cafe%40haskell.org/msg63381.html

quicksortA :: STUArray s Int Int -> Int -> Int -> ST s ()
quicksortA a lo hi 
    | lo < hi = do
        let lscan p h i
              | i < h = do
                v <- unsafeRead a i
                if p < v then return i else lscan p h (i + 1)
              | otherwise = return i
            rscan p l i = do
              | l < i = do
                v <- unsafeRead a i
                if v < p then return i else rscan p l (i - 1)
              | otherwise = return i
            swap j i = do
                v <- unsafeRead a i
                unsafeRead a j >>= unsafeWrite a i
                unsafeWrite a j v
            sloop p l h
              | l < h = do
                l1 <- lscan p h l
                h1 <- rscan p l1 h
                if (l1 < h1) then (swap l1 h1 >> sloop p l1 h1) else return l1
              | otherwise = return l
        piv <- unsafeRead a hi
        i <- sloop piv lo hi
        swap i hi
        quicksortA a lo (i-1)
        quicksortA a (i + 1) hi
    | otherwise = return ()
            
 -}
