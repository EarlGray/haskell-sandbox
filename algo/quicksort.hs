import Data.Array.Base (unsafeRead, unfaseWrite)
import Data.Array.ST
import Control.Monad.ST

{-
 -  http://www.mail-archive.com/haskell-cafe%40haskell.org/msg63381.html
 -}

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
            
