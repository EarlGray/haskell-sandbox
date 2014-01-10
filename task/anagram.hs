{-
 - https://www.hackerrank.com/challenges/anagram
 -}

import Data.List 

intrsect :: Ord a => [a] -> [a] -> [a]
intrsect (c1 : s1) (c2 : s2) =
  case c1 `compare` c2 of
    LT -> intrsect s1 (c2 : s2)
    EQ -> c1 : intrsect s1 s2
    GT -> intrsect (c1 : s1) s2
intrsect _ _ = []

dist :: (String, String) -> Int
dist (s1, s2) | length s1 /= length s2 = -1
dist (s1, s2) = length s1 - length (intrsect (sort s1) (sort s2))

main = do
  n <- readLn :: IO Int
  flip mapM [1..n] $ \_ -> do
    ln <- getLine
    print $ dist $ splitAt (length ln `div` 2) ln
