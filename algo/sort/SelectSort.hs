module SelectSort (
    --selectsortA,
    selectsort
) where
import SortingCommon

{-| List selection sort |-}
selectsort :: Ord a => [a] -> [a]
selectsort [] = []
selectsort xs = a : selectsort (bs ++ as)
  where (bs, (a:as)) = select xs

select (x:xs) = (bs, as) 
  where (bs, as, []) = select' ([], [x], xs)

select' a@(_, _, []) = a
select' (bs, as, (t:ts))
        | t < head as = select' (bs ++ as, [t], ts)
        | otherwise = select' (bs, as ++ [t], ts)

{-| Array selection sort |-
selectsortA s :: UArray Int Int -> UArray Int Int
selectsortA 
-}
