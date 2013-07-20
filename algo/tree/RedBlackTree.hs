module RBTree (
    RBTree(..), emptyRB,
    lookupBy, lookup, member,
    insertBy, insert 
) where 

import Data.Maybe (isJust)
import Prelude hiding (lookup)

data RB = Red | Black deriving (Show, Eq, Ord)

data RBTree a = RBEmpty | RBNode RB a (RBTree a) (RBTree a)
                deriving (Show, Eq, Ord)

lookupBy :: Ord a => (a -> a -> Ordering) -> a -> RBTree a -> Maybe a
lookupBy _ _ RBEmpty = Nothing
lookupBy cmp v (RBNode _ n l r) =
    case cmp v n of
      EQ -> Just n
      LT -> lookupBy cmp v l
      GT -> lookupBy cmp v r

lookup :: Ord a => a -> RBTree a -> Maybe a
lookup = lookupBy compare

member v = isJust . lookup v

emptyRB = RBEmpty
fromList :: Ord a => [a] -> RBTree a
fromList = foldr insert RBEmpty

insertBy :: Ord a => (a -> a -> Ordering) -> a -> RBTree a -> RBTree a
insertBy cmp v t = RBNode Black v' l' r'
  where (RBNode _ v' l' r') = insertBy' cmp v t

insertBy' cmp v RBEmpty = RBNode Red v RBEmpty RBEmpty
insertBy' cmp v t@(RBNode c n l r) =
    case cmp v n of
      EQ -> t
      LT -> rebalance $ RBNode c n (insertBy' cmp v l) r
      GT -> rebalance $ RBNode c n l (insertBy' cmp v r)

rebalance :: RBTree a -> RBTree a
rebalance (RBNode Black x a (RBNode Red y b (RBNode Red z c d))) =
    RBNode Red y (RBNode Black x a b) (RBNode Black z c d)
rebalance (RBNode Black x a (RBNode Red z (RBNode Red y b c) d)) =
    RBNode Red y (RBNode Black x a b) (RBNode Black z c d)
rebalance (RBNode Black z (RBNode Red x a (RBNode Red y b c)) d) =
    RBNode Red y (RBNode Black x a b) (RBNode Black z c d)
rebalance (RBNode Black z (RBNode Red y (RBNode Red x a b) c) d) =
    RBNode Red y (RBNode Black x a b) (RBNode Black z c d)
rebalance t = t

insert :: Ord a => a -> RBTree a -> RBTree a
insert = insertBy compare

pretty :: (Show a, Ord a) => RBTree a -> [String]
pretty t = pretty' 0 t
  where pretty' ind (RBEmpty) = [ replicate ind ' ' ++ "-" ]
        pretty' ind (RBNode c n RBEmpty RBEmpty) = [ node ind c n ]
        pretty' ind (RBNode c n l r) = [node ind c n] ++ lp ++ rp 
          where lp = pretty' (ind+2) l
                rp = pretty' (ind+2) r 
        node ind c n = replicate ind ' ' ++ "+" ++ show c ++ " " ++ show n

printRB :: (Ord a, Show a) => RBTree a -> IO ()
printRB = mapM_ putStrLn . pretty 
