{-
 -  http://h2.jaguarpaw.co.uk/posts/strictness-in-types/
 -}

{-# LANGUAGE TypeOperators #-}
module Strict (
  functionLike, FunctionLike, (:->)
) where

data a :-> b = Strict (a -> b)

infixr 0 :->

strictly :: (a -> b) -> a :-> b
strictly f = Strict (\a -> a `seq` f a)

(!) :: (a :-> b) -> a -> b
Strict f ! a = f a

foldl' :: (a -> b -> a) -> a :-> [b] -> a
foldl' f = strictly (\z xs -> case xs of
                          [] -> z
                          y:ys -> foldl' f !(f z y) $ ys)

class FunctionLike arrow where
  (?) :: (a `arrow` b) -> a -> b
  functionLike :: (a -> b) -> (a `arrow` b)

instance FunctionLike (->) where
  (?) = id
  functionLike = id

instance FunctionLike (:->) where
  (?) = (!)
  functionLike = strictly
