{-# LANGUAGE GADTs #-}
module SafeList where

data Empty
data NonEmpty

class Emptiness e 
instance Emptiness Empty
instance Emptiness NonEmpty
  
data List t s where
  Nil  :: List t Empty
  Cons :: t -> List t s -> List t NonEmpty

safeHead :: List t NonEmpty -> t
safeHead (Cons h _) = h

safeTail :: Emptiness s1 => List t NonEmpty -> List t s1
safeTail (Cons _ tl@(Cons y ys)) = safeTail tl    -- OMG
