{-# LANGUAGE GADTs #-}

data Term x where
  K :: Term (a -> b -> a)
  S :: Term ((a -> b -> c) -> (a -> b) -> c)
  Const :: a -> Term a
  (:@) :: Term (a -> b) -> (Term a) -> Term b
  
infixl 6 :@

eval :: Term a -> Term a
eval (K :@ x :@ y) = x
eval (S :@ x :@ y :@ z) = x :@ z :@ (y :@ z)
eval x = x



