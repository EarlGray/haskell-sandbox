
data Tree a = Leaf f
            | Fork (Tree a) (Tree a)

data Cntxt a = Top | L (Cntxt a) (Tree a) | R (Cntxt a) (Tree a)

eg = Fork (Fork (Leaf 1) (Leaf 2))
          (Fork (Leaf 3) (Leaf 4))


