module AVL where

data AVLTree a = AVLEmpty 
    | AVLBranch { 
        val :: a, 
        left, right :: AVLTree a 
    } 
    deriving (Show)

-- if tree nodes are not a single value and inserting a node may affect others, 
--  we need to change others with mutator'
insertMutating :: Ord a => (a -> a) -> a -> AVLTree a -> AVLTree a
insertMutating mutor val AVLEmpty = AVLBranch (mutor val) AVLEmpty AVLEmpty
insertMutating mutor val (AVLBranch root l r) | val < root = 
    AVLBranch (mutor root) (insertMutating mutor val l) (insertMutating mutor val r)
insertMutating mutor val (AVLBranch root l r) | val > root = 
    AVLBranch (mutor root) (insertMutating mutor val l) (insertMutating mutor val r)

insert :: (Ord a) => a -> AVLTree a -> AVLTree a
insert = insertMutating id

depth :: AVLTree a -> Int
depth AVLEmpty = 0
depth (AVLBranch _ l r) = 1 + max (depth r) (depth l)

balance :: AVLTree a -> Int
balance AVLEmpty = 0
balance (AVLBranch _ AVLEmpty r) = 0 - depth r
balance (AVLBranch _ l AVLEmpty) = depth l
balance (AVLBranch _ l r) = 
    if abl > abr 
    then if add > abl then dd else abl
    else if add > abr then dd else (-abr)
    where bl = balance l; br = balance r; dd = depth l - depth r
          abl = abs bl; abr = abs br; add = abs dd

rebalance :: Ord a => AVLTree a -> AVLTree a

rebalance AVLEmpty = AVLEmpty
rebalance tree | abs (balance tree) <= 1 = tree
rebalance tree | (balance tree == -2) && (balance r == (-1)) =
    AVLBranch (val r) (AVLBranch v l (left r)) (right r)
        where r = right tree
              l = left tree
              v = val tree
rebalance tree | (balance tree == -2) && (balance r == 1) =
    rebalance $ AVLBranch (val tree) (left tree) r'
        where r' = AVLBranch (val rl) (left rl) rr'
              rr' = AVLBranch (val r) (right rl) (right r)
              r = right tree
              rl = left r

rebalance tree | (balance tree == 2) && (balance l == 1) =
    AVLBranch (val l) (left l) (AVLBranch v (right l) r)
        where l = left tree
              r = right tree
              v = val tree
rebalance tree | (balance tree == 2) && (balance l == (-1)) =
    rebalance $ AVLBranch (val tree) l' (right tree)
        where l' = AVLBranch (val lr) ll' (right lr)
              ll' = AVLBranch (val l) (left l) (left lr)
              l = left tree
              lr = right l

rebalance (AVLBranch v l r) =
    if abs bl > abs br
    then AVLBranch v (rebalance l) r 
    else AVLBranch v l (rebalance r)
    where bl = balance l; br = balance r


pretty :: (Show a) => AVLTree a -> String
pretty AVLEmpty = "()"
pretty (AVLBranch v AVLEmpty AVLEmpty) = show v
pretty (AVLBranch v l r) = "(" ++ show v ++ " " ++ pretty l ++ " " ++ pretty r ++ ")"
