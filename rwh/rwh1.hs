data Customer = Customer {
    customerID      :: CustomerID,
    customerName    :: String,
    customerAddress :: Address
} deriving (Show)

type Address = String
type CustomerID = Int

-------
-- List
-------

data List a = Cons a (List a)
            | Nil
            deriving (Show)

fromList (x:xs) = Cons x (fromList  xs)
fromList []     = Nil

tidySecond :: [a] -> Maybe a
tidySecond (_:x:_)  = Just x
tidySecond _        = Nothing


data BinaryTree a = BinTree {
        val   :: a,
        left  :: BinaryTree a, 
        right :: BinaryTree a
    }
    | EmptyBranch
    deriving (Show)

nodesAreSame (a, _, _) (b, _, _)    | a == b    = Just a
nodesAreSame _ _                                = Nothing

height :: BinaryTree a -> Int
height EmptyBranch   = 0
height bt = 1 + max (height lb) (height rb)
    where lb = left bt; rb = right bt

bt = BinTree 4 (BinTree 2 (BinTree 0 EmptyBranch EmptyBranch) (BinTree 1 EmptyBranch EmptyBranch))  (BinTree 8 (BinTree 6 EmptyBranch EmptyBranch) EmptyBranch) 

------

myLength (Cons x xs) = 1 + myLength xs
myLength Nil         = 0

palindrome :: [a] -> [a]
palindrome xs = xs ++ myReverse xs
    where myReverse (x:xs) = myReverse xs ++ (x:[])
          myReverse []     = []

joinLists :: a -> [[a]] -> [a]
joinLists _ []              = []
joinLists _ (list:[])       = list 
joinLists sep (list:lists)  = list ++ (sep:(joinLists sep lists))
