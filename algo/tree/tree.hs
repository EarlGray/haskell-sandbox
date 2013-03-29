#! /usr/bin/env ghc

data Tree a = Leaf a | Branch (Tree a) (Tree a)
Branch      :: Tree a -> Tree a -> Tree a
Leaf        :: a -> Tree a

-- Tree a to String
--type ShowS  = String -> String
--shows   :: (Show a) => a -> ShowS

showsTree                :: (Show a) => Tree a -> ShowS
showsTree (Leaf x)       = shows x
showsTree (Branch l r) s = ('<':) . showsTree l . ('|':) . showsTree r . ('>':)

-- String to Tree of a
--type ReadS a    = String -> [(a, String)]
--reads   :: (Read a) => ReadS a

readsTree               :: (Read a) => ReadS (Tree a)
readsTree ('<':s)       = [ (Branch l r, u) | (l, '|':t) <- readsTree s,
                                              (r, '>':u) <- readsTree t ]
readsTree s             = [ (Leaf x, t) | (x, t)         <- reads s]

-- main
main :: IO ()
main = do 
    s <- getLine
    showsTree (readsTree s)
    return ()
