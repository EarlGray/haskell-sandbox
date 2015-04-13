#!/usr/bin/env runhaskell
-- module BTree (module BTree) where

data Tree e     = Fork e (Tree e) (Tree e) | EmptyTree

contents    (Fork e l r) = e
left        (Fork e l r) = l
right       (Fork e l r) = r

instance Show a => Show (Tree a) where
    showsPrec d EmptyTree    rest = "{}"
    showsPrec d (Fork e l r) rest = 
        ('<':) . ((showsPrec d l):) . ('|':) .
            ((showsPrec d e):) . ('|':) . ((showsPrec d r):) . ('>':rest)

t1 = Fork 2 (Fork 1 EmptyTree EmptyTree)
            (Fork 3 (Fork 4 EmptyTree EmptyTree) EmptyTree)

main :: IO ()
main = do 
    putStr (show t1)
