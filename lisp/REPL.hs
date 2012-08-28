module Main where

import AST
import Eval

repl :: Env -> IO ()
repl env = do
    line <- getLine
    if ":q" == line 
    then return ()
    else do
        let inp = readSExpr line
        let (out, env') = eval env inp
        print out
        repl env'

sayHello = do
    putStrLn "Welcome to Thumbelina Scheme"
    putStrLn "*** type :q to quit ***"

main :: IO ()
main = do
    sayHello
    repl $ makeEnv
