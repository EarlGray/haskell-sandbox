module Main where

{-
 -  Install HFuse package from cabal and compile the file with -threaded
 -
 -}

import Control.Exception
import System.IO
import System.Fuse

helloFUSE :: FuseOperations Handle
helloFUSE = defaultFuseOps {
    fuseInit = hfuseInit,
    fuseDestroy = hfuseDestroy
}

hfuseInit = putStrLn "hfuseInit" >> hFlush stdout

hfuseDestroy = putStrLn "hfuseDestroy" >> hFlush stdout

handleExeption :: SomeException -> IO Errno
handleExeption e = do
    putStrLn "ERROR"
    print e
    hFlush stdout
    return eFAULT

main :: IO ()
main = fuseMain helloFUSE handleExeption
