module Main where

import Control.Monad

import System.Environment (getArgs)
import System.Posix.User (setUserID, getUserEntryForName, UserEntry(..))

import Happstack.Server


serverConf = nullConf { port = 8080 }
serverRoutes root = serveDirectory EnableBrowsing ["index.htm", "index.html"] root

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ error "Path to the served directory must be specified"
    let sroot = head args

    simpleHTTP serverConf $ serverRoutes sroot
