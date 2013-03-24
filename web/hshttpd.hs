module Main where

import Happstack.Server
import System.Environment (getArgs)
import System.Posix.User (setUserID, getUserEntryForName, UserEntry(..))

import Control.Monad

serverConf = nullConf { port = 80 }
serverUser = "www"

serverRoutes root = serveDirectory EnableBrowsing ["index.htm", "index.html"] root

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ error "Path to the served directory must be specified"
    let sroot = head args

    sock <- bindPort serverConf
    getUserEntryForName serverUser >>= setUserID . userID

    simpleHTTPWithSocket sock serverConf $ serverRoutes sroot
