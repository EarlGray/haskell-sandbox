module Main where

import Happstack.Server (Browsing(EnableBrowsing), nullConf, serveDirectory, simpleHTTP)

main :: IO ()
main = simpleHTTP nullConf $ serveDirectory EnableBrowsing [] "."
