{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString
import Network hiding (accept)
import Network.Socket (accept)
import Network.Socket.ByteString (sendAll)
import Control.Monad ( forever )
import Control.Exception (bracket, finally)
import Control.Concurrent (forkIO)

main = 
  let loop s = forever $ forkIO . request . fst =<< accept s
      request c = sendAll c response `finally` sClose c
      response = "HTTP/1.0 200 OK\r\nContent-Length: 16\r\n\r\nGoodbye, world!\n"
  in bracket (listenOn $ PortNumber 8080) sClose loop
  
