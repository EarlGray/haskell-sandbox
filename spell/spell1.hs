import System.Environment     
import Control.Monad
import Data.Char

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashTable as H

type HashTable k v = H.HashTable k v

makedict :: IO (HashTable L.ByteString ())
makedict = do
  f  <- L.readFile "/usr/share/dict/words"
  H.fromList H.hashInt $ map ((flip (,) ()) . (L.map toLower)) $ L.lines f

main = do
    [s]  <- getArgs
    dict <- makedict
    g    <- L.readFile s
    mapM_ (spell dict) (L.words g)
        
clean' :: L.ByteString -> L.ByteString
clean' = L.map toLower . L.filter (\x -> isAlpha x || (x == '\'') )

spell d w = do res <- H.lookup d (clean' w)
               case res of
                 Just _  -> return ()
                 Nothing -> L.putStrLn w
