import System.Environment
import Control.Monad
import Data.Set
import Data.Char

import qualified Data.ByteString as S

main = do
    [s] <- getArgs
    f   <- readFile "/usr/share/dict/words"
    g   <- readFile s
    let dict = fromList (lines f) in
      mapM_ (spell dict) (words g)

alphabet = fromList ( ['a'..'z'] ++ ['\''] )

clean :: String -> String
clean [] = []
clean (c:cs) | member cl alphabet = cl : clean cs
             | otherwise          = clean cs
            where cl = toLower c

incorrect d w = notMember (clean w) d 

spell d w = when (incorrect d w) (putStrLn w)
