import Control.Monad
import System.Random

showStat = do
    vals <- replicateM 30 $ randomRIO (1 :: Integer, 6)
    mapM print vals
    return ()

main = showStat
