import Control.Monad
import Control.Monad.ST
import Data.STRef

sumST :: Num a => [a] -> a
sumST xs = runST $ do
    n <- newSTRef 0
    forM_ xs $ \x -> do
        modifySTRef n (+x)
    readSTRef n
