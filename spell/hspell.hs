import System.Environment (getArgs)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B
import qualified Data.Set as S
import Control.Monad 

bwords :: B.ByteString -> [B.ByteString]
bwords = map B.fromString . words . B.toString

main :: IO ()
main = do
    dict <- liftM (S.fromList . bwords) $ B.readFile "/usr/share/dict/words"
    (fname:_) <- getArgs
    file <- B.readFile fname
    let ws = bwords file
    mapM_ (putStrLn . B.toString) $ checkWords dict ws

checkWords :: S.Set B.ByteString -> [B.ByteString] -> [B.ByteString]
checkWords d ws = filter (flip S.notMember d) ws
