import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S
import Data.Char (isAlpha, toLower)
import Control.Monad 
import System.Environment (getArgs)

main :: IO ()
main = do
    dict <- liftM (S.fromList . B.lines) $ B.readFile "/usr/share/dict/words"
    (fname:_) <- getArgs
    file <- B.readFile fname
    let ws = B.words file
    mapM_ (putStrLn . show) $ checkWords dict ws

checkWords :: S.Set B.ByteString -> [B.ByteString] -> [B.ByteString]
checkWords d ws = filter (flip S.notMember d . clean) ws

clean = B.filter (\c -> isAlpha (toLower c) || c `elem` "'")
