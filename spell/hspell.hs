import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Char (isAlpha, toLower)
import Control.Monad 
import System.Environment (getArgs)

type MapS = M.Map B.ByteString ()

main :: IO ()
main = do
    dict <- liftM (M.fromList.map (flip (,)()).B.lines) $ B.readFile "/usr/share/dict/words"
    (fname:_) <- getArgs
    file <- B.readFile fname
    let ws = B.words file
    mapM_ (putStrLn . show) $ checkWords dict ws

checkWords :: MapS -> [B.ByteString] -> [B.ByteString]
checkWords d ws = filter (flip M.notMember d . clean) ws

clean = B.filter (\c -> isAlpha (toLower c) || c `elem` "'")
