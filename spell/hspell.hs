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
    mapM_ print $ checkWords dict ws

checkWords :: S.Set B.ByteString -> [B.ByteString] -> [B.ByteString]
checkWords d = filter (flip S.notMember d . clean)

clean = B.filter (\c -> isAlpha (toLower c) || c `elem` "'")
 -- (any id . (`map` [isAlpha.toLower, (`elem` "'")]) . flip ($))
