-- This is a dump of a solution of the Eight Queens Problem 
-- Made in GHCi, code style is preserved

import qualified Data.Array as A
import Data.Array ((//))

data Cell = Free | Beaten | Taken deriving Eq

showcell c = case c of { Free -> "_" ; Beaten -> "x" ; Taken -> "Q" ; }

clearboard = A.listArray ((0,0), (7,7)) (replicate (8 * 8) Free)

filtercells st = filter (\(_, s) -> s == st) . A.assocs 
queenupdate x y = ((x,y), Taken) : map (\i -> (i, Beaten)) ( [ (x', y) | x' <- [0..7], x' /= x] ++ [ (x, y') | y' <- [0..7], y' /= y] ++ [ (x', y') | x' <- [0..7], y' <- [0..7], abs(x' - x) == abs(y' - y), x' /= x ] )
setqueens n b = if n == 8 then [b] else concat $ map (\((x,y),_) -> setqueens (n + 1) (b // queenupdate x y)) (filtercells Free b)

boardline i = filter (\((x,y), _) -> x == i) . A.assocs
printline i = putStrLn . unwords . map (showcell . snd) . boardline i
printboard b = mapM_ (flip printline b) [0..7]

main = printboard $ head $ setqueens 0 clearboard
