import System.Environment

main = do [f] <- getArgs
          s <- readFile f
          putStr s
