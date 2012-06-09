import System.IO

file = "fio.hs"

main = do h <- openFile file ReadMode
          content <- hGetContents h
          putStr content
          hClose h
