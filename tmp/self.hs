import Language.Haskell.Parser
  
main = do
     s <- readFile "fio.hs"
     let parsed = parseModule s
     putStrLn . show $ parsed
