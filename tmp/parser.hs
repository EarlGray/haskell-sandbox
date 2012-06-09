import Language.Haskell.Parser
-- import Language.Haskell.Syntax

main = do
    s <- readFile "fio.hs"
    let parsed = parseModule s
    putStrLn . show $ parsed
