
splitBy n [] = []
splitBy n s  = (take n s) : (splitBy n (drop n s))
fourSplit s = unwords (splitBy 4 s)

spaceBy n s = unwords $ map (take n) $ takeWhile (not.null) $ iterate (drop n) s

main :: IO ()
main = do s <- getLine
--          putStr ((fourSplit s) ++ "\n")
          putStrLn $ fourSplit s ++ "\n"
