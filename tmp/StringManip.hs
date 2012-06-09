module StringManip where

import Data.Char

uppercase, lowercase :: String -> String
uppercase = map toUpper
lowercase = map toLower

capitalize :: String -> String
capitalize x =
    let capWord []     = []
        capWord (x:xs) = toUpper x: xs
    in unwords (map capWord (words x))

isL :: Char -> Bool
isL x = (x == 'l') 

concatenate :: String String -> String
concatenate s1 s2 = s1 ++ s2
