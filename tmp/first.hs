#! /usr/bin/env ghc

_getLine :: IO String
_getLine = do c <- getChar
              if c == '\n'
                 then return ""
                 else do l <- getLine
                         return (c:l)

_sequence_   :: [IO ()] -> IO ()
_sequence_   = foldr (>>) (return())

_putStr      :: String -> IO ()
_putStr s    = _sequence_ (map putChar s)

main :: IO ()
main = do s <- _getLine
          _putStr (s ++ "\n")

