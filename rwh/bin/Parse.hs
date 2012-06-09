module Parse where

data ParseState = ParseState {
  string :: L.ByteString,
  offset :: Int64
} deriving (Show)

simpleParse :: ParseState -> Either String (a, String)

newtype Parse a = Parse {
  runParser :: ParseState -> Either String (a, String)
}


