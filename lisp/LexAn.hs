module LexAn (
    Lexeme(..),
    lexicalAnalyzer
) where

import Data.Maybe (isJust, fromJust, fromMaybe)

{-
 -  Lexical analyzersym 
 -}

data Lexeme 
    = LSP     -- start parenthesis 
    | LCP     -- closing prntsis.
    | LQuote
    | LInt Integer
    | LFloat Float
    | LSymbol String
    | LString String
    | LError

data LState 
    = LSWhitespace
    | LSSymbol Char
    | LSWord String
    | LSInString String Bool -- string; is the character escaped?
    | LSString String
    | LSComment
    | LSError

instance Show Lexeme where
    show LSP = "<(> "
    show LCP = "<)> "
    show LQuote = "<'> "
    show (LInt int) = "<I@" ++ show int ++ "> "
    show (LFloat f) = "<F@" ++ show f ++ "> "
    show (LSymbol s) = "<S@" ++ show s ++ "> "
    show (LString s) = "<\"" ++ show s ++ "\"> "
    show LError = "ERROR"

instance Eq LState where
    LSWhitespace == LSWhitespace    = True
    LSSymbol _ == LSSymbol _        = True
    LSWord _ == LSWord _            = True
    LSInString _ _ == LSInString _ _ = True
    LSString _   == LSString _      = True
    LSComment == LSComment          = True
    _ == _                          = False

symbols = "()'"
whitespaces = "\n\t "
escapes = zip "ntr" "\n\t\r"

lexicalSM :: Char -> LState -> LState
lexicalSM c LSWhitespace | c `elem` whitespaces = LSWhitespace
lexicalSM c LSWhitespace | c `elem` symbols     = LSSymbol c
lexicalSM c LSWhitespace | c == '"'             = LSInString "" False
lexicalSM c LSWhitespace | c == ';'             = LSComment
lexicalSM c LSWhitespace                        = LSWord [c]

lexicalSM c (LSSymbol _) | c `elem` whitespaces     = LSWhitespace
lexicalSM c (LSSymbol _) | c `elem` symbols         = LSSymbol c
lexicalSM c (LSSymbol _) | c == '"'                 = LSInString "" False
lexicalSM c (LSSymbol _) | c == ';'                 = LSComment 
lexicalSM c (LSSymbol _)                            = LSWord [c]

lexicalSM c (LSWord _) | c `elem` whitespaces       = LSWhitespace
lexicalSM c (LSWord _) | c `elem` symbols           = LSSymbol c
lexicalSM c (LSWord _) | c == '"'                   = LSError
lexicalSM c (LSWord _) | c == ';'                   = LSComment
lexicalSM c (LSWord w)                              = LSWord (c:w)

lexicalSM c     (LSInString s True) | isJust ec  = LSInString ((fromJust ec):s) False
    where ec = lookup c escapes
lexicalSM c     (LSInString s True)         = LSInString (c:s) False
lexicalSM '\\'  (LSInString s False)        = LSInString s True
lexicalSM '"'   (LSInString s False)        = LSString s
lexicalSM c     (LSInString s False)        = LSInString (c:s) False

lexicalSM c (LSString _) | c `elem` whitespaces     = LSWhitespace
lexicalSM c (LSString _) | c `elem` symbols         = LSSymbol c
lexicalSM c (LSString _) | c == ';'                 = LSComment
lexicalSM c (LSString _)                            = LSError

lexicalSM c (LSComment) | c == '\n' = LSWhitespace
lexicalSM c (LSComment)             = LSComment

lexicalAnalyzer' :: LState -> String -> [Lexeme]
lexicalAnalyzer' s [] = [lexem]
    where lexem = fromMaybe LError $ maybeLexeme s Nothing
    
lexicalAnalyzer' s (c:cs) = 
    let ns = lexicalSM c s
        mbLexeme = maybeLexeme s (Just ns)
    in case mbLexeme of
        Just LError -> [ LError ]
        Just lexem -> lexem:(lexicalAnalyzer' ns cs)
        Nothing -> lexicalAnalyzer' ns cs

isValidStateAfterWord :: Maybe LState -> Bool
isValidStateAfterWord mbLs = 
    case mbLs of
        Nothing -> True
        Just (LSSymbol _) -> True
        Just state -> state `elem` [LSWhitespace, LSComment]
        
maybeLexeme :: LState -> Maybe LState -> Maybe Lexeme
maybeLexeme state nextState = 
    case state of
      LSSymbol c -> Just $ lexemFromSymbol c
      _ -> 
        if Just state == nextState then Nothing
        else case state of
            LSWord w -> if isValidStateAfterWord nextState
                        then Just $ lexemFromWord $ reverse w
                        else Just LError
            LSString s -> Just $ LString $ reverse s
            LSError -> Just LError
            _ -> Nothing

lexemFromWord :: String -> Lexeme
lexemFromWord w = 
    case reads w :: [(Integer, String)] of
        [] -> tryReadFloat w
        [(i, "")] -> LInt i
        _ -> tryReadFloat w    
    where tryReadFloat w = 
            case reads w :: [(Float, String)] of
                [] -> LSymbol w
                [(f, "")] -> LFloat f
                _ -> LSymbol w


lexemFromSymbol :: Char -> Lexeme
lexemFromSymbol c = case c of
    '(' -> LSP
    ')' -> LCP
    '\'' -> LQuote
    _ -> LError

lexicalAnalyzer :: String -> [Lexeme]
lexicalAnalyzer [] = []
lexicalAnalyzer s = lexicalAnalyzer' LSWhitespace s

