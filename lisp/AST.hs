module AST (
    SExpr(..),
    Atom(..),

    topSExprs, 
    makeSExp,
    toSymbol,
    quote,
) where

import LexAn

import Data.List (intercalate)

data SExpr = SList [SExpr]
           | SAtom Atom
           | SError String

data Atom = AtomInt Integer
          | AtomFloat Float
          | AtomString String
          | AtomSymbol String

instance Show SExpr where
    show (SError s) = "*** " ++ s
    show (SAtom atom) = show atom
    show (SList ss) = "(" ++ (intercalate " " $ map show ss) ++ ")"

instance Show Atom where
    show (AtomInt i) = show i ++ "i"
    show (AtomFloat f) = show f ++ "f"
    show (AtomString s) = show s
    show (AtomSymbol sym) = sym

toSymbol :: String -> SExpr
toSymbol s = SAtom $ AtomSymbol s

quote :: SExpr -> SExpr
quote sexp = SList [toSymbol "quote", sexp]

topSExprs :: [Lexeme] -> [SExpr]
topSExprs ls = topSExprs' [] ls
    where 
    topSExprs' ss [] = reverse ss
    topSExprs' ss ls = 
      let (s, ls') = makeSExp ls
      in topSExprs' (s:ss) ls'                 

makeSExp :: [Lexeme] -> (SExpr, [Lexeme])
makeSExp [] = (SError "no lexemes", [])
makeSExp (l:ls) = 
    case l of
      LInt i -> (SAtom $ AtomInt i, ls)
      LFloat f -> (SAtom $ AtomFloat f, ls)
      LString s -> (SAtom $ AtomString s, ls)
      LSymbol sym -> (SAtom $ AtomSymbol sym, ls)
      LSP -> makeSList ls (SList [])
      LQuote -> let (q, ls') = makeSExp ls
                in (quote q, ls')
      _ -> let err = SError $ "Syntax error: can't use lexeme " ++ show l
           in (err, ls)

makeSList :: [Lexeme] -> SExpr -> (SExpr, [Lexeme])
makeSList [] sexp = (SError "no lexemes", [])
makeSList (LCP : ls) (SList ss) = ((SList $ reverse ss), ls)
makeSList ls (SList ss) =
    let (item, ls') = makeSExp ls
    in makeSList ls' $ SList (item:ss)
