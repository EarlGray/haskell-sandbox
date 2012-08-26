module Eval (

) where

import AST
import LexAn

data Value 
    = ValInt Integer
    | ValFloat Float
    | ValString String
    | ValList [Value]
    | ValError String


eval :: SExpr -> Value
eval s | isSelfEvaluating s = evalSelf s
eval s | isSymbol s         = evalSymbol s

isSelfEvaluating :: SExpr -> Bool
isSelfEvaluating (SAtom a) = 
    case s of
      ValInt _ -> True
      ValFloat _ -> True
      ValString _ -> True
      ValError _ -> True
      _ -> False

evalSelf s
