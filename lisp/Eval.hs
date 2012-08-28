module Eval (
    Env,
    makeEnv,
    eval,
) where

import AST
import LexAn

import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as M

data Env = Env {
}

makeEnv :: Env
makeEnv = Env 

eval :: Env -> SExpr -> (SExpr, Env)

eval env err@(SError _) = (err, env)

eval env sAtom@(SAtom atom) =
    case atom of
      AtomSymbol "nil" -> (sAtom, env)
      AtomSymbol sym -> (symval, env) 
        where symval = lookupSymbol env sym
      _ -> (sAtom, env)

eval env sList@(SList []) = (SAtom $ AtomSymbol "nil", env)
eval env sList@(SList ss@((SAtom hd):tl)) = 
    case hd of
      AtomSymbol "quote" -> (val, env)
        where val = fromMaybe (head tl) $ assertFormLength 2 ss
      AtomSymbol "if" -> fromMaybe (evalIf env predf thenf elsef) $ withEnv env $ assertFormLength 4 ss
        where (predf:(thenf:(elsef:_))) = tl
      AtomSymbol _ -> (SError "eval: don't know how to evaluate the form", env)

eval env _ = (SError "eval error", env)

evalIf :: Env -> SExpr -> SExpr -> SExpr -> (SExpr, Env)
evalIf env predicate thenf elsef =
    let (p, env') = eval env predicate
    in case p of
        SError _ -> (p, env')
        _ -> if isTrue p then eval env thenf else eval env elsef

lookupSymbol :: Env -> String -> SExpr       -- maybe SError
lookupSymbol env sym = SError "symbol not found: lookup not implemented yet"

envAddSymbol :: String -> SExpr -> Env -> Env
envAddSymbol name val env = env


assertFormLength :: Int -> [SExpr] -> Maybe SExpr
assertFormLength n ss = 
    if length ss == n then Nothing
    else Just $ SError $ "invalid number of arguments, must be " ++ show n

withEnv :: Env -> Maybe SExpr -> Maybe (SExpr, Env)
withEnv env = fmap (flip (,) env)

