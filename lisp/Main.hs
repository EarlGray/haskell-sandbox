module YALisp where

import System.Console.Readline

type Error = String
data EvalResult = Evaluated LispEnv
                | EvalError Error
                | EvalQuit

data Atom = LString String
          | LNumber Double
          | LNil

data Symbol = Symbol (String, SExp)

data LispEnv = LispEnv {
}

data AST = Branch [AST]
         | Symbol 
         | Atom 
         | ParseError String

instance (Read a) => Read AST where
    

getSymbol   :: LispEnv -> String -> Symbol
maybeSymbol :: LispEnv -> String -> Maybe Symbol
putSymbol   :: LispEnv -> String -> LispEnv

initialEnv = { }

lRead   :: String -> AST
lEval   :: LispEnv -> AST -> EvalResult

lREPL   :: LispEnv -> IO ()
lError  :: LispEnv -> Error -> EvalResult

lREPL world = do
    input <- readline (getSymbol world "*prompt*")
    case input of
        Nothing  -> return ()
        Just str -> do
            result <- lEval world $ lRead str
            case result of 
                EvalQuit -> return ()
                EvalError err -> 
                    do
                        print err
                        lREPL world
                Evaluated newWorld -> 
                    do
                        print $ getSymbol newWorld "it"
                        lREPL newWorld

lReadS :: (

lRead s = lReadS (s, "")

lReadS ([], _)          = Branch []
lReadS ((' ':cs), s)    = lReadS (cs, s)
lReadS (('(':cs), s)    = Branch [ e | (

main = lREPL initialEnv
