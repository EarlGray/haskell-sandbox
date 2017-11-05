import Data.Map ((!))
import qualified Data.Map as M
import qualified Data.List as L

{-
 -  Types
 -}
type Var = String
type Val = Integer
type State = M.Map Var Val

type AST = CExp

data CExp 
    = CSeq [CExp]
    | CAssign Var AExp
    | CIf BExp CExp CExp
    | CWhile BExp CExp
    deriving Eq

data BExp
    = BLit Bool
    | BAnd BExp BExp 
    | BOr BExp BExp
    | BLt AExp AExp
    deriving (Show, Eq)

data Op
    = Plus | Minus | Mul | Div
    deriving (Show, Eq, Ord)

data AExp
    = ALit Val
    | AVar Var
    | AOp Op AExp AExp
    deriving (Show, Eq)

precAOp = M.fromList [
    (Div, 40), (Mul, 40),
    (Plus, 30), (Minus, 30) ]

{-=
 -	Pretty print
 -}
instance Show CExp where
  show (CSeq block) =
    L.intercalate " ;\n" $ map show block
  show (CAssign var aexp) = 
    L.intercalate " " [var, ":=", show aexp]
  show (CIf cond thenb elseb) =
    "if " ++ show cond ++ " then {\n" ++ show thenb ++ "\n} else {\n" ++ show elseb ++ "\n}"
  show (CWhile cond block) =
    "while " ++ show cond ++ " do {\n" ++ show block ++ "\n}"

{-
 -  Parsing
 -}
data Token
    = TVal Val
    | TBool Bool            -- "true", "false"
    | TId String
    | TIf | TThen | TElse
    | TWhile | TDo
    | TExpOpen | TExpClose  -- "(", ")"
    | TBAnd | TBOr
    | TAOp Op
    | TRLt | TRGt       -- "<", ">"
    | TBlockOpen | TBlockClose  -- "{", "}"
    | TSep              -- ";"
    | TAssign           -- ":="
    deriving (Show, Eq)

    
lexer :: String -> [Token]
lexer = map toToken . words 
  where
    toToken w =
      case M.lookup w literals of
        Just lit -> lit
        Nothing -> 
          case reads w of
            [(val, "")] -> TVal val
            _ -> TId w

    literals = M.fromList [
      ("if", TIf),
      ("then", TThen),
      ("else", TElse),
      ("while", TWhile),
      ("do", TDo),
      ("true", TBool True),
      ("false", TBool False),
      ("and", TBAnd),
      ("or", TBOr),
      ("(", TExpOpen), 
      (")", TExpClose),
      ("+", TAOp Plus),
      ("-", TAOp Minus),
      ("*", TAOp Mul),
      ("/", TAOp Div),
      ("<", TRLt),
      (">", TRGt),
      ("{", TBlockOpen),
      ("}", TBlockClose),
      (";", TSep),
      (":=", TAssign) ]

type Parser a = [Token] -> [(a, [Token])]

parse :: String -> AST
parse s = 
  case parseStmts $ lexer s of
    [(stmts, [])] -> CSeq stmts
    smth -> error $ "parse: " ++ show smth

parseStmts :: Parser [CExp]
parseStmts ts = 
  case parseStmt ts of
    [(stmt, TSep : ts1)] -> 
      case parseStmts ts1 of
        [(stmts, ts2)] -> [(stmt : stmts, ts2)]
        smth -> error $ "parseStmts (expected '<stmts>' after ';'): " ++ show ts1
    [(stmt, ts1)] -> [([stmt], ts1)]

parseStmt :: Parser CExp
parseStmt (TIf : ts) = 
  case parseBExp ts of
    [(bexp, TThen : TBlockOpen : ts1)] ->
      case parseStmts ts1 of
        [(thenStmts, TBlockClose : TElse : TBlockOpen : ts2)] ->
          case parseStmts ts2 of
            [(elseStmts, TBlockClose : ts3)] ->
              [(CIf bexp (CSeq thenStmts) (CSeq elseStmts), ts3)]
            _ -> error $ "parseStmt (expected '}' after <stmts> of 'else'): " ++ show ts2
        _ -> error $ "parseStmt (expected '}' after <stmts> of 'then'): " ++ show ts1
    _ -> error $ "parseStmt (expected '<bexp>_then_{' after 'if'): " ++ show ts
parseStmt (TWhile : ts) =
  case parseBExp ts of
    [(cond, TDo : TBlockOpen : ts1)] ->
      case parseStmts ts1 of
        [(stmts, TBlockClose : ts2)] -> 
          [(CWhile cond (CSeq stmts), ts2)]
        _ -> error $ "parseStmt (expected '}' after <stmts> of 'while'): " ++ show ts1
    _ -> error $ "parseStmt (expected '<bexp>_do_{' after 'while'): " ++ show ts
parseStmt (TId var : TAssign : ts) =
  case parseAExp ts of
    [(aexp, ts2)] ->
      [(CAssign var aexp, ts2)]
    _ -> error $ "parseStmt (expected <aexp> after ':='): " ++ show ts
parseStmt ts = error $ "parseStmt: " ++ show ts


parseBExp :: Parser BExp
parseBExp ts = let (bexp, ts1) = go ts ([], []) in [(bexp, ts1)]
  where
    {- shunting yard -}
    go :: [Token] -> ([Token], [BExp]) -> (BExp, [Token])
    -- go :: tokens -> (opstack, astq) -> (result, stream)
    go ts (ops, ast) =
      -- trace ("go ts=" ++ show (take 1 ts) ++ " ops=" ++ show ops ++ " ast=" ++ show ast) $ 
      case ts of
        {- values -}
        TBool b : ts1 -> go ts1 (ops, BLit b : ast)

        TExpOpen : ts1 ->
          case parseBExp ts1 of
            [(bexp, TExpClose : ts2)] ->
              go ts2 (ops, bexp : ast)
            _ -> error $ "parseBExp (expected <bexp> after '('): " ++ show ts1
        
        {- binops -}
        TBAnd : ts1 -> 
          let (ops1, ast1) = unwindAnd ops ast
          in go ts1 (TBAnd : ops1, ast1)
        TBOr : ts1 ->
          let (ops1, ast1) = unwindOr ops ast
          in go ts1 (TBOr : ops1, ast1)

        _ -> 
          case parseAExp ts of
            {- comparisons -}
            [(a1, TRLt : ts1)] ->
              case parseAExp ts1 of
                [(a2, ts2)] -> 
                  let bexp = BLt a1 a2
                  in go ts2 (ops, bexp : ast)
                _ -> error $ "parseBExp (expected <aexp> after '<'): " ++ show ts1
            [(a1, TRGt : ts1)] ->
              case parseAExp ts1 of
                [(a2, ts2)] -> 
                  let bexp = BLt a2 a1
                  in go ts2 (ops, bexp : ast)
                _ -> error $ "parseBExp (expected <aexp> after '>'): " ++ show ts1
            {- just return -}
            _ -> 
              case ops of
                [] -> (head ast, ts)
                _ -> go ts $ unwindOr ops ast

    unwindAnd (TBAnd : ops) (b2 : b1 : ast1) =
      unwindAnd ops (BAnd b1 b2 : ast1)
    unwindAnd ops ast = (ops, ast)
      
    unwindOr (TBAnd : ops) (b2 : b1 : ast1) = 
      unwindOr ops (BAnd b1 b2 : ast1)
    unwindOr (TBOr : ops) (b2 : b1 : ast1) = 
      unwindOr ops (BOr b1 b2 : ast1)
    unwindOr ops ast = (ops, ast)



parseAExp :: Parser AExp
parseAExp ts = 
    let (rpn, ts1) = go ts ([], []) 
        ast = grow_ast [] rpn
    in [(ast, ts1)]
  where
    {- shunting yard -}
    go ts (ops, outq) = 
      case ts of
        {- values -}
        TVal n : ts1 ->
          go ts1 (ops, TVal n : outq)
        TId var : ts1 ->
          go ts1 (ops, TId var : outq)
        
        {- binops -}
        op@(TAOp _) : ts1 ->
          go ts1 $ unwind op (ops, outq)

        {- brackets -}
        TExpOpen : ts1 ->
          go ts1 (TExpOpen : ops, outq)
        TExpClose : ts1 | TExpOpen `elem` ops ->
          go ts1 $ unwind TExpClose (ops, outq)

        _ -> 
          let rpn = reverse (reverse ops ++ outq)
          in (rpn, ts)

    unwind TExpClose (TExpOpen : ops, outq) = (ops, outq)
    unwind TExpClose (op : ops, outq) = unwind TExpClose (ops, op : outq)

    unwind theOp ([], outq) = ([theOp], outq)
    unwind theOp (ops@(TExpOpen : _), outq) = (theOp : ops, outq)
    unwind theOp@(TAOp tOp) (ops@(TAOp op : ops1), outq) =
      case (M.lookup op precAOp, M.lookup tOp precAOp) of
        (Just opprec, Just topprec) | opprec >= topprec ->
          unwind theOp (ops1, TAOp op : outq)
        _ -> (theOp : ops, outq)

    unwind top (ops, outq) = error $ "unwind top:" ++ show top ++ " ops:" ++ show ops ++ " outq:" ++ show outq

    grow_ast [ast] [] = ast
    grow_ast ast (TVal n : rpn) =
      grow_ast (ALit n : ast) rpn
    grow_ast ast (TId v : rpn) =
      grow_ast (AVar v : ast) rpn
    grow_ast (a1 : a2 : ast) (TAOp op : rpn) =
      grow_ast (AOp op a2 a1 : ast) rpn
    grow_ast ast rpn = error $ "grow_ast, ast:" ++ show ast ++ ", rpn:" ++ show rpn

         
{-
 -  Evaluation
 -}
eval :: AST -> State -> State
eval (CAssign var aexp) st =
    let val = aeval aexp st
    in -- trace (var ++ " := " ++ show val ++ " ;    # " ++ show aexp) $
       M.insert var val st
eval (CIf cond thenb elseb) st =
    eval (if beval cond st then thenb else elseb) st
eval (CWhile cond block) st = 
    if beval cond st 
    then
      let st1 = eval block st
      in eval (CWhile cond block) st1
    else st
eval (CSeq stmts) st = L.foldl' (\st stmt -> eval stmt st) st stmts

beval :: BExp -> State -> Bool
beval (BLit b) _ = b
beval (BAnd b1 b2) st = beval b1 st && beval b2 st
beval (BOr b1 b2) st = beval b1 st || beval b2 st
beval (BLt a1 a2) st = aeval a1 st < aeval a2 st

aeval :: AExp -> State -> Val
aeval (ALit val) _ = val
aeval (AVar var) st = st ! var
aeval (AOp op a1 a2) st = 
  let n1 = aeval a1 st
      n2 = aeval a2 st
  in case op of
    Plus -> n1 + n2
    Minus -> n1 - n2
    Mul -> n1 * n2
    Div -> n1 `div` n2

interpret :: AST -> State
interpret ast = eval ast M.empty

showState :: State -> String
showState = unlines . map format . M.toAscList
  where format (name, val) = name ++ " " ++ show val


main = do
    source <- getContents
    let ast = parse source
    -- traceIO $ show ast ++ "\n==================\n"
    let state = interpret ast
    putStr $ showState state
