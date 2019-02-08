import Data.Char
import Data.List
import qualified Data.Map as M

data Operator = Plus | Minus | Times | Div | And | Or | Not
    deriving (Show, Eq)

--tokenizer 
data Token = TokOp Operator
           | TokAssign
           | TokLParen
           | TokRParen
           | TokIdent String
           | TokNum Double
           | TokBool Bool
           | TokEnd
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div
           | c == '&' = And
           | c == '|' = Or
           | c == '!' = Not


tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs) 
    | elem c "+-*/&|!" = TokOp (operator c) : tokenize cs
    | c == '='  = TokAssign : tokenize cs
    | c == '('  = TokLParen : tokenize cs
    | c == ')'  = TokRParen : tokenize cs
    -- | c == 'T' = TokBool True : tokenize cs
    -- | c == 'F'  = TokBool False : tokenize cs
    | isAlpha c && isUpper c && c == 'T' = trueKeyword cs
    | isAlpha c && isUpper c && c == 'F' = falseKeyword cs
    | isDigit c = number c cs
    | isAlpha c && isLower c = identifier c cs
    | isSpace c = tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]


identifier :: Char -> String -> [Token]
identifier c cs = let (name, cs') = span isAlphaNum cs in
                  TokIdent (c:name) : tokenize cs'

number :: Char -> String -> [Token]
number c cs = 
   let (digs, cs') = span isDigit cs in
   TokNum (read (c : digs)) : tokenize cs'


trueKeyword :: String -> [Token]
trueKeyword cs = case stripPrefix "RUE" cs of
                 Just restOfString -> TokBool (True) : tokenize restOfString
                 Nothing -> error $ "Cannot tokenize "


falseKeyword :: String -> [Token]
falseKeyword cs = case stripPrefix "ALSE" cs of
                 Just restOfString -> TokBool (False) : tokenize restOfString
                 Nothing -> error $ "Cannot tokenize "


---- parser ----

data Expr = AExpr 
          | BExpr
          -- | Stmt
          deriving(Show)

data AExpr = SumNode Operator AExpr AExpr
          | ProdNode Operator AExpr AExpr
          | AssignNode String AExpr
          | UnaryNode Operator AExpr
          | NumNode Double
          | VarNode String
    deriving Show

data BExpr = LogicNode Operator BExpr BExpr
          | AssignBoolNode String BExpr
          | BoolVarNode String
          | BoolNode Bool
          | UnaryBoolNode Operator BExpr
    deriving Show



lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (t:ts) = t

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts

a_expression :: [Token] -> (AExpr, [Token])
a_expression toks = 
   let (termTree, toks') = a_term toks
   in
      case lookAhead toks' of
         (TokOp op) | elem op [Plus, Minus] -> 
            let (exTree, toks'') = a_expression (accept toks') 
            in (SumNode op termTree exTree, toks'')
         TokAssign ->
            case termTree of
               VarNode str -> 
                  let (exTree, toks'') = a_expression (accept toks') 
                  in (AssignNode str exTree, toks'')
               _ -> error "Only variables can be assigned to"
         _ -> (termTree, toks')


a_term :: [Token] -> (AExpr, [Token])
a_term toks = 
   let (facTree, toks') = a_factor toks
   in
      case lookAhead toks' of
         (TokOp op) | elem op [Times, Div] ->
            let (termTree, toks'') = a_term (accept toks') 
            in (ProdNode op facTree termTree, toks'')
         _ -> (facTree, toks')


a_factor :: [Token] -> (AExpr, [Token])
a_factor toks = 
   case lookAhead toks of
      (TokNum x)     -> (NumNode x, accept toks)
      (TokIdent str) -> (VarNode str, accept toks)
      (TokOp op) | elem op [Plus, Minus] -> 
            let (facTree, toks') = a_factor (accept toks) 
            in (UnaryNode op facTree, toks')
      TokLParen      -> 
         let (expTree, toks') = a_expression (accept toks)
         in
            if lookAhead toks' /= TokRParen 
            then error "Missing right parenthesis"
            else (expTree, accept toks')
      _ -> error $ "Parse error on token: " ++ show toks


b_expression :: [Token] -> (BExpr, [Token])
b_expression toks = 
   let (termTree, toks') = b_term toks
   in
      case lookAhead toks' of
         (TokOp op) | elem op [And, Or, Not] -> 
            let (exTree, toks'') = b_expression (accept toks') 
            in (LogicNode op termTree exTree, toks'')
         TokAssign ->
            case termTree of
               BoolVarNode str -> 
                  let (exTree, toks'') = b_expression (accept toks') 
                  in (AssignBoolNode str exTree, toks'')
               _ -> error "Only variables can be assigned to"
         _ -> (termTree, toks')


b_term :: [Token] -> (BExpr, [Token])
b_term toks = 
   case lookAhead toks of
      (TokBool x)     -> (BoolNode x, accept toks)
      (TokIdent str) -> (BoolVarNode str, accept toks)
      (TokOp op) | elem op [Not] -> 
            let (facTree, toks') = b_term (accept toks) 
            in (UnaryBoolNode op facTree, toks')
      TokLParen      -> 
         let (expTree, toks') = b_expression (accept toks)
         in
            if lookAhead toks' /= TokRParen 
            then error "Missing right parenthesis"
            else (expTree, accept toks')
      _ -> error $ "Parse error on token: " ++ show toks


parse :: [Token] -> BExpr
parse toks = let (tree, toks') = b_expression toks
             in
               if null toks' 
               then tree
               else error $ "Leftover tokens: " ++ show toks'

-- main = (print . parse . tokenize) "x1 = -15 / (2 + x2)"

-- type SymTab = M.Map String String 

-- rd :: String -> SymTab -> (String, SymTab)
-- rd str symTab = 
--     case M.lookup str symTab of
--       Just v -> (v, symTab)
--       Nothing -> error $ "Undefined variable " ++ str

-- wr :: String -> String -> SymTab -> ((), SymTab)
-- wr str val symTab = 
--     let symTab' = M.insert str val symTab
--     in ((), symTab')

-- eval :: Tree -> SymTab -> Double
evalA :: AExpr -> Double
evalA ex = case ex of
-- eval ex r = case ex of
  NumNode n -> n
  -- VarNode x -> x
  SumNode Plus a b -> evalA a + evalA b
  SumNode Minus a b -> evalA a - evalA b
  ProdNode Times a b -> evalA a * evalA b
  ProdNode Div a b -> evalA a / evalA b
  UnaryNode Plus a -> evalA a
  UnaryNode Minus a -> - (evalA a)


evalB :: BExpr -> Bool
evalB ex = case ex of
  BoolNode b -> b
  LogicNode And a b -> evalB a && evalB b
  LogicNode Or a b -> evalB a || evalB b
  UnaryBoolNode Not a -> not(evalB a)


-- run :: String -> SymTab -> IO()
-- run x r = do
--     let t = (tokenize) x
--     let p = (parse) t
--     let e = (eval) p r
--     print t
--     print p
--     print e

-- run :: String -> IO()
-- run x = (print . eval . parse . tokenize) x