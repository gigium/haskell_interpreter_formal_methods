import Data.Char
import Data.List
import qualified Data.Map as M

data Operator = Plus | Minus | Times | Div 
                | And | Or | Not 
                | Greater | GreaterEqual | Less | LessEqual | Equal | NotEqual 
                | While | If 
    deriving (Show, Eq)

--tokenizer 
data Token = TokOp Operator
           | TokAssign
           | TokLParen
           | TokRParen
           | TokCondStart
           | TokCondFinish
           | TokSeqStart
           | TokSeqFinish 
           | TokIdent String
           | TokNum Double
           | TokBool Bool
           | TokStmEnd
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
    | c == '['  = TokCondStart : tokenize cs
    | c == ']'  = TokCondFinish : tokenize cs
    | c == '{'  = TokSeqStart : tokenize cs
    | c == '}'  = TokSeqFinish : tokenize cs
    | c == ';'  = TokStmEnd : tokenize cs
    | isAlpha c && isUpper c && c == 'G' = greaterKeyword cs    
    | isAlpha c && isUpper c && c == 'L' = lessKeyword cs    
    | isAlpha c && isUpper c && c == 'E' = equalKeyword cs    
    | isAlpha c && isUpper c && c == 'T' = trueKeyword cs
    | isAlpha c && isUpper c && c == 'F' = falseKeyword cs
    | isAlpha c && isUpper c && c == 'W' = whileKeyword cs
    | isAlpha c && isUpper c && c == 'I' = ifKeyword cs
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
                 Nothing -> error $ "Cannot tokenize [TRUE not recognized]"


falseKeyword :: String -> [Token]
falseKeyword cs = case stripPrefix "ALSE" cs of
                 Just restOfString -> TokBool (False) : tokenize restOfString
                 Nothing -> error $ "Cannot tokenize [FALSE not recognized]"

greaterKeyword :: String -> [Token]
greaterKeyword cs = case stripPrefix "REATER_EQUAL" cs of
                 Just restOfString -> TokOp (GreaterEqual) : tokenize restOfString
                 Nothing -> case stripPrefix "REATER" cs of
                            Just restOfString -> TokOp (Greater) : tokenize restOfString
                            Nothing -> error $ "Cannot tokenize [GREATER/GREATER_EQUAL not recognized]"


lessKeyword :: String -> [Token]
lessKeyword cs = case stripPrefix "ESS_EQUAL" cs of
                 Just restOfString -> TokOp (LessEqual) : tokenize restOfString
                 Nothing -> case stripPrefix "ESS" cs of
                            Just restOfString -> TokOp (Less) : tokenize restOfString
                            Nothing -> error $ "Cannot tokenize [LESS/LESS_EQUAL not recognized]"

equalKeyword :: String -> [Token]
equalKeyword cs = case stripPrefix "QUAL_NOT" cs of
                 Just restOfString -> TokOp (NotEqual) : tokenize restOfString
                 Nothing -> case stripPrefix "QUAL" cs of
                            Just restOfString -> TokOp (Equal) : tokenize restOfString
                            Nothing -> error $ "Cannot tokenize [EQUAL/EQUAL_NOT not recognized]"


whileKeyword :: String -> [Token]
whileKeyword cs = case stripPrefix "HILE" cs of
                 Just restOfString -> TokOp (While) : tokenize restOfString
                 Nothing -> error $ "Cannot tokenize [WHILE not recognized]"


ifKeyword :: String -> [Token]
ifKeyword cs = case stripPrefix "F" cs of
                 Just restOfString -> TokOp (If) : tokenize restOfString
                 Nothing -> error $ "Cannot tokenize [IF not recognized]"

--parser--

data Expr = ConditionalNode Operator Expr Expr
          | SeqNode [Expr]
          | AssignNode String Expr
          | VarNode String
          | CommandNode Expr 
          | LogicNode Operator Expr Expr
          | ArithmeticLogicNode Operator Expr Expr
          | BoolNode Bool
          | BoolVarNode String
          | UnaryBoolNode Operator Expr
          | SumNode Operator Expr Expr
          | ProdNode Operator Expr Expr
          | UnaryNode Operator Expr
          | NumNode Double
    deriving Show          


lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (t:ts) = t


accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts



sqnc :: ([Token], [Expr]) -> (Expr, [Token])
sqnc (toks, expr_array) = 
  case lookAhead toks of
    TokSeqStart -> sqnc(accept toks, expr_array)
    TokSeqFinish -> (SeqNode expr_array, accept toks)
    _ ->
       let (commandTree, toks') = command toks 
       in
          case lookAhead toks' of
              TokStmEnd -> sqnc(accept toks', expr_array++[commandTree])
              _ -> error $ "error on token: " ++ show toks'



command :: [Token] -> (Expr, [Token])
command toks = 
    let (expTree, toks') = expression(toks) 
      in case lookAhead toks'  of
        TokStmEnd -> (CommandNode expTree, toks')
        _ -> (expTree,  toks')




expression :: [Token] -> (Expr, [Token])
expression toks = 
   let (termTree, toks') = term toks
   in
      case lookAhead toks' of
         (TokOp op) | elem op [And, Or, Not] -> 
            let (exTree, toks'') = expression (accept toks') 
            in (LogicNode op termTree exTree, toks'')
         (TokOp op) | elem op [Greater, GreaterEqual, Less, LessEqual, Equal, NotEqual ] -> 
            let (exTree, toks'') = expression (accept toks') 
            in (ArithmeticLogicNode op termTree exTree, toks'')
         (TokOp op) | elem op [Plus, Minus] -> 
            let (exTree, toks'') = expression (accept toks') 
            in (SumNode op termTree exTree, toks'')
         TokAssign ->
            case termTree of
               VarNode str -> 
                  let (exTree, toks'') = expression (accept toks') 
                  in (AssignNode str exTree, toks'')
               _ -> error "Only variables can be assigned to"
         _ -> (termTree, toks')



term :: [Token] -> (Expr, [Token])
term toks = 
    case lookAhead toks of
      TokNum n -> a_term(toks)
      TokBool b -> b_term(toks)
      (TokOp op) | elem op [While, If] ->
        let (condTree, seqTree, toks') = c_term(accept toks)
        in 
          (ConditionalNode op condTree seqTree, toks')
      (TokOp op) | elem op [Not] -> b_term(toks)
      (TokOp op) | elem op [Plus, Minus] -> a_term(toks)
      _ -> error $ " " ++ show toks



c_term :: [Token] -> (Expr, Expr, [Token])
c_term toks = 
    case lookAhead toks of
    TokCondStart      -> 
       let (expTree, toks') = expression (accept toks)
       in
          if lookAhead toks' /= TokCondFinish 
          then error $ "Missing ] in " ++ show toks'
          else let toks'' = accept toks' 
            in 
              case lookAhead toks'' of
                TokSeqStart -> 
                  let (seqTree, toks_r) = sqnc(toks'', [])
                  in
                    (expTree, seqTree, toks_r)
    _ -> error $"Error !CONDSTART " ++ (show . accept) toks


b_term :: [Token] -> (Expr, [Token])
b_term toks = 
   case lookAhead toks of
      (TokBool x)     -> (BoolNode x, accept toks)
      (TokIdent str) -> (BoolVarNode str, accept toks)
      (TokOp op) | elem op [Not] -> 
            let (facTree, toks') = b_term (accept toks) 
            in (UnaryBoolNode op facTree, toks')
      TokLParen      -> 
         let (expTree, toks') = expression (accept toks)
         in
            if lookAhead toks' /= TokRParen 
            then error "Missing right parenthesis"
            else (expTree, accept toks')
      _ -> error $ "Parse error on token: " ++ show toks



a_term :: [Token] -> (Expr, [Token])
a_term toks = 
   let (facTree, toks') = a_factor toks
   in
      case lookAhead toks' of
         (TokOp op) | elem op [Times, Div] ->
            let (termTree, toks'') = a_term (accept toks') 
            in (ProdNode op facTree termTree, toks'')
         _ -> (facTree, toks')


a_factor :: [Token] -> (Expr, [Token])
a_factor toks = 
   case lookAhead toks of
      (TokNum x)     -> (NumNode x, accept toks)
      (TokIdent str) -> (VarNode str, accept toks)
      (TokOp op) | elem op [Plus, Minus] -> 
            let (facTree, toks') = a_factor (accept toks) 
            in (UnaryNode op facTree, toks')
      TokLParen      -> 
         let (expTree, toks') = expression (accept toks)
         in
            if lookAhead toks' /= TokRParen 
            then error "Missing right parenthesis"
            else (expTree, accept toks')
      _ -> error $ "Parse error on token: " ++ show toks


--evaluating--

type Store = [(String, String)]

see :: (Eq a) => a -> [(a,b)] -> Maybe b
see _key [] = Nothing
see key ((x,y):xys)
 | key == x = Just y
 | otherwise = see key xys


execute :: Expr -> Store -> Store
execute (SeqNode []) r = r
execute (SeqNode (s : ss)) r =  execute (SeqNode ss) (execute s r)
execute (CommandNode e) r = let d = evaluate e r in r



evaluate :: Expr -> Store -> Store
evaluate (AssignNode s e) r = (s, show (evaluate e r)) : r
evaluate (ConditionalNode If b st) r | b_evaluate b r /= False = evaluate st r
evaluate (ConditionalNode While b s) r | b_evaluate b r /= False = execute (SeqNode [s,ConditionalNode While b s]) r
 | otherwise = r
--arithmetic
evaluate (NumNode n) r = 
  let  d = a_evaluate (NumNode n) r in r
evaluate (SumNode Plus e1 e2) r = 
  let  d = a_evaluate (SumNode Plus e1 e2) r in r
evaluate (SumNode Minus e1 e2) r = 
  let  d = a_evaluate (SumNode Minus e1 e2) r in r
evaluate (UnaryNode Plus e1) r = 
  let  d = a_evaluate (UnaryNode Plus e1) r in r
evaluate (UnaryNode Minus e1) r = 
  let  d = a_evaluate (UnaryNode Minus e1) r in r
evaluate (ProdNode Times e1 e2) r = 
  let  d = a_evaluate (ProdNode Times e1 e2) r in r
evaluate (ProdNode Div e1 e2) r = 
  let  d = a_evaluate (ProdNode Div e1 e2) r in r

--bool
evaluate (BoolNode b) r = 
  let  d = b_evaluate (BoolNode b) r in r

evaluate (LogicNode And e1 e2) r = 
  let  d = b_evaluate (LogicNode And e1 e2) r in r
evaluate (LogicNode Or e1 e2) r = 
  let  d = b_evaluate (LogicNode Or e1 e2) r in r
evaluate (UnaryBoolNode Not e1) r = 
  let  d = b_evaluate (UnaryBoolNode Not e1) r in r

evaluate (ArithmeticLogicNode Greater e1 e2) r = 
  let  d = b_evaluate (ArithmeticLogicNode Greater e1 e2) r in r
evaluate (ArithmeticLogicNode GreaterEqual e1 e2) r = 
  let  d = b_evaluate (ArithmeticLogicNode GreaterEqual e1 e2) r in r
evaluate (ArithmeticLogicNode Less e1 e2) r = 
  let  d = b_evaluate (ArithmeticLogicNode Less e1 e2) r in r
evaluate (ArithmeticLogicNode LessEqual e1 e2) r = 
  let  d = b_evaluate (ArithmeticLogicNode LessEqual e1 e2) r in r
evaluate (ArithmeticLogicNode Equal e1 e2) r = 
  let  d = b_evaluate (ArithmeticLogicNode Equal e1 e2) r in r
evaluate (ArithmeticLogicNode NotEqual e1 e2) r = 
  let  d = b_evaluate (ArithmeticLogicNode NotEqual e1 e2) r in r
 

          -- | SumNode Operator Expr Expr
          -- | ProdNode Operator Expr Expr
          -- | UnaryNode Operator Expr
          -- | NumNode Double

a_evaluate ::  Expr -> Store -> Double
a_evaluate (NumNode n) r = n
a_evaluate (VarNode x) r = case see x r of
 Nothing -> error ("unbound variable `" ++ x ++ "'")
 Just v -> read v
a_evaluate (SumNode Plus e1 e2) r = a_evaluate e1 r + a_evaluate e2 r
a_evaluate (SumNode Minus e1 e2) r = a_evaluate e1 r - a_evaluate e2 r
a_evaluate (UnaryNode Plus e) r = 0.0+( a_evaluate e r)
a_evaluate (UnaryNode Minus e) r = 0.0- (a_evaluate e r)
a_evaluate (ProdNode Times e1 e2) r = a_evaluate e1 r * a_evaluate e2 r
a_evaluate (ProdNode Div e1 e2) r = a_evaluate e1 r / a_evaluate e2 r

--           | LogicNode Operator Expr Expr
--           | ArithmeticLogicNode Operator Expr Expr
--           | AssignBoolNode String Expr
--           | BoolVarNode String
--           | BoolNode Bool
--           | UnaryBoolNode Operator Expr

b_evaluate ::  Expr -> Store -> Bool
b_evaluate (BoolNode b) r = b
b_evaluate (BoolVarNode x) r = case see x r of
 Nothing -> error ("unbound variable `" ++ x ++ "'")
 Just v -> read v
b_evaluate (LogicNode And e1 e2) r = b_evaluate e1 r && b_evaluate e2 r
b_evaluate (LogicNode Or e1 e2) r = b_evaluate e1 r || b_evaluate e2 r
b_evaluate (ArithmeticLogicNode Greater e1 e2) r = b_evaluate e1 r > b_evaluate e2 r
b_evaluate (ArithmeticLogicNode GreaterEqual e1 e2) r = b_evaluate e1 r >= b_evaluate e2 r
b_evaluate (ArithmeticLogicNode Less e1 e2) r = b_evaluate e1 r < b_evaluate e2 r
b_evaluate (ArithmeticLogicNode LessEqual e1 e2) r = b_evaluate e1 r <= b_evaluate e2 r
b_evaluate (ArithmeticLogicNode Equal e1 e2) r = b_evaluate e1 r == b_evaluate e2 r
b_evaluate (ArithmeticLogicNode NotEqual e1 e2) r = b_evaluate e1 r /= b_evaluate e2 r


run:: String -> Store -> IO()
run str store = 
  let toks = tokenize str in
    let (tree, _) = sqnc(toks, []) in 
      let r = execute (tree) store in do
        print toks
        print " " 
        print tree
