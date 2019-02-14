import Data.Char
import Data.List
import System.IO 

{-___________________________________________________ TOKENIZER ______________________________________________________-}

data Operator = Plus | Minus | Times | Div 
                | And | Or | Not 
                | Greater | GreaterEqual | Less | LessEqual | Equal | NotEqual 
                | While | If 
    deriving (Show, Eq)

data Token = TokOp Operator
           | TokAssign -- =
           | TokLParen -- (
           | TokRParen -- )
           | TokCondStart -- [
           | TokCondFinish -- ]
           | TokSeqStart -- {
           | TokSeqFinish -- }
           | TokIdent String -- alphannum starting with lowercase letter
           | TokNum Integer
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

{-___________________________________________________ PARSER ______________________________________________________-}

--Definition of expression types -> nodes in the parser tree
data Tree = SeqNode [Tree]
          | StatementNode Tree 
          | AssignNode String Tree
          --arithmetic
          | SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | UnaryNode Operator Tree
          --boolean logic
          | LogicNode Operator Tree Tree
          | ArithmeticLogicNode Operator Tree Tree
          | UnaryBoolNode Operator Tree
          -- While, If
          | CommandNode Operator Tree Tree
          --terminals
          | NumNode Integer | BoolNode Bool | VarNode String          
    deriving Show          



lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (t:ts) = t


accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts


parse :: [Token] -> Tree
parse toks = let (tree, toks') = sqnc(toks, [])
             in
               if null toks' 
               then tree
               else error $ "Leftover tokens: " ++ show toks'



sqnc :: ([Token], [Tree]) -> (Tree, [Token])
sqnc (toks, expr_array) = 
  case lookAhead toks of
    TokSeqStart -> sqnc(accept toks, expr_array)
    TokSeqFinish -> (SeqNode expr_array, accept toks)
    _ ->
       let (commandTree, toks') = statement toks 
       in
          case lookAhead toks' of
              TokStmEnd -> sqnc(accept toks', expr_array++[commandTree])
              _ -> (commandTree, toks')



statement :: [Token] -> (Tree, [Token])
statement toks = 
    let (expTree, toks') = expression(toks) 
      in case lookAhead toks'  of
        TokStmEnd -> (StatementNode expTree, toks')
        _ -> (expTree,  toks')



expression :: [Token] -> (Tree, [Token])
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
         (TokOp op) | elem op [Times, Div] -> 
            let (exTree, toks'') = expression (accept toks') 
            in (ProdNode op termTree exTree, toks'')
         TokAssign ->
            case termTree of
               VarNode str -> 
                  let (exTree, toks'') = expression (accept toks') 
                  in (AssignNode str exTree, toks'')
               _ -> error "Only variables can be assigned to"
         _ -> (termTree, toks')



term :: [Token] -> (Tree, [Token])
term toks = 
    case lookAhead toks of
      TokNum n -> a_term(toks)
      TokBool b -> b_term(toks)
      (TokOp op) | elem op [While, If] ->
        let (condTree, seqTree, toks') = c_term(accept toks)
        in 
          (CommandNode op condTree seqTree, toks')
      (TokOp op) | elem op [Not] -> b_term(toks)
      (TokOp op) | elem op [Plus, Minus] -> a_term(toks)
      TokIdent str -> (VarNode str, accept toks) 
      _ -> error $ " " ++ show toks



c_term :: [Token] -> (Tree, Tree, [Token])
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


b_term :: [Token] -> (Tree, [Token])
b_term toks = 
   case lookAhead toks of
      (TokBool x)     -> (BoolNode x, accept toks)
      (TokIdent str) -> (VarNode str, accept toks)
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



a_term :: [Token] -> (Tree, [Token])
a_term toks = 
   let (facTree, toks') = a_factor toks
   in
      case lookAhead toks' of
         (TokOp op) | elem op [Times, Div] ->
            let (termTree, toks'') = a_term (accept toks') 
            in (ProdNode op facTree termTree, toks'')
         _ -> (facTree, toks')


a_factor :: [Token] -> (Tree, [Token])
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


{-___________________________________________________ EVALUATION ______________________________________________________-}

type Memory = [(String, String)]


--Reads the memory, if variables with the same name are in memory it reads the last updated value (STACK mode).
lookUp :: (Eq a) => a -> [(a,b)] -> Maybe b
lookUp _key [] = Nothing
lookUp key ((x,y):xys)
 | key == x = Just y
 | otherwise = lookUp key xys

--Executes a sequence of commands, returning the updated Memory. 
execute :: Tree -> Memory -> Memory
execute tr r = 
   case tr of
      (SeqNode []) -> r
      (SeqNode (s : ss)) -> execute (SeqNode ss) (execute s r)
      (StatementNode e) -> execute e r
      (AssignNode s e) -> (s, evaluate e r) : r
      (CommandNode If b st) -> 
         let cond = read(evaluate b r) in 
            if cond /= False then execute st r else r
      (CommandNode While b s) -> 
         let cond = read(evaluate b r) in 
            if cond /= False then execute (SeqNode [s,CommandNode While b s]) r else r
      _ -> 
        ("stdOut", evaluate tr r) : r


{-Evaluates the different Trees (logic and arithmetic), converting the value received by the type specific functions (boolean or integer) into
	a String storable in Memory-}
evaluate :: Tree -> Memory -> String 
--arithmetic
evaluate (NumNode n) r = show (a_evaluate (NumNode n) r)
evaluate (SumNode Plus e1 e2) r = show (a_evaluate (SumNode Plus e1 e2) r) 
evaluate (SumNode Minus e1 e2) r = show (a_evaluate (SumNode Minus e1 e2) r)
evaluate (UnaryNode Plus e1) r = show (a_evaluate (UnaryNode Plus e1) r )
evaluate (UnaryNode Minus e1) r = show (a_evaluate (UnaryNode Minus e1) r)
evaluate (ProdNode Times e1 e2) r = show (a_evaluate (ProdNode Times e1 e2) r) 
evaluate (ProdNode Div e1 e2) r = show (a_evaluate (ProdNode Div e1 e2) r) 
--bool
evaluate (BoolNode b) r = show(b_evaluate (BoolNode b) r)
evaluate (LogicNode And e1 e2) r = show(b_evaluate (LogicNode And e1 e2) r)
evaluate (LogicNode Or e1 e2) r = show(b_evaluate (LogicNode Or e1 e2) r)
evaluate (UnaryBoolNode Not e1) r = show(b_evaluate (UnaryBoolNode Not e1) r)
evaluate (ArithmeticLogicNode Greater e1 e2) r = show(b_evaluate (ArithmeticLogicNode Greater e1 e2) r)
evaluate (ArithmeticLogicNode GreaterEqual e1 e2) r = show(b_evaluate (ArithmeticLogicNode GreaterEqual e1 e2) r)
evaluate (ArithmeticLogicNode Less e1 e2) r = show(b_evaluate (ArithmeticLogicNode Less e1 e2) r)
evaluate (ArithmeticLogicNode LessEqual e1 e2) r = show(b_evaluate (ArithmeticLogicNode LessEqual e1 e2) r)
evaluate (ArithmeticLogicNode Equal e1 e2) r = show(b_evaluate (ArithmeticLogicNode Equal e1 e2) r)
evaluate (ArithmeticLogicNode NotEqual e1 e2) r = show(b_evaluate (ArithmeticLogicNode NotEqual e1 e2) r)
--var
evaluate (VarNode x) r = case lookUp x r of
  Nothing -> error ("unbound variable `" ++ x ++ "'")
  Just v -> v

--Evaluates arithmetic expressions returning the resulting integer value
a_evaluate ::  Tree -> Memory -> Integer
a_evaluate (NumNode n) r = n
a_evaluate (VarNode x) r = case lookUp x r of
 Nothing -> error ("unbound variable `" ++ x ++ "'")
 Just v -> read v
a_evaluate (SumNode Plus e1 e2) r = a_evaluate e1 r + a_evaluate e2 r
a_evaluate (SumNode Minus e1 e2) r = a_evaluate e1 r - a_evaluate e2 r
a_evaluate (UnaryNode Plus e) r = 0 +( a_evaluate e r)
a_evaluate (UnaryNode Minus e) r = 0 -(a_evaluate e r)
a_evaluate (ProdNode Times e1 e2) r = a_evaluate e1 r * a_evaluate e2 r
a_evaluate (ProdNode Div e1 e2) r = a_evaluate e1 r `div` a_evaluate e2 r

--Evaluates logic expressions returning the resulting boolean value
b_evaluate ::  Tree -> Memory -> Bool
b_evaluate (BoolNode b) r = b
b_evaluate (VarNode x) r = case lookUp x r of
 Nothing -> error ("unbound variable `" ++ x ++ "'")
 Just v -> read v
b_evaluate (LogicNode And e1 e2) r = b_evaluate e1 r && b_evaluate e2 r
b_evaluate (LogicNode Or e1 e2) r = b_evaluate e1 r || b_evaluate e2 r
b_evaluate (ArithmeticLogicNode Greater e1 e2) r = a_evaluate e1 r > a_evaluate e2 r
b_evaluate (ArithmeticLogicNode GreaterEqual e1 e2) r = a_evaluate e1 r >= a_evaluate e2 r
b_evaluate (ArithmeticLogicNode Less e1 e2) r = a_evaluate e1 r < a_evaluate e2 r
b_evaluate (ArithmeticLogicNode LessEqual e1 e2) r = a_evaluate e1 r <= a_evaluate e2 r
b_evaluate (ArithmeticLogicNode Equal e1 e2) r = a_evaluate e1 r == a_evaluate e2 r
b_evaluate (ArithmeticLogicNode NotEqual e1 e2) r = a_evaluate e1 r /= a_evaluate e2 r


--___________________________________________________ RUNNING FUNCTIONS ______________________________________________________ 

--rename of the String type in Program type
type Program = String

--Runs the given Program, on the specified Memory, returning the modified stack
_run :: Program -> Memory -> Memory
_run program store =
  let toks = tokenize program in
    let tree = parse toks in 
      let r = execute (tree) store in r


run :: Program -> VarName -> ProgramInput -> ProgramOutput -> IO()
run p var inp out =
  print $ lookUp out $ _run p [(var, inp)]


analyze :: Program -> Memory -> IO()
analyze str store = 
  let toks = tokenize str in
    let tree = parse toks in 
      let r = execute (tree) store in do
        print toks
        putStrLn " " 
        print tree


type VarName = String
type ProgramInput = String
type ProgramOutput = String

-- factorial "5"
factorial :: ProgramInput -> IO()
factorial number = 
  print $ lookUp "result" $ _run "{exit=1; n=num; result=num; WHILE[n EQUAL_NOT exit]{n=n-1; result=result*n;};}" [("num", number)]

-- fibonacci "5"
fibonacci :: ProgramInput -> IO()
fibonacci number = 
  print $ lookUp "result" $ _run "{result=0; n=num; w=0; y=1; WHILE[n EQUAL_NOT w]{z=result+y; result=y; y=z; n=n-1;};}" [("num", number)]

-- power "2" "4"
power :: ProgramInput -> ProgramInput -> IO()
power number exp = 
  print $ lookUp "result" $ _run "{result=1; count=0; n=num; ex=exp; WHILE[count LESS ex]{count=count+1;result=result*n;};}" [("num", number), ("exp", exp)]





--Command line interpreter
main = do
   putStrLn "pippo Command Line Interpreter"
   putStrLn ""
   loop []

loop mem = do
   putStr "pippo> "
   hFlush stdout 
   str <- getLine
   if null str
   then
      return ()
   else
      let mem' = _run(str) (("stdOut", "null"):mem)
      in let out = lookUp "stdOut" mem' in
        do
          print out
          loop mem'
