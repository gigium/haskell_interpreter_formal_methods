import           Control.Applicative
import           Data.Char




newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P $ \inp ->
  case inp of
    []     -> []
    (x:xs) -> [(x, xs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x
  then return x
  else empty

symbol :: String -> Parser String
symbol xs = token (string xs)

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

space :: Parser ()
space = do
  many (sat isSpace)
  return ()




-- lower-case letters
lower :: Parser Char
lower = sat isLower

-- upper-case letters
upper :: Parser Char
upper = sat isUpper

-- arbitrary letters
letter :: Parser Char
letter = sat isAlpha

-- alphanumeric characters
alphanum :: Parser Char
alphanum = sat isAlphaNum

ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x:xs)

-- Using token, we can now define parsers that
-- ignore spacing around identifiers
identifier :: Parser String
identifier = token ident

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

-- ignore spacing around natural numbers
natural :: Parser Int
natural = token nat

digit :: Parser Char
digit = sat isDigit

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)

char :: Char -> Parser Char
char x = sat (== x)


true_keyword :: Parser String
true_keyword = symbol "TRUE"

false_keyword :: Parser String
false_keyword = symbol "FALSE"


bool_val :: Parser Bool
bool_val = do 
  true_keyword
  return (True)
  <|> 
  do 
    false_keyword
    return(False)
  

instance Functor Parser where
  fmap f p = P $ \inp ->
    case parse p inp of
      []         -> []
      [(v, out)] -> [(f v, out)]

instance Applicative Parser where
  pure v = P $ \inp -> [(v, inp)]

  pf <*> px = P $ \inp ->
    case parse pf inp of
      []         -> []
      [(f, out)] -> parse (fmap f px) out

instance Monad Parser where
  p >>= f = P $ \inp ->
    case parse p inp of
      []         -> []
      [(v, out)] -> parse (f v) out

instance Alternative Parser where
  empty = P $ \inp -> []

  p <|> q = P $ \inp ->
    case parse p inp of
      []         -> parse q inp
      [(v, out)] -> [(v, out)]

-----------------------------------------------------------------------------

data Operator = Plus | Minus | Times | Div 
                | And | Or | Not 
                | Greater | GreaterEqual | Less | LessEqual | Equal | NotEqual 
                | While | If 
    deriving (Show, Eq)

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
          | NumNode Int | BoolNode Bool | VarNode String          
    deriving Show     



-----------------------------------------------------------------------------
program :: Parser Tree
program = do 
  symbol "{"
  s <-  sqnc
  symbol "}"
  return (SeqNode s)


stm :: Parser Tree
stm = do
    c <- comm
    return (StatementNode c)
    <|>
    do
      e <- expr
      return (StatementNode e)




sqnc :: Parser [Tree]
sqnc = do 
  s <- stm 
  symbol ";"
  sq <- sqnc
  return ([s]++sq)
  <|>
  do 
    s <- stm 
    symbol ";"
    return ([s])




comm :: Parser Tree
comm = do 
  var <- identifier
  do symbol "="
     e <- expr
     return (AssignNode var e)
  <|> 
  do symbol "IF"
     symbol "["
     b <- expr
     symbol "]"
     s <-  program
     return (CommandNode If b s)
  <|> 
  do symbol "WHILE"
     symbol "["
     b <- expr
     symbol "]"
     s <-  program
     return (CommandNode While b s)


-----------------------------------------------------------------------------

expr :: Parser Tree
expr = do
  t <- term
  do symbol "+"
     e <- expr
     return (SumNode Plus  t e)
   <|> do symbol "-"
          e <- expr
          return (SumNode Minus  t e)
   <|> do symbol "AND"
          e <- expr
          return (LogicNode And t e)
   <|> do symbol "OR"
          e <- expr
          return (LogicNode And t e)
   <|> do symbol "GREATER"
          e <- expr
          return (ArithmeticLogicNode Greater t e)
   <|> do symbol "GREATER_EQUAL"
          e <- expr
          return (ArithmeticLogicNode GreaterEqual t e)
   <|> do symbol "LESS"
          e <- expr
          return (ArithmeticLogicNode Less t e)
   <|> do symbol "LESS_EQUAL"
          e <- expr
          return (ArithmeticLogicNode LessEqual t e)
   <|> do symbol "EQUAL"
          e <- expr
          return (ArithmeticLogicNode Equal t e)
   <|> do symbol "NOT_EQUAL"
          e <- expr
          return (ArithmeticLogicNode NotEqual t e)
   <|> return t



term :: Parser Tree
term = 
  do aterm
    <|>
    do bterm


aterm :: Parser Tree
aterm = do
  f <- afactor
  do symbol "*"
     t <- aterm
     return (ProdNode Times t f)
   <|> do symbol "/"
          t <- aterm
          return (ProdNode Div t f)
   <|> return f


afactor :: Parser Tree
afactor = 
  do symbol "("
     e <- expr
     symbol ")"
     return e
     <|> 
  do symbol "+"
     a <- afactor
     return (UnaryBoolNode Plus a)
     <|> 
  do symbol "-"
     a <- afactor
     return (UnaryBoolNode Minus a)
     <|> 
  do 
     id <-identifier
     return (VarNode id)
     <|> 
  do 
     n <- natural
     return (NumNode n)



bterm :: Parser Tree
bterm = 
  do symbol "("
     e <- expr
     symbol ")"
     return e
     <|> 
  do symbol "NOT"
     b <- bterm
     return (UnaryBoolNode Not b)
     <|> 
  do 
     id <-identifier
     return (VarNode id)
     <|> 
  do 
     b <- bool_val
     return (BoolNode b)



-- parse :: String -> Tree
-- eval xs = case (parse program xs) of
--   [(n, [])]  -> n
--   [(_, out)] -> error ("Unused input " ++ out)
--   []         -> error "Invalid input"

-----------------------------------------------------------------------------

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
a_evaluate ::  Tree -> Memory -> Int
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


--rename of the String type in Program type
type Program = String

type VarName = String
type ProgramInput = String
type ProgramOutput = String

run :: Program -> Memory -> Memory
run p env = case (parse program p) of
  [(t, [])]  -> execute (t) env
  [(_, out)] -> error ("Unused input " ++ out)
  []         -> error "Invalid input"

