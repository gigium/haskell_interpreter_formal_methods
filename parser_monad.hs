import           Control.Applicative
import           Data.Char
import           Data.List
import           Data.Functor
import           Control.Monad


-- this declaration states that a parser of type a is a function that takes an input string and produces a list of results, 
-- each of which is a pair comprising a result value of type a and an output string.
-- To allow the Parser type to be made into instances of classes, it is first redefined using newtype, with
-- a dummy constructor called P
newtype Parser a = P (String -> [(a, String)])

-- Parser of this type can then be applied to an input string using a function that simply removes the dummy
-- constructor
parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp
  


-- We now make the parser type into an instance of the functor, applicative and monad classes, in order that
-- the do notation can then be used to combine parsers in sequence. The declarations are similar to those for
-- state transformers, except that we also need to take account of the possibility that a parser may fail. The
-- first step is to make the Parser type into a functor
instance Functor Parser where
-- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P $ \inp ->
    case parse p inp of
      []         -> []
      [(v, out)] -> [(f v, out)]
-- That is, fmap applies a function to the result value of a parser if the parser succeeds, and propagates the
-- failure otherwise (failure is the [] empty list). 



-- The Parser type can then be made into an applicative functor as follows
instance Applicative Parser where 
-- pure :: a -> Parser a
  pure v = P $ \inp -> [(v, inp)]
-- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = P $ \inp ->
    case parse pf inp of
      []         -> []
      [(f, out)] -> parse (fmap f px) out



-- Finally, we make the Parser type into a monad
instance Monad Parser where
-- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P $ \inp ->
    case parse p inp of
      []         -> []
      [(v, out)] -> parse (f v) out
-- That is, the parser p >>= f fails if the application of the parser p to the input string inp fails, and
-- otherwise applies the function f to the result value v to give another parser f v, which is then applied to
-- the output string out that was produced by the first parser to give the final result

-- Because Parser is a monadic type, the do notation can now be used to sequence parsers and process
-- their result values




-- Another natural way of combining parsers is to apply one parser to
-- the input string, and if this fails to then apply another to the same input instead. We now consider how
-- such a choice operator can be defined for parsers.
-- That is, for an applicative functor to be an instance of the Alternative class, it must support empty and
-- <|> primitives of the specified types.
-- empty represents an alternative that has failed, and <|>
-- is an appropriate choice operator for the type.
instance Alternative Parser where 
-- empty :: Parser a
  empty = P $ \inp -> []
-- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P $ \inp ->
    case parse p inp of
      []         -> parse q inp
      [(v, out)] -> [(v, out)]



-----------------------------------DEFINITION OF OPERATOR DATA STRUCTURE------------------------------------------

data Operator = Plus | Minus | Times | Div 
                | And | Or | Not 
                | Greater | GreaterEqual | Less | LessEqual | Equal | NotEqual 
                | While | If 
    deriving (Show, Eq)

-----------------------------------DEFINITION OF TREE DATA STRUCTURE (AST)------------------------------------------

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



{-___________________________________________________ PARSER ______________________________________________________-}

{-
  The parsing process will construct a tree structure (defined above) by composing different specific parsers.
  Every parser program, sqnc, stm ecc, are constructed by functorial and monadic implementation.
  Using monads it is easy then to construct a parser following the language grammar.

  -----------------------------------------------------------------------------
  
  -- <program> ::= {<sqnc>}
  -- <sqnc> ::= <stm>; | <stm>; <sqnc>
  -- <stm> ::= <expr> | <command>

  -- <comm> ::= <identifier> = <expr> | <IF>[<expr>]<program> | <WHILE>[<expr>]<program>
  -- <expr> ::= <aexpr> | <bexpr>

  -- <aexpr> ::=  <aterm> | <aterm> + <aexpr> | <aterm> - <aexpr>
  -- <aterm> ::=  <afactor> | <afactor> * <aterm>  | <afactor> / <aterm> 
  -- <afactor> ::=  ( <aexpr> ) | + <natural> | - <natural> | <identifier> | <natural> 

  -- <bexpr> ::=  <bterm> | <bterm> AND <bexpr> | <bterm> OR <bexpr> 
  -- <bterm> ::=  <aexpr> | ( <bexpr> ) | NOT <bterm> | <identifier> | <bool_val>
      --        | <bterm> GREATER <aexpr> | <bterm> GREATER_EQUAL <aexpr> 
      --        | <bterm> LESS <aexpr> | <bterm> LESS_EQUAL <aexpr> 
      --        | <bterm> EQUAL <aexpr> | <bterm> NOT_EQUAL <aexpr>

  -- <natural> ::=  0 | 1 | 2 | ...
  -- <bool_val> ::=  TRUE | FALSE
  -- <identifier> ::= lowercase alphabetic char followed by any alphanumeric string
  -----------------------------------------------------------------------------

  Functions that are used to construct the parser are ordered, in the following code, from the most high level to the atomic ones.
-}

----------------------------------PARSING OF THE WHOLE PROGRAM-------------------------------------------
-- <program> ::= {<sqnc>}
program :: Parser Tree
program = do 
  symbol "{"
  s <-  sqnc
  symbol "}"
  return (SeqNode s)


----------------------------------PARSING OF SEQUENCES OF STATEMENTS-------------------------------------------
-- <sqnc> ::= <stm>; | <stm>; <sqnc>
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


----------------------------------PARSING OF STATEMENTS-------------------------------------------
-- <stm> ::= <expr> | <comm>
stm :: Parser Tree
stm = do
    c <- comm
    return (StatementNode c)
    <|>
    do
      e <- expr
      return (StatementNode e)



----------------------------------PARSING OF COMMANDS-------------------------------------------
-- <comm> ::= <identifier> = <expr> | <IF>[<expr>]<program> | <WHILE>[<expr>]<program>
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


----------------------------------PARSING OF EXPRESSIONS-------------------------------------------
-- <expr> ::= <aepr> | <bexpr>
expr :: Parser Tree
expr = do
    bexpr <|> aexpr


----------------------------------PARSING OF ARITHMETIC EXPRESSIONS-------------------------------------------
-- <aexpr> ::=  <aterm> | <aterm> + <aexpr> | <aterm> - <aexpr>
aexpr :: Parser Tree
aexpr = do
  t <- aterm
  do symbol "+"
     e <- aexpr
     return (SumNode Plus  t e)
   <|> do symbol "-"
          e <- aexpr
          return (SumNode Minus  t e)
   <|> return t


-- <aterm> ::=  <afactor> | <afactor> * <aterm>  | <afactor> / <aterm> 
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


-- <afactor> ::=  ( <aexpr> ) | + <natural> | - <natural> | <identifier> | <natural> 
afactor :: Parser Tree
afactor = 
  do symbol "("
     e <- aexpr
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

----------------------------------PARSING OF BOOLEAN EXPRESSIONS-------------------------------------------
-- <bexpr> ::=  <bterm> | <bterm> AND <bexpr> | <bterm> OR <bexpr> 
--        | <bterm> GREATER <aexpr> | <bterm> GREATER_EQUAL <aexpr> 
--        | <bterm> LESS <aexpr> | <bterm> LESS_EQUAL <aexpr> 
--        | <bterm> EQUAL <aexpr> | <bterm> NOT_EQUAL <aexpr>
bexpr :: Parser Tree
bexpr = do 
  t <- bterm
  do symbol "AND"
     e <- bexpr
     return (LogicNode And t e)
   <|> do symbol "OR"
          e <- bexpr
          return (LogicNode And t e)
   <|> do symbol "GREATER"
          e <- aexpr
          return (ArithmeticLogicNode Greater t e)
   <|> do symbol "GREATER_EQUAL"
          e <- aexpr
          return (ArithmeticLogicNode GreaterEqual t e)
   <|> do symbol "LESS"
          e <- aexpr
          return (ArithmeticLogicNode Less t e)
   <|> do symbol "LESS_EQUAL"
          e <- aexpr
          return (ArithmeticLogicNode LessEqual t e)
   <|> do symbol "EQUAL"
          e <- aexpr
          return (ArithmeticLogicNode Equal t e)
   <|> do symbol "NOT_EQUAL"
          e <- aexpr
          return (ArithmeticLogicNode NotEqual t e)
   <|> return t


  -- <bterm> ::=  ( <bexpr> ) | NOT <bterm> | <identifier> | <bool_val> | <aexpr>
bterm :: Parser Tree
bterm = 
  do 
     b <- aexpr
     return b
     <|> 
  do symbol "("
     e <- bexpr
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


----------------------------------TERMINAL PARSERS-------------------------------------------
-- <natural> ::=  0 | 1 | 2 | ...
natural :: Parser Int
natural = token nat

-- <identifier> ::= lowercase alphabetic char followed by any alphanumeric stringnatural :: Parser Int
identifier :: Parser String
identifier = token ident

-- <bool_val> ::=  TRUE | FALSE
bool_val :: Parser Bool
bool_val = do 
  true_keyword
  return (True)
  <|> 
  do 
    false_keyword
    return(False) 

----------------------------------BASIC PARSERS-------------------------------------------
--Using some (already implemented in alternative) and digit: parser for natural numbers comprising one or more digits
nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

-- Using many and some (already implemented in alternative): parser for identifiers (variable names) 
-- comprising a lowercase letter followed by zero or more alphanumeric characters
ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x:xs)

-- parser for TRUE keyword
true_keyword :: Parser String
true_keyword = symbol "TRUE"

-- parser for FALSE keyword
false_keyword :: Parser String
false_keyword = symbol "FALSE"




-- Using sat and appropriate predicates from the library Data.Char: parsers for digits
digit :: Parser Char
digit = sat isDigit


-- using token and string parsers: parser that ignore spacing around special symbols
symbol :: String -> Parser String
symbol xs = token (string xs)

-- using char: parser for string xs, with the string itself returned as the result value
string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)




-- primitive that ignores any space before and after applying a parser for a token
token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

-- and spacing comprising zero or more space, tab, and newline characters
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

-- specific characters
char :: Char -> Parser Char
char x = sat (== x)

-- parser sat p for single characters that satisfy the predicate p:
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x
  then return x
  else empty

----------------------------------PRIMITIVE PARSERS-------------------------------------------
-- parsing primitive, which fails if the input string is empty, and succeeds with the first character as the result value otherwise
item :: Parser Char
item = P $ \inp ->
  case inp of
    []     -> []
    (x:xs) -> [(x, xs)]



{-------------------------------------------EVALUATION---------------------------------------------}

-- Definition of Value data type which represents values that can be stored in the store(memory).
-- A value can be of type Int for arithmetic operations or Bool for logic operations.
-- Value is a kind of container for boolean or integer values, for example: IntVal 3 or BoolVal False.
-- In this case IntVal and BoolVal are a wrapper for Int and Bool values respectively. 
data Value =
   IntVal  Int
 | BoolVal Bool
 deriving(Show)

-- Definition of Value as a Num instance (For operations between IntVal) which makes it possible to add/multiply/subtract values of type: IntVal 3 + IntVal 4 = IntVal 7
instance Num Value where
  IntVal a + IntVal b = IntVal(a+b)
  IntVal a * IntVal b = IntVal(a*b) 
  IntVal a - IntVal b = IntVal(a-b)
  abs (IntVal a) = IntVal(abs(a))
  signum(IntVal a) = IntVal(signum(a))



-- Definition of Value as a Eq instance which makes it possible to confront two IntVal(s) or two BoolVal(s) values;
-- Example: IntVal 4 == IntVal 4 yelds True
instance Eq Value where
  IntVal a == IntVal b = a==b
  BoolVal a == BoolVal b = a==b


-- Definition of some fundamental operations between Value data types

-- division (safe)
m_div :: Value -> Value -> Value
m_div (IntVal a) (IntVal b) = IntVal(a `div` b)

-- boolean and, or, not
m_and :: Value -> Value -> Value
m_and (BoolVal a) (BoolVal b) = BoolVal(a && b)
m_or :: Value -> Value -> Value
m_or (BoolVal a) (BoolVal b) = BoolVal(a || b)
m_not :: Value -> Value
m_not (BoolVal b) = BoolVal(not b)

-- operators for compare IntVal(s): >, >=, <, <=, ==, /= which yeld a BoolVal: BoolVal True / BoolVal False
greater :: Value -> Value -> Value
greater (IntVal a) (IntVal b) = BoolVal(a>b)
greater_eq :: Value -> Value -> Value
greater_eq (IntVal a) (IntVal b) = BoolVal(a>=b)

less :: Value -> Value -> Value
less (IntVal a) (IntVal b) = BoolVal(a<b)
less_eq :: Value -> Value -> Value
less_eq (IntVal a) (IntVal b) = BoolVal(a<=b)

eq :: Value -> Value -> Value
eq (IntVal a) (IntVal b) = BoolVal(a==b)
not_eq :: Value -> Value -> Value
not_eq (IntVal a) (IntVal b) = BoolVal(a/=b)


-- Defining the store type which is a list of tuples: String which represents the name of a variable and Value (defined above) that is the the value stored in the corresponding variable name.
type Store = [(String, Value)]


-- there are two "effects" that are candidates for being captured by a monadic structure:
-- 		1. The passing around and updating of the store.
-- 		2. Aborting running the program when a run-time error is encountered. (In the implementation above, the interpreter simply crashes when such an error occurs.)
-- The first effect is typically captured by a state monad, the second by an error monad.

-- We can use monad transformers to construct a composite monad for our two effects by combining a basic state monad and a basic error monad.
-- Here, however, we simply construct the composite monad in one go.
newtype Interp a = Interp { runInterp :: Store -> Either String (a, Store) }

instance Monad Interp where
  return x = Interp $ \r -> Right (x, r)
  i >>= k  = Interp $ \r -> case runInterp i r of
               Left msg      -> Left msg
               Right (x, r') -> runInterp (k x) r'
  fail msg = Interp $ \_ -> Left msg


-- Since the Applicative Monad Proposal (AMP) every Monad must also be an instance of Functor and Applicative.
instance Functor Interp where
  fmap = liftM -- imported from Control.Monad

instance Applicative Interp where
  pure  = return
  (<*>) = ap -- imported from Control.Monad


-- For reading from and writing to the store, we introduce effectful functions rd and wr
-- Note that rd produces a Left-wrapped error message if a variable lookup fails.
rd :: String -> Interp Value
rd x = Interp $ \r -> case lookup x r of
         Nothing -> Left ("unbound variable `" ++ x ++ "'")
         Just v  -> Right (v, r)

wr :: String -> Value -> Interp ()
wr x v = Interp $ \r -> Right ((), (x, v) : r)


-- For the execution of statements we have
exec :: Tree -> Interp ()
exec (SeqNode [])       = 
  do return ()
exec (SeqNode (s : ss)) = 
  do 
    exec s
    exec (SeqNode ss)
exec(StatementNode e) = do exec e    
exec (AssignNode x e) = 
  do 
    v <- eval e
    wr x v
exec (CommandNode While e s) = 
  do 
    v <- eval e
    when (v /= (BoolVal False)) (exec (SeqNode [s,CommandNode While e s]))
exec (CommandNode If e s) = 
  do 
    v <- eval e
    when (v /= (BoolVal False)) (exec s)
exec(n) = 
  do 
  	v <- eval n
  	wr "stdOut" v 

-- The monadic version of the expression evaluator
-- In the case of a division by zero that results in an error message being produced through the Monad-method fail,
-- which, for Interp, reduces to wrapping the message in a Left-value
eval :: Tree -> Interp Value
eval (NumNode n) = do return (IntVal n)
eval (VarNode x) = do rd x
eval (SumNode Plus e1 e2) =
  do 
    v1 <- eval e1
    v2 <- eval e2
    return (v1 + v2)
eval (SumNode Minus e1 e2) =
  do 
    v1 <- eval e1
    v2 <- eval e2
    return (v1 - v2)
eval (ProdNode Times e1 e2) =
  do 
    v1 <- eval e1
    v2 <- eval e2
    return (v1 * v2)
eval (ProdNode Div e1 e2) =
  do
	v1 <- eval e1
	v2 <- eval e2
	if v2 == (IntVal 0)
	  then fail "division by zero"
	  else return (m_div v1 v2)

eval (BoolNode n) = do return (BoolVal n)
eval (LogicNode And e1 e2) =
  do 
    v1 <- eval e1
    v2 <- eval e2
    return (m_and (v1) (v2))
eval (LogicNode Or e1 e2) =
  do 
    v1 <- eval e1
    v2 <- eval e2
    return (m_or (v1) (v2))
eval (UnaryBoolNode Not e1) =
  do 
    v1 <- eval e1
    return (m_not (v1))

eval (ArithmeticLogicNode Greater e1 e2) =
  do 
    v1 <- eval e1
    v2 <- eval e2
    return (greater (v1) (v2))
eval (ArithmeticLogicNode GreaterEqual e1 e2) =
  do 
    v1 <- eval e1
    v2 <- eval e2
    return (greater_eq (v1) (v2))

eval (ArithmeticLogicNode Less e1 e2) =
  do 
    v1 <- eval e1
    v2 <- eval e2
    return (less (v1) (v2))
eval (ArithmeticLogicNode LessEqual e1 e2) =
  do 
    v1 <- eval e1
    v2 <- eval e2
    return (less_eq (v1) (v2))

eval (ArithmeticLogicNode Equal e1 e2) =
  do 
    v1 <- eval e1
    v2 <- eval e2
    return (eq (v1) (v2))
eval (ArithmeticLogicNode NotEqual e1 e2) =
  do 
    v1 <- eval e1
    v2 <- eval e2
    return (not_eq (v1) (v2))




type Program = String


run :: Program -> Store -> IO()
run p r = 
  case parse program p of
    [(a,b)] -> case runInterp (exec a) r of 
		Right (_,r')-> print r' 

  
