import           Control.Applicative
import           Data.Char


type Env = [(Char, Int)]

newtype Parser a = P (Env -> String -> [(Env, a, String)])

parse :: Parser a -> Env -> String -> [(Env, a, String)]
parse (P p) inp = p inp

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P $ \env inp ->
    case parse p env inp of
      [] -> []
      [(env, v,out)] -> [(env, g v, out)]

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\env inp -> [(env, v,inp)])
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\env inp -> case parse pg env inp of
    [] -> []
    [(env, g,out)] -> parse (fmap g px) env out)

instance Monad Parser where
  p >>= f = P (\env inp -> case parse p env inp of
    [] -> []
    [(env, v,out)] -> parse (f v) env out)
  return a = P (\env cs -> [(env, a,cs)])


instance Alternative Parser where
  -- empty :: Parser a
  empty = P $ \env inp -> []
  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P $ \env inp ->
    case parse p  env inp of
      []         -> parse q env inp
      [(env,v, out)] -> [(env,v, out)]

-- we define a parser sat p for single characters that satisfy the predicate p:
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x
  then return x
  else empty

-- Using sat and appropriate predicates from the library Data.Char, we can now define parsers for
-- digits
digit :: Parser Char
digit = sat isDigit

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

-- using char we can define a parser string xs for the string of characters xs, with the string
-- itself returned as the result value
string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)


-- Using many and some (already implemented in alternative), we can now define parsers for 
-- identifiers (variable names) comprising a lowercase letter followed by zero or more alphanumeric characters
ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x:xs)

 -- natural numbers comprising one or more digits
nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

 -- and spacing comprising zero or more space, tab, and newline characters
space :: Parser ()
space = do
  many (sat isSpace)
  return ()


-- using nat it is now straightforward to define a parser for integer values
int :: Parser Int
int = do
  char '-'
  n <- nat
  return (-n) <|> nat


-- To handle spacing, we define a new primitive that ignores any space before and after applying a parser for a token
token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

-- Using token, we can now define parsers that
-- ignore spacing around identifiers
identifier :: Parser String
identifier = token ident

-- ignore spacing around natural numbers
natural :: Parser Int
natural = token nat

-- ignore spacing around integers
integer :: Parser Int
integer = token int

-- ignore spacing around special symbols
symbol :: String -> Parser String
symbol xs = token (string xs)


item :: Parser Char
item = P (\env inp -> case inp of
  [] -> []
  (x:xs) -> [(env, x,xs)])

varValue :: Parser Int
varValue = P(\env inp -> case inp of
  [] -> []
  (x:xs) -> [(env, getVarFun env x, xs)])


getVarFun :: Env -> Char -> Int
getVarFun [(a,b)] c = 3+3


parseTrue :: Parser String
parseTrue = do {
  symbol "t";
  symbol "r";
  symbol "u";
  symbol "e";
  }

parseFalse :: Parser String
parseFalse = do {
  symbol "f";
  symbol "a";
  symbol "l";
  symbol "s";
  symbol "e";
  }


aExpr :: Parser Int
aExpr = do {
  t <- term;
  do {
    symbol "+";
    e <- aExpr;
    return (t + e);
    }
  <|>
  do {
    symbol "-";
    e <- aExpr;
    return (t - e);
    }
  <|> return t;
  }


term :: Parser Int
term = do {
  f <- factor;
  do {
    symbol "*";
    t <- term;
    return (f * t);
    }
  <|>
  do{
    symbol "/";
    t <- term;
    return (f `div` t);
    }
  <|>
  return f;
  }


factor :: Parser Int
factor = do {
  ;symbol "("
  ;e <- aExpr
  ;symbol ")"
  ;return e
  }
  <|> natural
  <|> varValue


bool:: Parser Bool
bool = do{
  do{
    parseTrue;
    return True;
    }
  <|>
  do {
    parseFalse;
    return False;
    }
  <|>
  do {
    num1 <- aExpr;
    symbol "<";
    num2 <- aExpr;
    if num1 < num2 then return True else return False;
    }
  <|>
  do {
    num1 <- aExpr;
    symbol "<=";
    num2 <- aExpr;
    if num1 <= num2 then return True else return False;
    }
  <|>
  do{
    num1 <- aExpr;
    symbol ">";
    num2 <- aExpr;
    if num1 > num2 then return True else return False;
    }
  <|>
  do {
    num1 <- aExpr;
    symbol ">=";
    num2 <- aExpr;
    if num1 >= num2 then return True else return False;
    }
  <|>
  do{
    num1 <- aExpr;
    symbol "==";
    num2 <- aExpr;
    if num1 == num2 then return True else return False;
    }
  <|>
  do{
    symbol "not";
    symbol "(";
    t1 <- bool;
    symbol ")";
    return (not t1);
    }
  }


bExpr :: Parser Bool
bExpr = do{
  do{
    t1 <- bExprTemp;
    symbol "and";
    t2 <- bExprTemp;
    return (t1 && t2);
    }
  <|>
  do{
    t1 <- bExprTemp;
    symbol "or";
    t2 <- bExprTemp;
    return (t1 || t2);
    }
  <|>
  do{
    t1 <- bExprTemp;
    return t1;
    }
  }
 
bExprTemp :: Parser Bool
bExprTemp = do{
  do{
    symbol "(";
    t1 <- bExprTemp;
    symbol "and";
    t2 <- bExprTemp;
    symbol ")";
    return (t1 && t2);
    }
  <|>
  do{
    symbol "(";
    t1 <- bExprTemp;
    symbol "or";
    t2 <- bExprTemp;
    symbol ")";
    return (t1 || t2);
    }
  <|>
  do{
    symbol "(";
    t1 <- bExprTemp;
    symbol ")";
    return t1;
    }
  <|>
  do{
    t1 <- bool;
    return t1;
    }
  <|>
  bExpr;
  }


updateEnv :: Char -> Int -> Parser String
updateEnv k v = P (\env inp -> case inp of
  xs -> [((modify env k v), "" ,xs)])

modify :: Env -> Char -> Int -> Env
modify [] k v = [(k,v)]
modify (x:xs) k v = if x == k then [(k,v)] ++ xs else [x] ++ modify xs k v


command :: Parser String
command = do {
  do{
    parseSkip;
    symbol ";";
    }
  <|>
  do {
    assignment;
    symbol ";";
    }
  }

assignment :: Parser String
assignment = do{
  var <- item;
  symbol ":=";
  num <- aExpr;
  updateEnv var num;
  }


cExpr :: Parser String
cExpr = do {
  do{
    command;
    cExpr;
    }
  <|>
  do{
    symbol "if";
    cond <- bExpr;
    symbol "then";
    if cond == True then do {
      cExpr;
      parseTilEndif 1;
      }
    else do{
      do{
        parseTilElse;
        cExpr;
        symbol "endif;";
        }
      <|>
      do{
        parseTilEndif 1;
        }
      };
    cExpr;
    }
  <|>
  do{
    symbol "while";
    copyWhile;
    cond <- bExpr;
    symbol "do";
    if cond == True then do{
      cExpr;
      symbol "endwhile;";
      cExpr;
      }
    else do {
      deleteFirstCopy 1;
      deleteSecondCopy 0;
      cExpr;
      };
    }
  <|>
  do{
    symbol "while";
    copyWhile;
    cond <- bExpr;
    symbol "do";
    if cond == True then do{
      cExpr;
      symbol "endwhile;";
      cExpr;
      }
    else do {
      deleteFirstCopy 1;
      deleteSecondCopy 0;
      };
  }
  <|>
  do{
    command;
    }
  } 


eval :: Env -> String -> String
eval env xs = case (parse cExpr env xs) of
  [(env, n, [])] -> getMemory env
  [(env, _, out)] -> error "unused input: " ++ out
  [] -> error "Invalid input"
