-- We begin by importing two standard libraries for applicative functors and characters that will be used in
-- our implementation
import           Control.Applicative
import           Data.Char

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


-- Our first parsing primitive is called item, which fails if the input string is empty, and succeeds with the
-- first character as the result value otherwise
item :: Parser Char
item = P $ \inp ->
  case inp of
    []     -> []
    (x:xs) -> [(x, xs)]


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

-- using these primitives a parser for a non-empty list of natural numbers that ignores spacing
-- around tokens can be defined as follows
nats :: Parser [Int]
nats = do
  symbol "["
  n <- natural
  ns <- many $ do
    symbol ","
    natural
  symbol "]"
  return (n:ns)
-- This definition states that such a list begins with an opening square bracket and a natural number,
-- followed by zero or more commas and natural numbers, and concludes with a closing square bracket.
-- Note that nats only succeeds if a complete list in precisely this format is consumed


-- The following grammar can be translated straightforwardly into a parser for expressions. 
-----------------------------------------------------------------------------
-- expr   ::=  term (+ expr | ∊)
-- term   ::=  factor (* term | ∊)
-- factor ::=  ( expr ) | nat
-- nat    ::=  0 | 1 | 2 | ...
-----------------------------------------------------------------------------
-- Sequencing in the grammar is translated into the do notation, choice | is translated into the <|> operator, the empty string ϵ becomes the
-- empty parser, special symbols such as + and * are handled using the symbol function, and natural
-- numbers are parsed using the natural primitive
expr :: Parser Int
expr = do
  t <- term
  do
    symbol "+"
    e <- expr
    return (t + e) <|> return t

term :: Parser Int
term = do
  f <- factor
  do
    symbol "*"
    t <- term
    return (f * t) <|> return f

factor :: Parser Int
factor = do
  symbol "("
  e <- expr
  symbol ")"
  return e <|> natural

-- Note that each of the above parsers returns the integer value of the expression that was parsed, rather
-- than some form of expression tree.

-- using expr we define a function that returns the integer value that results from parsing and
-- evaluating an expression. To handle the cases of unconsumed and invalid input, we use the library
-- function error :: String -> a that displays an error message and then terminates the program
eval :: String -> Int
eval xs = case (parse expr xs) of
  [(n, [])]  -> n
  [(_, out)] -> error ("Unused input " ++ out)
  []         -> error "Invalid input"
















