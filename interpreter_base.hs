import Data.Function
import Data.List
import Control.Monad
import Control.Applicative -- Otherwise you can't do the Applicative instance.

--The essence of an imperative language is that it has some form of mutable variables.
--Here, variables simply represented by strings:

type Var = String

--Next, we define ArithmeticAexpressions.
--Expressions are constructed from integer constants, variable references, and arithmetic operations.


infix 1 `IS`
infixl 6 `SUM`, `DIF`
infixl 7 `MUL`, `DIV`
infixl 8 `AND`, `OR`, `Not`
infixl 9 `EQUAL`, `NOT_EQUAL`, `LESS_THAN`, `GREATER_THAN`,`GREATER_EQUAL_TO`, `LESS_EQUAL_TO`

data ArithmeticAexp
  = C Int        -- constant                                                     
  | V Var        -- variable                                                     
  | ArithmeticAexp `SUM` ArithmeticAexp  -- addition                                                     
  | ArithmeticAexp `DIF` ArithmeticAexp  -- subtraction                                                  
  | ArithmeticAexp `MUL` ArithmeticAexp  -- multiplication                                               
  | ArithmeticAexp `DIV` ArithmeticAexp  -- division

data BooleanAexp = Boolean Bool -- Constant
 | B Var -- Variable
 | BooleanAexp `AND` BooleanAexp -- Logic and
 | BooleanAexp `OR` BooleanAexp -- Logic or
 | Not BooleanAexp -- Negation
 |ArithmeticAexp `EQUAL` ArithmeticAexp -- Equality between twoArithmeticAexp
 |ArithmeticAexp `NOT_EQUAL` ArithmeticAexp --Inequality between two Axp
-- Comparison amongArithmeticAexp
 |ArithmeticAexp `LESS_THAN` ArithmeticAexp
 |ArithmeticAexp `GREATER_THAN` ArithmeticAexp
 |ArithmeticAexp `GREATER_EQUAL_TO` ArithmeticAexp
 |ArithmeticAexp `LESS_EQUAL_TO` ArithmeticAexp

  --The statement language is rather minimal.
  --We have three forms of statements: variable assignments, while loops, and sequences.


data Stmt
  = Var `IS` ArithmeticAexp      -- assignment                                                
  | While ArithmeticAexp Stmt  -- loop                                                      
  | Seq [Stmt]      -- sequence


--A program is just a statement.

type Prog = Stmt

--While running a program, we need to keep track of the values assigned to the `dif`ferent variables in the programs. 
--These values are just integers and as a representation of our "memory" we just use lists of pairs consisting of 
--a variable and a value.

type Val = Int
type Store = [(Var, Val)]

--Expressions are evaluated by mapping constants to their value, looking up the values of variables in the store, 
--and mapping arithmetic operations to their Haskell counterparts.

-- Note that if the store contains multiple bindings for a variable, 
--lookup selects the bindings that comes first in the store.

eval :: ArithmeticAexp -> Store -> Val
eval (C n) r       = n
eval (V x) r       = case lookup x r of
                       Nothing -> error ("unbound variable `" ++ x ++ "'")
                       Just v  -> v
eval (e1 `SUM` e2) r = eval e1 r + eval e2 r
eval (e1 `DIF` e2) r = eval e1 r - eval e2 r
eval (e1 `MUL` e2) r = eval e1 r * eval e2 r
eval (e1 `DIV` e2) r = eval e1 r `div` eval e2 r

-- While the evaluation of an ArithmeticAexpression cannot alter the contents of the store, executing a statement may in fact result in an update of the store. Hence,
-- the function for executing a statement takes a store as an argument and produces a possibly updated store.

-- Note that, in the case of assignments, we simply push a new binding for the updated variable to the store,
-- effectively shadowing any previous bindings for that variable.

exec :: Stmt -> Store -> Store
exec (x `IS` e) r                    = (x, eval e r) : r
exec (While e s) r | eval e r /= 0 = exec (Seq [s, While e s]) r
                   | otherwise     = r
exec (Seq []) r                    = r
exec (Seq (s : ss)) r              = exec (Seq ss) (exec s r)

--Running a program reduces to executing its top-level statement in the context of an initial store.
--After executing the statement we clean up any shadowed bindings,
--so that we can easily read off the contents of the final store.

run :: Prog -> Store -> Store
run p r = nubBy ((==) `on` fst) (exec p r)

--As an example, consider the following program that computes the Fibonacci number 
--of the number stored in the variable n and stores its result in the variable x.

fib :: Prog
fib = Seq
  [ "x" `IS` C 0
  , "y" `IS` C 1
  , While (V "n") $ Seq
      [ "z" `IS` V "x" `SUM` V "y"
      , "x" `IS` V "y"
      , "y" `IS` V "z"
      , "n" `IS` V "n" `DIF` C 1
      ]
  ]

