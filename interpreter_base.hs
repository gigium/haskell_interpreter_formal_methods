import Data.Function
import Data.List

--The essence of an imperative language is that it has some form of mutable variables.
--Here, variables simply represented by strings:

type Var = String

--Next, we define ArithmeticExpressions.
--Expressions are constructed from integer constants, variable references, and arithmetic operations.


infix 1 `IS`
infixl 6 `SUM`, `DIF`
infixl 7 `MUL`, `DIV`


data ArithmeticExp
  = C Int        -- constant                                                     
  | V Var        -- variable                                                     
  | SUM [ArithmeticExp]  [ArithmeticExp]  -- addition                                                     
  | DIF [ArithmeticExp]  [ArithmeticExp]  -- subtraction                                                  
  | MUL [ArithmeticExp]  [ArithmeticExp]  -- multiplication                                               
  | DIV [ArithmeticExp]  [ArithmeticExp]  -- division

  --The statement language is rather minimal.
  --We have three forms of statements: variable assignments, while loops, and sequences.


data Stmt
  = Var `IS` ArithmeticExp      -- assignment                                                
  | While ArithmeticExp Stmt  -- loop                                                      
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

eval :: ArithmeticExp -> Store -> Val
eval (C n) r       = n
eval (V x) r       = case lookup x r of
                       Nothing -> error ("unbound variable `" ++ x ++ "'")
                       Just v  -> v
eval (SUM [e1]  [e2]) r = eval e1 r + eval e2 r
eval (DIF [e1]  [e2]) r = eval e1 r - eval e2 r
eval (MUL [e1]  [e2]) r = eval e1 r * eval e2 r
eval (DIV [e1]  [e2]) r = eval e1 r `div` eval e2 r

-- While the evaluation of an ArithmeticExpression cannot alter the contents of the store, executing a statement may in fact result in an update of the store. Hence,
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
      [ "z" `IS` SUM [V "x"]  [V "y"]
      , "x" `IS` V "y"
      , "y" `IS` V "z"
      , "n" `IS` DIF [V "n"]  [C 1]
      ]
  ]

