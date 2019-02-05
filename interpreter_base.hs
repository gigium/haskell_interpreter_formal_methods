import Data.Function
import Data.List
import Control.Monad
import Control.Applicative -- Otherwise you can't do the Applicative instance.

--The essence of an imperative language is that it has some form of mutable variables.
--Here, variables simply represented by strings:

type Var = String

--Next, we define expressions.
--Expressions are constructed from integer constants, variable references, and arithmetic operations.


infixl 6 :+:, :-:
infixl 7 :*:, :/:

data Exp
  = C Int        -- constant                                                     
  | V Var        -- variable                                                     
  | Exp :+: Exp  -- addition                                                     
  | Exp :-: Exp  -- subtraction                                                  
  | Exp :*: Exp  -- multiplication                                               
  | Exp :/: Exp  -- division

  --The statement language is rather minimal.
  --We have three forms of statements: variable assignments, while loops, and sequences.

infix 1 :=

data Stmt
  = Var := Exp      -- assignment                                                
  | While Exp Stmt  -- loop                                                      
  | Seq [Stmt]      -- sequence


--A program is just a statement.

type Prog = Stmt

--While running a program, we need to keep track of the values assigned to the different variables in the programs. 
--These values are just integers and as a representation of our "memory" we just use lists of pairs consisting of 
--a variable and a value.

type Val = Int
type Store = [(Var, Val)]

--Expressions are evaluated by mapping constants to their value, looking up the values of variables in the store, 
--and mapping arithmetic operations to their Haskell counterparts.

-- Note that if the store contains multiple bindings for a variable, 
--lookup selects the bindings that comes first in the store.

eval :: Exp -> Store -> Val
eval (C n) r       = n
eval (V x) r       = case lookup x r of
                       Nothing -> error ("unbound variable `" ++ x ++ "'")
                       Just v  -> v
eval (e1 :+: e2) r = eval e1 r + eval e2 r
eval (e1 :-: e2) r = eval e1 r - eval e2 r
eval (e1 :*: e2) r = eval e1 r * eval e2 r
eval (e1 :/: e2) r = eval e1 r `div` eval e2 r

-- While the evaluation of an expression cannot alter the contents of the store, executing a statement may in fact result in an update of the store. Hence,
-- the function for executing a statement takes a store as an argument and produces a possibly updated store.

-- Note that, in the case of assignments, we simply push a new binding for the updated variable to the store,
-- effectively shadowing any previous bindings for that variable.

exec :: Stmt -> Store -> Store
exec (x := e) r                    = (x, eval e r) : r
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
  [ "x" := C 0
  , "y" := C 1
  , While (V "n") $ Seq
      [ "z" := V "x" :+: V "y"
      , "x" := V "y"
      , "y" := V "z"
      , "n" := V "n" :-: C 1
      ]
  ]


--We can use monad transformers to construct a composite monad for our two effects by combining a basic state monad and a basic error monad.
--Here, however, we simply construct the composite monad in one go.
{-
newtype Interp a = Interp { runInterp :: Store -> Either String (a, Store) }

instance Monad Interp where
  return x = Interp $ \r -> Right (x, r)
  i >>= k  = Interp $ \r -> case runInterp i r of
               Left msg      -> Left msg
               Right (x, r') -> runInterp (k x) r'
  fail msg = Interp $ \_ -> Left msg

-}
--to the imports and make Interp an instance of Functor and Applicative like this
{-
instance Functor Interp where
  fmap = liftM -- imported from Control.Monad

instance Applicative Interp where
  pure  = return
  (<*>) = ap -- imported from Control.Monad
-}

--For reading from and writing to the store, we introduce effectful functions rd and wr:
--Note that rd produces a Left-wrapped error message if a variable lookup fails.
{-
rd :: Var -> Interp Val
rd x = Interp $ \r -> case lookup x r of
         Nothing -> Left ("unbound variable `" ++ x ++ "'")
         Just v  -> Right (v, r)

wr :: Var -> Val -> Interp ()
wr x v = Interp $ \r -> Right ((), (x, v) : r)
-}

--Note that rd produces a Left-wrapped error message if a variable lookup fails.
--The monadic version of the expression evaluator now reads
{-
eval :: Exp -> Interp Val
eval (C n)       = do return n
eval (V x)       = do rd x
eval (e1 :+: e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 + v2)
eval (e1 :-: e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 - v2)
eval (e1 :*: e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 * v2)
eval (e1 :/: e2) = do v1 <- eval e1
                      v2 <- eval e2
                      if v2 == 0
                        then fail "division by zero"
                        else return (v1 `div` v2)
-}

--For the execution of statements we have
--The type of exec conveys that statements do not result in values but are executed only 
--for their effects on the store or the run-time errors they may trigger.
{-
exec :: Stmt -> Interp ()
exec (x := e)       = do v <- eval e
                         wr x v
exec (While e s)    = do v <- eval e
                         when (v /= 0) (exec (Seq [s, While e s]))
exec (Seq [])       = do return ()
exec (Seq (s : ss)) = do exec s
                         exec (Seq ss)
-}
--Finally, in the function run we perform a monadic computation and process its effects.
{-
run :: Prog -> Store -> Either String Store
run p r = case runInterp (exec p) r of
            Left msg      -> Left msg
            Right (_, r') -> Right (nubBy ((==) `on` fst) r')
-}            