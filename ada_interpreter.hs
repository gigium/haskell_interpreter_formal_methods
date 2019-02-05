module Interpreter where
--Import of Necessary libraries
import Data.Function
import Data.List
import Data.Bool

--Data structures
--Fixed precedence Of grammar operators described above
infix 1 :=
infixl 6 `SUM`, `DIFF`
infixl 7 `MUL`, `DIV`
infixl 8 `AND`, `OR`, `Not`
infixl 9 `EQUAL`, `NOT_EQUAL`, `LESS_THAN`, `GREATER_THAN`,`GREATER_EQUAL_TO`, `LESS_EQUAL_TO`
--Arithmetic expression declarations
data Aexp = Integer Int -- constant
 | V Var -- variable
 | Aexp `SUM` Aexp -- addition
 | Aexp `DIFF` Aexp -- subtraction
 | Aexp `MUL` Aexp -- multiplication
 | Aexp `DIV` Aexp -- division
--Boolean expression declarations
data Bexp = Boolean Bool -- Constant
 | B Var -- Variable
 | Bexp `AND` Bexp -- Logic and
 | Bexp `OR` Bexp -- Logic or
 | Not Bexp -- Negation
 | Aexp `EQUAL` Aexp -- Equality between two Aexp
 | Aexp `NOT_EQUAL` Aexp --Inequality between two Axp
-- Comparison among Aexp
 | Aexp `LESS_THAN` Aexp
 | Aexp `GREATER_THAN` Aexp
 | Aexp `GREATER_EQUAL_TO` Aexp
 | Aexp `LESS_EQUAL_TO` Aexp
--Expression declarations
data Stmt = Skip
 | Var := Aexp -- assignment
 | IF Bexp Stmt Stmt -- cond
 | While Bexp Stmt -- loop
 | Seq [Stmt] -- sequence
--Declaration of useful types
--As we run the program, we need to keep track of the values assigned to different variables in
--the program. These values are of various types (INT and Bool) and as a representation of
--our environment we use a list of variable-value pairs.
type Var = String
type Val = Int
type Boolean = Bool
type Prog = Stmt
type Store = [(Var, String)]
--See function in store
--This function is useful for taking, given a variable, the value contained in it, if the variable is
--present within the environment, then returns the associated value, otherwise Nothing.
see :: (Eq a) => a -> [(a,b)] -> Maybe b
see _key [] = Nothing
see key ((x,y):xys)
 | key == x = Just y
 | otherwise = see key xys

--Arithmetic expressions evaluations
--Arithmetic expressions are evaluated by mapping constants with their value, by looking at
--the value of variables in memory, and by mapping arithmetic operations to their respective
--Haskell counterparts.
eval :: Aexp -> Store -> Val
eval (Integer n) r = n
eval (V x) r = case see x r of
 Nothing -> error ("unbound variable `" ++ x ++ "'")
 Just v -> read v
--read transform the string in value
eval (e1 `SUM` e2) r = eval e1 r + eval e2 r
eval (e1 `DIFF` e2) r = eval e1 r - eval e2 r
eval (e1 `MUL` e2) r = eval e1 r * eval e2 r
eval (e1 `DIV` e2) r = eval e1 r `div` eval e2 r
--Boolean expression evaluations
--Boolean expressions are evaluated by mapping Boolean constants (True and False) with
--their respective value, variables with their value in memory (not implemented at the
--statement level because the IMP language does not expect it), operations have been
--associated Logical Boolean to Bexp, for example by mapping `AND` With &&, and the
--comparison operations between the AEXP, for example `GREATER_EQUAL_TO` With >=.
evalB :: Bexp -> Store -> Boolean
evalB (Boolean b) r = b
evalB (B x) r = case see x r of
 Nothing -> error ("unbound variable `" ++ x ++ "'")
 Just v -> read v
evalB (b1 `AND` b2) r = evalB b1 r && evalB b2 r
evalB (b1 `OR` b2) r = evalB b1 r || evalB b2 r
evalB (Not b1) r = not (evalB b1 r)
evalB (e1 `EQUAL` e2) r = eval e1 r == eval e2 r
evalB (e1 `NOT_EQUAL` e2) r = eval e1 r /= eval e2 r
evalB (e1 `LESS_THAN` e2) r = eval e1 r < eval e2 r
evalB (e1 `GREATER_THAN` e2) r = eval e1 r > eval e2 r
evalB (e1 `GREATER_EQUAL_TO` e2) r = eval e1 r >= eval e2 r
evalB (e1 `LESS_EQUAL_TO` e2) r = eval e1 r <= eval e2 r
--Executing expressions
--While evaluating an expression cannot alter the memory content (Store), executing a
--declaration can actually result in a memory update. Then, the function for executing a
--statement takes an Environment, called in the Store interpreter, as an argument and returns
--the same updated (that is, it takes the old memory location and possibly the update).
exec :: Stmt -> Store -> Store
exec (x := e) r = (x, show (eval e r)) : r
-- to allow the assignment of bexp also, one solution can be
--concatenated to aexp all the bexp
exec (IF b st se) r | evalB b r /= False = exec st r
 | otherwise = exec se r
exec (While b s) r | evalB b r /= False = exec (Seq [s, While b s]) r
 | otherwise = r
exec (Seq []) r = r
exec (Seq (s : ss)) r = exec (Seq ss) (exec s r)
--Note that, in the case of assignments, you insert a new key value pair within the
--environment, effectively obscing any previous bindings for that variable. Instead, to run the
--while, you make the fixed-point operator, that is, the associated statement executes, after
--the condition is verified, and the same while is concatenated in the queue

--Performing the Interpreter
--It was decided to insert an abstraction to the interpreter, adding a special function to run a
--program, that is the function run, note that the type Prog is an abstraction of the stmt type
--and that invokes only the function exec.
run :: Prog -> Store -> Store
run p r = exec p r

--Combining the function see and function run You can run a program and see, of a variable
--of interest, its value in memory.


--Factorial 
fact :: Prog 
fact = Seq     ["e" := Integer 1     ,"r" := V "n"     , While(V "n" `NOT_EQUAL` V "e") $ Seq         [ "n" := V "n" `DIFF` Integer 1         , "r" := V "r" `MUL` V "n"         ]     ] 
--Fibonacci 
fib :: Prog 
fib = Seq   [ "x" := Integer 0   , "y" := Integer 1   , "w" := Integer 0   , While (V "n" `NOT_EQUAL` V "w") $ Seq       [ "z" := V "x" `SUM` V "y"       , "x" := V "y"       , "y" := V "z"       , "n" := V "n" `DIFF` Integer 1       ]   ] 
--Example if 
pIf :: Prog 
pIf = Seq  [  "c" := Integer 1 , IF (V "c" `LESS_EQUAL_TO` V "z")  (Seq  ["n" := Integer 10 , "x" := Integer 11] )(Seq  ["n" := Integer 10  , "x" := Integer 1]  ) ]  