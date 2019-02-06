import Data.Function
import Data.List
import Control.Monad
import Control.Applicative -- Otherwise you can't do the Applicative instance.


{-
We can use monad transformers to construct a composite monad for our two effects by combining 
a basic state monad and a basic error monad. 
Here, however, we simply construct the composite monad in one go.
-}
newtype Interp a = Interp { runInterp :: Store -> Either String (a, Store) }

instance Monad Interp where
  return x = Interp $ \r -> Right (x, r)
  i >>= k  = Interp $ \r -> case runInterp i r of
               Left msg      -> Left msg
               Right (x, r') -> runInterp (k x) r'
  fail msg = Interp $ \_ -> Left msg

{-
to the imports and make Interp an instance of Functor and Applicative like this
-}
instance Functor Interp where
  fmap = liftM -- imported from Control.Monad

instance Applicative Interp where
  pure  = return
  (<*>) = ap -- imported from Control.Monad

{-For reading from and writing to the store, we introduce effectful functions rd and wr:-}
rd :: Var -> Interp Val
rd x = Interp $ \r -> case lookup x r of
         Nothing -> Left ("unbound variable `" ++ x ++ "'")
         Just v  -> Right (v, r)

wr :: Var -> Val -> Interp ()
wr x v = Interp $ \r -> Right ((), (x, v) : r)



type Var = String
type Boolean = Bool
type Val = Int
type Store = [(Var, Val)]

infixl 6 `SUM`, `DIF`
infixl 7 `MUL`, `DIV`

data ArithmeticExpression
  = C Val        -- constant                                                     
  | V Var        -- variable                                                     
  | ArithmeticExpression `SUM` ArithmeticExpression  -- addition                                                     
  | ArithmeticExpression `DIF` ArithmeticExpression  -- subtraction                                                  
  | ArithmeticExpression `MUL` ArithmeticExpression  -- multiplication                                               
  | ArithmeticExpression `DIV` ArithmeticExpression  -- division

-- infixl 8 `AND`, `OR`, `Not`
-- infixl 9 `EQUAL`, `NOT_EQUAL`, `LESS_THAN`, `GREATER_THAN`,`GREATER_EQUAL_TO`, `LESS_EQUAL_TO`

data BooleanExpression = Boolean Bool -- Constant
--  | B Var -- Variable
--  | BooleanExpression `AND` BooleanExpression -- Logic and
--  | BooleanExpression `OR` BooleanExpression -- Logic or
--  | Not BooleanExpression -- Negation
--  | ArithmeticExpression `EQUAL` ArithmeticExpression -- Equality between two ArithmeticExpression
--  | ArithmeticExpression `NOT_EQUAL` ArithmeticExpression --Inequality between two Axp
-- -- Comparison among ArithmeticExpression
--  | ArithmeticExpression `LESS_THAN` ArithmeticExpression
--  | ArithmeticExpression `GREATER_THAN` ArithmeticExpression
--  | ArithmeticExpression `GREATER_EQUAL_TO` ArithmeticExpression
--  | ArithmeticExpression `LESS_EQUAL_TO` ArithmeticExpression

infix 1 `IS`

data Stmt
  = Var `IS` ArithmeticExpression      -- assignment                                                
  | While BooleanExpression Stmt  -- loop                                                      
  | Seq [Stmt]      -- sequence

type Prog = Stmt



{-Note that rd produces a Left-wrapped error message if a variable lookup fails.
The monadic version of the expression evaluator now reads-}
eval :: ArithmeticExpression -> Interp Val
eval (C n)       = do return n
eval (V x)       = do rd x
eval (e1 `SUM` e2) = do { v1 <- eval e1
                      ;v2 <- eval e2
                      ;return (v1 + v2)}
eval (e1 `DIF` e2) = do {v1 <- eval e1
                      ;v2 <- eval e2
                      ;return (v1 - v2)}
eval (e1 `MUL` e2) = do {v1 <- eval e1
                      ;v2 <- eval e2
                      ;return (v1 * v2)}
eval (e1 `DIV` e2) = do {v1 <- eval e1
                      ;v2 <- eval e2
                      ;if v2 == 0
                        ;then fail "division by zero"
                        ;else return (v1 `div` v2)}


evalB :: BooleanExpression -> Interp Val
evalB (Boolean n) r = do return n
-- evalB (B x) r = do rd x
-- evalB (b1 `AND` b2) r = do { v1 <- evalB b1
--                       ;v2 <- evalB b2
--                       ;return (v1 && v2)}
-- evalB (b1 `OR` b2) r = do { v1 <- evalB b1
--                       ;v2 <- evalB b2
--                       ;return (v1 || v2)}
-- evalB (Not b1) r = do return not (evalB b1 r)
-- evalB (e1 `EQUAL` e2) r =  do { v1 <- evalB e1
--                       ;v2 <- evalB e2
--                       ;return (v1 == v2)}
-- evalB (e1 `NOT_EQUAL` e2) r =  do { v1 <- evalB e1
--                       ;v2 <- evalB e2
--                       ;return (v1 /= v2)}
-- evalB (e1 `LESS_THAN` e2) r = do { v1 <- evalB e1
--                       ;v2 <- evalB e2
--                       ;return (v1 < v2)}
-- evalB (e1 `GREATER_THAN` e2) r =  do { v1 <- evalB e1
--                       ;v2 <- evalB e2
--                       ;return (v1 > v2)}
-- evalB (e1 `GREATER_EQUAL_TO` e2) r =  do { v1 <- evalB e1
--                       ;v2 <- evalB e2
--                       ;return (v1 >= v2)}
-- evalB (e1 `LESS_EQUAL_TO` e2) r =  do { v1 <- evalB e1
--                       ;v2 <- evalB e2
--                       ;return (v1 <= v2)}


{-In the case for `DIV`, division by zero results in an error message being produced 
through the Monad-method fail, which, for Interp, reduces to wrapping the message in a Left-value.
For the execution of statements we have-}
exec :: Stmt -> Interp ()
exec (x `IS` e)       = do {v <- eval e
                         ;wr x v}
-- exec (While e s)    = do v <- evalB e
--                          when (v /= False) (exec (Seq [s, While e s]))
exec (Seq [])       = do return ()
exec (Seq (s : ss)) = do exec s
                         exec (Seq ss)

{-The type of exec conveys that statements do not result in values but are executed only for their 
effects on the store or the run-time errors they may trigger.
Finally, in the function run we perform a monadic computation and process its effects.-}
run :: Prog -> Store -> Either String Store
run p r = case runInterp (exec p) r of
            Left msg      -> Left msg
            Right (_, r') -> Right (nubBy ((==) `on` fst) r')


-- fib :: Prog
-- fib = Seq
--   [ "x" `IS` C 0
--   , "y" `IS` C 1
--   , "w" `IS` C 0
--   , While (V "n" `NOT_EQUAL` V "w") $ Seq
--       [ "z" `IS` V "x" `SUM` V "y"
--       , "x" `IS` V "y"
--       , "y" `IS` V "z"
--       , "n" `IS` V "n" `DIF` C 1
--       ]
--   ]

--Example if 
-- pIf :: Prog 
-- pIf = Seq 
-- 	[ "c" `IS` Integer 1 
-- 	, IF (V "c" `LESS_EQUAL_TO` V "z")  (Seq  [
-- 		"n" `IS` Integer 10 , "x" `IS` Integer 11] )(Seq  [
-- 			"n" `IS` Integer 10  , "x" `IS` Integer 1
-- 			]  ) 
-- 		]  