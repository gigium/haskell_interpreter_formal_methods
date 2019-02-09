import Data.Char
import Data.List
import qualified Data.Map as M

data Operator = Plus | Minus | Times | Div 
                | And | Or | Not 
                | Greater | GreaterEqual | Less | LessEqual | Equal | NotEqual 
                | While | If 
    deriving (Show, Eq)

--tokenizer 
data Token = TokOp Operator
           | TokAssign
           | TokLParen
           | TokRParen
           | TokCondStart
           | TokCondFinish
           | TokSeqStart
           | TokSeqFinish 
           | TokIdent String
           | TokNum Double
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