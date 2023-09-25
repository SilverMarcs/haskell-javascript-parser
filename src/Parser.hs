{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Implementation of a parser-combinator.
module Parser where

import Control.Applicative
-- import Control.Monad (void, when)
-- import Control.Monad
import Data.Char
  ( isAlpha,
    isDigit,
    isLower,
    isSpace,
    isUpper,
  )
import Data.Functor (($>))
import Data.List (intercalate)
import Instances

-- | -------------------------------------------------
-- | --------------- Core parsers --------------------
-- | -------------------------------------------------

-- | Return a parser that always fails with the given error.
--
-- >>> isErrorResult (parse (failed UnexpectedEof) "abc")
-- True
failed :: ParseError -> Parser a
failed = Parser . const . Error

-- | Produces a parser that always fails with 'UnexpectedChar' using the given
-- character.
unexpectedCharParser :: Char -> Parser a
unexpectedCharParser = Parser . const . Error . UnexpectedChar

-- | Return a parser that succeeds with a character off the input or fails with
-- an error if the input is empty.
char :: Parser Char
char = Parser f
  where
    f "" = Error UnexpectedEof
    f (x : xs) = Result xs x

-- | Parse numbers as int until non-digit
int :: Parser Int
int = Parser f
  where
    -- This is okay because the case statement is small
    f "" = Error UnexpectedEof
    f x = case readInt x of
      Just (v, rest) -> Result rest v
      Nothing -> Error $ UnexpectedChar (head x)

-- | Write a parser that asserts that there is no remaining input.
eof :: Parser ()
eof = Parser f
  where
    f "" = Result "" ()
    f x = Error $ ExpectedEof x

-- | -------------------------------------------------
-- | --------------- Satisfy parsers -----------------
-- | -------------------------------------------------
-- | All of these parsers use the `satisfy` parser!
-- | Return a parser that produces a character but fails if:
--   * the input is empty; or
--   * the character does not satisfy the given predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = char >>= f'
  where
    -- This is okay because guards are small
    f' c
      | f c = pure c
      | otherwise = unexpectedCharParser c

-- | Return a parser that produces the given character but fails if:
--   * the input is empty; or
--   * the produced character is not equal to the given character.
is :: Char -> Parser Char
is = satisfy . (==)

-- | Return a parser that produces any character but fails if:
--   * the input is empty; or
--   * the produced character is equal to the given character.
isNot :: Char -> Parser Char
isNot = satisfy . (/=)

-- | Write a function that parses one of the characters in the given string.
oneof :: String -> Parser Char
oneof = satisfy . flip elem

-- | Write a function that parses any character, but fails if it is in the
-- given string.
noneof :: String -> Parser Char
noneof = satisfy . flip notElem

-- | Return a parser that produces a character between '0' and '9' but fails if
digit :: Parser Char
digit = satisfy isDigit

-- | Return a parser that produces a space character but fails if
space :: Parser Char
space = satisfy isSpace

-- | Return a parser that produces a lower-case character but fails if:
--   * the input is empty; or
--   * the produced character is not lower-case.
lower :: Parser Char
lower = satisfy isLower

-- | Return a parser that produces an upper-case character but fails if:
upper :: Parser Char
upper = satisfy isUpper

-- | Return a parser that produces an alpha character but fails if:
--   * the input is empty; or
--   * the produced character is not alpha.
alpha :: Parser Char
alpha = satisfy isAlpha

-- | Write a parser that will parse zero or more spaces.
spaces :: Parser String
spaces = many space

-- | Return a parser that produces one or more space characters (consuming
-- until the first non-space) but fails if:
--
--   * the input is empty; or
--
--   * the first produced character is not a space.
spaces1 :: Parser String
spaces1 = some space

-- | Write a function that parses the given string (fails otherwise).
string :: String -> Parser String
string = traverse is

-- | -------------------------------------------------
-- | --------------- Token parsers -------------------
-- | -------------------------------------------------

-- |  a function that applies the given parser, then parses 0 or more
-- spaces, then produces the result of the original parser.
tok :: Parser a -> Parser a
tok p = spaces *> p <* spaces

-- |  a function that parses the given char followed by 0 or more spaces.
charTok :: Char -> Parser Char
charTok = tok . is

-- | parser that parses a comma ',' followed by 0 or more spaces.
commaTok :: Parser Char
commaTok = charTok ','

-- | parses the given string, followed by 0 or more spaces.
stringTok :: String -> Parser String
stringTok = tok . string

-- | Attempts to parse one or more occurrences of `p`, separated by `sep`.
-- | If no `p` can be parsed, it returns an empty list.
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

-- | is like sepBy, but requires at least one occurrence of p
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> spaces *> p)

-- | parses a value between the given opening and closing characters.
bracketed :: Char -> Char -> Parser a -> Parser a
bracketed open close p = tok (is open) *> p <* tok (is close)

-- | parses a value between round brackets.
roundBracketed :: Parser a -> Parser a
roundBracketed = bracketed '(' ')'

-- | parses a value between square brackets.
squareBracketed :: Parser a -> Parser a
squareBracketed = bracketed '[' ']'

-- | parses zero or more occurrences of `p`, separated by commas.
commaSeparated :: Parser a -> Parser [a]
commaSeparated p = sepBy p commaTok

-- | parses a value `p` followed by a semicolon.
endWithSemicolon :: Parser a -> Parser a
endWithSemicolon p = p <* charTok ';'

-- | Parses a double quote character.
quote :: Parser Char
quote = is '"'

-- | Parses any character except a double quote.
notQuote :: Parser Char
notQuote = isNot '"'

-- | Parses a variable name, which consists of one or more alphabetic characters, digits, or underscores.
varName :: Parser String
varName = some (alpha <|> digit <|> is '_')

-- | wrapper function to parse various keyword tokens as strings and return the result
constToken, functionToken, ifToken, elseToken, returnToken, trueToken, falseToken, lambdaToken :: Parser String
constToken = stringTok "const"
functionToken = stringTok "function"
ifToken = stringTok "if"
elseToken = stringTok "else"
returnToken = stringTok "return"
trueToken = stringTok "true"
falseToken = stringTok "false"
lambdaToken = stringTok "=>"

-- | -----------------Exercises------------------ | --

-- | ------------------------
-- | Custom JsValue types ---
-- | ------------------------
data JSValue
  = JSInt Int
  | JSString String
  | JSBool Bool
  | JSList [JSValue]
  | JsVariable String
  deriving (Eq, Show)

-- | Parses an integer value from the input  and returns a 'JSInt' value.
jsInt :: Parser JSValue
jsInt = tok $ JSInt <$> integer
  where
    integer = (negate <$> (is '-' *> natural)) <|> natural
    natural = read <$> some digit

-- | Parses a string value from the input  and returns a 'JSString' value.
jsString :: Parser JSValue
jsString = tok $ JSString <$> (quote *> many notQuote <* quote)

-- | Parses a boolean value from the input  and returns a 'JSBool' value.
jsBool :: Parser JSValue
jsBool = tok $ (trueToken $> JSBool True) <|> (falseToken $> JSBool False)

-- | Parses a variable name from the input  and returns a 'JsVariable' value.
jsVar :: Parser JSValue
jsVar = JsVariable <$> varName

-- | Parses a list of 'JSValue's from the input  and returns a 'JSList' value.
jsList :: Parser JSValue
jsList = JSList <$> squareBracketed (commaSeparated jsValue)

-- | Parses a 'JSValue' from the input .
jsValue :: Parser JSValue
jsValue = jsInt <|> jsString <|> jsBool <|> jsVar <|> jsList

-- | -------------------------
-- | ------- Operators -------
-- | -------------------------
-- | Parses a binary operation between expressions
binaryOp :: String -> (Expr -> Expr -> a) -> Parser a
binaryOp op constructor = do
  lhs <- expr -- parse the left hand side of the expression
  stringTok op  -- parse the operator
  constructor lhs <$> expr -- parse the right hand side of the expression

-- | Parses a unary operation of an expression
unaryOp :: String -> (Expr -> a) -> Parser a
unaryOp op constructor = do
  stringTok op -- parse the operator
  constructor <$> expr -- parse the expression

-- | -------------------------
-- | -- Logical Expressions --
-- | -------------------------
-- | The 'LogicExpr' data type represents a logical expression in the form of a boolean value or a combination of logical operators.
data LogicExpr
  = LAnd Expr Expr
  | LOr Expr Expr
  | LNot Expr
  | LBool JSValue
  deriving (Eq, Show)

-- | The 'logicNot' parser parses a logical NOT operator.
logicNot :: Parser LogicExpr
logicNot = unaryOp "!" LNot

-- | The 'logicAnd' parser parses a logical AND operator.
logicAnd :: Parser LogicExpr
logicAnd = binaryOp "&&" LAnd

-- | The 'logicOr' parser parses a logical OR operator.
logicOr :: Parser LogicExpr
logicOr = binaryOp "||" LOr

-- | The 'jsBoolValue' parser parses a boolean value without any logical operators.
jsBoolValue :: Parser LogicExpr
jsBoolValue = LBool <$> (roundBracketed jsBool <|> jsBool)

-- | The 'logicExpr' parser parses a logical expression.
logicExpr :: Parser LogicExpr
logicExpr = roundBracketed (logicNot <|> logicAnd <|> logicOr <|> jsBoolValue)

-- | The 'evalLogic' function evaluates a logical expression and returns a 'JSValue'.
evalLogic :: LogicExpr -> Maybe JSValue
evalLogic (LAnd e1 e2) = liftA2 land (eval e1) (eval e2) where land (JSBool a) (JSBool b) = JSBool (a && b)
evalLogic (LOr e1 e2)  = liftA2 lor (eval e1) (eval e2) where lor  (JSBool a) (JSBool b) = JSBool (a || b)
evalLogic (LNot e)     = fmap lnot (eval e)     where lnot (JSBool a) = JSBool (not a)
evalLogic (LBool b)    = Just b

-- | The 'evalLogicExpr' parser evaluates a logical expression and returns a 'JSValue'.
evalLogicExpr :: Parser JSValue
evalLogicExpr = evalExpr logicExpr evalLogic

-- | ----------------------------
-- | -- Arithmetic Expressions --
-- | ----------------------------
-- | Represents an arithmetic expression.
data ArithExpr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Eq, Show)

-- | Parses an addition operator.
addOp :: Parser ArithExpr
addOp = binaryOp "+" Add

-- | Parses a subtraction operator.
subOp :: Parser ArithExpr
subOp = binaryOp "-" Sub

-- | Parses a multiplication operator.
mulOp :: Parser ArithExpr
mulOp = binaryOp "*" Mul

-- | Parses a division operator.
divOp :: Parser ArithExpr
divOp = binaryOp "/" Div

-- | Parses a power operator.
powOp :: Parser ArithExpr
powOp = binaryOp "**" Pow

-- | Parses an arithmetic expression.
arithExpr :: Parser ArithExpr
arithExpr = roundBracketed (addOp <|> subOp <|> mulOp <|> divOp <|> powOp)

-- | Evaluates an arithmetic expression.
evalArith :: ArithExpr -> Maybe JSValue
evalArith (Add e1 e2) = liftA2 add (eval e1) (eval e2) where add (JSInt a) (JSInt b) = JSInt (a + b)
evalArith (Sub e1 e2) = liftA2 sub (eval e1) (eval e2) where sub (JSInt a) (JSInt b) = JSInt (a - b)
evalArith (Mul e1 e2) = liftA2 mul (eval e1) (eval e2) where mul (JSInt a) (JSInt b) = JSInt (a * b)
evalArith (Div e1 e2) = liftA2 divi (eval e1) (eval e2) where divi (JSInt a) (JSInt b) = JSInt (a `div` b)
evalArith (Pow e1 e2) = liftA2 pow (eval e1) (eval e2) where pow (JSInt a) (JSInt b) = JSInt (a ^ b)

-- | Parses and evaluates an arithmetic expression.
evalArithExpr :: Parser JSValue
evalArithExpr = evalExpr arithExpr evalArith

-- | ----------------------------
-- | -- Comparison Expressions --
-- | ----------------------------
-- | Data type representing a comparison expression.
data CompareExpr
  = Equals Expr Expr
  | NotEquals Expr Expr
  | GreaterThan Expr Expr
  | LessThan Expr Expr
  deriving (Eq, Show)

-- | Parser for the "===" operator.
equalsOp :: Parser CompareExpr
equalsOp = binaryOp "===" Equals

-- | Parser for the "!==" operator.
notEqualsOp :: Parser CompareExpr
notEqualsOp = binaryOp "!==" NotEquals

-- | Parser for the ">" operator.
greaterThanOp :: Parser CompareExpr
greaterThanOp = binaryOp ">" GreaterThan

-- | Parser for the "<" operator.
lessThanOp :: Parser CompareExpr
lessThanOp = binaryOp "<" LessThan

-- | Parser for a comparison expression.
compareExpr :: Parser CompareExpr
compareExpr = roundBracketed (equalsOp <|> notEqualsOp <|> greaterThanOp <|> lessThanOp)

-- | Function for evaluating a comparison expression.
evalCompare :: CompareExpr -> Maybe JSValue
evalCompare (Equals e1 e2)       = liftA2 eq (eval e1) (eval e2) where eq a b = JSBool (a == b)
evalCompare (NotEquals e1 e2)    = liftA2 neq (eval e1) (eval e2) where neq a b = JSBool (a /= b)
evalCompare (GreaterThan e1 e2)  = liftA2 gt (eval e1) (eval e2) where gt (JSInt a) (JSInt b) = JSBool (a > b)
evalCompare (LessThan e1 e2)     = liftA2 lt (eval e1) (eval e2) where lt (JSInt a) (JSInt b) = JSBool (a < b)

-- | Parser for evaluating a comparison expression.
evalCompareExpr :: Parser JSValue
evalCompareExpr = evalExpr compareExpr evalCompare


-- | -------------------------
-- | -- Ternary Expressions --
-- | -------------------------
-- | Represents a ternary expression in the form of `condition ? trueValue : falseValue`.
data TernaryExpr
  = Ternary Expr Expr Expr -- ^ The three expressions that make up the ternary expression.
  deriving (Eq, Show)

-- | Parses a ternary expression.
ternaryExpr :: Parser TernaryExpr
ternaryExpr =
  roundBracketed $
    Ternary
      <$> expr -- Parse the condition expression.
      <* charTok '?' 
      <*> expr -- Parse the true value expression.
      <* charTok ':' 
      <*> expr -- Parse the false value expression.

-- | -------------------------
-- | --- Unified Expression --
-- | -------------------------
-- | The 'Expr' data type represents an expression in javascript
data Expr
  = FuncCallExpr FuncCall
  | JsVal JSValue
  | Arithmetic ArithExpr
  | Logical LogicExpr
  | Comparison CompareExpr
  | TernaryOp TernaryExpr
  | LambdaFunc LambdaExpr
  deriving (Eq, Show)

-- | The 'expr' parser parses an expression in javascript
expr :: Parser Expr
expr =
  FuncCallExpr <$> funcCall
    <|> JsVal <$> jsValue
    <|> Arithmetic <$> arithExpr
    <|> Logical <$> logicExpr
    <|> Comparison <$> compareExpr
    <|> TernaryOp <$> ternaryExpr
    <|> LambdaFunc <$> lambdaExpr


-- | -------------------------
-- | -- Evaluate Expression --
-- | -------------------------
-- | 'eval' function takes an 'Expr' and returns a 'Maybe' 'JSValue' after evaluating the expression. At the moment, any other expression results in a Nothing.
eval :: Expr -> Maybe JSValue
eval (JsVal v)          = Just v
eval (Arithmetic e)     = evalArith e
eval (Logical e)        = evalLogic e
eval (Comparison e)     = evalCompare e
eval _                  = Nothing


-- | 'evalUnifiedExpr' is a parser that parses an expression and returns a 'JSValue' after evaluating it.
evalUnifiedExpr :: Parser JSValue
evalUnifiedExpr =
  evalArithExpr
  <|> evalLogicExpr
  <|> evalCompareExpr


-- | 'evalExpr' is a higher-order function that takes a parser 'p' and an evaluator function 'evaluator'.
-- It parses an expression using the parser 'p' and then evaluates it using the evaluator function 'evaluator'.
-- If the evaluation is successful, it returns the 'JSValue' obtained from the evaluation.
-- If the evaluation fails, it returns a parser failure with an error message.
evalExpr :: Parser a -> (a -> Maybe JSValue) -> Parser JSValue
evalExpr p evaluator = do
  e <- p
  case evaluator e of
    Just v  -> return v
    Nothing -> failed (UnexpectedString "Failed to evaluate expression.")



-- | ------------------------
-- | -- Const declaration ---
-- | ------------------------

-- | Represents a constant declaration with a name and an expression.
data ConstDecl = ConstDecl String Expr deriving (Eq, Show)

-- | Parses a constant declaration, which starts with the keyword "const", followed by a variable name, an equals sign, and an expression.
constDecl :: Parser ConstDecl
constDecl = endWithSemicolon $ ConstDecl <$> (constToken *> tok varName <* charTok '=') <*> expr


-- | ------------------------
-- | ---- Conditionals ------
-- | ------------------------

-- | Represents an "if" statement with a condition, a "then" block, and an optional "else" block.
data Conditional = If Expr Block (Maybe Block) deriving (Eq, Show)

-- | Parses an "if" statement, which starts with the keyword "if", followed by a condition in round brackets, a "then" block in curly brackets, and an optional "else" block in curly brackets.
conditional :: Parser Conditional
conditional =
  If
    <$> (ifToken *> roundBracketed expr) -- Parses the condition
    <*> block -- Parses the "then" block
    <*> optional (elseToken *> block) -- Parses the optional "else" block


-- | ------------------------
-- | ------- Blocks ---------
-- | ------------------------
-- | Represents a block of statements.
newtype Block = Block [Stmt] deriving (Eq, Show)

-- | Parses a block of statements, which starts with an opening curly bracket, followed by zero or more statements, and ends with a closing curly bracket.
block :: Parser Block
block = Block <$> (charTok '{' *> stmts <* charTok '}')


-- | ------------------------
-- | ---- Lambda Funcs- -----
-- | ------------------------

-- | Represents a lambda expression with a list of parameter names and an expression.
data LambdaExpr = LambdaExpr [String] Expr deriving (Eq, Show)

-- | Parses a lambda expression.
lambdaExpr :: Parser LambdaExpr
lambdaExpr = do
    params <- roundBracketed (commaSeparated varName)
    l <- lambdaToken
    LambdaExpr params <$> expr


-- | ------------------------
-- | ---- Functions -----
-- | ------------------------

-- | Represents a function call with a function name and a list of expressions as arguments.
data FuncCall = FuncCall String [Expr] deriving (Eq, Show)

-- | Parses a function call.
funcCall :: Parser FuncCall
funcCall = FuncCall <$> varName <*> roundBracketed (commaSeparated expr)

-- | Represents a return statement with either an expression or a JSValue
data ReturnStmt
  = ReturnExpr Expr
  | ReturnVal JSValue
  deriving (Eq, Show)

-- | Parses a return statement.
returnStmt :: Parser ReturnStmt
returnStmt = ReturnExpr <$> endWithSemicolon (returnToken *> expr)

-- | Represents a function declaration with a function name, a list of parameter names, and a block of statements.
data FuncDecl
  = TailRecursiveFunc String [String] Block
  | NonTailRecursiveFunc String [String] Block
  deriving (Eq, Show)

-- | Parses a function declaration.
funcDecl :: Parser FuncDecl
funcDecl = do
  functionToken -- Parses the "function" keyword
  fname <- varName -- Parses the function name
  params <- roundBracketed (commaSeparated varName) -- Parses the function parameters
  body <- block -- Parses the function body
  return $
    if isTailRecursiveFunc fname params body -- Determines if the function is tail-recursive
      then TailRecursiveFunc fname params body -- Returns a tail-recursive function declaration
      else NonTailRecursiveFunc fname params body -- Returns a non-tail-recursive function declaration

-- | Determines whether a function is tail-recursive by checking if the last statement in the block is a tail-recursive return statement.
isTailRecursiveFunc :: String -> [String] -> Block -> Bool
isTailRecursiveFunc fname params block = maybe False (isTailRecursiveReturn fname params) (lastReturnStmt block)

-- | Determines whether a return statement is tail-recursive by checking if it returns a function call with the same name as the function being declared, with the same number of arguments as the number of parameters, and with no nested function calls.
isTailRecursiveReturn :: String -> [String] -> ReturnStmt -> Bool
isTailRecursiveReturn fname params (ReturnExpr (FuncCallExpr (FuncCall fname' args))) = fname == fname' && length args == length params && not (any hasNestedFuncCall args)
isTailRecursiveReturn _ _ _ = False

-- | Determines whether an expression contains a nested function call.
hasNestedFuncCall :: Expr -> Bool
hasNestedFuncCall (FuncCallExpr _) = True
hasNestedFuncCall _ = False

-- | Returns the last return statement in a block, if it exists.
lastReturnStmt :: Block -> Maybe ReturnStmt
lastReturnStmt (Block stmts) = case last stmts of
  StmtReturn returnStmt -> Just returnStmt
  _ -> Nothing

-- | ------------------------
-- | ----- Statements -------
-- | ------------------------
-- | 'Stmt' represents a statement in JavaScript
data Stmt
  = StmtConst ConstDecl
  | StmtIf Conditional
  | StmtFuncCall FuncCall
  | StmtReturn ReturnStmt
  | StmtFuncDecl FuncDecl
  deriving (Eq, Show)

-- | Parses a single statement.
stmt :: Parser Stmt
stmt =
  StmtConst <$> constDecl
    <|> StmtIf <$> conditional
    <|> StmtReturn <$> returnStmt
    <|> (StmtFuncCall <$> funcCall <* charTok ';') -- because funcCall part of an expression or appear as a statement, we only consume semi colon if it is a statement
    <|> StmtFuncDecl <$> funcDecl

-- | Parses a list of statements.
stmts :: Parser [Stmt]
stmts = many stmt

-- Helper function to extract function details from a string
-- we need this since our custom function for detecting tail recursion has a different method signature
-- this function takes a string and returns a data type that can be analysed by our custom isTailRecursiveFunc function
parseFunction :: String -> Maybe (String, [String], Block)
parseFunction str =
  case parse funcDecl str of
    Result _ (TailRecursiveFunc fname params body) -> Just (fname, params, body)
    Result _ (NonTailRecursiveFunc fname params body) -> Just (fname, params, body)
    _ -> Nothing


-- | ------------------------
-- | ----- Preety Print -----
-- | ------------------------

-- pretty printing helper funcstion --

-- | Wraps a string with the given start and end characters.
wrapWith :: Char -> Char -> String -> String
wrapWith start end s = start : s ++ [end]

-- | Parenthesizes a string.
parenthesize :: String -> String
parenthesize = wrapWith '(' ')'

-- | Parenthesizes a string and adds spaces around it.
spaceParenthesize :: String -> String
spaceParenthesize content = "(" ++ " " ++ content ++ " " ++ ")"

-- | Determines if a string should be printed on multiple lines.
shouldBeMultiLine :: String -> Bool
shouldBeMultiLine s = length s > 42

-- | Indents a string with two spaces.
indent :: String -> String
indent = unlines . map ("  " ++) . lines

-- | Interpolates a list of arguments.
interpolateArgs :: [String] -> String
interpolateArgs = intercalate ", "

-- | Adds a semicolon to the end of a string.
appendSemicolon :: String -> String
appendSemicolon s = s ++ ";"

-- | Helper function for pretty printing binary operations.
binaryOpPrettyPrint :: String -> Expr -> Expr -> String
binaryOpPrettyPrint op a b = parenthesize $ prettyPrintExpr a ++ " " ++ op ++ " " ++ prettyPrintExpr b

-- | Pretty prints an arithmetic expression.
prettyPrintArith :: ArithExpr -> String
prettyPrintArith (Add a b) = binaryOpPrettyPrint "+" a b
prettyPrintArith (Sub a b) = binaryOpPrettyPrint "-" a b
prettyPrintArith (Mul a b) = binaryOpPrettyPrint "*" a b
prettyPrintArith (Div a b) = binaryOpPrettyPrint "/" a b
prettyPrintArith (Pow a b) = binaryOpPrettyPrint "**" a b

-- | Pretty prints a logic expression.
prettyPrintLogic :: LogicExpr -> String
prettyPrintLogic (LAnd a b) = binaryOpPrettyPrint "&&" a b
prettyPrintLogic (LOr a b) = binaryOpPrettyPrint "||" a b
prettyPrintLogic (LNot a) = parenthesize $ "!" ++ prettyPrintExpr a
prettyPrintLogic (LBool v) = parenthesize $ prettyPrintJSValue v  -- one of the test inputs have braces around signle boolean, so i parenthsized it

-- | Pretty prints a comparison expression.
prettyPrintComp :: CompareExpr -> String
prettyPrintComp (Equals a b) = binaryOpPrettyPrint "===" a b
prettyPrintComp (NotEquals a b) = binaryOpPrettyPrint "!==" a b
prettyPrintComp (GreaterThan a b) = binaryOpPrettyPrint ">" a b
prettyPrintComp (LessThan a b) = binaryOpPrettyPrint "<" a b

-- | Pretty prints a JSValue.
prettyPrintJSValue :: JSValue -> String
prettyPrintJSValue (JSInt i) = show i
prettyPrintJSValue (JSString s) = "\"" ++ s ++ "\""
prettyPrintJSValue (JSBool b) = if b then "true" else "false"
prettyPrintJSValue (JsVariable s) = s
prettyPrintJSValue (JSList lst) = "[" ++ intercalate ", " (map prettyPrintJSValue lst) ++ "]"

-- | Pretty prints a combined expression.
prettyPrintExpr :: Expr -> String
prettyPrintExpr (JsVal v) = prettyPrintJSValue v
prettyPrintExpr (Arithmetic a) = prettyPrintArith a
prettyPrintExpr (Logical l) = prettyPrintLogic l
prettyPrintExpr (Comparison c) = prettyPrintComp c
prettyPrintExpr (TernaryOp t) = prettyPrintTernary t
prettyPrintExpr (FuncCallExpr f) = prettyPrintFuncCall f
prettyPrintExpr (LambdaFunc l) = prettyPrintLambda l

-- Pretty Printers
prettyPrintLambda :: LambdaExpr -> String
prettyPrintLambda (LambdaExpr params expr) = parenthesize (interpolateArgs params) ++ " => " ++ prettyPrintExpr expr

prettyPrintFuncCall :: FuncCall -> String
prettyPrintFuncCall (FuncCall name expr) = name ++ parenthesize (interpolateArgs (map prettyPrintExpr expr))

prettyPrintFuncDeclCommon :: String -> [String] -> String -> String
prettyPrintFuncDeclCommon name args blockContent = "function " ++ name ++ parenthesize (interpolateArgs args) ++ " " ++ blockContent

prettyPrintFuncDecl :: FuncDecl -> String
prettyPrintFuncDecl (TailRecursiveFunc name args block) = prettyPrintFuncDeclCommon name args (prettyPrintTailOptimizedBlock name args block)
prettyPrintFuncDecl (NonTailRecursiveFunc name args block) = prettyPrintFuncDeclCommon name args (prettyPrintBlock block)

prettyPrintConstDecl :: ConstDecl -> String
prettyPrintConstDecl (ConstDecl name expr) = appendSemicolon $ "const " ++ name ++ " = " ++ prettyPrintExpr expr

prettyPrintStmt :: Stmt -> String
prettyPrintStmt (StmtConst constDecl) = prettyPrintConstDecl constDecl
prettyPrintStmt (StmtIf conditional) = prettyPrintConditional conditional
prettyPrintStmt (StmtFuncCall funcCall) = appendSemicolon $ prettyPrintFuncCall funcCall  -- print semi colon only when function call is done standalone
prettyPrintStmt (StmtReturn returnStmt) = prettyPrintReturnStmt returnStmt
prettyPrintStmt (StmtFuncDecl funcDecl) = prettyPrintFuncDecl funcDecl

prettyPrintStmts :: [Stmt] -> String
prettyPrintStmts = unlines . map prettyPrintStmt

prettyPrintReturnStmt :: ReturnStmt -> String
prettyPrintReturnStmt (ReturnExpr expr) = appendSemicolon $ "return " ++ prettyPrintExpr expr
prettyPrintReturnStmt (ReturnVal jsVal) = appendSemicolon $ "return " ++ prettyPrintJSValue jsVal


-- | Formats the Ternary Expression
formatTernary :: String -> String -> String -> String
formatTernary conditionStr trueBranchStr falseBranchStr =
    let combinedStr = conditionStr ++ "? " ++ trueBranchStr ++ ": " ++ falseBranchStr
        delimiter = if shouldBeMultiLine combinedStr then "\n" else " "
    in  conditionStr
        ++ delimiter
        ++ "? "
        ++ trueBranchStr
        ++ delimiter
        ++ ": "
        ++ falseBranchStr

-- | Pretty print for TernaryExpr
prettyPrintTernary :: TernaryExpr -> String
prettyPrintTernary (Ternary condition trueBranch falseBranch) =
    parenthesize $
    formatTernary (prettyPrintExpr condition) 
                  (prettyPrintExpr trueBranch) 
                  (prettyPrintExpr falseBranch)


prettyPrintTailOptimizedBlock :: String -> [String] -> Block -> String
prettyPrintTailOptimizedBlock fname params (Block stmts) =
  "{\n  while (true) {\n"
    ++ (indent . init . prettyPrintStmts $ initStmts)
    ++ "    ["
    ++ intercalate ", " params
    ++ "] = "
    ++ tailOptimizedAssignment (last stmts)
    ++ ";\n  }\n}"
  where
    initStmts = init stmts -- All statements except the last one
    indent = unlines . map ("    " ++) . lines -- Four spaces for indenting
    tailOptimizedAssignment :: Stmt -> String
    tailOptimizedAssignment (StmtReturn (ReturnExpr (FuncCallExpr (FuncCall _ expr)))) =
      "[" ++ intercalate ", " (map prettyPrintExpr expr) ++ "]"


-- | Returns a string representation of a 'Block' with no newline characters.
prettyPrintBlock :: Block -> String
prettyPrintBlock (Block []) = "{ }" -- if no statements, just print empty braces. later create HOF to parenthesize with third brackets or even somethign that lets you input hwich bracket you wanna insert. create valid data type for three types of braces TODO
prettyPrintBlock (Block [stmt]) = "{ " ++ prettyPrintStmt stmt ++ " }" -- if only one statement, don't put it on a new line
prettyPrintBlock (Block stmts) =
  -- if multiple statements, put each on a new line
  "{\n" ++ indent (prettyPrintStmts stmts) ++ "}"
  
-- | Returns a string representation of a 'Block' with newline characters.
prettyPrintBlockWithNewline :: Block -> String
prettyPrintBlockWithNewline (Block []) = "{ }"
prettyPrintBlockWithNewline (Block [stmt]) = "{" ++ prettyPrintStmt stmt ++ "}"
prettyPrintBlockWithNewline (Block stmts) =
  "{\n" ++ indent (prettyPrintStmts stmts) ++ "}"
  
-- | Returns a string representation of a 'Block' with a space character added before the opening brace if an else block is present.
-- A helper function to add space for the if block when an else block is present
prettyPrintBlockWithSpace :: Block -> String
prettyPrintBlockWithSpace block =
  init (prettyPrintBlock block) ++ "\n"
  
-- | Returns a string representation of a 'Conditional'.
prettyPrintConditional :: Conditional -> String
prettyPrintConditional (If expr ifBlock Nothing) =
  "if " ++ spaceParenthesize (prettyPrintExpr expr) ++ " " ++ prettyPrintBlock ifBlock
prettyPrintConditional (If expr ifBlock (Just elseBlock)) =
  "if "
    ++ spaceParenthesize (prettyPrintExpr expr)
    ++ " "
    ++ prettyPrintBlockWithSpace ifBlock
    ++ "} else "
    ++ prettyPrintBlock elseBlock
