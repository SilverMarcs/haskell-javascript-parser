-- This module contains the skeleton code for the assignment.
--
-- Please do not change the names of the parseExerciseX functions, as they
-- are used by the test suite.
--
-- You may, and are highly encouraged, to create your functions.
module Assignment where

import Control.Applicative
import Instances
import Parser
import Data.Functor (($>))
import Data.List (intercalate)

data ADT
  = ExprValue Expr
  | StatementValues [Stmt]
  deriving (Eq, Show)

-- | Exercise A
parseExerciseA :: Parser ADT
parseExerciseA = ExprValue <$> expr

prettyPrintExerciseA :: ADT -> String
prettyPrintExerciseA (ExprValue e) = prettyPrintExpr e

-- | Exercise B
parseExerciseB :: Parser ADT
parseExerciseB = StatementValues <$> stmts

prettyPrintExerciseB :: ADT -> String
prettyPrintExerciseB (StatementValues stmts) = prettyPrintStmts 1 stmts


-- | Exercise C
-- This function should determine if the given code is a tail recursive function
isTailRecursive :: String -> Bool
isTailRecursive funcStr =
  case parseFunction funcStr of
    Just (fname, params, body) -> isTailRecursiveFunc fname params body
    Nothing -> False

parseExerciseC :: Parser ADT
parseExerciseC = StatementValues <$> stmts

prettyPrintExerciseC :: ADT -> String
prettyPrintExerciseC (StatementValues stmts) = prettyPrintStmts 1 stmts


-------  Extra Bonus section  -------

-- | Evaluates, parses and pretty prints simple arithmatic, logical and comparison expressions. Check out the test cases and results in javascript/inputs/D and javascript/output/D
-- | Exercise D
parseExerciseD :: Parser ADT
parseExerciseD = ExprValue . JsVal <$> evalUnifiedExpr

prettyPrintExerciseD :: ADT -> String
prettyPrintExerciseD (ExprValue (JsVal v)) = prettyPrintJSValue v

-- | Parses and pretty prints lambda const declarations. Check out the test cases and results in javascript/inputs/E and javascript/output/E
-- | Exercise E
parseExerciseE :: Parser ADT
parseExerciseE = StatementValues <$> stmts

prettyPrintExerciseE :: ADT -> String
prettyPrintExerciseE (StatementValues stmts) = prettyPrintStmts 1 stmts


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
  stringTok op -- parse the operator
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
evalLogic (LAnd e1 e2) = liftA2 land (eval e1) (eval e2) where land a b = JSBool (toBool a && toBool b)
evalLogic (LOr e1 e2)  = liftA2 lor (eval e1) (eval e2) where lor a b = JSBool (toBool a || toBool b)
evalLogic (LNot e)     = fmap lnot (eval e)     where lnot a = JSBool (not (toBool a))
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
evalCompare (Equals e1 e2)       = liftA2 eq (eval e1) (eval e2) where eq a b = JSBool (toBool a == toBool b)
evalCompare (NotEquals e1 e2)    = liftA2 neq (eval e1) (eval e2) where neq a b = JSBool (toBool a /= toBool b)
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
  = Ternary Expr Expr Expr
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

-- | Converts a 'JSValue' to a boolean according to JavaScript's rules.
toBool :: JSValue -> Bool
toBool (JSBool b) = b
toBool (JSInt 0) = False
toBool (JSString "") = False
toBool _ = True

-- | 'evalExpr' is a higher-order function that takes a parser 'p' and an evaluator function 'evaluator'.
-- It parses an expression using the parser 'p' and then evaluates it using the evaluator function 'evaluator'.
-- If the evaluation is successful, it returns the 'JSValue' obtained from the evaluation.
-- If the evaluation fails, it returns a parser failure with an error message.
evalExpr :: Parser a -> (a -> Maybe JSValue) -> Parser JSValue
evalExpr p evaluator = do
  e <- p
  case evaluator e of
    Just v -> return v
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
isTailRecursiveReturn fname params (ReturnExpr (FuncCallExpr (FuncCall fname' args))) 
  = fname == fname'  -- function name is the same
  && length args == length params  -- number of arguments is the same as the number of parameters
  && not (any hasNestedFuncCall args) -- there are no nested function calls
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
  | StmtBlock Block
  deriving (Eq, Show)

-- | Parses a single statement.
stmt :: Parser Stmt
stmt =
  StmtConst <$> constDecl
    <|> StmtIf <$> conditional
    <|> StmtReturn <$> returnStmt
    <|> (StmtFuncCall <$> funcCall <* charTok ';') -- because funcCall part of an expression or appear as a statement, we only consume semi colon if it is a statement
    <|> StmtFuncDecl <$> funcDecl
    <|> StmtBlock <$> block

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
-- | ----- Pretty Print -----
-- | ------------------------

-- Helper Functions --

-- | Wraps a string with the given start and end characters.
wrapWith :: String -> String -> String -> String
wrapWith start end s = start ++ s ++ end

-- | Parenthesizes a string.
parenthesize :: String -> String
parenthesize = wrapWith "(" ")"

-- | Parenthesizes a string and adds spaces around it.
spaceParenthesize :: String -> String
spaceParenthesize = wrapWith "( " " )"

-- | Wraps a string with square brackets.
squareBracketize :: String -> String
squareBracketize = wrapWith "[" "]"

-- | Wraps a string with curly brackets.
curlyBracketize :: String -> String
curlyBracketize = wrapWith "{" "}"

-- | Wraps a string with curly brackets and adds spaces around it.
spaceCurlyBracketize :: String -> String
spaceCurlyBracketize = wrapWith "{ " " }"

-- | Adds a semicolon to the end of a string.
appendSemicolon :: String -> String
appendSemicolon s = s ++ ";"

-- | Interpolates a list of arguments.
interpolateArgs :: [String] -> String
interpolateArgs = intercalate ", "

-- | Determines if a string should be printed on multiple lines.
shouldBeMultiLine :: String -> Bool
shouldBeMultiLine s = length s > 42

-- | Indents a string with two spaces.
indent :: Int -> String -> String
indent n = unlines . map (replicate (2 * n) ' ' ++) . lines

-- Pretty Printing Expressions --

-- | Helper function for pretty printing binary operations.
binaryOpPrettyPrint :: String -> Expr -> Expr -> String
binaryOpPrettyPrint op a b = parenthesize $ prettyPrintExpr a ++ " " ++ op ++ " " ++ prettyPrintExpr b

-- | Pretty prints a JSValue.
prettyPrintJSValue :: JSValue -> String
prettyPrintJSValue (JSInt i) = show i
prettyPrintJSValue (JSString s) = "\"" ++ s ++ "\""
prettyPrintJSValue (JSBool b) = if b then "true" else "false"
prettyPrintJSValue (JsVariable s) = s
prettyPrintJSValue (JSList lst) = squareBracketize (interpolateArgs (map prettyPrintJSValue lst))

-- | Pretty prints a combined expression.
prettyPrintExpr :: Expr -> String
prettyPrintExpr (JsVal v) = prettyPrintJSValue v
prettyPrintExpr (Arithmetic a) = prettyPrintArith a
prettyPrintExpr (Logical l) = prettyPrintLogic l
prettyPrintExpr (Comparison c) = prettyPrintComp c
prettyPrintExpr (TernaryOp t) = prettyPrintTernary t
prettyPrintExpr (FuncCallExpr f) = prettyPrintFuncCall f
prettyPrintExpr (LambdaFunc l) = prettyPrintLambda l

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
prettyPrintLogic (LBool v) = parenthesize $ prettyPrintJSValue v -- one of the test inputs have braces around single boolean, so I parenthsized it

-- | Pretty prints a comparison expression.
prettyPrintComp :: CompareExpr -> String
prettyPrintComp (Equals a b) = binaryOpPrettyPrint "===" a b
prettyPrintComp (NotEquals a b) = binaryOpPrettyPrint "!==" a b
prettyPrintComp (GreaterThan a b) = binaryOpPrettyPrint ">" a b
prettyPrintComp (LessThan a b) = binaryOpPrettyPrint "<" a b

-- | Pretty print for TernaryExpr
prettyPrintTernary :: TernaryExpr -> String
prettyPrintTernary (Ternary condition trueBranch falseBranch) =
  parenthesize $
    formatTernary
      (prettyPrintExpr condition) -- print the condition
      (prettyPrintExpr trueBranch) -- print the true branch
      (prettyPrintExpr falseBranch) -- print the false branch

-- | Formats the Ternary Expression
formatTernary :: String -> String -> String -> String
formatTernary conditionStr trueBranchStr falseBranchStr =
  let combinedStr = conditionStr ++ "? " ++ trueBranchStr ++ ": " ++ falseBranchStr
      delimiter = if shouldBeMultiLine combinedStr then "\n" else " "
   in conditionStr
        ++ delimiter -- if the ternary expression is too long, put the delimiter on a new line
        ++ "? " -- print the condition
        ++ trueBranchStr -- print the true branch
        ++ delimiter -- if the ternary expression is too long, put the delimiter on a new line
        ++ ": " -- print the false branch
        ++ falseBranchStr -- print the false branch

-- Function and Lambda Expressions --

-- | Pretty prints a lambda expression.
prettyPrintLambda :: LambdaExpr -> String
prettyPrintLambda (LambdaExpr params expr) = 
    parenthesize (interpolateArgs params) ++ " => " ++ prettyPrintExpr expr -- print the lambda parameters and the expression

-- | Pretty prints a function call.
prettyPrintFuncCall :: FuncCall -> String
prettyPrintFuncCall (FuncCall name expr) = 
    name ++ parenthesize (interpolateArgs (map prettyPrintExpr expr)) -- print the function name and arguments

-- | Pretty prints a function declaration.
prettyPrintFuncDeclCommon :: Int -> String -> [String] -> String -> String
prettyPrintFuncDeclCommon n name args blockContent = 
    "function " ++ name -- print the function name
    ++ parenthesize (interpolateArgs args) -- print the function arguments
    ++ " "
    ++ blockContent -- print the function body

-- | Pretty prints a function declaration.
prettyPrintFuncDecl :: Int -> FuncDecl -> String
prettyPrintFuncDecl n (TailRecursiveFunc name args block) =
    prettyPrintFuncDeclCommon n name args (prettyPrintTailOptimizedBlock n name args block) -- print the function declaration with the tail-optimized block

prettyPrintFuncDecl n (NonTailRecursiveFunc name args block) = 
    prettyPrintFuncDeclCommon n name args (prettyPrintBlock n block) -- print the function declaration with the normal block

-- | Pretty prints a tail-optimized block.
prettyPrintTailOptimizedBlock :: Int -> String -> [String] -> Block -> String
prettyPrintTailOptimizedBlock n fname params (Block stmts) =
    "{\n"
    ++ indent n "while ( true ) {\n"
    ++ indent (n + 1) (init (prettyPrintStmts (n + 1) initStmts)) -- print all statements except the last one
    ++ indent (n + 1) (squareBracketize (interpolateArgs params) ++ " = " ++ appendSemicolon (tailOptimizedAssignment (last stmts))) -- print the last statement (destructuring return statement)
    ++ indent n "}\n" -- close while loop
    ++ indent (n - 1) "}" -- close function body
  where
    initStmts = init stmts -- all statements except the last one because we want to get rid of the return statement

-- | Pretty prints a tail-optimized assignment.
tailOptimizedAssignment :: Stmt -> String
tailOptimizedAssignment (StmtReturn (ReturnExpr (FuncCallExpr (FuncCall _ expr)))) = 
    squareBracketize (interpolateArgs (map prettyPrintExpr expr))

-- Statements and Declarations --

-- | Pretty prints a constant declaration.
prettyPrintConstDecl :: ConstDecl -> String
prettyPrintConstDecl (ConstDecl name expr) = 
    appendSemicolon $ "const " ++ name ++ " = " ++ prettyPrintExpr expr

-- | Pretty prints a statement.
prettyPrintStmt :: Int -> Stmt -> String
prettyPrintStmt n (StmtConst constDecl) = prettyPrintConstDecl constDecl
prettyPrintStmt n (StmtIf conditional) = prettyPrintConditional n conditional
prettyPrintStmt n (StmtFuncCall funcCall) = appendSemicolon $ prettyPrintFuncCall funcCall
prettyPrintStmt n (StmtReturn returnStmt) = prettyPrintReturnStmt returnStmt
prettyPrintStmt n (StmtFuncDecl funcDecl) = prettyPrintFuncDecl n funcDecl
prettyPrintStmt n (StmtBlock block) = prettyPrintBlock n block  -- added this line to handle nested blocks

-- | Pretty prints a list of statements.

prettyPrintStmts :: Int -> [Stmt] -> String
prettyPrintStmts n = unlines . map (prettyPrintStmt n) -- print each statement on a new line

-- | Pretty prints a return statement.
prettyPrintReturnStmt :: ReturnStmt -> String
prettyPrintReturnStmt (ReturnExpr expr) = appendSemicolon $ "return " ++ prettyPrintExpr expr -- print the expression
prettyPrintReturnStmt (ReturnVal jsVal) = appendSemicolon $ "return " ++ prettyPrintJSValue jsVal -- print the JSValue

-- | Pretty prints a block.
prettyPrintBlock :: Int -> Block -> String
prettyPrintBlock n (Block []) = curlyBracketize " " -- if no statements, put a space between curly brackets
prettyPrintBlock n (Block [stmt]) = spaceCurlyBracketize (prettyPrintStmt n stmt) -- if one statement, put it on the same line
prettyPrintBlock n (Block stmts) = curlyBracketize ("\n" ++ indent n (prettyPrintStmts n stmts)) -- if multiple statements, put each on a new line

-- | Pretty prints a block with an extra line.
prettyPrintBlockWithExtraLine :: Int -> Block -> String
prettyPrintBlockWithExtraLine n block = init (prettyPrintBlock n block) ++ "\n" -- print the block without the last curly bracket and add a new line

-- | Pretty prints a conditional.
prettyPrintConditional :: Int -> Conditional -> String
prettyPrintConditional n (If expr ifBlock Nothing) =
    "if " ++ spaceParenthesize (prettyPrintExpr expr) ++ " " ++ prettyPrintBlock n ifBlock -- print the if statement

prettyPrintConditional n (If expr ifBlock (Just elseBlock)) =
    "if " ++ spaceParenthesize (prettyPrintExpr expr) ++ " " -- print the if statement
    ++ prettyPrintBlockWithExtraLine n ifBlock -- print the if block
    ++ "} else " ++ prettyPrintBlock n elseBlock -- print the else block