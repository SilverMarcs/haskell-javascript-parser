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

-- | arses the given string, followed by 0 or more spaces.
stringTok :: String -> Parser String
stringTok = tok . string

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> spaces *> p)

bracketed :: Char -> Char -> Parser a -> Parser a
bracketed open close p = tok (is open) *> p <* tok (is close)

roundBracketed :: Parser a -> Parser a
roundBracketed = bracketed '(' ')'

squareBracketed :: Parser a -> Parser a
squareBracketed = bracketed '[' ']'

commaSeparated :: Parser a -> Parser [a]
commaSeparated p = sepBy p commaTok

endWithSemicolon :: Parser a -> Parser a
endWithSemicolon p = p <* charTok ';'

quote :: Parser Char
quote = is '"'

notQuote :: Parser Char
notQuote = isNot '"'

varName :: Parser String
varName = some (alpha <|> digit <|> is '_')

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

jsInt :: Parser JSValue
jsInt = tok $ JSInt <$> integer
  where
    integer = (negate <$> (is '-' *> natural)) <|> natural
    natural = read <$> some digit

jsString :: Parser JSValue
jsString = tok $ JSString <$> (quote *> many notQuote <* quote)

jsBool :: Parser JSValue
jsBool = tok $ (trueToken $> JSBool True) <|> (falseToken $> JSBool False)

jsVar :: Parser JSValue
jsVar = JsVariable <$> varName

jsList :: Parser JSValue
jsList = JSList <$> squareBracketed (commaSeparated jsValue)

jsValue :: Parser JSValue
jsValue = jsInt <|> jsString <|> jsBool <|> jsVar <|> jsList

-- Operations

binaryOp :: String -> (Expr -> Expr -> a) -> Parser a
binaryOp op constructor = do
  lhs <- expr
  stringTok op
  constructor lhs <$> expr

unaryOp :: String -> (Expr -> a) -> Parser a
unaryOp op constructor = do
  stringTok op
  constructor <$> expr

-- | -------------------------
-- | -- Logical Expressions --
-- | -------------------------
data LogicExpr
  = LAnd Expr Expr
  | LOr Expr Expr
  | LNot Expr
  | LBool JSValue
  deriving (Eq, Show)

logicNot :: Parser LogicExpr
logicNot = unaryOp "!" LNot

logicAnd :: Parser LogicExpr
logicAnd = binaryOp "&&" LAnd

logicOr :: Parser LogicExpr
logicOr = binaryOp "||" LOr

jsBoolValue :: Parser LogicExpr -- we need this to handle the case where we have a boolean value without any logical operators
jsBoolValue = LBool <$> (roundBracketed jsBool <|> jsBool)

logicExpr :: Parser LogicExpr
logicExpr = roundBracketed (logicNot <|> logicAnd <|> logicOr <|> jsBoolValue)

evalLogic :: LogicExpr -> Maybe JSValue
evalLogic (LAnd e1 e2) = liftA2 land (eval e1) (eval e2) where land (JSBool a) (JSBool b) = JSBool (a && b)
evalLogic (LOr e1 e2)  = liftA2 lor (eval e1) (eval e2) where lor  (JSBool a) (JSBool b) = JSBool (a || b)
evalLogic (LNot e)     = fmap lnot (eval e)     where lnot (JSBool a) = JSBool (not a)
evalLogic (LBool b)    = Just b

evalLogicExpr :: Parser JSValue
evalLogicExpr = do
    e <- logicExpr
    case eval (Logical e) of
        Just v  -> return v
        Nothing -> failed (UnexpectedString "Failed to evaluate logic expression.")

-- | ----------------------------
-- | -- Arithmetic Expressions --
-- | ----------------------------
data ArithExpr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Eq, Show)

addOp :: Parser ArithExpr
addOp = binaryOp "+" Add

subOp :: Parser ArithExpr
subOp = binaryOp "-" Sub

mulOp :: Parser ArithExpr
mulOp = binaryOp "*" Mul

divOp :: Parser ArithExpr
divOp = binaryOp "/" Div

powOp :: Parser ArithExpr
powOp = binaryOp "**" Pow

arithExpr :: Parser ArithExpr
arithExpr = roundBracketed (addOp <|> subOp <|> mulOp <|> divOp <|> powOp)

evalArith :: ArithExpr -> Maybe JSValue
evalArith (Add e1 e2) = liftA2 add (eval e1) (eval e2) where add (JSInt a) (JSInt b) = JSInt (a + b)
evalArith (Sub e1 e2) = liftA2 sub (eval e1) (eval e2) where sub (JSInt a) (JSInt b) = JSInt (a - b)
evalArith (Mul e1 e2) = liftA2 mul (eval e1) (eval e2) where mul (JSInt a) (JSInt b) = JSInt (a * b)
evalArith (Div e1 e2) = liftA2 divi (eval e1) (eval e2) where divi (JSInt a) (JSInt b) = JSInt (a `div` b)
evalArith (Pow e1 e2) = liftA2 pow (eval e1) (eval e2) where pow (JSInt a) (JSInt b) = JSInt (a ^ b)

evalArithExpr :: Parser JSValue
evalArithExpr = do
    e <- arithExpr
    case eval (Arithmetic e) of
        Just v  -> return v
        Nothing -> failed (UnexpectedString "Failed to evaluate arithmetic expression.")

-- | ----------------------------
-- | -- Comparison Expressions --
-- | ----------------------------
data CompareExpr
  = Equals Expr Expr
  | NotEquals Expr Expr
  | GreaterThan Expr Expr
  | LessThan Expr Expr
  deriving (Eq, Show)

equalsOp :: Parser CompareExpr
equalsOp = binaryOp "===" Equals

notEqualsOp :: Parser CompareExpr
notEqualsOp = binaryOp "!==" NotEquals

greaterThanOp :: Parser CompareExpr
greaterThanOp = binaryOp ">" GreaterThan

lessThanOp :: Parser CompareExpr
lessThanOp = binaryOp "<" LessThan

compareExpr :: Parser CompareExpr
compareExpr = roundBracketed (equalsOp <|> notEqualsOp <|> greaterThanOp <|> lessThanOp)

evalCompare :: CompareExpr -> Maybe JSValue
evalCompare (Equals e1 e2)       = liftA2 eq (eval e1) (eval e2) where eq a b = JSBool (a == b)
evalCompare (NotEquals e1 e2)    = liftA2 neq (eval e1) (eval e2) where neq a b = JSBool (a /= b)
evalCompare (GreaterThan e1 e2)  = liftA2 gt (eval e1) (eval e2) where gt (JSInt a) (JSInt b) = JSBool (a > b)
evalCompare (LessThan e1 e2)     = liftA2 lt (eval e1) (eval e2) where lt (JSInt a) (JSInt b) = JSBool (a < b)

evalCompareExpr :: Parser JSValue
evalCompareExpr = do
    e <- compareExpr
    case eval (Comparison e) of
        Just v  -> return v
        Nothing -> failed (UnexpectedString "Failed to evaluate comparison expression.")

-- | -------------------------
-- | -- Ternary Expressions --
-- | -------------------------
data TernaryExpr
  = Ternary Expr Expr Expr
  deriving (Eq, Show)

ternaryExpr :: Parser TernaryExpr
ternaryExpr =
  roundBracketed $
    Ternary
      <$> expr
      <* charTok '?'
      <*> expr
      <* charTok ':'
      <*> expr

-- | -------------------------
-- | --- Unified Expression --
-- | -------------------------
data Expr
  = FuncCallExpr FuncCall
  | JsVal JSValue
  | Arithmetic ArithExpr
  | Logical LogicExpr
  | Comparison CompareExpr
  | TernaryOp TernaryExpr
  | LambdaFunc LambdaExpr
  deriving (Eq, Show)

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
eval :: Expr -> Maybe JSValue
eval (JsVal v)          = Just v
eval (Arithmetic e)     = evalArith e
eval (Logical e)        = evalLogic e
eval (Comparison e)     = evalCompare e

evalUnifiedExpr :: Parser JSValue
evalUnifiedExpr =
    evalArithExpr
    <|> evalLogicExpr
    <|> evalCompareExpr


-- | ------------------------
-- | -- Const declaration ---
-- | ------------------------
data ConstDecl = ConstDecl String Expr deriving (Eq, Show)

constDecl :: Parser ConstDecl
constDecl = endWithSemicolon $ ConstDecl <$> (constToken *> tok varName <* charTok '=') <*> expr

-- | ------------------------
-- | ---- Conditionals ------
-- | ------------------------
data Conditional = If Expr Block (Maybe Block) deriving (Eq, Show)

conditional :: Parser Conditional
conditional =
  If
    <$> (ifToken *> roundBracketed expr)
    <*> block
    <*> optional (elseToken *> block)

-- | ------------------------
-- | ------- Blocks ---------
-- | ------------------------
newtype Block = Block [Stmt] deriving (Eq, Show)

block :: Parser Block
block = Block <$> (charTok '{' *> stmts <* charTok '}')

-- | ------------------------
-- | ---- Lambda Funcs- -----
-- | ------------------------
data LambdaExpr = LambdaExpr [String] Expr deriving (Eq, Show)

lambdaExpr :: Parser LambdaExpr
lambdaExpr = do
    params <- roundBracketed (commaSeparated varName)
    l <- lambdaToken
    LambdaExpr params <$> expr


-- | ------------------------
-- | ---- Functions -----
-- | ------------------------
data FuncCall = FuncCall String [Expr] deriving (Eq, Show)

funcCall :: Parser FuncCall
funcCall = FuncCall <$> varName <*> roundBracketed (commaSeparated expr)

-- ReturnStmt Data & Parser
data ReturnStmt
  = ReturnExpr Expr
  | ReturnVal JSValue
  deriving (Eq, Show)

returnStmt :: Parser ReturnStmt
returnStmt = ReturnExpr <$> endWithSemicolon (returnToken *> expr)

-- Function related Data & Parsers
data FuncDecl
  = TailRecursiveFunc String [String] Block
  | NonTailRecursiveFunc String [String] Block
  deriving (Eq, Show)

funcDecl :: Parser FuncDecl
funcDecl = do
  functionToken
  fname <- varName
  params <- roundBracketed (commaSeparated varName)
  body <- block
  return $
    if isTailRecursiveFunc fname params body
      then TailRecursiveFunc fname params body
      else NonTailRecursiveFunc fname params body

isTailRecursiveFunc :: String -> [String] -> Block -> Bool
isTailRecursiveFunc fname params block = maybe False (isTailRecursiveReturn fname params) (lastReturnStmt block)

isTailRecursiveReturn :: String -> [String] -> ReturnStmt -> Bool
isTailRecursiveReturn fname params (ReturnExpr (FuncCallExpr (FuncCall fname' args))) = fname == fname' && length args == length params && not (any hasNestedFuncCall args)
isTailRecursiveReturn _ _ _ = False

hasNestedFuncCall :: Expr -> Bool
hasNestedFuncCall (FuncCallExpr _) = True
hasNestedFuncCall _ = False

lastReturnStmt :: Block -> Maybe ReturnStmt
lastReturnStmt (Block stmts) = case last stmts of
  StmtReturn returnStmt -> Just returnStmt
  _ -> Nothing

-- | ------------------------
-- | ----- Statements -------
-- | ------------------------
data Stmt
  = StmtConst ConstDecl
  | StmtIf Conditional
  | StmtFuncCall FuncCall
  | StmtReturn ReturnStmt
  | StmtFuncDecl FuncDecl
  deriving (Eq, Show)

stmt :: Parser Stmt
stmt =
  StmtConst <$> constDecl
    <|> StmtIf <$> conditional
    <|> StmtReturn <$> returnStmt
    <|> (StmtFuncCall <$> funcCall <* charTok ';') -- because funcCall part of an expression or appear as a statement, we only consume semi colon if it is a statement
    <|> StmtFuncDecl <$> funcDecl

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

-- pretty printing utility funcs --

wrapWith :: Char -> Char -> String -> String
wrapWith start end s = start : s ++ [end]

parenthesize :: String -> String
parenthesize = wrapWith '(' ')'

spaceParenthesize :: String -> String
spaceParenthesize content = "(" ++ " " ++ content ++ " " ++ ")"

shouldBeMultiLine :: String -> Bool
shouldBeMultiLine s = length s > 42

-- Indentation logic
indent :: String -> String
indent = unlines . map ("  " ++) . lines

-- Helper function for binary operations
binaryOpPrettyPrint :: String -> Expr -> Expr -> String
binaryOpPrettyPrint op a b = parenthesize $ prettyPrintExpr a ++ " " ++ op ++ " " ++ prettyPrintExpr b

-- Pretty print for ArithExpr
prettyPrintArith :: ArithExpr -> String
prettyPrintArith (Add a b) = binaryOpPrettyPrint "+" a b
prettyPrintArith (Sub a b) = binaryOpPrettyPrint "-" a b
prettyPrintArith (Mul a b) = binaryOpPrettyPrint "*" a b
prettyPrintArith (Div a b) = binaryOpPrettyPrint "/" a b
prettyPrintArith (Pow a b) = binaryOpPrettyPrint "**" a b

-- Pretty print for LogicExpr
prettyPrintLogic :: LogicExpr -> String
prettyPrintLogic (LAnd a b) = binaryOpPrettyPrint "&&" a b
prettyPrintLogic (LOr a b) = binaryOpPrettyPrint "||" a b
prettyPrintLogic (LNot a) = parenthesize $ "!" ++ prettyPrintExpr a
prettyPrintLogic (LBool v) = parenthesize $ prettyPrintJSValue v

-- Pretty print for CompareExpr
prettyPrintComp :: CompareExpr -> String
prettyPrintComp (Equals a b) = binaryOpPrettyPrint "===" a b
prettyPrintComp (NotEquals a b) = binaryOpPrettyPrint "!==" a b
prettyPrintComp (GreaterThan a b) = binaryOpPrettyPrint ">" a b
prettyPrintComp (LessThan a b) = binaryOpPrettyPrint "<" a b

-- Pretty print for JSValue
prettyPrintJSValue :: JSValue -> String
prettyPrintJSValue (JSInt i) = show i
prettyPrintJSValue (JSString s) = "\"" ++ s ++ "\""
prettyPrintJSValue (JSBool b) = if b then "true" else "false"
prettyPrintJSValue (JsVariable s) = s
prettyPrintJSValue (JSList lst) = "[" ++ intercalate ", " (map prettyPrintJSValue lst) ++ "]"

-- Pretty print the combined Expr
prettyPrintExpr :: Expr -> String
prettyPrintExpr (JsVal v) = prettyPrintJSValue v
prettyPrintExpr (Arithmetic a) = prettyPrintArith a
prettyPrintExpr (Logical l) = prettyPrintLogic l
prettyPrintExpr (Comparison c) = prettyPrintComp c
prettyPrintExpr (TernaryOp t) = prettyPrintTernary t
prettyPrintExpr (FuncCallExpr f) = prettyPrintFuncCall f
prettyPrintExpr (LambdaFunc l) = prettyPrintLambda l

-- Pretty print for lambda expressions
prettyPrintLambda :: LambdaExpr -> String
prettyPrintLambda (LambdaExpr params expr) =
  parenthesize (intercalate ", " params)
      ++ " => "
      ++ prettyPrintExpr expr


-- Pretty print for TernaryExpr
prettyPrintTernary :: TernaryExpr -> String
prettyPrintTernary (Ternary condition trueBranch falseBranch) =
  let conditionStr = prettyPrintExpr condition
      trueBranchStr = prettyPrintExpr trueBranch
      falseBranchStr = prettyPrintExpr falseBranch
      totalLength = length conditionStr + length trueBranchStr + length falseBranchStr

      delimiter
        | totalLength > 42 = "\n"
        | otherwise = " "
   in parenthesize $
        conditionStr
          ++ delimiter
          ++ "? "
          ++ trueBranchStr
          ++ delimiter
          ++ ": "
          ++ falseBranchStr

-- Pretty print for FuncCall
prettyPrintFuncCall :: FuncCall -> String
prettyPrintFuncCall (FuncCall name expr) =
  name ++ "(" ++ intercalate ", " (map prettyPrintExpr expr) ++ ")"

-- pretty print a single ConstDecl
prettyPrintConstDecl :: ConstDecl -> String
prettyPrintConstDecl (ConstDecl name expr) =
  "const " ++ name ++ " = " ++ prettyPrintExpr expr ++ ";"

prettyPrintStmt :: Stmt -> String
prettyPrintStmt (StmtConst constDecl) = prettyPrintConstDecl constDecl
prettyPrintStmt (StmtIf conditional) = prettyPrintConditional conditional
prettyPrintStmt (StmtFuncCall funcCall) = prettyPrintFuncCall funcCall ++ ";"
prettyPrintStmt (StmtReturn returnStmt) = prettyPrintReturnStmt returnStmt
prettyPrintStmt (StmtFuncDecl funcDecl) = prettyPrintFuncDecl funcDecl

prettyPrintStmts :: [Stmt] -> String
prettyPrintStmts = unlines . map prettyPrintStmt

prettyPrintReturnStmt :: ReturnStmt -> String
prettyPrintReturnStmt (ReturnExpr expr) = "return " ++ prettyPrintExpr expr ++ ";"
prettyPrintReturnStmt (ReturnVal jsVal) = "return " ++ prettyPrintJSValue jsVal ++ ";"

prettyPrintFuncDecl :: FuncDecl -> String
prettyPrintFuncDecl (TailRecursiveFunc name args block) =
  "function " ++ name ++ "(" ++ intercalate ", " args ++ ") " ++ prettyPrintTailOptimizedBlock name args block
prettyPrintFuncDecl (NonTailRecursiveFunc name args block) =
  "function " ++ name ++ "(" ++ intercalate ", " args ++ ") " ++ prettyPrintBlock block

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


prettyPrintBlock :: Block -> String
prettyPrintBlock (Block []) = "{ }" -- if no statements, just print empty braces. later create HOF to parenthesize with third brackets or even somethign that lets you input hwich bracket you wanna insert. create valid data type for three types of braces TODO
prettyPrintBlock (Block [stmt]) = "{ " ++ prettyPrintStmt stmt ++ " }" -- if only one statement, don't put it on a new line
prettyPrintBlock (Block stmts) =
  -- if multiple statements, put each on a new line
  "{\n" ++ indent (prettyPrintStmts stmts) ++ "}"

prettyPrintBlockWithNewline :: Block -> String
prettyPrintBlockWithNewline (Block []) = "{ }"
prettyPrintBlockWithNewline (Block [stmt]) = "{" ++ prettyPrintStmt stmt ++ "}"
prettyPrintBlockWithNewline (Block stmts) =
  "{\n" ++ indent (prettyPrintStmts stmts) ++ "}"

-- A helper function to add space for the if block when an else block is present
prettyPrintBlockWithSpace :: Block -> String
prettyPrintBlockWithSpace block =
  init (prettyPrintBlock block) ++ "\n"

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
