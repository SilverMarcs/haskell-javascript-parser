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
import Data.List (intercalate)
import Instances
import Data.Functor (($>))

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


-- | -----------------Exercises------------------ | --

-- custom JsValue types --
data JSValue = JSInt Int
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
jsString = tok $ JSString <$> (is '"' *> many (isNot '"') <* is '"')

jsBool :: Parser JSValue
jsBool = tok $ (stringTok "true" $> JSBool True) <|> (stringTok "false" $> JSBool False)

jsVar :: Parser JSValue
jsVar = JsVariable <$> varName

jsList :: Parser JSValue
jsList = JSList <$> squareBracketed (commaSeparated jsValue)

jsValue :: Parser JSValue
jsValue = jsInt <|> jsString <|> jsBool <|> jsVar <|> jsList 

-- Operations

binOp :: String -> (Expr -> Expr -> a) -> Parser a
binOp op constructor = do
    lhs <- expr
    stringTok op
    constructor lhs <$> expr

unaryOp :: String -> (Expr -> a) -> Parser a
unaryOp op constructor = do
    stringTok op
    constructor <$> expr


-- Logical --

data LogicExpr = LAnd Expr Expr
               | LOr Expr Expr
               | LNot Expr
               | LBool JSValue
               deriving (Eq, Show)

logicNot :: Parser LogicExpr
logicNot = unaryOp "!" LNot

logicAnd :: Parser LogicExpr
logicAnd = binOp "&&" LAnd

logicOr :: Parser LogicExpr
logicOr = binOp "||" LOr

jsBoolValue :: Parser LogicExpr   -- we need this to handle the case where we have a boolean value without any logical operators
jsBoolValue = LBool <$> (roundBracketed jsBool <|> jsBool)

logicExpr :: Parser LogicExpr
logicExpr = roundBracketed (logicNot <|> logicAnd <|> logicOr <|> jsBoolValue)

-- Arithmetic
data ArithExpr = Add Expr Expr
               | Sub Expr Expr
               | Mul Expr Expr
               | Div Expr Expr
               | Pow Expr Expr
               deriving (Eq, Show)

addOp :: Parser ArithExpr
addOp = binOp "+" Add

subOp :: Parser ArithExpr
subOp = binOp "-" Sub

mulOp :: Parser ArithExpr
mulOp = binOp "*" Mul

divOp :: Parser ArithExpr
divOp = binOp "/" Div

powOp :: Parser ArithExpr
powOp = binOp "**" Pow

arithExpr :: Parser ArithExpr
arithExpr = roundBracketed (addOp <|> subOp <|> mulOp <|> divOp <|> powOp)


-- Comparison --

data CompareExpr = Equals Expr Expr
                 | NotEquals Expr Expr
                 | GreaterThan Expr Expr
                 | LessThan Expr Expr
                 deriving (Eq, Show)

equalsOp :: Parser CompareExpr
equalsOp = binOp "===" Equals

notEqualsOp :: Parser CompareExpr
notEqualsOp = binOp "!==" NotEquals

greaterThanOp :: Parser CompareExpr
greaterThanOp = binOp ">" GreaterThan

lessThanOp :: Parser CompareExpr
lessThanOp = binOp "<" LessThan

compareExpr :: Parser CompareExpr
compareExpr = roundBracketed (equalsOp <|> notEqualsOp <|> greaterThanOp <|> lessThanOp )


-- Ternary --

data TernaryExpr
    = Ternary Expr Expr Expr
    deriving (Eq, Show)

ternaryExpr :: Parser TernaryExpr
ternaryExpr = roundBracketed $
    Ternary <$> expr <* charTok '?'
            <*> expr <* charTok ':'
            <*> expr


-- General Expression --

data Expr =
     FuncCallExpr FuncCall
    | JsVal JSValue
    | Arithmetic ArithExpr
    | Logical LogicExpr
    | Comparison CompareExpr
    | TernaryOp TernaryExpr
    deriving (Eq, Show)

expr :: Parser Expr
expr = FuncCallExpr <$> funcCall
   <|> JsVal <$> jsValue
   <|> Arithmetic <$> arithExpr
   <|> Logical <$> logicExpr
   <|> Comparison <$> compareExpr
   <|> TernaryOp <$> ternaryExpr


-- const declaration --

data ConstDecl = ConstDecl String Expr deriving (Eq, Show)

varName :: Parser String
varName = some (alpha <|> digit <|> is '_')

constDecl :: Parser ConstDecl
constDecl = ConstDecl <$> (stringTok "const" *> tok varName <* charTok '=') <*> expr <* charTok ';'


-- functions --

-- funcArg is an expr
data FuncCall = FuncCall String [Expr] deriving (Eq, Show)

funcCall :: Parser FuncCall
funcCall = FuncCall <$> varName <*> roundBracketed (commaSeparated expr)

data FuncDecl = TailRecursiveFunc String [String] Block
              | NonTailRecursiveFunc String [String] Block
              deriving (Eq, Show)

funcDecl :: Parser FuncDecl
funcDecl = do
    stringTok "function"
    fname <- varName
    params <- roundBracketed (commaSeparated varName)
    body <- block
    let constructor = if isTailRecursiveFunc fname params body then TailRecursiveFunc else NonTailRecursiveFunc
    return $ constructor fname params body


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

returnStmt :: Parser ReturnStmt
returnStmt = ReturnExpr <$> (stringTok "return" *> expr <* charTok ';')


data ReturnStmt = ReturnExpr Expr
                | ReturnVal JSValue deriving
                (Eq, Show)

data Stmt = StmtConst ConstDecl
          | StmtIf Conditional
          | StmtFuncCall FuncCall
          | StmtReturn ReturnStmt
          | StmtFuncDecl FuncDecl
          deriving (Eq, Show)

data Conditional = If Expr Block (Maybe Block) deriving (Eq, Show)

newtype Block = Block [Stmt]
  deriving (Eq, Show)

stmt :: Parser Stmt
stmt = StmtConst <$> constDecl
   <|> StmtIf <$> conditional
   <|> StmtReturn <$> returnStmt
   <|> (StmtFuncCall <$> funcCall <* charTok ';')  -- because funcCall part of an epxression or appear as a statement, we only consume semi colon if it is a statement
   <|> StmtFuncDecl <$> funcDecl


stmts :: Parser [Stmt]
stmts = many stmt

block :: Parser Block
block = Block <$> (charTok '{' *> stmts <* charTok '}')

conditional :: Parser Conditional
conditional = do
    stringTok "if"
    condition <- roundBracketed expr
    ifBlock <- block
    elseBlock <- optional (stringTok "else" *> block)
    return $ If condition ifBlock elseBlock


-- direct parsing --

-- Helper function to extract function details from a string
-- we need this since our custom function for detecting tail recursion has a different method signature
-- this function takes a string and returns a data type that can be analysed by our custom isTailRecursiveFunc function
parseFunction :: String -> Maybe (String, [String], Block)
parseFunction str =
    case parse funcDecl str of
        Result _ (TailRecursiveFunc fname params body) -> Just (fname, params, body)
        Result _ (NonTailRecursiveFunc fname params body) -> Just (fname, params, body)
        _ -> Nothing


-- pretty printing --

-- Pretty print for ArithExpr
prettyPrintArith :: ArithExpr -> String
prettyPrintArith (Add a b) = parenthesize $ prettyPrintExpr a ++ " + " ++ prettyPrintExpr b
prettyPrintArith (Sub a b) = parenthesize $ prettyPrintExpr a ++ " - " ++ prettyPrintExpr b
prettyPrintArith (Mul a b) = parenthesize $ prettyPrintExpr a ++ " * " ++ prettyPrintExpr b
prettyPrintArith (Div a b) = parenthesize $ prettyPrintExpr a ++ " / " ++ prettyPrintExpr b
prettyPrintArith (Pow a b) = parenthesize $ prettyPrintExpr a ++ " ** " ++ prettyPrintExpr b

-- Pretty print for LogicExpr
prettyPrintLogic :: LogicExpr -> String
prettyPrintLogic (LAnd a b) = parenthesize $ prettyPrintExpr a ++ " && " ++ prettyPrintExpr b
prettyPrintLogic (LOr a b) = parenthesize $ prettyPrintExpr a ++ " || " ++ prettyPrintExpr b
prettyPrintLogic (LNot a) = parenthesize $ "!" ++ prettyPrintExpr a
prettyPrintLogic (LBool v) = prettyPrintJSValue v


-- Pretty print for CompareExpr
prettyPrintComp :: CompareExpr -> String
prettyPrintComp (Equals a b) = parenthesize $ prettyPrintExpr a ++ " === " ++ prettyPrintExpr b
prettyPrintComp (NotEquals a b) = parenthesize $ prettyPrintExpr a ++ " !== " ++ prettyPrintExpr b
prettyPrintComp (GreaterThan a b) = parenthesize $ prettyPrintExpr a ++ " > " ++ prettyPrintExpr b
prettyPrintComp (LessThan a b) = parenthesize $ prettyPrintExpr a ++ " < " ++ prettyPrintExpr b


-- Pretty print for TernaryExpr
prettyPrintTernary :: TernaryExpr -> String
prettyPrintTernary (Ternary condition trueBranch falseBranch) =
    let
        conditionStr = prettyPrintExpr condition
        trueBranchStr = prettyPrintExpr trueBranch
        falseBranchStr = prettyPrintExpr falseBranch
        totalLength = length conditionStr + length trueBranchStr + length falseBranchStr

        delimiter
            | totalLength > 42 = "\n"
            | otherwise       = " "
    in
        parenthesize $ conditionStr
                    ++ delimiter ++ "? "
                    ++ trueBranchStr
                    ++ delimiter ++ ": "
                    ++ falseBranchStr

-- Pretty print for JSValue
prettyPrintJSValue :: JSValue -> String
prettyPrintJSValue (JSInt i) = show i
prettyPrintJSValue (JSString s) = "\"" ++ s ++ "\""
prettyPrintJSValue (JSBool True) = "true"
prettyPrintJSValue (JSBool False) = "false"
prettyPrintJSValue (JsVariable s) = s
prettyPrintJSValue (JSList lst) = "[" ++ intercalate ", " (map prettyPrintJSValue lst) ++ "]"

-- Helper function to wrap a string with parentheses and ensure spaces are handled correctly
parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")"

-- Pretty print the combined Expr
prettyPrintExpr :: Expr -> String
prettyPrintExpr (JsVal v) = prettyPrintJSValue v
prettyPrintExpr (Arithmetic a) = prettyPrintArith a
prettyPrintExpr (Logical l) = prettyPrintLogic l
prettyPrintExpr (Comparison c) = prettyPrintComp c
prettyPrintExpr (TernaryOp t) = prettyPrintTernary t
prettyPrintExpr (FuncCallExpr f) = prettyPrintFuncCall f

-- Pretty print for FuncCall
prettyPrintFuncCall :: FuncCall -> String
prettyPrintFuncCall (FuncCall name expr) =
    name ++ "(" ++ intercalate ", " (map prettyPrintExpr expr) ++ ")"


-- use this in other places too
shouldBeMultiLine :: String -> Bool
shouldBeMultiLine s = length s > 42

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
    "{\n  while (true) {\n" ++ (indent . init . prettyPrintStmts $ initStmts)
    ++ "    [" ++ intercalate ", " params ++ "] = "
    ++ tailOptimizedAssignment (last stmts) ++ ";\n  }\n}"
  where
    initStmts = init stmts  -- All statements except the last one
    indent = unlines . map ("    " ++) . lines  -- Four spaces for indenting

    tailOptimizedAssignment :: Stmt -> String
    tailOptimizedAssignment (StmtReturn (ReturnExpr (FuncCallExpr (FuncCall _ expr)))) =
        "[" ++ intercalate ", " (map prettyPrintExpr expr) ++ "]"



prettyPrintStmts :: [Stmt] -> String
prettyPrintStmts = unlines . map prettyPrintStmt


prettyPrintBlock :: Block -> String
prettyPrintBlock (Block []) = "{ }" -- if no statements, just print empty braces. later create HOF to parenthesize with third brackets or even somethign that lets you input hwich bracket you wanna insert. create valid data type for three types of braces TODO
prettyPrintBlock (Block [stmt]) = "{" ++ prettyPrintStmt stmt ++ "}"  -- if only one statement, don't put it on a new line
prettyPrintBlock (Block stmts) =  -- if multiple statements, put each on a new line
    "{\n" ++ indent (prettyPrintStmts stmts) ++ "}"
  where
    indent = unlines . map ("  " ++) . lines  -- Two spaces for indenting


prettyPrintConditional :: Conditional -> String
prettyPrintConditional (If expr ifBlock Nothing) =
    "if " ++ parenthesize (prettyPrintExpr expr) ++ " " ++ prettyPrintBlock ifBlock

prettyPrintConditional (If expr ifBlock (Just elseBlock)) =
    "if " ++ parenthesize (prettyPrintExpr expr) ++ " " ++ prettyPrintBlockIf ifBlock ++ " else " ++ prettyPrintBlockElse elseBlock
  where
    prettyPrintBlockIf (Block []) = "{ }"
    prettyPrintBlockIf (Block [stmt]) = "{" ++ prettyPrintStmt stmt ++ "}"
    prettyPrintBlockIf (Block stmts) =  -- if multiple statements, put each on a new line
        "{\n" ++ indent (prettyPrintStmts stmts) ++ "\n}"

    prettyPrintBlockElse (Block []) = "{ }"
    prettyPrintBlockElse (Block [stmt]) = "{" ++ prettyPrintStmt stmt ++ "}"
    prettyPrintBlockElse (Block stmts) =  -- if multiple statements, put each on a new line
        "{\n" ++ indent (prettyPrintStmts stmts) ++ "}"

    indent = unlines . map ("  " ++) . lines  -- Two spaces for indenting

