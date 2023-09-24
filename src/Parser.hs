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
--
-- >>> parse char "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse char "")
-- True
char :: Parser Char
char = Parser f
  where
    f "" = Error UnexpectedEof
    f (x : xs) = Result xs x

-- | Parse numbers as int until non-digit

---- >>> parse int "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse int "")
-- True
--
-- >>> isErrorResult (parse int "a")
-- True
int :: Parser Int
int = Parser f
  where
    -- This is okay because the case statement is small
    f "" = Error UnexpectedEof
    f x = case readInt x of
      Just (v, rest) -> Result rest v
      Nothing -> Error $ UnexpectedChar (head x)

-- | Write a parser that asserts that there is no remaining input.
--
-- >>> parse eof ""
-- Result >< ()
--
-- >>> isErrorResult (parse eof "abc")
-- True
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
--
--   * the input is empty; or
--
--   * the character does not satisfy the given predicate.
--
-- >>> parse (satisfy isUpper) "Abc"
-- Result >bc< 'A'
--
-- >>> isErrorResult (parse (satisfy isUpper) "abc")
-- True
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = char >>= f'
  where
    -- This is okay because guards are small
    f' c
      | f c = pure c
      | otherwise = unexpectedCharParser c

-- | Return a parser that produces the given character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not equal to the given character.
--
-- >>> parse (is 'c') "c"
-- Result >< 'c'
--
-- >>> isErrorResult (parse (is 'c') "")
-- True
--
-- >>> isErrorResult (parse (is 'c') "b")
-- True
is :: Char -> Parser Char
is = satisfy . (==)

-- | Return a parser that produces any character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is equal to the given character.
--
-- >>> parse (isNot 'c') "b"
-- Result >< 'b'
--
-- >>> isErrorResult (parse (isNot 'c') "")
-- True
--
-- >>> isErrorResult (parse (isNot 'c') "c")
-- True
isNot :: Char -> Parser Char
isNot = satisfy . (/=)

-- | Write a function that parses one of the characters in the given string.
--
-- /Hint/: What does `elem` do? What are its parameters?
--
-- >>> parse (oneof "abc") "bcdef"
-- Result >cdef< 'b'
--
-- >>> isErrorResult (parse (oneof "abc") "def")
-- True
oneof :: String -> Parser Char
oneof = satisfy . flip elem

-- | Write a function that parses any character, but fails if it is in the
-- given string.
--
-- /Hint/: What does `notElem` do? What are its parameters?
--
-- >>> parse (noneof "bcd") "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (noneof "abcd") "abc")
-- True
noneof :: String -> Parser Char
noneof = satisfy . flip notElem

-- | Return a parser that produces a character between '0' and '9' but fails if
--
--   * the input is empty; or
--
--   * the produced character is not a digit.
--
-- /Hint/: Use the 'isDigit' function
digit :: Parser Char
digit = satisfy isDigit

-- | Return a parser that produces a space character but fails if
--
--   * the input is empty; or
--
--   * the produced character is not a space.
--
-- /Hint/: Use the 'isSpace' function
space :: Parser Char
space = satisfy isSpace

-- | Return a parser that produces a lower-case character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not lower-case.
--
-- /Hint/: Use the 'isLower' function
lower :: Parser Char
lower = satisfy isLower

-- | Return a parser that produces an upper-case character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not upper-case.
--
-- /Hint/: Use the 'isUpper' function
upper :: Parser Char
upper = satisfy isUpper

-- | Return a parser that produces an alpha character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not alpha.
--
-- /Hint/: Use the 'isAlpha' function
alpha :: Parser Char
alpha = satisfy isAlpha

-- | Write a parser that will parse zero or more spaces.
--
-- /Hint/: Remember the `space` parser!
--
-- >>> parse spaces " abc"
-- Result >abc< " "
--
-- >>> parse spaces "abc"
-- Result >abc< ""
spaces :: Parser String
spaces = many space

-- | Return a parser that produces one or more space characters (consuming
-- until the first non-space) but fails if:
--
--   * the input is empty; or
--
--   * the first produced character is not a space.
--
-- /Hint/: Remember the `space` parser!
--
-- >>> parse spaces1 " abc"
-- Result >abc< " "
--
-- >>> isErrorResult $ parse spaces1 "abc"
-- True
spaces1 :: Parser String
spaces1 = some space

-- | Write a function that parses the given string (fails otherwise).
--
-- /Hint/: Use 'is' and 'traverse'.
--
-- >>> parse (string "abc") "abcdef"
-- Result >def< "abc"
--
-- >>> isErrorResult (parse (string "abc") "bcdef")
-- True
string :: String -> Parser String
string = traverse is

-- | -------------------------------------------------
-- | --------------- Token parsers -------------------
-- | -------------------------------------------------

-- | Write a function that applies the given parser, then parses 0 or more
-- spaces, then produces the result of the original parser.
--
-- /Hint/: You can use the Monad instance or Applicatives
--
-- >>> parse (tok (is 'a')) "a bc"
-- Result >bc< 'a'
--
-- >>> parse (tok (is 'a')) "abc"
-- Result >bc< 'a'
tok :: Parser a -> Parser a
tok p = spaces *> p <* spaces

-- tok p = do
--   r <- p
--   spaces
--   pure r

-- | Write a function that parses the given char followed by 0 or more spaces.
--
-- /Hint/: Remember the `is` parser
--
-- >>> parse (charTok 'a') "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (charTok 'a') "dabc")
-- True
charTok :: Char -> Parser Char
charTok = tok . is

-- | Write a parser that parses a comma ',' followed by 0 or more spaces.
--
-- /Hint/: We just implemented `charTok`
--
-- >>> parse commaTok ",123"
-- Result >123< ','
--
-- >>> isErrorResult( parse commaTok "1,23")
-- True
commaTok :: Parser Char
commaTok = charTok ','

-- | Write a function that parses the given string, followed by 0 or more
-- spaces.
--
-- /Hint/: Remember the `string` parser
--
-- >>> parse (stringTok "abc") "abc  "
-- Result >< "abc"
--
-- >>> isErrorResult (parse (stringTok "abc") "bc  ")
-- True
stringTok :: String -> Parser String
stringTok = tok . string

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> spaces *> p)

bracketed :: Char -> Char -> Parser a -> Parser a
bracketed open close p = tok (is open) *> p <* tok (is close)

parenthesized :: Parser a -> Parser a
parenthesized = bracketed '(' ')'

-- the 'choice' function returns the first successful parser from a list
-- choice :: [Parser a] -> Parser a
-- choice = foldr (<|>) (fail "No choice matched")

-- try :: Parser a -> Parser a
-- try p = Parser $ \input -> case parse p input of
--     Error _ -> Error (UnexpectedString input)
--     success -> success


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
jsBool = tok $ (string "true" $> JSBool True) <|> (string "false" $> JSBool False)

jsVar :: Parser JSValue
jsVar = JsVariable <$> varName

jsValue :: Parser JSValue
jsValue = jsInt <|> jsString <|> jsBool <|> mixedList <|> jsVar

jsListItem :: Parser a -> Parser [a]
jsListItem p = bracketed '[' ']' (sepBy p commaTok)

mixedList :: Parser JSValue
mixedList = JSList <$> jsListItem jsValue

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
jsBoolValue = LBool <$> (parenthesized jsBool <|> jsBool)

logicExpr :: Parser LogicExpr
logicExpr = parenthesized (logicNot <|> logicAnd <|> logicOr <|> jsBoolValue)

-- Arithmetic
data ArithExpr = Add Expr Expr
               | Sub Expr Expr
               | Mul Expr Expr
               | Div Expr Expr
               | Pow Expr Expr
               deriving (Eq, Show)

-- The main expression parser
arithExpr :: Parser ArithExpr
arithExpr = parenthesized (binaryOp expr)  

-- General binary operation parser using the specialized parsers
binaryOp :: Parser Expr -> Parser ArithExpr
binaryOp p = addOp p <|> subOp p <|> mulOp p <|> divOp p <|> powOp p

-- Helper to generate a parser for a binary operation
binArithOp :: Parser Expr -> Char -> (Expr -> Expr -> ArithExpr) -> Parser ArithExpr
binArithOp p op constructor = do
    lhs <- p
    tok (is op)
    constructor lhs <$> p

-- Parsers for individual operations
addOp :: Parser Expr -> Parser ArithExpr
addOp p = binArithOp p '+' Add

subOp :: Parser Expr -> Parser ArithExpr
subOp p = binArithOp p '-' Sub

mulOp :: Parser Expr -> Parser ArithExpr
mulOp p = binArithOp p '*' Mul

divOp :: Parser Expr -> Parser ArithExpr
divOp p = binArithOp p '/' Div

powOp :: Parser Expr -> Parser ArithExpr
powOp p = do
    lhs <- p
    tok (is '*' >> is '*')
    Pow lhs <$> p


-- comparison --

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
compareExpr = parenthesized (equalsOp <|> notEqualsOp <|> greaterThanOp <|> lessThanOp ) 

-- ternary --

data TernaryExpr
    = Ternary Expr Expr Expr
    deriving (Eq, Show)

ternaryExpr :: Parser TernaryExpr
ternaryExpr = parenthesized $
    Ternary <$> expr <* tok (is '?')
            <*> expr <* tok (is ':')
            <*> expr


-- general expression --

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
constDecl = do
    tok (string "const")
    name <- varName
    spaces
    tok (is '=')
    e <- expr
    tok (is ';')
    return $ ConstDecl name e

-- functions --

data FuncArg = ArgVal JSValue | ArgExpr Expr deriving (Eq, Show)
data FuncCall = FuncCall String [FuncArg] deriving (Eq, Show)

funcArg :: Parser FuncArg
funcArg = (ArgExpr <$> (parenthesized expr <|> expr)) <|> (ArgVal <$> jsValue) 

funcCall :: Parser FuncCall
funcCall = do
    name <- varName
    args <- parenthesized $ sepBy funcArg commaTok
    return $ FuncCall name args


data FuncDecl = TailRecursiveFunc String [String] Block
              | NonTailRecursiveFunc String [String] Block
              deriving (Eq, Show)


funcDecl :: Parser FuncDecl
funcDecl = do
    tok (string "function")
    fname <- varName
    params <- parenthesized $ sepBy varName commaTok
    body <- block
    if isTailRecursiveFunc fname params body
        then return $ TailRecursiveFunc fname params body
        else return $ NonTailRecursiveFunc fname params body


isTailRecursiveFunc :: String -> [String] -> Block -> Bool
isTailRecursiveFunc fname params (Block stmts) =
    case last stmts of
        StmtReturn returnStmt -> isTailRecursiveReturn fname params returnStmt
        _ -> False

isReturn :: Stmt -> Bool
isReturn (StmtReturn _) = True
isReturn _ = False

hasFuncCall :: String -> Stmt -> Bool
hasFuncCall fname (StmtReturn (ReturnExpr (FuncCallExpr (FuncCall fname' _)))) = fname == fname'
hasFuncCall _ _ = False

isTailRecursiveReturn :: String -> [String] -> ReturnStmt -> Bool
isTailRecursiveReturn fname params (ReturnExpr (FuncCallExpr (FuncCall fname' args)))
    | fname /= fname' = False
    | length args /= length params = False
    | any hasNestedFuncCall args = False
    | otherwise = True
isTailRecursiveReturn _ _ _ = False


hasNestedFuncCall :: FuncArg -> Bool
hasNestedFuncCall (ArgExpr (FuncCallExpr _)) = True
hasNestedFuncCall _ = False


returnStmt :: Parser ReturnStmt
returnStmt = do
    tok (string "return")
    e <- ReturnExpr <$> expr -- This handles all expressions, including JSValue
    tok (is ';')
    return e


data ReturnStmt = ReturnExpr Expr 
                | ReturnVal JSValue deriving 
                (Eq, Show)

-- block --

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
   <|> (StmtFuncCall <$> funcCall <* tok (is ';'))  -- because funcCall part of an epxression or appear as a statement, we only consume semi colon if it is a statement
   <|> StmtFuncDecl <$> funcDecl 


stmts :: Parser [Stmt]
stmts = many stmt

block :: Parser Block
block = do
    tok (is '{')
    statements <- stmts
    tok (is '}')
    return $ Block statements

conditional :: Parser Conditional
conditional = do
    tok (string "if")
    condition <- parenthesized expr
    ifBlock <- block
    elseBlock <- optional (tok (string "else") *> block)
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

prettyPrintFuncArg :: FuncArg -> String
prettyPrintFuncArg (ArgVal jsVal) = prettyPrintJSValue jsVal
prettyPrintFuncArg (ArgExpr expr) = prettyPrintExpr expr

-- Pretty print for FuncCall
prettyPrintFuncCall :: FuncCall -> String
prettyPrintFuncCall (FuncCall name args) =
    name ++ "(" ++ intercalate ", " (map prettyPrintFuncArg args) ++ ")"


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

prettyPrintArg :: FuncArg -> String
prettyPrintArg (ArgVal jsVal) = prettyPrintJSValue jsVal
prettyPrintArg (ArgExpr expr) = prettyPrintExpr expr

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
    tailOptimizedAssignment (StmtReturn (ReturnExpr (FuncCallExpr (FuncCall _ args)))) = 
        "[" ++ intercalate ", " (map prettyPrintFuncArg args) ++ "]"



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

