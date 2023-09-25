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

data ADT
  = ExprValue Expr
  | BlockValue Block
  | StatementValues [Stmt]
  deriving (Eq, Show)

-- | Exercise A
parseExerciseA :: Parser ADT
parseExerciseA = ExprValue <$> expr

prettyPrintExerciseA :: ADT -> String
prettyPrintExerciseA (ExprValue e) = prettyPrintExpr e

-- | Exercise B
parseExerciseB :: Parser ADT
parseExerciseB =
  BlockValue <$> block
    <|> StatementValues <$> stmts

prettyPrintExerciseB :: ADT -> String
prettyPrintExerciseB (StatementValues stmts) = prettyPrintStmts stmts
prettyPrintExerciseB (BlockValue blk) = prettyPrintBlock blk

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
prettyPrintExerciseC (StatementValues stmts) = prettyPrintStmts stmts


-- | Extra Bonus section that parses and evaluates simple arithmatic, logical and comparison expressions
-- check out the test cases and results in javascript/inputs/D and javascript/output/D
parseExerciseD :: Parser ADT
parseExerciseD = ExprValue . JsVal <$> evalUnifiedExpr

prettyPrintExerciseD :: ADT -> String
prettyPrintExerciseD (ExprValue (JsVal v)) = prettyPrintJSValue v
