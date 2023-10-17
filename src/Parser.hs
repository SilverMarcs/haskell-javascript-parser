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