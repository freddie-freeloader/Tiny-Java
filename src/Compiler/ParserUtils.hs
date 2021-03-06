-- |
-- Module      :  Compiler.ParserUtils
--
-- This module contains definitions and some useful helpers for "Compiler.Parser"

module Compiler.ParserUtils where

import Data.Void
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | 'Parser' is using Void for custom errors. This might change someday.
type Parser = Parsec (ErrorFancy Void) String

-- | 'makeSingleton' puts the result of a parser into an array
makeSingleton :: Parser a -> Parser [a]
makeSingleton = (<$>) $ flip (:) []

-- | 'spaceConsumer' consumes spaces and comments
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- | 'lexeme' is a wrapper for lexemes that consumes all spaces and comments after a lexeme
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Symbol
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | 'braces' parses something between parenthesis.
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | 'integer' parses an integer.
integer :: Parser Integer
integer = lexeme L.decimal

-- | 'semicolon' parses a semicolon.
semicolon :: Parser ()
semicolon = void (symbol ";")

-- | 'comma' parses a comma.
comma :: Parser ()
comma = void (symbol ",")

-- | 'kword' parses a reserved word (keyword).
kword :: String -> Parser ()
kword w = lexeme (string w *> notFollowedBy alphaNumChar)

-- | 'binOp' defines recursive binary operation with a different parser
-- for the left side.
binOp :: String -> (a -> a -> a) -> Parser a -> Parser a
binOp opStr op nextParser = nextParser >>=
                              (\left -> option left (recursive left))
                              -- Returns just left if recursive fails
  where
    recursive left = op left <$> (symbol opStr *> binOp opStr op nextParser)
