-- |
-- Module      :  Parser
--
-- This module contains the complete parser

module Parser where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Ast

type Parser = Parsec Void String

-- | 'spaceConsumer' Consumes spaces and comments

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

semicolon :: Parser String
semicolon = symbol ";"

-- | 'kword' parses a reserved word (keyword).

kword :: String -> Parser ()
kword w = lexeme (string w *> notFollowedBy alphaNumChar)

-- | 'mods' is a list of available modifiers
mods :: [(Mod,String)]
mods = [(Protected,"protected"),(Public,"public"),(Private,"private"),(Static,"static"),(Abstract,"abstract")]

-- | 'kwords' is a list of reserved words (keywords).

kwords :: [String]
kwords = ["if","then","else","while","do","skip","true","false","not","and","or","class"] ++ map snd mods


-- | 'identifier' parses an identifier. Identifiers start with a lower case letter.

-- TODO Add underscore and dollar sign as allowed chars
identifier :: Parser Identifier
identifier = Identifier <$> (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` kwords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

program :: Parser [Class]
program = between spaceConsumer eof decls

-- | 'decls' parses class declarations
decls :: Parser [Class]
decls = many decl

decl :: Parser Class
decl = do
  mods <- modifiers
  kword "class"
  id <- identifier
  body <- bodyDecls
  return $ Class id body

modifiers :: Parser [Mod]
modifiers = many modifier

modifier :: Parser Mod
modifier = choice modParsers
  where
    modParsers = map createModParser mods
    createModParser :: (Mod,String) -> Parser Mod
    createModParser (m,s) = const m <$> kword s

-- sepBy semicolon?
bodyDecls :: Parser [Decl]
bodyDecls = concat <$> many bodyDecl

-- TODO 'constructorDecl' and 'methodDecl' may be ambiguous and we should use 'try' one the first one then
bodyDecl :: Parser [Decl]
bodyDecl = fieldDecl
       <|> constructorDecl
       <|> methodDecl


fieldDecl :: Parser [Decl]
fieldDecl = do
  fType <- jType
  vars <- varDecls fType
  semicolon
  return vars



jType :: Parser Type
jType = undefined

varDecls :: Type -> Parser [Decl]
varDecls = undefined

constructorDecl :: Parser [Decl]
constructorDecl = undefined

methodDecl :: Parser [Decl]
methodDecl = undefined

stmts :: Parser [Stmt]
stmts = sepBy stmt semicolon

stmt :: Parser Stmt
stmt = ifStmt
  <|> whileStmt
  <|> skipStmt
  <|> assignStmt
  <|> parens stmt

ifStmt :: Parser Stmt
ifStmt = do
  kword "if"
  cond  <- bExpr
  kword "then"
  stmt1 <- stmt
  kword "else"
  stmt2 <- stmt
  return (If cond stmt1 stmt2)

whileStmt :: Parser Stmt
whileStmt = do
  kword "while"
  cond <- bExpr
  kword "do"
  stmt1 <- stmt
  return (While cond stmt1)

assignStmt :: Parser Stmt
assignStmt = do
  var  <- identifier
  void (symbol ":=")
  expr <- aExpr
  return (Assign var expr)

skipStmt :: Parser Stmt
skipStmt = Skip <$ kword "skip"

aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators

aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix (Neg <$ symbol "-") ]
  , [ InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide   <$ symbol "/") ]
  , [ InfixL (ABinary Add      <$ symbol "+")
    , InfixL (ABinary Subtract <$ symbol "-") ]
  ]

bOperators :: [[Operator Parser BExpr]]
bOperators =
  [ [Prefix (Not <$ kword "not") ]
  , [InfixL (BBinary And <$ kword "and")
    , InfixL (BBinary Or <$ kword "or") ]
  ]

aTerm :: Parser AExpr
aTerm = parens aExpr
  <|> Var      <$> identifier
  <|> IntConst <$> integer

bTerm :: Parser BExpr
bTerm =  parens bExpr
  <|> (BoolConst True  <$ kword "true")
  <|> (BoolConst False <$ kword "false")
  <|> rExpr

rExpr :: Parser BExpr
rExpr = do
  a1 <- aExpr
  op <- relation
  a2 <- aExpr
  return (RBinary op a1 a2)

relation :: Parser RBinOp
relation = (symbol ">" *> pure Greater)
  <|> (symbol "<" *> pure Less)
