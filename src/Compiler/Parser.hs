-- |
-- Module      :  Compiler.Parser
--
-- This module contains the complete parser

module Compiler.Parser where

import Control.Monad (void)
import Compiler.ParserUtils
import Compiler.Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L


-- | 'mods' is a list of available modifiers
mods :: [(Mod,String)]
mods = [(Protected,"protected"),(Public,"public"),(Private,"private"),(Static,"static"),(Abstract,"abstract")]

-- | 'kwords' is a list of reserved words (keywords).

kwords :: [String]
kwords = ["if","then","else","while","do","skip","true","false","not","and","or","class"] ++ map snd mods


-- | 'name' parses an name. Names start with a lower case letter.

-- TODO Add underscore and dollar sign as allowed chars
name :: Parser Name
name = Name <$> (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` kwords
                then fail $ "keyword " ++ show x ++ " cannot be an name"
                else return x

program :: Parser [Class]
program = between spaceConsumer eof decls

-- | 'decls' parses class declarations
decls :: Parser [Class]
decls = many decl

decl :: Parser Class
decl = do
  modis <- modifiers
  kword "class"
  iden <- name
  body <- braces bodyDecls
  return $ Class iden modis body

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
bodyDecl = try fieldDecl
       <|> try (makeSingleton methodDecl)
       <|> makeSingleton constructorDecl

fieldDecl :: Parser [Decl]
fieldDecl = do
  modis <- modifiers
  fType <- identifier
  vars <- map Field <$> varDecls modis fType
  semicolon
  return vars


varDecls :: [Mod] -> Identifier -> Parser [VarDecl]
varDecls modis t = sepBy1 (varDecl modis t) comma

varDecl :: [Mod] -> Identifier -> Parser VarDecl
varDecl modis t = do
  iden <- name
  e <- optional varAssignment
  return $ (VarDecl iden modis t e)
  where
    varAssignment :: Parser Expression
    varAssignment = symbol "=" *> expression

expression :: Parser Expression
expression = try assignment
         <|> conditional

conditional :: Parser Expression
conditional = condOrExpr
          <|> shortIf
  where
    -- | Short notation for If with question mark and colon
    shortIf :: Parser Expression
    shortIf = do
      condE <- condOrExpr
      _ <- symbol "?"
      thenE <- expression
      _ <- symbol ":"
      elseE <- conditional -- TODO Why is this not a expression?
      return $ TernaryIf condE thenE elseE

condOrExpr :: Parser Expression
condOrExpr = makeExprParser (makeExprParser (unaryExpr <* spaceConsumer) aOperators) bOperators

bOperators :: [[Operator Parser Expression]]
bOperators =
  [ [ InfixL (PrimBinOp Less       <$ symbol "<")
    , InfixL (PrimBinOp LessEq     <$ symbol "<=")
    , InfixL (PrimBinOp Greater    <$ symbol ">")
    , InfixL (PrimBinOp GreaterEq  <$ symbol ">=")
    , InfixL (PrimBinOp InstanceOf <$ kword "instanceof")]
  , [ InfixL (PrimBinOp Eq         <$ symbol "==")
    , InfixL (notEq                <$ symbol "!="
             )]
  , [ InfixL (PrimBinOp And        <$ symbol "&&")]
  , [ InfixL (PrimBinOp Or         <$ symbol "||")]
  ]
  where
    notEq l r = PrimUnOp Not $ PrimBinOp Eq l r

aOperators :: [[Operator Parser Expression]]
aOperators =
  [ [ Prefix  (PrimUnOp Neg       <$ symbol "-")
    , Prefix  (id                 <$ symbol "+")
    , Prefix  (PrimUnOp Not       <$ symbol "!")
    , Prefix  (PrimUnOp BitCompl  <$ symbol "~")
    , Prefix  (PrimUnOp PreIncr   <$ symbol "++")
    , Postfix (PrimUnOp PostIncr  <$ symbol "++")
    , Prefix  (PrimUnOp PreDecr   <$ symbol "--")
    , Postfix (PrimUnOp PostDecr  <$ symbol "--")]
  , [ InfixL  (PrimBinOp Multiply <$ symbol "*")
    , InfixL  (PrimBinOp Divide   <$ symbol "/")
    , InfixL  (PrimBinOp Modulo   <$ symbol "%")]
  , [ InfixL  (PrimBinOp Add      <$ symbol "+")
    , InfixL  (PrimBinOp Subtract <$ symbol "-")]
  ]

unaryExpr :: Parser Expression
unaryExpr = try primary <|> (Iden <$> identifier) <|> try castExpr

castExpr :: Parser Expression
castExpr = fail "cast not implemented"

-- we probably have to place some tries here
primary :: Parser Expression
primary = literal
      <|> this
      <|> parens expression
      <|> instanceCreation
      <|> try methodInvocation
      <|> fieldAccess

literal :: Parser Expression
literal = Literal <$> (intLit <|> booleanLit <|> charLit <|> stringLit <|> nullLit)
  where
    intLit = IntegerL <$> integer
    nullLit = Null <$ kword "null"

booleanLit :: Parser Lit
booleanLit = trueLit <|> falseLit
  where
    trueLit = BooleanL True <$ kword "true"
    falseLit = BooleanL False <$ kword "false"

charLit :: Parser Lit
charLit = CharL <$> (char '\'' *> L.charLiteral <* char '\'')

stringLit :: Parser Lit
stringLit = StringL <$> (char '"' *> manyTill L.charLiteral (char '"'))

this :: Parser Expression
this = This <$ kword "this"

instanceCreation :: Parser Expression
instanceCreation = do
  _ <- kword "new"
  clType <- classType
  args <- parens $ sepBy expression comma
  return $ Instantiation clType args

classType :: Parser Identifier
classType = identifier

identifier :: Parser Identifier
identifier = createType <$> raw
  where
    raw :: Parser [Name]
    raw = sepBy1 name (symbol ".")
    createType :: [Name] -> Identifier
    createType ids = Identifier (init ids) (last ids)

-- TODO identifier and fieldAccess seem ambiguous. Does the grammar ensure differentiation?
fieldAccess :: Parser Expression
fieldAccess = parens primary <* symbol "." *> fieldAccess


methodInvocation :: Parser Expression
methodInvocation = do
  fun <- Iden <$> identifier <|> parens primary
  args <- parens $ sepBy expression comma
  return $ Apply fun args


assignment :: Parser Expression
assignment = do
  iden <- identifier
  op <- assignmentOp
  rhs <- expression
  return $ Assign op iden rhs

assignmentOp :: Parser AssignOp
assignmentOp = choice $ map opParser assignOperators
  where
    opParser (rep,str) = rep <$ symbol str
    -- TODO Complete for all operators
    assignOperators = [(NormalAssign,"="),(PlusAssign,"+="),(MinusAssign,"-=")
                      ,(MultiplyAssign,"*=")]

constructorDecl :: Parser Decl
constructorDecl = fail "not implemented"

methodDecl :: Parser Decl
methodDecl = do
  modis   <- modifiers
  rType  <- returnType
  mName  <- name
  params <- formalParamList
  mBody  <- methodBody
  return $ Method mName modis rType params mBody
  where
    returnType :: Parser Type
    returnType = try (voidType <$ kword "void") <|> identifier
    methodBody :: Parser (Maybe Expression)
    methodBody = (Nothing <$ symbol ";") <|> (Just <$> block)

formalParamList :: Parser [(Type,Name)]
formalParamList = parens (sepBy formalParam comma)
  where
    formalParam = do paramType <- typeIden; paramName <- name; return (paramType,paramName)

block :: Parser Expression
block = (Block . concat) <$> braces (many blockStatement)
  where
    blockStatement = try localVarDecl <|> makeSingleton statement

localVarDecl :: Parser [Expression]
localVarDecl = do
  vType <- typeIden
  vDecs <- varDecls [] vType
  semicolon
  return $ map LocalVar vDecs

statement :: Parser Expression
statement = try statementWithoutTrailing
        <|> try ifStmt -- TODO Is it possible to merge if and ifThen?
        <|> ifThenStmt
        <|> whileStmt

ifStmt :: Parser Expression
ifStmt = do
  kword "if"
  cond <- parens expression
  thenBranch <- statement
  return $ If cond thenBranch Nothing

ifThenStmt :: Parser Expression
ifThenStmt = do
  kword "if"
  cond <- parens expression
  thenBranch <- statementNoShortIf
  kword "else"
  elseBranch <- statement
  return $ If cond thenBranch $ Just elseBranch

statementNoShortIf :: Parser Expression
statementNoShortIf = statementWithoutTrailing
                 <|> ifThenStmtNoShortIf
                 <|> whileStmtNoShortIf

ifThenStmtNoShortIf :: Parser Expression
ifThenStmtNoShortIf = do
  kword "if"
  cond <- parens expression
  thenBranch <- statementNoShortIf
  kword "else"
  elseBranch <- statementNoShortIf
  return $ If cond thenBranch $ Just elseBranch

whileStmt :: Parser Expression
whileStmt = do
  kword "while"
  cond <- parens expression
  body <- statement
  return $ While cond body

whileStmtNoShortIf :: Parser Expression
whileStmtNoShortIf = do
  kword "while"
  cond <- parens expression
  body <- statementNoShortIf
  return $ While cond body

statementWithoutTrailing :: Parser Expression
statementWithoutTrailing = block
                       <|> try emptyStmt
                       <|> try expressionStmt
                       <|> returnStmt
  where
    -- TODO What should be the returned node here?
    emptyStmt :: Parser Expression
    emptyStmt = EmptyStmt <$ semicolon

-- TODO Refactor this
expressionStmt :: Parser Expression
expressionStmt = statementExpr <* semicolon
  where
    statementExpr = try assignment
                <|> preIncr
                <|> preDecr
                <|> try postIncr
                <|> try postDecr
                <|> methodInvocation
                <|> instanceCreation
    preIncr = PrimUnOp PreIncr <$> (symbol "++" *> unaryExpr)
    preDecr = PrimUnOp PreDecr <$> (symbol "--" *> unaryExpr)
    postIncr = do
      e <- postFixExpr
      ops <- many $ void (symbol "++")
      return $  makeSeqOp PostIncr ops e
    postDecr = do
      e <- postFixExpr
      ops <- many $ void (symbol "--")
      return $  makeSeqOp PostDecr ops e
    postFixExpr = try primary <|> (Iden <$> identifier)
    makeSeqOp :: UnOp -> [()] -> Expression -> Expression
    makeSeqOp constr ops e = foldr (\_ r -> PrimUnOp constr r) e ops

returnStmt :: Parser Expression
returnStmt = Return <$> (kword "return" *> optional expression <* semicolon)

-- TODO Add primitive types
typeIden :: Parser Identifier
typeIden = identifier
