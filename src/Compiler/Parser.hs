-- |
-- Module      :  Compiler.Parser
--
-- This module contains the complete parser

module Compiler.Parser(parseTestString) where

import           Compiler.Ast
import           Compiler.ParserUtils
import           Control.Monad              (void)
import           Data.Maybe                 (catMaybes)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr


-- | 'mods' is a list of available modifiers
mods :: [(Mod,String)]
mods = [(Protected,"protected"),(Public,"public"),(Private,"private"),(Static,"static"),(Abstract,"abstract")]

-- | 'kwords' is a list of reserved words (keywords).

kwords :: [String]
kwords = ["if","then","else","while","do","skip","true","false","not","and","or","class"] ++ map snd mods


-- | 'identifier' parses an identifier.

-- TODO Add underscore and dollar sign as allowed chars
identifier :: Parser Identifier
identifier = Identifier <$> (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` kwords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

-- | 'parseTest' parses a String, which contains multiple classes
parseTestString :: String -> Maybe [Class]
parseTestString = parseMaybe program

-- | 'program' parses multiple classes
program :: Parser [Class]
program = between spaceConsumer eof decls

-- | 'decls' parses class declarations
decls :: Parser [Class]
decls = many decl

decl :: Parser Class
decl = do
  modis <- modifiers
  kword "class"
  iden <- identifier
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
  fType <- name
  vars <- map Field <$> varDecls modis fType
  semicolon
  return vars


varDecls :: [Mod] -> Name -> Parser [VarDecl]
varDecls modis t = sepBy1 (varDecl modis t) comma

varDecl :: [Mod] -> Name -> Parser VarDecl
varDecl modis t = do
  iden <- identifier
  e <- optional varAssignment
  return $ VarDecl iden modis t e
  where
    varAssignment :: Parser Expression
    varAssignment = symbol "=" *> expression

expression :: Parser Expression
expression = try (ExprExprStmt <$> assignment)
         <|> conditional

conditional :: Parser Expression
conditional = condOrExpr
          <|> ternaryIf
  where
    -- | Short notation for If with question mark and colon
    ternaryIf :: Parser Expression
    ternaryIf = do
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
    , Prefix  (makeUnOp PreIncr   <$ symbol "++")
    , Postfix (makeUnOp PostIncr  <$ symbol "++")
    , Prefix  (makeUnOp PreDecr   <$ symbol "--")
    , Postfix (makeUnOp PostDecr  <$ symbol "--")]
  , [ InfixL  (PrimBinOp Multiply <$ symbol "*")
    , InfixL  (PrimBinOp Divide   <$ symbol "/")
    , InfixL  (PrimBinOp Modulo   <$ symbol "%")]
  , [ InfixL  (PrimBinOp Add      <$ symbol "+")
    , InfixL  (PrimBinOp Subtract <$ symbol "-")]
  ]
  where
    makeUnOp :: IncrOrDecr -> Expression -> Expression
    makeUnOp op e = ExprExprStmt $ SEUnOp op e

unaryExpr :: Parser Expression
unaryExpr = try primary <|> (Iden <$> name) <|> try castExpr

castExpr :: Parser Expression
castExpr = fail "cast not implemented"

-- we probably have to place some tries here
primary :: Parser Expression
primary = literal
      <|> this
      <|> parens expression
      <|> ExprExprStmt <$> instanceCreation
      <|> try (ExprExprStmt <$> methodInvocation)
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

instanceCreation :: Parser StmtExpr
instanceCreation = do
  _ <- kword "new"
  clType <- classType
  args <- parens $ sepBy expression comma
  return $ Instantiation clType args

classType :: Parser Name
classType = name

name :: Parser Name
name = createType <$> raw
  where
    raw :: Parser [Identifier]
    raw = sepBy1 identifier (symbol ".")
    createType :: [Identifier] -> Name
    createType ids = Name (init ids) (last ids)

-- TODO name and fieldAccess seem ambiguous. Does the grammar ensure differentiation?
fieldAccess :: Parser Expression
fieldAccess = parens primary <* symbol "." *> fieldAccess


methodInvocation :: Parser StmtExpr
methodInvocation = do
  fun <- Iden <$> name <|> parens primary
  args <- parens $ sepBy expression comma
  return $ Apply fun args


assignment :: Parser StmtExpr
assignment = do
  iden <- name
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
  mIdentifier  <- identifier
  params <- formalParamList
  mBody  <- methodBody
  return $ Method mIdentifier modis rType params mBody
  where
    returnType :: Parser Type
    returnType = try (voidType <$ kword "void") <|> name
    methodBody :: Parser (Maybe Statement)
    methodBody = (Nothing <$ symbol ";") <|> (Just <$> block)

formalParamList :: Parser [(Type,Identifier)]
formalParamList = parens (sepBy formalParam comma)
  where
    formalParam = do paramType <- typeIden; paramIdentifier <- identifier; return (paramType,paramIdentifier)

block :: Parser Statement
block = (Block . concat) <$> braces (many blockStatement)
  where
    blockStatement :: Parser [Statement]
    blockStatement = try localVarDecl <|> catMaybes <$> makeSingleton statement

localVarDecl :: Parser [Statement]
localVarDecl = do
  vType <- typeIden
  vDecs <- varDecls [] vType
  semicolon
  return $ map LocalVar vDecs

statement :: Parser (Maybe Statement)
statement = try statementWithoutTrailing
        <|> try (Just <$> ifStmt) -- TODO Is it possible to merge if and ifThen?
        <|> Just <$> ifThenStmt
        <|> Just <$> whileStmt

ifStmt :: Parser Statement
ifStmt = do
  kword "if"
  cond <- parens expression
  thenBranch <- statement
  return $ If cond thenBranch Nothing

ifThenStmt :: Parser Statement
ifThenStmt = do
  kword "if"
  cond <- parens expression
  thenBranch <- statementNoShortIf
  kword "else"
  elseBranch <- statement
  return $ If cond thenBranch elseBranch

statementNoShortIf :: Parser (Maybe Statement)
statementNoShortIf = statementWithoutTrailing
                 <|> Just <$> ifThenStmtNoShortIf
                 <|> Just <$> whileStmtNoShortIf

ifThenStmtNoShortIf :: Parser Statement
ifThenStmtNoShortIf = do
  kword "if"
  cond <- parens expression
  thenBranch <- statementNoShortIf
  kword "else"
  elseBranch <- statementNoShortIf
  return $ If cond thenBranch elseBranch

whileStmt :: Parser Statement
whileStmt = do
  kword "while"
  cond <- parens expression
  body <- statement
  return $ While cond body

whileStmtNoShortIf :: Parser Statement
whileStmtNoShortIf = do
  kword "while"
  cond <- parens expression
  body <- statementNoShortIf
  return $ While cond body

statementWithoutTrailing :: Parser (Maybe Statement)
statementWithoutTrailing = Just <$> block
                       <|> try emptyStmt
                       <|> try (Just <$> expressionStmt)
                       <|> Just <$> returnStmt
  where
    -- TODO What should be the returned node here?
    emptyStmt :: Parser (Maybe Statement)
    emptyStmt = Nothing <$ semicolon

-- TODO Refactor this
expressionStmt :: Parser Statement
expressionStmt = StmtExprStmt <$> statementExpr <* semicolon
  where
    statementExpr :: Parser StmtExpr
    statementExpr = try assignment
                <|> preIncr
                <|> preDecr
                <|> try postIncr
                <|> try postDecr
                <|> methodInvocation
                <|> instanceCreation
    preIncr = SEUnOp PreIncr <$> (symbol "++" *> unaryExpr)
    preDecr = SEUnOp PreDecr <$> (symbol "--" *> unaryExpr)
    postIncr = do
      e <- postFixExpr
      ops <- many $ void (symbol "++")
      return (case makeSeqOp PostIncr ops e of ExprExprStmt inner -> inner; _ -> undefined)
    postDecr = do
      e <- postFixExpr
      ops <- many $ void (symbol "--")
      return (case makeSeqOp PostDecr ops e of ExprExprStmt inner -> inner; _ -> undefined)
    postFixExpr = try primary <|> (Iden <$> name)
    makeSeqOp :: IncrOrDecr -> [()] -> Expression -> Expression
    makeSeqOp constr ops e = foldr (\_ r -> (\x y-> ExprExprStmt $ SEUnOp x y) constr r) e ops

returnStmt :: Parser Statement
returnStmt = Return <$> (kword "return" *> optional expression <* semicolon)

-- TODO Add primitive types
typeIden :: Parser Name
typeIden = name
