-- |
-- Module      :  Compiler.Ast
--
-- This module contains the AST

module Compiler.Ast where

-- | 'Identifier' is a simple identifier, e.g. for a parameter
data Identifier = Identifier String
                | This
                | Super
  deriving (Show,Eq)

-- | 'Name' represents a Name of something with the relative path to it
data Name = Name { path          :: [Identifier]
                 , getIdentifier :: Identifier }
  deriving (Show, Eq)

-- | 'Type' are different types of types
data Type = PrimType PType -- ^ Represents a primitive java type
          | RefType Name -- ^ Represents a reference type
          | JVoid -- ^ Represents the void type in java
  deriving (Show, Eq)

-- | 'PType' represent the primitive types
data PType = Boolean
           | Int
           | Char
  deriving (Show, Eq)

-- | 'Mod' are the different Modifiers in Java
data Mod = Public | Protected | Private | Static | Abstract
  deriving (Show, Eq)

-- | 'Class' is a java class definition
data Class = Class Identifier [Mod] [Decl]
  deriving (Show,Eq)

-- | 'Decl' are the different types of declaration inside a class
data Decl = Field VarDecl
          | Constructor { getIdentifier :: Identifier
                        , getMods       :: [Mod]
                        , getParamList  :: [(Type, Identifier)]
                        , getBody       :: Maybe Statement}
          | Method { getIdentifier :: Identifier
                   , getMods       :: [Mod]
                   , getReturnType :: Type
                   , getParamList  :: [(Type, Identifier)]
                   , getBody       :: Maybe Statement}
  deriving (Show, Eq)

-- | 'VarDecl' is used for field definitions and local variable declarations
data VarDecl = VarDecl { getIdentifier :: Identifier
                       , getMods       :: [Mod]
                       , getType       :: Type
                       , getRHS        :: Maybe Expression}
  deriving (Show, Eq)

-- | 'Expression' is something that can be evaluated to a value
data Expression = TernaryIf { getCond     :: Expression
                            , getElseStmt :: Expression
                            , getThenStmt :: Expression
                            } -- ^ Short notation if, e.g. @someBool? 42 : 1337@
                | PrimBinOp BinOp Expression Expression -- ^ Primitive binary Operation
                | PrimUnOp UnOp Expression -- ^ Primitive unary Operation
                | Iden Name -- ^ A variable
                | Select Expression Identifier
                | Literal Lit -- ^ All kind of literals
                | ExprExprStmt StmtExpr -- ^ A StatementExpression that is in an Expression position
                | Cast Type Expression
                | TypedExpression(Expression, Type)
  deriving (Show, Eq)

-- | 'Statement' is dual to 'Expression' since it does not evaluate to a value
data Statement = While { getCond :: Expression
                       , getBody :: Maybe Statement }
               | If { getCond     :: Expression
                    , getThenStmt :: Maybe Statement
                    , getElseStmt :: Maybe Statement }
               | Block [Statement]
               | Return (Maybe Expression)
               | LocalVar VarDecl
               | StmtExprStmt StmtExpr
               | TypedStatement(Statement, Type)
               | Continue
               | Break
  deriving (Show, Eq)

-- | 'StmtExpr' can be a 'Statement' as well as an 'Expression'
data StmtExpr = Assign AssignOp Name Expression
              | Instantiation Name [Expression] -- ^ Using new
              | Apply Expression [Expression]
              | SEUnOp IncrOrDecr Expression -- ^ UnOp that returns something and has a side effect
              | TypedStmtExpr(StmtExpr, Type)
  deriving (Show, Eq)

-- | 'Lit' are the Java literals
data Lit = IntegerL Integer
         | BooleanL Bool
         | CharL Char
         | StringL String
         | Null
  deriving (Show, Eq)

-- | 'AssignOp' represent different ways to assign a value
data AssignOp = NormalAssign
              | MultiplyAssign
              | DivideAssign
              | ModuloAssign
              | PlusAssign
              | MinusAssign
              | ShiftLeftAssign
              | SignedShiftRightAssign
              | USignedShiftRightAssign
              | AndAssign
              | XOrAssign
              | OrAssign
  deriving (Show, Eq)

-- | 'BinOp' are all primitive binary operations
data BinOp = And
           | Or
           | XOr
           | Eq
           | Less
           | LessEq
           | Greater
           | GreaterEq
           | InstanceOf
           | Multiply
           | Divide
           | Add
           | Subtract
           | Modulo
  deriving (Show, Eq)

-- | 'UnOp' are all primitive unary operations
data UnOp = Not
          | Neg
          | BitCompl -- ^ Tilde-Operator performs a bitwise complement
  deriving (Show, Eq)

-- | 'IncrOrDecr' represent different ways of increment/decrement a field in an effectful way
data IncrOrDecr = PreIncr
                | PostIncr
                | PreDecr
                | PostDecr
  deriving (Show, Eq)
