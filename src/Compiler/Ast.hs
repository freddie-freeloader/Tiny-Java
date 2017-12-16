-- |
-- Module      :  Compiler.Ast
--
-- This module contains the AST

{-# LANGUAGE DuplicateRecordFields #-}

module Compiler.Ast where

newtype Identifier = Identifier String
  deriving (Show,Eq)

data Name = Name { path          :: [Identifier]
                 , getIdentifier :: Identifier }
  deriving (Show, Eq)

-- | 'Class' is a java class definition
data Class = Class Identifier [Mod] [Decl]
  deriving (Show,Eq)

-- | 'Decl' are the different types of declaration inside a class
data Decl = Field VarDecl
          | Constructor
          | Method { getIdentifier :: Identifier
                   , getMods       :: [Mod]
                   , getReturnType :: Type
                   , getParamList  :: [(Type, Identifier)]
                   , getBody       :: Maybe Statement}
  deriving (Show, Eq)

-- TODO Should this be a type or name?
voidType :: Name
voidType = Name [] $ Identifier "void"

-- TODO Add primitive types here
type Type = Name

-- | 'VarDecl' is used for field definitions and local variable declarations
data VarDecl = VarDecl { getIdentifier :: Identifier
                       , getMods       :: [Mod]
                       , getType       :: Type
                       , getRHS        :: Maybe Expression}
  deriving (Show, Eq)

-- TODO Add some more named fields
-- | 'Expression' is something that can be evaluated to a value
data Expression = TernaryIf Expression Expression Expression -- ^ Short notation if, e.g. @someBool? 42 : 1337@
                | PrimBinOp BinOp Expression Expression -- ^ Primitive binary Operation
                | PrimUnOp UnOp Expression -- ^ Primitive unary Operation
                | This -- ^ this keyword
                | Iden Name -- ^ A variable
                | Select Expression Identifier
                | Literal Lit -- ^ All kind of literals
                | ExprExprStmt StmtExpr -- ^ A StatementExpression that is in an Expression position
  deriving (Show, Eq)

data Statement = While { getCond :: Expression
                       , getBody :: Maybe Statement }
               | If { getCond     :: Expression
                    , getThenStmt :: Maybe Statement
                    , getElseStmt :: Maybe Statement }
               | Block [Statement]
               | Return (Maybe Expression)
               | LocalVar VarDecl
               | StmtExprStmt StmtExpr
  deriving (Show, Eq)

data StmtExpr = Assign AssignOp Name Expression
              | Instantiation Name [Expression] -- ^ Using new
              | Apply Expression [Expression]
              | SEUnOp IncrOrDecr Expression -- ^ UnOp that returns something and has a side effect
  deriving (Show, Eq)

data Lit = IntegerL Integer
         | BooleanL Bool
         | CharL Char
         | StringL String
         | Null
  deriving (Show, Eq)

-- TODO Should we desugar those?
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

data IncrOrDecr = PreIncr
                | PostIncr
                | PreDecr
                | PostDecr
  deriving (Show, Eq)

data UnOp = Not
          | Neg
          | BitCompl -- ^ Tilde-Operator performs a bitwise complement
  deriving (Show, Eq)

data Mod = Public | Protected | Private | Static | Abstract
  deriving (Show, Eq)
