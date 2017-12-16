-- |
-- Module      :  Compiler.Ast
--
-- This module contains the AST

{-# LANGUAGE DuplicateRecordFields #-}

module Compiler.Ast where

data Class = Class Identifier [Mod] [Decl]
  deriving (Show,Eq)

newtype Identifier = Identifier String
  deriving (Show,Eq)


-- TODO What are Types?
data Name = Name { path :: [Identifier]
                 , getIdentifier :: Identifier }
  deriving (Show, Eq)

voidType :: Name
voidType = Name [] $ Identifier "void"

-- TODO Add primitive types here
type Type = Name

-- | 'VarDecl' is used for field definitions and local variable declarations
data VarDecl = VarDecl { getIdentifier :: Identifier
                       , getMods :: [Mod]
                       , getType :: Type
                       , getRHS :: (Maybe Expression)}
  deriving (Show, Eq)

-- TODO Add some more named fields
data Expression = TernaryIf Expression Expression Expression
                | PrimBinOp BinOp Expression Expression -- ^ Primitive binary Operation
                | PrimUnOp UnOp Expression -- ^ Primitive unary Operation
                | This -- ^ this keyword
                | Iden Name
                | Select Expression Identifier
                | Literal Lit
                | ExprExprStmt StmtExpr
  deriving (Show, Eq)

data Statement = While { getCond :: Expression
                       , getBody :: Maybe Statement }
               | If Expression (Maybe Statement) (Maybe Statement)
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

-- TODO Add some more named fields
data Decl = Field VarDecl
            -- TODO Maybe switch Mods and Type
          | Constructor
          | Method { getIdentifier :: Identifier
                   , getMods :: [Mod]
                   , getReturnType :: Type
                   , getParamList :: [(Type, Identifier)]
                   , getBody :: Maybe Statement}
  deriving (Show, Eq)

data Mod = Public | Protected | Private | Static | Abstract
  deriving (Show, Eq)
