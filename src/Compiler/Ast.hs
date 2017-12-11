-- |
-- Module      :  Compiler.Ast
--
-- This module contains the AST

{-# LANGUAGE DuplicateRecordFields #-}

module Compiler.Ast where

data Class = Class Name [Mod] [Decl]
  deriving (Show,Eq)

newtype Name = Name String
  deriving (Show,Eq)


-- TODO What are Types?
data Identifier = Identifier { path :: [Name]
                             , getName :: Name }
  deriving (Show, Eq)

voidType :: Identifier
voidType = Identifier [] $ Name "void"

-- TODO Add primitive types here
type Type = Identifier

-- | 'VarDecl' is used for field definitions and local variable declarations
data VarDecl = VarDecl { getName :: Name
                       , getMods :: [Mod]
                       , getType :: Type
                       , getRHS :: (Maybe Expression)}
  deriving (Show, Eq)

-- TODO Add some more named fields
data Expression = TernaryIf Expression Expression Expression
                | If Expression Expression (Maybe Expression)
                | While { getCond :: Expression, getBody :: Expression }
                | Assign AssignOp Identifier Expression
                | PrimBinOp BinOp Expression Expression -- ^ Primitive binary Operation
                | PrimUnOp UnOp Expression -- ^ Primitive unary Operation
                | This -- ^ this keyword
                | Instantiation Identifier [Expression] -- ^ Using new
                | Iden Identifier
                | Select Expression Name
                | Apply Expression [Expression]
                | Literal Lit
                | LocalVar VarDecl
                | Block [Expression]
                | EmptyStmt -- TODO this should not be used
                | Return (Maybe Expression)
  deriving (Show, Eq)

data Lit = IntegerL Integer
         | BooleanL Bool
         | CharL Char
         | StringL String
         | Null
  deriving (Show, Eq)

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

data UnOp = Not
          | Neg
          | PreIncr
          | PostIncr
          | PreDecr
          | PostDecr
          | BitCompl -- ^ Tilde-Operator performs a bitwise complement
  deriving (Show, Eq)

-- TODO Add some more named fields
data Decl = Field VarDecl
            -- TODO Maybe switch Mods and Type
          | Constructor
          | Method { getName :: Name
                   , getMods :: [Mod]
                   , getReturnType :: Type
                   , getParamList :: [(Type, Name)]
                   , getBody :: Maybe Expression}
  deriving (Show, Eq)

data Mod = Public | Protected | Private | Static | Abstract
  deriving (Show, Eq)
