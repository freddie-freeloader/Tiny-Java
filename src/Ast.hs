module Ast where

newtype Identifier = Identifier String
  deriving Show

data Class = Class Identifier [Decl]
  deriving Show

-- | What are Types?
data Type = Type String
  deriving Show

data Expression = If Expression Expression Expression
                | Assign
                | PrimBinOp BinOp Expression Expression -- | Primitive binary Operation
                | PrimUnOp UnOp Expression -- | Primitive unary Operation
  deriving Show

data BinOp = And
           | Or
           | XOr
           | Eq
  deriving Show

data UnOp = Not
  deriving Show

data Decl = Field Identifier [Mod] Type (Maybe Expression) -- TODO Maybe switch Mods and Type
          | Method
          | Constructor
   deriving Show

data Mod = Public | Protected | Private | Static | Abstract
  deriving Show

-- data BExpr
--   = BoolConst Bool
--   | Not BExpr
--   | BBinary BBinOp BExpr BExpr
--   | RBinary RBinOp AExpr AExpr
--   deriving (Show)

-- Data BBinOp
--   = And
--   | Or
--   deriving (Show)

-- data RBinOp
--   = Greater
--   | Less
--   deriving (Show)

-- data AExpr
--   = Val Identifier
--   | IntConst Integer
--   | Neg AExpr
--   | ABinary ABinOp AExpr AExpr
--   deriving (Show)

-- data ABinOp
--   = Add
--   | Subtract
--   | Multiply
--   | Divide
--   deriving (Show)

-- data Stmt
--   = Assign Identifier AExpr
--   | If BExpr Stmt Stmt
--   | While BExpr Stmt
--   | Skip
--   deriving (Show)
