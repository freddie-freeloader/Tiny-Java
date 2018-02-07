-- |
-- Module      :  Compiler.Utils
--
-- This module contains utils that are shared by all compiler phases

module Compiler.Utils where

-- | 'Error' represents the different kinds of errors
data Error
  = ParseError String -- ^ 'ParseError' is created by the parser if the input in not syntactically correct
  | SemanticError String -- ^ 'SemanticError' is created by the semantic checker and indicates a semantic or type error
  | InternalError String -- ^ 'InternalError' can be created by any phase and indicates an internal error of the compiler
  deriving (Show)
