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

-- | This instance is needed for a prettier output in the case the compiler fails with an error
instance Show Error where
  show (ParseError s) = s
  show (SemanticError s) = "A semantic error was found:\n" ++ s
  show (InternalError s) = "An internal error was encountered:\n" ++ s
