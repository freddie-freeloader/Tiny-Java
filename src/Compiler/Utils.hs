-- | -- Module      :  Compiler.Utils
--
-- This module contains utils that are shared by all compiler phases

module Compiler.Utils where

-- | 'Error' represents the different kinds of errors
data Error
  = ParseError String
  | SemanticError String
  | InternalError String
