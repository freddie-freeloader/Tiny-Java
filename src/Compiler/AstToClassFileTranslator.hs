module Compiler.AstToClassFileTranslator where

import Compiler.Ast
import Compiler.Utils
import Compiler.AstToClassFileTranslator.GenerateAbstractClassFile
import Compiler.BytecodeGeneration.ByteFileGenerator


generateClassFileByte :: Either Error [Class] -> [IO ()]
generateClassFileByte (Right classes) = map (\c -> byteCode c) classes
  where byteCode class_@(Compiler.Ast.Class (Identifier name) _ _) = generateByteFile (show name ++ ".class") (translateToAbstractClassFile class_)
