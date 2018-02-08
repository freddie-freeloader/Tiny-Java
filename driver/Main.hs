{-# LANGUAGE OverloadedStrings #-}

import           Compiler.AbstractBytecode                     (ClassFile)
import           Compiler.Ast                                  (Class (..),
                                                                Identifier (..))
import           Compiler.BytecodeGeneration.ByteFileGenerator (generateByteFile)
import           Compiler.Parser                               (parseSrc)
import           Compiler.Type_Check                           (typecheck)
import           Compiler.Utils                                (Error (..))
import           Control.Applicative                           (optional, (<|>))
import           Control.Monad.Loops                           (unfoldM)
import           Data.Maybe                                    (fromMaybe)
import qualified Data.Text                                     as Text (pack,
                                                                        unpack)
import qualified Turtle                                        as T (FilePath,
                                                                     Text,
                                                                     linesToText)
import           Turtle.Format
import           Turtle.Options
import           Turtle.Prelude


main :: IO ()
main = run

cliParser :: Parser (Maybe T.FilePath, Bool)
cliParser = (,) <$>
  (optional (argPath "FilePath" "Path to .java-file") <|> pure Nothing)
  <*> switch "type-only" 't' "If this flag is set, the compiler will only type-check the code."

run :: IO ()
run =
  do
    (filePath,typeCheckFlag) <- options "Tiny Java Compiler" cliParser
    input <- getInput filePath
    let inputName = fromMaybe "StdIn" $ show <$> filePath
    if typeCheckFlag
      then parseAndTypecheck inputName input
      else fullCompilation inputName input
  where
    fullCompilation :: String -> T.Text -> IO ()
    fullCompilation inputName input =
      do
        let compilationResult = runFullPipeline inputName (Text.unpack input)
        either printError (handleSuccess inputName) compilationResult
    parseAndTypecheck :: String -> T.Text -> IO ()
    parseAndTypecheck inputName input =
      do
        let compilationResult = runUntilTypecheck inputName (Text.unpack input)
        either printError (const $ printSuccessMessage inputName "type-checked") compilationResult

printError :: Error -> IO ()
printError = eprintf (errorTextLeft%w%errorTextRight)
  where
    errorTextLeft = "=== Compile Error ===\n\nThe reason the compilation failed:\n~~~\n"
    errorTextRight = "\n~~~\n"

type ClassFileName = String
type Result = [(ClassFileName,ClassFile)]

handleSuccess :: String -> Result -> IO ()
handleSuccess inputName results = do
  mapM_ (uncurry generateByteFile) results
  printSuccessMessage inputName "compiled"

printSuccessMessage :: String -> String ->  IO ()
printSuccessMessage inputName jobName = printf ("=== Success ===\n\n~~~\n"%s%" was successfully "%s%"!\n~~~\n") (Text.pack inputName) (Text.pack jobName)

runUntilTypecheck :: String -> String -> Either Error [Class]
runUntilTypecheck filePath input =
  do
    nonEmptyInput <- if null input
                       then Left $ ParseError "The input appears to be empty."
                       else return input
    parseResult <- parseSrc filePath  nonEmptyInput
    typecheck parseResult

runFullPipeline :: String -> String -> Either Error Result
runFullPipeline filePath input =
  do
    typeCheckResult <- runUntilTypecheck filePath input
    compilationResult <- mapM compileByteCode typeCheckResult
    return $ zip (map getClassName typeCheckResult) compilationResult
  where
    compileByteCode :: Class -> Either Error ClassFile
    compileByteCode = const $ Left $ InternalError "Byte-code-generation failed!"
    getClassName :: Class -> ClassFileName
    getClassName (Class (Identifier className) _ _ ) = className ++ ".class"
    getClassName _ = error "Partial function"

getInput :: Maybe T.FilePath -> IO T.Text
getInput (Just filePath) = readTextFile filePath
getInput Nothing         = T.linesToText <$> unfoldM readline
