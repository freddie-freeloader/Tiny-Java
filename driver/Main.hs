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

cliParser :: Parser (Maybe T.FilePath, Bool, Bool)
cliParser = (\ a b c -> (a,b,c)) <$>
  (optional (argPath "FilePath" "Path to .java-file") <|> pure Nothing)
  <*> switch "type-only" 't' "If this flag is set, the compiler will only type-check the code."
  <*> switch "verbose" 'v' "If this flag is set, the final result will be output on stdout."

run :: IO ()
run =
  do
    (filePath,typeCheckFlag, verboseFlag) <- options "Tiny Java Compiler" cliParser
    input <- getInput filePath
    let inputName = fromMaybe "StdIn" $ show <$> filePath
    if typeCheckFlag
      then parseAndTypecheck inputName input verboseFlag
      else fullCompilation inputName input verboseFlag
  where
    fullCompilation :: String -> T.Text -> Bool -> IO ()
    fullCompilation inputName input verboseFlag =
      do
        let compilationResult = runFullPipeline inputName (Text.unpack input)
        either printError (handleSuccess inputName verboseFlag) compilationResult
    parseAndTypecheck :: String -> T.Text -> Bool -> IO ()
    parseAndTypecheck inputName input verboseFlag =
      do
        let compilationResult = runUntilTypecheck inputName (Text.unpack input)
        either printError (handleTypeCheckSuccess inputName verboseFlag) compilationResult
        -- if verboseFlag then printf ("Typed AST"%w%) compilationResult else return ()

printError :: Error -> IO ()
printError = eprintf (errorTextLeft%w%errorTextRight)
  where
    errorTextLeft = "=== Compile Error ===\n\nThe reason the compilation failed:\n~~~\n"
    errorTextRight = "\n~~~\n"

type ClassFileName = String
type Result = [(ClassFileName,ClassFile)]

handleTypeCheckSuccess :: String -> Bool -> [(String,Class)] -> IO ()
handleTypeCheckSuccess inputName verboseFlag results = do
  printSuccessMessage inputName "type-checked"
  if verboseFlag
    then mapM_ (\(name,res) -> printf("\nClass Name: "%w%"\nByte-code:\n"%w%"\n") name res) results
    else return ()

handleSuccess :: String -> Bool -> Result -> IO ()
handleSuccess inputName verboseFlag results = do
  mapM_ (uncurry generateByteFile) results
  printSuccessMessage inputName "compiled"
  if verboseFlag
    then mapM_ (\(name,res) -> printf("\nClass Name: "%w%"\nByte-code:\n"%w%"\n") name res) results
    else return ()

printSuccessMessage :: String -> String ->  IO ()
printSuccessMessage inputName jobName = printf ("=== Success ===\n\n~~~\n"%s%" was successfully "%s%"!\n~~~\n") (Text.pack inputName) (Text.pack jobName)

runUntilTypecheck :: String -> String -> Either Error [(String,Class)]
runUntilTypecheck filePath input =
  do
    nonEmptyInput <- if null input
                       then Left $ ParseError "The input appears to be empty."
                       else return input
    parseResult <- parseSrc filePath  nonEmptyInput
    typeCheckResult <- typecheck parseResult
    return $ zip (map getClassName typeCheckResult) typeCheckResult
  where
    getClassName :: Class -> ClassFileName
    getClassName (Class (Identifier className) _ _ ) = className ++ ".class"
    getClassName _ = error "Partial function"

runFullPipeline :: String -> String -> Either Error Result
runFullPipeline filePath input =
  do
    typeCheckResult <- runUntilTypecheck filePath input
    compilationResult <- mapM (compileByteCode . snd) typeCheckResult
    return $ zip (map fst typeCheckResult) compilationResult
  where
    compileByteCode :: Class -> Either Error ClassFile
    compileByteCode = const $ Left $ InternalError "Byte-code-generation failed!"

getInput :: Maybe T.FilePath -> IO T.Text
getInput (Just filePath) = readTextFile filePath
getInput Nothing         = T.linesToText <$> unfoldM readline
