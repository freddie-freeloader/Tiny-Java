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

cliParser :: Parser (Maybe T.FilePath, Bool, Bool, Bool)
cliParser = (\ a b c d -> (a,b,c,d)) <$>
  (optional (argPath "FilePath" "Path to .java-file") <|> pure Nothing)
  <*> switch "parse-only" 'p' "If this flag is set, the compiler will only type-check the code."
  <*> switch "type-only" 't' "If this flag is set, the compiler will only type-check the code."
  <*> switch "verbose" 'v' "If this flag is set, the final result will be output on stdout."

type Pipeline result = String -> String -> Either Error result
type SuccessHandler result = String -> Bool -> result -> IO ()

run :: IO ()
run =
  do
    (filePath,parserFlag,typeCheckFlag,verboseFlag) <- options "Tiny Java Compiler" cliParser
    input <- getInput filePath
    let inputName = fromMaybe "StdIn" $ show <$> filePath
    if parserFlag
      then run untilParser (handleSuccess "parsed") inputName input verboseFlag
      else (if typeCheckFlag
               then run untilTypecheck (handleSuccess "type-checked") inputName input verboseFlag
               else run runFullPipeline handleSuccessAndWrite inputName input verboseFlag)
  where
    run :: Pipeline a -> SuccessHandler a -> String -> T.Text -> Bool -> IO ()
    run pipeline successHandler inputName input verboseFlag =
      do
        let compilationResult = pipeline inputName (Text.unpack input)
        either printError (successHandler inputName verboseFlag) compilationResult

printError :: Error -> IO ()
printError = eprintf (errorTextLeft%w%errorTextRight)
  where
    errorTextLeft = "=== Compile Error ===\n\nThe reason the compilation failed:\n~~~\n"
    errorTextRight = "\n~~~\n"

type ClassFileName = String
type Result = [(ClassFileName,ClassFile)]

handleSuccess :: Show result => String -> SuccessHandler [(String,result)]
handleSuccess jobName inputName verboseFlag results = do
  printSuccessMessage inputName jobName
  if verboseFlag
    then mapM_ (\(name,res) -> printf("\nClass Name: "%w%"\nByte-code:\n"%w%"\n") name res) results
    else return ()

handleSuccessAndWrite :: SuccessHandler Result
handleSuccessAndWrite inputName verboseFlag results = do
  mapM_ (uncurry generateByteFile) results
  printSuccessMessage inputName "compiled"
  if verboseFlag
    then mapM_ (\(name,res) -> printf("\nClass Name: "%w%"\nByte-code:\n"%w%"\n") name res) results
    else return ()

printSuccessMessage :: String -> String ->  IO ()
printSuccessMessage inputName jobName = printf ("=== Success ===\n\n~~~\n"%s%" was successfully "%s%"!\n~~~\n") (Text.pack inputName) (Text.pack jobName)

untilParser :: Pipeline [(String,Class)]
untilParser filePath input =
  do
    nonEmptyInput <- if null input
                       then Left $ ParseError "The input appears to be empty."
                       else return input
    parseResult <- parseSrc filePath  nonEmptyInput
    return $ zip (map getClassName parseResult) parseResult
  where
    getClassName :: Class -> ClassFileName
    getClassName (Class (Identifier className) _ _ ) = className ++ ".class"
    getClassName _ = error "Partial function"

untilTypecheck :: Pipeline [(String,Class)]
untilTypecheck filePath input =
  do
    parseResult <- untilParser filePath input
    typeCheckResult <- typecheck (map snd parseResult)
    return $ zip (map fst parseResult) typeCheckResult

runFullPipeline :: Pipeline Result
runFullPipeline filePath input =
  do
    typeCheckResult <- untilTypecheck filePath input
    compilationResult <- mapM (compileByteCode . snd) typeCheckResult
    return $ zip (map fst typeCheckResult) compilationResult
  where
    compileByteCode :: Class -> Either Error ClassFile
    compileByteCode = const $ Left $ InternalError "Byte-code-generation failed!"

getInput :: Maybe T.FilePath -> IO T.Text
getInput (Just filePath) = readTextFile filePath
getInput Nothing         = T.linesToText <$> unfoldM readline
