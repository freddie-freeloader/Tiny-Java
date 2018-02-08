{-# LANGUAGE OverloadedStrings #-}

import Compiler.Ast (Class)
import           Compiler.Parser        (parseSrc)
import           Compiler.Utils         (Error (..))
import           Control.Applicative    ((<|>),optional)
import Compiler.Type_Check (typecheck)
import           Control.Monad.Loops    (unfoldM)
import qualified Data.Text              as Text (pack, unpack)
import qualified Turtle                 as T (FilePath, Text,linesToText)
import           Turtle.Format
import           Turtle.Options
import           Turtle.Prelude
import Data.Maybe (fromMaybe)


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
    either printError (printSuccess inputName) (runPipeline inputName (Text.unpack input) typeCheckFlag)

printError :: Error -> IO ()
printError = eprintf (errorTextLeft%w%errorTextRight)
  where
    errorTextLeft = "=== Compile Error ===\n\nThe reason the compilation failed:\n~~~\n"
    errorTextRight = "\n~~~\n"

type Result = [Class]

printSuccess :: String -> Result -> IO ()
printSuccess = const . printf ("== Success ==\n"%s%" was successfully compiled!\n") . Text.pack

runPipeline :: String -> String -> Bool -> Either Error Result
runPipeline filePath input typeCheckFlag =
  do
    nonEmptyInput <- if null input
                       then Left $ ParseError "The input appears to be empty."
                       else return input
    parseResult <- parseSrc filePath  nonEmptyInput
    typeCheckResult <- typecheck parseResult
    return typeCheckResult

getInput :: Maybe T.FilePath -> IO T.Text
getInput (Just filePath) = readTextFile filePath
getInput Nothing         = T.linesToText <$> unfoldM readline
