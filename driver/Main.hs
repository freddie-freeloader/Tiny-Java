{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Compiler.AbstractBytecode                                   (ClassFile)
import           Compiler.Ast                                                (Class (..), Identifier (..))
import           Compiler.AstToClassFileTranslator.GenerateAbstractClassFile (translateToAbstractClassFile)
import           Compiler.BytecodeGeneration.ByteFileGenerator               (generateByteFile)
import           Compiler.Parser                                             (parseSrc)
import           Compiler.Type_Check                                         (typecheck)
import           Compiler.Utils                                              (Error (..))
import           Control.Arrow                                               (first)
import           Control.Applicative                                         (optional, (<|>))
import           Control.Monad                                               (when, liftM2)
import           Control.Monad.Loops                                         (unfoldM)
import           Data.Maybe                                                  (fromMaybe)
import qualified Data.Text as Text                                           (pack, unpack)
import           Control.Monad.Except                                        (MonadError,throwError)
import qualified Turtle as T                                                 (FilePath, Text, linesToText)
import           Turtle.Format                                               (eprintf,printf,w,s,(%))
import           Turtle.Options                                              (switch,Parser,argPath,options)
import qualified Turtle.Prelude as T                                         (readTextFile,readline)
import           Control.Monad.Reader                                        (MonadReader,MonadIO,ask,runReaderT,liftIO)

main :: IO ()
main = run

cliParser :: Parser (Maybe T.FilePath, Bool, Bool, Bool)
cliParser = (\ a b c d -> (a,b,c,d)) <$>
  (optional (argPath "FilePath" "Path to .java-file") <|> pure Nothing)
  <*> switch "parse-only" 'p' "If this flag is set, the compiler will only parse the code."
  <*> switch "type-only" 't' "If this flag is set, the compiler will only type-check the code."
  <*> switch "verbose" 'v' "If this flag is set, the final result will be output on stdout."

-- Basic types
newtype InputName = InputName String
newtype ClassName = ClassName { fileName :: String }
newtype RawInput = RawInput T.Text

-- Pipeline
type PipelineMonad a = forall m . (MonadReader (InputName, RawInput) m, MonadError Error m) => m a
type Pipeline result = PipelineMonad [(ClassName,result)]

-- Handler for successful compilation
data HandlerEnv result = HandlerEnv [(ClassName,result)] InputName Bool
type SuccessHandler result = forall m . (MonadReader (HandlerEnv result) m, MonadIO m) => m ()


run :: forall io . MonadIO io => io ()
run =
  do
    (filePath, parserFlag, typeCheckFlag,verboseFlag) <- options programInfo cliParser
    input <- getInput filePath
    let inputName = InputName <$> fromMaybe "StdIn" $ show <$> filePath
    if | parserFlag    -> run untilParser    (printSuccess "parsed") inputName input verboseFlag
       | typeCheckFlag -> run untilTypecheck (printSuccess "type-checked") inputName input verboseFlag
       | otherwise     -> run fullPipeline   (do printSuccess "fully compiled"; writeToFile) inputName input verboseFlag
  where

    programInfo = "Tiny Java Compiler\n\nA compiler for a subset of java"

    getInput (Just filePath) = liftIO $ RawInput <$> T.readTextFile filePath
    getInput Nothing         = RawInput . T.linesToText <$> unfoldM T.readline

    run :: Pipeline a -> SuccessHandler a -> InputName -> RawInput -> Bool -> io ()
    run pipeline successHandler inputName input verboseFlag =
      do
        let compilationResult = runReaderT pipeline (inputName, input)
        either printError (\res -> runReaderT successHandler (HandlerEnv res inputName verboseFlag)) compilationResult

    printError = eprintf (errorTextLeft%w%errorTextRight)

    errorTextLeft = "=== Compile Error ===\n\nThe reason the compilation failed:\n~~~\n"
    errorTextRight = "\n~~~\n"


printSuccess :: Show result => String -> SuccessHandler result
printSuccess jobName =
  do
    (HandlerEnv results (InputName inputName) verboseFlag) <- ask
    printSuccessMessage (Text.pack inputName) (Text.pack jobName)
    when verboseFlag $
      mapM_ (printRawTuple . first fileName)  results
  where
    printSuccessMessage = printf ("=== Success ===\n\n~~~\n"%s%" was successfully "%s%"!\n~~~\n")
    printRawTuple = uncurry $ printf ("\n=======\nThe raw result:\n\nClass Name: "%w%"\nAST:\n"%w%"\n")

writeToFile :: SuccessHandler ClassFile
writeToFile =
  do
    (HandlerEnv results _ verboseFlag) <- ask
    liftIO $ mapM_ (uncurry generateByteFile . first (addClassSuffix . fileName)) results
    when verboseFlag $
      printf "\nWrote to file\n"
  where
    addClassSuffix = flip (++) ".class"

-- Pipelines

untilParser :: Pipeline Class
untilParser  = fmap zipWithClassName $ (fromEither . parse) =<< ask
  where
    parse (InputName inputName, RawInput input) = parseSrc inputName (Text.unpack input)

    zipWithClassName xs = zip (map getClassName xs) xs

    getClassName (Class (Identifier className) _ _ ) = ClassName className
    getClassName _ = error "Internal error: Class name should not be this or super"

untilTypecheck ::  Pipeline Class
untilTypecheck = untilParser >>= applySndAndZip (fromEither . typecheck)

fullPipeline :: Pipeline ClassFile
fullPipeline = untilTypecheck >>= mapM (mapM compileByteCode)
  where
    -- As long as 'translateToAbstractClassFile' is not exposing an
    -- internal error (this would be sane!), we wrap its output in pure
    compileByteCode = pure . translateToAbstractClassFile



-- Utilities

fromEither :: MonadError e m => Either e a -> m a
fromEither = either throwError pure

-- This looks like it could use some lens magic
applySndAndZip :: (Monad m, Monad m2)  => (m b -> m2 (m c)) -> m (a, b) -> m2 (m (a, c))
applySndAndZip f xs = f (fmap snd xs) >>= pure . liftM2 (,) (fmap fst xs)
