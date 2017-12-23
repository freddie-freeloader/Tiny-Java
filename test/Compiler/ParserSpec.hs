module Compiler.ParserSpec (main, spec) where

import           Compiler.Ast
import           Compiler.Parser
import           Test.Hspec

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser" $ do
     it "parses an empty class" $ parseTestString "class Test {}"
       `shouldBe`
       Just [Class (Identifier "Test") [] []]
     it "parses an object class" $ parseTestString "class Objectclass {void objectMethod(){new Objectclass();}}"
       `shouldBe`
       Just [Class (Identifier "Objectclass") []
               [Method { getIdentifier = Identifier "objectMethod"
                       , getMods = []
                       , getReturnType = JVoid
                       , getParamList = []
                       , getBody = Just (
                           Block [StmtExprStmt
                                  (Instantiation
                                   Name { path = []
                                        , getIdentifier = Identifier "Objectclass"}
                                    []
                                  )])}]]
