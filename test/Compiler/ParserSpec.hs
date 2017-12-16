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
  describe "program" $ do
     it "parses an empty class" $ do
       (parseTestString "class Test {}") `shouldBe` Just [(Class (Identifier "Test") [] [])]
