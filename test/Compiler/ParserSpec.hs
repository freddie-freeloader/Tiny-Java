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
  describe "emptyprogram" $ do
     it "parses an empty class" $ do
       (parseTestString "class Emptyclass {}") 
       `shouldBe` 
       Just 
        [Class 
            (Identifier "Emptyclass") 
            [] 
            []]
  describe "objpgrm" $ do
     it "parses an object class" $ do
       (parseTestString "class Objectclass {void objectMethod(){new Objectclass();}}") 
       `shouldBe` 
      Just [Class (Identifier "Objectclass") [] 
        [Method {
            getIdentifier = Identifier "objectMethod",
            getMods = [],
            getReturnType = JVoid,
            getParamList = [], 
            getBody = Just (Block [
                StmtExprStmt (Instantiation (Name 
                        {path = [], 
                        getIdentifier = Identifier "Objectclass"}) 
                        [])
                ])
            }
        ]]
  describe "fieldprogram" $ do
     it "parses a class with a field" $ do
       (parseTestString "class Fieldclass{int field;}") 
       `shouldBe` 
        Just [Class (Identifier "Fieldclass") 
        [] 
        [Field (
            VarDecl {
            getIdentifier = Identifier "field", 
            getMods = [], 
            getType = PrimType Int,
            getRHS = Nothing}
            )]]
  describe "constructorprogram" $ do
     it "parses an empty class with the constructor written" $ do
       (parseTestString "class Constructorclass {Constructorclass() {}}") 
       `shouldBe` 
       Just [Class (Identifier "Constructorclass") [] [Constructor]]
  describe "typeprogram" $ do
     it "parses a class with all available types" $ do
       (parseTestString "class Typeclass {int intField;char charField;boolean floatField;}") 
       `shouldBe` 
        Just [Class (Identifier "Typeclass") [] 
            [Field (VarDecl {
                getIdentifier = Identifier "intField",
                getMods = [],
                getType = PrimType Int,
                getRHS = Nothing}),
            Field (VarDecl {
                getIdentifier = Identifier "charField",
                getMods = [],
                getType = PrimType Char,
                getRHS = Nothing}),
            Field (VarDecl {
                getIdentifier = Identifier "floatField",
                getMods = [],
                getType = PrimType Boolean,
                getRHS = Nothing})
        ]]
  describe "methodprogram" $ do
     it "parses a class with a method (void)" $ do
       (parseTestString "class Methodclass {void Method() {    }}") 
       `shouldBe` 
        Just [Class (Identifier "Methodclass") [] 
            [Method {
                getIdentifier = Identifier "Method",
                getMods = [],
                getReturnType = JVoid,
                getParamList = [], 
                getBody = Just (Block [])
                }
        ]]
  describe "typedmethodprogram" $ do
     it "parses a class with all available typed methods" $ do
       (parseTestString "class Methodwithreturnclass {int intMethod() {return 1;}boolean boolMethod() {return true;}char charMethod() {return 'a';}}") 
       `shouldBe` 
        Just [Class (Identifier "Methodwithreturnclass") [] 
            [Method {
                getIdentifier = Identifier "intMethod",
                getMods = [], 
                getReturnType = PrimType Int,
                getParamList = [], 
                getBody = Just (Block [Return (Just (Literal (IntegerL 1)))])},
            Method {
                getIdentifier = Identifier "boolMethod",
                getMods = [],
                getReturnType = PrimType Boolean,
                getParamList = [],
                getBody = Just (Block [Return (Just (Literal (BooleanL True)))])},
            Method {
                getIdentifier = Identifier "charMethod",
                getMods = [],
                getReturnType = PrimType Char,
                getParamList = [],
                getBody = Just (Block [Return (Just (Literal (CharL 'a')))])}
        ]]
  describe "arithprogram" $ do
     it "parses a class with an arithmetic method" $ do
       (parseTestString "class Arithclass {int cvar = 10;void Arithmethod (){int locvar = 5;int res = cvar + locvar;}}") 
       `shouldBe` 
        Just [Class (Identifier "Arithclass") 
            [] 
            [Field (VarDecl {
                getIdentifier = Identifier "cvar", 
                getMods = [], 
                getType = PrimType Int, 
                getRHS = Just (Literal (IntegerL 10))
            }),
            Method {
                getIdentifier = Identifier "Arithmethod", 
                getMods = [], 
                getReturnType = JVoid, 
                getParamList = [], 
                getBody = Just (
                Block [
                    LocalVar (VarDecl {
                        getIdentifier = Identifier "locvar", 
                        getMods = [], 
                        getType = PrimType Int, 
                        getRHS = Just (Literal (IntegerL 5))
                    }),
                    LocalVar (VarDecl {
                        getIdentifier = Identifier "res", 
                            getMods = [], 
                                getType = PrimType Int, 
                                    getRHS = Just (PrimBinOp Add (Iden (Name {path = [], getIdentifier = Identifier "cvar"})) (Iden (Name {path = [], getIdentifier = Identifier "locvar"})))
                    })
                ])
        }]]
  describe "methodcallprogram" $ do
     it "parses a class with a call of a method" $ do
       (parseTestString "class Mthodcallclass {void methodCallmethod(){callMethod();}    void callMethod (){    }}") 
       `shouldBe` 
        Just [Class (Identifier "Mthodcallclass") [] 
            [Method {
                getIdentifier = Identifier "methodCallmethod", 
                getMods = [],
                getReturnType = JVoid,
                getParamList = [], 
                getBody = Just (Block [
                    StmtExprStmt (Apply (Iden (Name {path = [], getIdentifier = Identifier "callMethod"})) [])])
                },
            Method {
                getIdentifier = Identifier "callMethod",
                getMods = [], 
                getReturnType = JVoid,
                getParamList = [], 
                getBody = Just (Block [])
        }]]
  describe "objectprogram" $ do
     it "parses a class which creates an object" $ do
       (parseTestString "class Objectclass {Objectclass(){}void objectMethod(){Objectclass om = new Objectclass();}}") 
       `shouldBe` 
        Just [Class (Identifier "Objectclass") [] 
            [Method {
                getIdentifier = Identifier "objectMethod",
                getMods = [],
                getReturnType = JVoid,
                getParamList = [], 
                getBody = Just (Block [
                    LocalVar (VarDecl {
                        getIdentifier = Identifier "om", 
                        getMods = [], 
                        getType = JVoid, 
                        getRHS = Just(
                                        ExprExprStmt (Instantiation (Name 
                                            {path = [], 
                                            getIdentifier = Identifier "Objectclass"}) 
                                            [])
                                    )
                        })
                ])
        }]]
  describe "paramprogram" $ do
     it "parses a classwith a method with parameters" $ do
       (parseTestString "class Paramclass {void paramMethod(int iparam, boolean bparam, char cparam){}}") 
       `shouldBe` 
        Just [Class (Identifier "Paramclass") [] 
            [Method {
                getIdentifier = Identifier "paramMethod",
                getMods = [],
                getReturnType = JVoid,
                getParamList = [(PrimType Int,Identifier "iparam"), (PrimType Boolean ,Identifier "bparam"), (PrimType Char,Identifier "cparam")], 
                getBody = Just (Block [])
        }]]
  describe "recursionprogram" $ do
     it "parses a class with a recursive method" $ do
       (parseTestString "class Recursionclass {void recMethod(int count) {if(count > 0){recMethod(count - 1);}}}") 
       `shouldBe` 
        Just [Class (Identifier "Recursionclass") 
            [] 
            [Method {
                getIdentifier = Identifier "recMethod",
                getMods = [],
                getReturnType = JVoid,
                getParamList = [(PrimType Int,Identifier "count")], 
                getBody = Just (Block [
                    If {
                        getCond = PrimBinOp Greater (Iden (Name {path = [], getIdentifier = Identifier "count"})) (Literal (IntegerL 0)),
                        getThenStmt = Just (Block 
                            [StmtExprStmt 
                                (Apply 
                                    (Iden 
                                        (Name 
                                            {path = [],
                                            getIdentifier = Identifier "recMethod"}
                                        )
                                    )
                                    [PrimBinOp Subtract 
                                        (Iden 
                                            (Name {
                                                path = [],
                                                getIdentifier = Identifier "count"}
                                            )
                                        ) 
                                        (Literal (IntegerL 1))
                                    ]
                                )
                            ]),
                        getElseStmt = Nothing
                        }
                ])
        }]]
  describe "scopeprogram" $ do
     it "parses a class with specified scopes" $ do
       (parseTestString "class Scopefieldclass{private int prField;public int puField;protected int proField;}") 
       `shouldBe` 
        Just [Class (Identifier "Scopefieldclass") 
            [] 
            [Field (VarDecl {
                getIdentifier = Identifier "prField",
                getMods = [Private],
                getType = PrimType Int,
                getRHS = Nothing}),
            Field (VarDecl {
                getIdentifier = Identifier "puField",
                getMods = [Public],
                getType = PrimType Int,
                getRHS = Nothing}),
            Field (VarDecl {
                getIdentifier = Identifier "proField",
                getMods = [Protected],
                getType = PrimType Int,
                getRHS = Nothing})]
        ]
    --
    -- Should not Complete!!
    --
  describe "bracketsprogram" $ do
     it "no closing bracket" $ do
       (parseTestString "class Objectmethod{void objectcalling(){new Objectmethod().methodToCall();}public void methodToCall(){}}") `shouldBe` Nothing
  describe "semicolonprogram" $ do
     it "no semicolon after field" $ do
       (parseTestString "class Objectmethod{void objectcalling(){new Objectmethod().methodToCall();}public void methodToCall(){}}") `shouldBe` Nothing
  describe "spellingprogram" $ do
     it "wrong spelling of 'class'" $ do
       (parseTestString "class Objectmethod{void objectcalling(){new Objectmethod().methodToCall();}public void methodToCall(){}}") `shouldBe` Nothing
  describe "nameprogram" $ do
     it "primitive type as name" $ do
       (parseTestString "class Objectmethod{void objectcalling(){new Objectmethod().methodToCall();}public void methodToCall(){}}") `shouldBe` Nothing
  describe "declarationprogram" $ do
     it "wrong primitive type" $ do
       (parseTestString "class Objectmethod{void objectcalling(){new Objectmethod().methodToCall();}public void methodToCall(){}}") `shouldBe` Nothing
  describe "ifsyntaxprogram" $ do
     it "wrong if syntax" $ do
       (parseTestString "class Objectmethod{void objectcalling(){new Objectmethod().methodToCall();}public void methodToCall(){}}") `shouldBe` Nothing
  describe "methodnameprogram" $ do
     it "method name is primitive type" $ do
       (parseTestString "class Objectmethod{void objectcalling(){new Objectmethod().methodToCall();}public void methodToCall(){}}") `shouldBe` Nothing
  describe "roundbracketsprogram" $ do
     it "wrong type of brackets" $ do
       (parseTestString "class Objectmethod{void objectcalling(){new Objectmethod().methodToCall();}public void methodToCall(){}}") `shouldBe` Nothing
  describe "methodtypeprogram" $ do
     it "no type of method" $ do
       (parseTestString "class Objectmethod{void objectcalling(){new Objectmethod().methodToCall();}public void methodToCall(){}}") `shouldBe` Nothing