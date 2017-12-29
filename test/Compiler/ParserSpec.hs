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
  describe "The Parser parses" $ do
     it "an empty class" $
       parseTestString "class Emptyclass {}"
       `shouldBe`
       Just
        [Class
            (Identifier "Emptyclass")
            []
            []]
     it "an object class" $
       parseTestString "class Objectclass {void objectMethod(){new Objectclass();}}"
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
     it "a class with a field" $
       parseTestString "class Fieldclass{int field;}"
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
     it "an empty class with the constructor written" $
       parseTestString "class Constructorclass {Constructorclass() {}}"
       `shouldBe`
       Just [Class (Identifier "Constructorclass") []
                   [Constructor (Identifier "Constructorclass") [] []
                                (Just (Block []))]]
     it "a class with all available types" $
       parseTestString "class Typeclass {int intField;char charField;boolean floatField;}"
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
     it "a class with a method (void)" $
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
     it "a class with all available typed methods" $
       parseTestString "class Methodwithreturnclass {int intMethod() {return 1;}boolean boolMethod() {return true;}char charMethod() {return 'a';}}"
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
     it "a class with an arithmetic method" $
       parseTestString "class Arithclass {int cvar = 10;void Arithmethod (){int locvar = 5;int res = cvar + locvar;}}"
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
     it "a class with a call of a method" $
       parseTestString "class Mthodcallclass {void methodCallmethod(){callMethod();}    void callMethod (){    }}"
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
     it "a class which creates an object" $
       parseTestString "class Objectclass {Objectclass(){}void objectMethod(){Objectclass om = new Objectclass();}}"
       `shouldBe`
        Just [Class (Identifier "Objectclass") []
            [Constructor { getIdentifier = Identifier "Objectclass", getMods = [], getParamList = [], getBody = Just (Block [])},
             Method {
                getIdentifier = Identifier "objectMethod",
                getMods = [],
                getReturnType = JVoid,
                getParamList = [],
                getBody = Just (Block [
                    LocalVar (VarDecl {
                        getIdentifier = Identifier "om",
                        getMods = [],
                        getType = RefType Name { path = []
                                               , getIdentifier = Identifier "Objectclass"},
                        getRHS = Just(
                                        ExprExprStmt (Instantiation (Name
                                            {path = [],
                                            getIdentifier = Identifier "Objectclass"})
                                            [])
                                    )
                        })
                ])
        }]]
     it "a classwith a method with parameters" $
       parseTestString "class Paramclass {void paramMethod(int iparam, boolean bparam, char cparam){}}"
       `shouldBe`
        Just [Class (Identifier "Paramclass") []
            [Method {
                getIdentifier = Identifier "paramMethod",
                getMods = [],
                getReturnType = JVoid,
                getParamList = [(PrimType Int,Identifier "iparam"), (PrimType Boolean ,Identifier "bparam"), (PrimType Char,Identifier "cparam")],
                getBody = Just (Block [])
        }]]
     it "a class with a recursive method" $
       parseTestString "class Recursionclass {void recMethod(int count) {if(count > 0){recMethod(count - 1);}}}"
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
     it "a class with specified scopes" $
       parseTestString "class Scopefieldclass{private int prField;public int puField;protected int proField;}"
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
                getIdentifier = Identifier "proFiel",
                getMods = [Protected],
                getType = PrimType Int,
                getRHS = Nothing})]
        ]
  describe "The Parser should fail on" $ do
     it "no closing bracket" $
       parseTestString "class Objectmethod{void objectcalling(){new Objectmethod().methodToCall();}public void methodToCall(){}}" `shouldBe` Nothing
     it "no semicolon after field" $
       parseTestString "class Objectmethod{void objectcalling(){new Objectmethod().methodToCall();}public void methodToCall(){}}" `shouldBe` Nothing
     it "wrong spelling of 'class'" $
       parseTestString "class Objectmethod{void objectcalling(){new Objectmethod().methodToCall();}public void methodToCall(){}}" `shouldBe` Nothing
     it "primitive type as name" $
       parseTestString "class Objectmethod{void objectcalling(){new Objectmethod().methodToCall();}public void methodToCall(){}}" `shouldBe` Nothing
     it "wrong primitive type" $
       parseTestString "class Objectmethod{void objectcalling(){new Objectmethod().methodToCall();}public void methodToCall(){}}" `shouldBe` Nothing
     it "wrong if syntax" $
       parseTestString "class Objectmethod{void objectcalling(){new Objectmethod().methodToCall();}public void methodToCall(){}}" `shouldBe` Nothing
     it "method name is primitive type" $
       parseTestString "class Objectmethod{void objectcalling(){new Objectmethod().methodToCall();}public void methodToCall(){}}" `shouldBe` Nothing
     it "wrong type of brackets" $
       parseTestString "class Objectmethod{void objectcalling(){new Objectmethod().methodToCall();}public void methodToCall(){}}" `shouldBe` Nothing
     it "no type of method" $
       parseTestString "class Objectmethod{void objectcalling(){new Objectmethod().methodToCall();}public void methodToCall(){}}" `shouldBe` Nothing
