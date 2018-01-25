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
                getIdentifier = Identifier "proField",
                getMods = [Protected],
                getType = PrimType Int,
                getRHS = Nothing})]
        ]
     it "a method which is called from an object" $
       parseTestString "class ObjCall {void CallMethod(){ObjCall oc = new ObjCall();oc.CallMe();}public void CallMe(){}}"
       `shouldBe`
       Just 
	   [Class 
	   (Identifier "ObjCall") 
	   [] 
	   [Method {
		getIdentifier = Identifier "CallMethod", 
		getMods = [], 
		getReturnType = JVoid, 
		getParamList = [], 
		getBody = 
			Just 
				(Block 
				[LocalVar 
					(VarDecl {
						getIdentifier = Identifier "oc", 
						getMods = [], 
						getType = RefType 
							(Name {
								path = [], 
								getIdentifier = Identifier "ObjCall"}), 
						getRHS = 
							Just 
								(ExprExprStmt 
									(Instantiation 
										(Name {
											path = [], 
											getIdentifier = Identifier "ObjCall"}) 
									[]))}),
					StmtExprStmt 
						(Apply 
							(Iden 
								(Name {
									path = 
									[Identifier "oc"], 
									getIdentifier = Identifier "CallMe"})) 
							[])])},
	    Method {
		 getIdentifier = Identifier "CallMe", 
		 getMods = [Public], 
		 getReturnType = JVoid, 
		 getParamList = [], 
		 getBody = Just (Block [])}]]
     it "a method call on right hand side" $
	   parseTestString "class RetToVar{    void MethodToVar(){int i = GetInt();}    public int GetInt(){return 0;}}"
       `shouldBe`
       Just 
	   [Class 
	    (Identifier "RetToVar") 
	    [] 
	    [Method {
		 getIdentifier = Identifier "MethodToVar", 
		 getMods = [], 
		 getReturnType = JVoid, 
		 getParamList = [], 
		 getBody = 
		  Just 
		   (Block 
		    [LocalVar 
			 (VarDecl {
			  getIdentifier = Identifier "i", 
			  getMods = [], 
			  getType = PrimType Int, 
			  getRHS = 
			   Just 
			    (ExprExprStmt 
				 (Apply 
				  (Iden 
				   (Name {
				    path = [], 
					getIdentifier = Identifier "GetInt"})) 
				 []))})])},
		Method {
		 getIdentifier = Identifier "GetInt", 
		 getMods = [Public], 
		 getReturnType = PrimType Int, 
		 getParamList = [], 
		 getBody = 
		  Just 
		   (Block 
		    [Return 
			 (Just 
			  (Literal 
			   (IntegerL 0)))])}]]
     it "a ternary operator" $
	   parseTestString "class Ternary{void ternary(){int tern = 5 < 7 ? 1 : 2;}}"
	   `shouldBe`
       Just
        [Class
            (Identifier "Ternary")
            []
            [Method {
		     getIdentifier = Identifier "ternary", 
		     getMods = [Public], 
		     getReturnType = JVoid, 
		     getParamList = [], 
		     getBody = 
		      Just 
		       (Block 
		        [LocalVar 
			     (VarDecl {
			      getIdentifier = Identifier "i", 
			      getMods = [], 
			      getType = PrimType Int, 
			      getRHS = 
			       Just 
			        (TernaryIf {
					 getCond = PrimBinOp  Less (Literal (IntegerL 5)) (Literal (IntegerL 7)),
					 getElseStmt = Literal (IntegerL 2),
					 getThenStmt = Literal (IntegerL 1)
					})})])}]]
     it "if-else expression" $
	   parseTestString "class IfClass{ void ifMethod(){if(1 > 0){}else{}}}"
	   `shouldBe`
       Just 
	    [Class 
		 (Identifier "IfClass") 
		 [] 
		 [Method {
		  getIdentifier = Identifier "ifMethod", 
		  getMods = [], 
		  getReturnType = JVoid, 
		  getParamList = [], 
		  getBody = 
		   Just (Block 
		    [If {
			 getCond = PrimBinOp Greater 
			  (Literal (IntegerL 1)) (Literal (IntegerL 0)), 
			 getThenStmt = 
			  Just (Block []), 
			 getElseStmt = Just (Block[])}])}]]
     it "if-elseif expression" $
       parseTestString "class IfClass{ void ifMethod(){if(1 > 0){}else if(1 < 0){}}}"
	   `shouldBe`
       Just 
	    [Class 
		 (Identifier "IfClass") 
		 [] 
		 [Method {
		  getIdentifier = Identifier "ifMethod", 
		  getMods = [], 
		  getReturnType = JVoid, 
		  getParamList = [], 
		  getBody = 
		   Just (Block 
		    [If {
			 getCond = PrimBinOp Greater 
			  (Literal (IntegerL 1)) (Literal (IntegerL 0)), 
			 getThenStmt = 
			  Just (Block []), 
			 getElseStmt = Just (Block[])}])}]]
     it "if-elseif-else expression" $
       parseTestString "class IfClass{ void ifMethod(){if(1 > 0){}else if(1 < 0){}else{}}}"
	   `shouldBe`
       Just 
	    [Class 
		 (Identifier "IfClass") 
		 [] 
		 [Method {
		  getIdentifier = Identifier "ifMethod", 
		  getMods = [], 
		  getReturnType = JVoid, 
		  getParamList = [], 
		  getBody = 
		   Just (Block 
		    [If {
			 getCond = PrimBinOp Greater 
			  (Literal (IntegerL 1)) (Literal (IntegerL 0)), 
			 getThenStmt = 
			  Just (Block []), 
			 getElseStmt = Just (Block[])}])}]]
     it "a void method with return" $
       parseTestString "class VRet{void MyVoid(){return;}}"
	   `shouldBe`
       Just
        [Class
            (Identifier "VRet")
            []
            [Method {
		     getIdentifier = Identifier "MyVoid", 
		     getMods = [], 
		     getReturnType = JVoid, 
		     getParamList = [], 
		     getBody = Just (Block[Return Nothing])}]]
     it "an Object as parameter" $
	   parseTestString "class ObjParam{void ObjParamMethod(ObjParam op){}}"
	   `shouldBe`
       Just 
	    [Class 
		 (Identifier "ObjParam") 
		 [] 
		 [Method {
		  getIdentifier = Identifier "ObjParamMethod", 
		  getMods = [], 
		  getReturnType = JVoid, 
		  getParamList = 
		   [(RefType 
		    (Name {
			 path = [], 
			 getIdentifier = Identifier "ObjParam"}),
			Identifier "op")], 
		  getBody = Just (Block [])}]]
     it "a method call with Object as parameter" $
	   parseTestString "class ObjParamCall {    void CallParamMethod(){ObjParamCall opc = new ObjParamCall();opc.CallMe(opc);}    public void CallMe(ObjParamCall oPC){}}"
	   `shouldBe`
       Just 
	    [Class 
		 (Identifier "ObjParamCall") 
		  [] 
		  [Method {
		   getIdentifier = Identifier "CallParamMethod", 
		   getMods = [], 
		   getReturnType = JVoid, 
		   getParamList = [], 
		   getBody = 
		    Just 
			 (Block 
			  [LocalVar 
			   (VarDecl {
			    getIdentifier = Identifier "opc", 
				getMods = [], 
				getType = RefType 
				 (Name {
				  path = [], 
				  getIdentifier = Identifier "ObjParamCall"}), 
				getRHS = 
				 Just 
				  (ExprExprStmt 
				   (Instantiation 
				    (Name {
					 path = [], 
					 getIdentifier = Identifier "ObjParamCall"}) 
					[]))}),
			   StmtExprStmt 
			    (Apply  
				 (Iden 
				  (Name {
				   path = [Identifier "opc"], 
				   getIdentifier = Identifier "CallMe"})) 
				 [Iden (Name {path = [], getIdentifier = Identifier "opc"})])])},
		  Method {
		   getIdentifier = Identifier "CallMe", 
		   getMods = [Public], 
		   getReturnType = JVoid, 
		   getParamList = 
		    [(RefType (Name {path = [], getIdentifier = Identifier "ObjParamCall"}),
			 Identifier "oPC")], 
		   getBody = Just (Block [])}]]
     it "this keyword" $
	   parseTestString "class UseThis{int hand; void thisMethod(){this.hand = 2;}}"
	   `shouldBe`
       Just
        [Class
            (Identifier "UseThis")
            []
            [Field (VarDecl {
                getIdentifier = Identifier "hand",
                getMods = [],
                getType = PrimType Int,
                getRHS = Nothing}),
			 Method {
			  getIdentifier = Identifier "thisMethod", 
			  getMods = [], 
			  getReturnType = JVoid, 
			  getParamList = [], 
			  getBody = 
			   Just (Block 
			    [StmtExprStmt 
				 (Assign NormalAssign
				  (Name {
				   path = [This], 
				   getIdentifier = Identifier "hand"})
				  (Literal (IntegerL 2)))])}
			]]
     it "break keyword" $
       parseTestString "class UseBreak{void breakMethod(){while(true){break;}}}"
	   `shouldBe`
	   Just 
	   [Class
	    (Identifier "UseBreak")
		[]
		[Method 
		 {getIdentifier = Identifier "breakMethod",
		 getMods = [],
		 getReturnType = JVoid,
		 getParamList = [],
		 getBody = 
		  Just (Block 
		   [While {
			 getCond = Literal (BooleanL True), 
			 getBody = Just (Block [Break])}])}]]
     it "some method calls on rhs and calculations with methods (method + method)" $
	   parseTestString "class MethodCalc{void Calc(){int i = intMethod();char c = 'a';if(boolMethod())c = charMethod();boolean b;b = boolMethod();        int q = intMethod() + i;        int a = intMethod() + intMethod();}    int intMethod(){return 1;}    boolean boolMethod(){return true;}    char charMethod(){return 'Z';}    }"
	   `shouldBe`
       Just 
	    [Class 
		 (Identifier "MethodCalc") 
		 [] 
		 [Method {
		  getIdentifier = Identifier "Calc", 
		  getMods = [], 
		  getReturnType = JVoid, 
		  getParamList = [], 
		  getBody = 
		   Just 
		    (Block 
			 [LocalVar 
			  (VarDecl {
			   getIdentifier = Identifier "i", 
			   getMods = [], 
			   getType = PrimType Int, 
			   getRHS = 
			    Just 
				 (ExprExprStmt 
				  (Apply 
				   (Iden 
				    (Name {
					 path = [], 
					 getIdentifier = Identifier "intMethod"})) 
					 []))}),
			 LocalVar 
			  (VarDecl {
			   getIdentifier = Identifier "c", 
			    getMods = [], 
				getType = PrimType Char, 
				getRHS = 
				 Just 
				  (Literal (CharL 'a'))}),
			 If {
			  getCond = ExprExprStmt 
			   (Apply 
			    (Iden 
				 (Name { 
				  path = [], 
				  getIdentifier = Identifier "boolMethod"})) []), 
			  getThenStmt = 
			   Just 
			    (StmtExprStmt 
				 (Assign NormalAssign 
				  (Name {
				   path = [], 
				   getIdentifier = Identifier "c"}) 
				  (ExprExprStmt 
				   (Apply 
				    (Iden 
					 (Name {
					  path = [], 
					  getIdentifier = Identifier "charMethod"})) [])))), 
			  getElseStmt = Nothing},
			 LocalVar  
			  (VarDecl {
			   getIdentifier = Identifier "b", 
			   getMods = [], 
			   getType = PrimType Boolean, 
			   getRHS = Nothing}),
			  StmtExprStmt 
			  (Assign NormalAssign 
			   (Name {
			    path = [], 
				 getIdentifier = Identifier "b"}) 
			  (ExprExprStmt 
			   (Apply 
			    (Iden 
				 (Name {
				  path = [], 
				  getIdentifier = Identifier "boolMethod"})) []))),
			 LocalVar 
			  (VarDecl {
			   getIdentifier = Identifier "q", 
			   getMods = [], 
			   getType = PrimType Int, 
			   getRHS = 
			    Just 
				 (PrimBinOp Add 
				  (ExprExprStmt 
				   (Apply 
				    (Iden 
					 (Name {
					  path = [], 
					  getIdentifier = Identifier "intMethod"})) [])) 
				  (Iden (Name {path = [], getIdentifier = Identifier "i"})))}),
			 LocalVar 
			  (VarDecl {
			   getIdentifier = Identifier "a", 
			   getMods = [], 
			   getType = PrimType Int, 
			   getRHS = 
			    Just 
				 (PrimBinOp Add 
				  (ExprExprStmt 
				   (Apply 
				    (Iden 
					 (Name {
					  path = [], 
					  getIdentifier = Identifier "intMethod"})) [])) 
				  (ExprExprStmt 
				   (Apply 
				    (Iden 
					 (Name {
					  path = [], 
					  getIdentifier = Identifier "intMethod"})) [])))})])},
			Method {
			 getIdentifier = Identifier "intMethod", 
			 getMods = [], 
			 getReturnType = PrimType Int, 
			 getParamList = [], 
			 getBody = 
			  Just 
			   (Block 
			    [Return (Just (Literal (IntegerL 1)))])},
			Method {
			 getIdentifier = Identifier "boolMethod", 
			 getMods = [], 
			 getReturnType = PrimType Boolean, 
			 getParamList = [], 
			 getBody = 
			  Just 
			   (Block 
			    [Return (Just (Literal (BooleanL True)))])},
		    Method {
			 getIdentifier = Identifier "charMethod", 
			 getMods = [], 
			 getReturnType = PrimType Char, 
			 getParamList = [], 
			 getBody = 
			  Just (Block [Return (Just (Literal (CharL 'Z')))])}]]
     it "a method that calls another method as return" $
	   parseTestString "class RetMethod{    int retIntMeth(){return intMethod();}    int intMethod(){return 0;}}"
	   `shouldBe`
       Just 
	    [Class 
		 (Identifier "RetMethod") 
		 [] 
		 [Method {
		  getIdentifier = Identifier "retIntMeth", 
		  getMods = [], 
		  getReturnType = PrimType Int, 
		  getParamList = [], 
		  getBody = 
		   Just 
		   (Block 
		    [Return 
			 (Just 
			  (ExprExprStmt 
			   (Apply 
			    (Iden 
				 (Name {
				  path = [], 
				  getIdentifier = Identifier "intMethod"})) [])))])},
		  Method {
		   getIdentifier = Identifier "intMethod", 
		   getMods = [], 
		   getReturnType = PrimType Int, 
		   getParamList = [], 
		   getBody = 
		    Just (Block [Return (Just (Literal (IntegerL 0)))])}]]
     it "some calculations + - * /" $
	   parseTestString "class Calculations{Calculations(){int i = 0 + 5;i = 2 - 1;i = 5 / 2;i = 8 * 9;}}"
	   `shouldBe`
       Just 
	    [Class 
		 (Identifier "Calculations") 
		 [] 
		 [Constructor {
		  getIdentifier = Identifier "Calculations", 
		  getMods = [], 
		  getParamList = [], 
		  getBody = 
		   Just 
		    (Block 
			 [LocalVar 
			  (VarDecl {
			   getIdentifier = Identifier "i", 
			   getMods = [], 
			   getType = PrimType Int, 
			   getRHS = 
			    Just 
				 (PrimBinOp Add (Literal (IntegerL 0)) (Literal (IntegerL 5)))}),
			 StmtExprStmt 
			  (Assign NormalAssign 
			   (Name {
			    path = [], 
				getIdentifier = Identifier "i"}) 
			  (PrimBinOp Subtract 
			   (Literal (IntegerL 2)) (Literal (IntegerL 1)))), 
			 StmtExprStmt 
			  (Assign NormalAssign 
			   (Name {
			    path = [], 
				getIdentifier = Identifier "i"}) 
			   (PrimBinOp Divide 
			    (Literal (IntegerL 5)) 
			    (Literal (IntegerL 2)))),
			 StmtExprStmt 
			  (Assign NormalAssign 
			   (Name {
			    path = [], 
				getIdentifier = Identifier "i"}) 
			   (PrimBinOp Multiply 
			    (Literal (IntegerL 8)) 
				(Literal (IntegerL 9))))])}]]
     it "a method that converts char to int" $
	   parseTestString "class CtoI{int castToInt(char c){return (int)c;}}"
	   `shouldBe`
       Just 
	    [Class 
		 (Identifier "CtoI") 
		 [] 
		 [Method {
		  getIdentifier = Identifier "castToInt", 
		  getMods = [], 
		  getReturnType = PrimType Int, 
		  getParamList = [(PrimType Char,Identifier "c")], 
		  getBody = Just 
		   (Block 
		    [Return 
			 (Just 
			  (Cast (PrimType Int) 
			   (Iden (Name {path = [], getIdentifier = Identifier "c"}))))])}]]
     it "a class with a while loop" $
	   parseTestString "class WLoop{void whileLoop(){while(true){}}}"
	   `shouldBe`
       Just 
	    [Class 
		 (Identifier "WLoop") 
		 [] 
		 [Method {
		  getIdentifier = Identifier "whileLoop", 
		  getMods = [], 
		  getReturnType = JVoid, 
		  getParamList = [], 
		  getBody = 
		   Just (Block 
		    [While {
			 getCond = Literal (BooleanL True), 
			 getBody = Just (Block [])}])}]]
     it "BouncyBall TestGame" $
	   parseTestString "class BouncyBall{   int rng; char input;char gameState;int height;int chargedPower; int hitObjectChance;BouncyBall(){new BouncyBall(50).mainLoop();}BouncyBall(int startRNG){int i = startRNG; while(i > 0) {i--; nextRandomNumber();}gameState = 's';height = 50;chargedPower = 0;hitObjectChance = 65535/5;  input = 's';} void mainLoop(){boolean running = true;while(running){nextRandomNumber();if(rng < 65535/3)input = 's'; else if(rng < 65535*2/3)input = 'l'; else input = 'r';  if(gameState ==  's') {chargedPower += 10 + height/4;hitObjectChance = 65535/5; gameState = input;}else if(gameState == 'l') {chargedPower += 10 + height/10;hitObjectChance = 65535/10; height = height*5/6;gameState = input;}else if(gameState == 'r') {height += chargedPower;chargedPower = 0;hitObjectChance = 65535/5;  gameState = input;} if(chargedPower > 10000)gameState = 'r'; nextRandomNumber();if(rng < hitObjectChance){nextRandomNumber();if(rng < 65535/3)height = height * 5/6;  else if(rng < 65535*2/3)height = height  * 3/4; else height = height  * 1/2;  } nextRandomNumber();if(rng < 65535/150){running = false;}}}int nextRandomNumber(){rng = rngFunction(rng);return rng;}    int rngFunction(int input){int s0 = (input << 8); s0 ^= input;input = ((s0 & 255) << 8) | ((s0 & 65280) >>> 8);s0 = (s0 << 1) ^ input;int s1 = (s0 >>> 1) ^ 65408;if((s0 & 1) == 0){if(s1 == 43605) input = 0;else input = s1^8180;}else input = s1 ^33152;return input & 65535;}}"
	   `shouldBe`
       Just
        [Class
            (Identifier "BouncyBall")
            []
            []]
     it "a class with comments" $ do
       parseTestString "class Comments{/*Multiline Comment*/ Comments(){/*int ignoreMe;*/}}"
       `shouldBe`
       Just
        [Class
            (Identifier "Comments")
            []
            [Constructor {
            getIdentifier = Identifier "Comments",
            getMods = [],
            getParamList = [],
            getBody = Just (Block[])}]]
     it "a class with binary operators" $ do
       parseTestString "class BinaryOperands{ void binaryMethod(){int i = 1 & 2; i = 1 | 2; i = 1 >> 2; i = 1 >>> 2; i = 1 << 2; i ^= 2;}}"
       `shouldBe`
       Just
	    [Class 
		 (Identifier "BinaryOperands")
		 []
		 []]
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
