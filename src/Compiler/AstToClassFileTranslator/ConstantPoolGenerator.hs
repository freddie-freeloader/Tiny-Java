module Compiler.AstToClassFileTranslator.ConstantPoolGenerator where
  import Data.List (elemIndex)
  import Compiler.Ast
  import Compiler.AbstractBytecode

  type AstClass = Compiler.Ast.Class
  type BytecodeClass = Compiler.AbstractBytecode.Class
  type Dictionary = [(String, Int)]
  type VarTypeTable = [(String, Type)]


  -- generateClassFiles :: [AstClass] -> [ClassFile]



  -- generateClassFile :: AstClass -> ClassFile
  defaultSuperClass :: String
  defaultSuperClass = "java/lang/Object"


  defaultSuperClassType :: Name
  defaultSuperClassType = Name [Identifier "java", Identifier "lang"] (Identifier "Object")

  defaultConstructorMethodName :: String
  defaultConstructorMethodName = "<init>"

  generateCP :: AstClass -> ([Constant], Dictionary)
  generateCP class_@(Compiler.Ast.Class (Identifier name) _ _) =
    collectConstants class_ cp dictionary
    where cp = [CONSTANT_MethodRef 3 7
               ,CONSTANT_Class 8
               ,CONSTANT_Class 9
               ,CONSTANT_Utf8 "<init>"
               ,CONSTANT_Utf8 "()V"
               ,CONSTANT_Utf8 "Code"
               ,CONSTANT_NameAndType 4 5
               ,CONSTANT_Utf8 name
               ,CONSTANT_Utf8 defaultSuperClass
               ,CONSTANT_MethodRef 13 19
               ,CONSTANT_MethodRef 15 20
               ,CONSTANT_MethodRef 17 21
               ,CONSTANT_Class 14
               ,CONSTANT_Utf8 "java/lang/Integer"
               ,CONSTANT_Class 16
               ,CONSTANT_Utf8 "java/lang/Character"
               ,CONSTANT_Class 18
               ,CONSTANT_Utf8 "java/lang/Boolean"
               ,CONSTANT_NameAndType 22 23
               ,CONSTANT_NameAndType 22 24
               ,CONSTANT_NameAndType 22 25
               ,CONSTANT_Utf8 "valueOf"
               ,CONSTANT_Utf8 "(I)Ljava/lang/Integer;"
               ,CONSTANT_Utf8 "(C)Ljava/lang/Character;"
               ,CONSTANT_Utf8 "(Z)Ljava/lang/Boolean;"
               ]
          dictionary = [("Method java/lang/Object.<init>:()V", 1),
                        ("Code", 6),
                        ("class " ++ defaultSuperClass, 3),
                        ("class " ++ name, 2),
                        ("class this", 2),
                        ("class Integer", 13),
                        ("class Character", 15),
                        ("class Boolean", 17),
                        ("Method valueOf:(I)Ljava/lang/Integer;", 10),
                        ("Method valueOf:(C)Ljava/lang/Character;", 11),
                        ("Method valueOf:(Z)Ljava/lang/Boolean;", 12)
                       ]


  -- | Helper to make clear which parameters are used
  collectConstants :: AstClass -> [Constant] -> Dictionary -> ([Constant], Dictionary)
  collectConstants class_@(Compiler.Ast.Class _ _ decl) cp dictionary =
    declsToCP (reverse decl) class_ cp dictionary -- reverse is only used for testing purposes, remove after final


  -- | Will translate every declaration to additions to the constant pool
  declsToCP :: [Decl] -> AstClass -> [Constant] -> Dictionary -> ([Constant], Dictionary)
  declsToCP decls class_ cp dictionary = foldr (\decl (cpAcc, dictAcc) -> declToCP decl class_ cpAcc dictAcc) (cp, dictionary) decls


  -- | Will translate a declaration to addition to the constant pool
  declToCP :: Decl -> AstClass -> [Constant] -> Dictionary -> ([Constant], Dictionary)
  declToCP (Compiler.Ast.Field
            (VarDecl
             (Identifier name)
             _
             type_
             expression))
    class_@(Compiler.Ast.Class (
        Identifier classId) _ _)
    cp
    dictionary =
    let
      (cpAfterFieldRef, dictAfterFieldRef) = fieldRefToCP classId name type_ cp dictionary
      expressionDecode (Just expr) = expressionValueToCP class_ expr [] cpAfterFieldRef dictAfterFieldRef
      expressionDecode Nothing = ([], cpAfterFieldRef, dictAfterFieldRef)
      (_, cpAfterExpression, dictAfterExpression) = expressionDecode expression
    in
      (cpAfterExpression, dictAfterExpression)
  declToCP (Compiler.Ast.Method
            (Identifier name)
            _
            type_
            param
            statement)
    class_@(Compiler.Ast.Class (
        Identifier classId) _ _)
    cp
    dictionary =
    let
      (cpAfterMethodRef, dictAfterMethodRef) = methodRefToCP classId name (map fst param) type_ cp dictionary
      statementDecode (Just statement) = statementValueToCP class_ statement [] cpAfterMethodRef dictAfterMethodRef
      statementDecode Nothing = ([], cpAfterMethodRef, dictAfterMethodRef)
      (_, cpAfterStatement, dictAfterStatement) = statementDecode statement
    in
      (cpAfterStatement, dictAfterStatement)
  declToCP (Compiler.Ast.Constructor
            _
            _
            param
            statement)
    class_@(Compiler.Ast.Class (
        Identifier classId) _ _)
    cp
    dictionary =
    let
      (cpAfterMethodRef, dictAfterMethodRef) = methodRefToCP defaultSuperClass defaultConstructorMethodName (map fst param) JVoid cp dictionary
      statementDecode (Just statement) = statementValueToCP class_ statement [] cpAfterMethodRef dictAfterMethodRef
      statementDecode Nothing = ([], cpAfterMethodRef, dictAfterMethodRef)
      (_, cpAfterStatement, dictAfterStatement) = statementDecode statement
    in
      (cpAfterStatement, dictAfterStatement)



  statementValueToCP :: Compiler.Ast.Class -> Compiler.Ast.Statement -> VarTypeTable -> [Constant] -> Dictionary -> (VarTypeTable, [Constant], Dictionary)
  statementValueToCP class_ (TypedStatement(While cond statement, _)) varTypeTable cp dictionary =
    let
      (varTyAfterExpression, cpAfterExpression, dictAfterExpression) = expressionValueToCP class_ cond varTypeTable cp dictionary
      (varTyAfterStatement, cpAfterStatement, dictAfterStatement) = case statement of
        Just stmt -> statementValueToCP class_ stmt varTyAfterExpression cpAfterExpression dictAfterExpression
        Nothing   -> (varTyAfterExpression, cpAfterExpression, dictAfterExpression)
    in
      (varTyAfterStatement, cpAfterStatement, dictAfterStatement)
  statementValueToCP class_ (TypedStatement(If cond thenCase elseCase, _)) varTypeTable cp dictionary =
    let
      (varTyAfterExpression, cpAfterExpression, dictAfterExpression) = expressionValueToCP class_ cond varTypeTable cp dictionary
      (varTyAfterThen, cpAfterThen, dictAfterThen) = case thenCase of
        Just stmt -> statementValueToCP class_ stmt varTyAfterExpression cpAfterExpression dictAfterExpression
        Nothing   -> (varTyAfterExpression, cpAfterExpression, dictAfterExpression)
      (varTyAfterElse, cpAfterElse, dictAfterElse) = case elseCase of
        Just stmt -> statementValueToCP class_ stmt varTyAfterThen cpAfterThen dictAfterThen
        Nothing   -> (varTyAfterThen, cpAfterThen, dictAfterThen)
    in
      (varTyAfterElse, cpAfterElse, dictAfterElse)
  statementValueToCP class_ (TypedStatement(Block xs, _)) varTypeTable cp dictionary =
    foldr (\stmt (varTyAcc, cpAcc, dictAcc) -> statementValueToCP class_ stmt varTyAcc cpAcc dictAcc) (varTypeTable, cp, dictionary) (reverse xs) -- NOTE: Reverese for easy understandig
  statementValueToCP class_ (TypedStatement(Return (Just expression), _)) varTypeTable cp dictionary =
    expressionValueToCP class_ expression varTypeTable cp dictionary
  statementValueToCP _ (TypedStatement(Return Nothing, _)) varTypeTable cp dictionary =
    (varTypeTable, cp, dictionary)
  statementValueToCP class_ (TypedStatement(LocalVar (VarDecl (Identifier name) _ type_@(RefType typeName) (Just expression)), _)) varTypeTable cp dictionary =
    let
      (cpAfterClassInMethod, dictAfterClassInMethod) = classInMethodToCP class_ typeName cp dictionary
      varTyAfterLocalVar = updateVarTypeTable name type_ varTypeTable
    in
      expressionValueToCP class_ expression varTyAfterLocalVar cpAfterClassInMethod dictAfterClassInMethod
  statementValueToCP class_ (TypedStatement(LocalVar (VarDecl _ _ _ (Just expression)), _)) varTypeTable cp dictionary =
      expressionValueToCP class_ expression varTypeTable cp dictionary
  statementValueToCP class_ (TypedStatement(LocalVar (VarDecl (Identifier name) _ type_@(RefType typeName) Nothing), _)) varTypeTable cp dictionary =
    let
      (cpAfterClassInMethod, dictAfterClassInMethod) = classInMethodToCP class_ typeName cp dictionary
      varTyAfterLocalVar = updateVarTypeTable name type_ varTypeTable
    in
      (varTyAfterLocalVar, cpAfterClassInMethod, dictAfterClassInMethod)
  statementValueToCP _ (TypedStatement(LocalVar (VarDecl _ _ _ Nothing), _)) varTypeTable cp dictionary =
    (varTypeTable, cp, dictionary)
  statementValueToCP class_ (TypedStatement(StmtExprStmt stmtExpr, _)) varTypeTable cp dictionary =
    stmtExprToCP class_ stmtExpr varTypeTable cp dictionary
  statementValueToCP _ (TypedStatement(Continue, _)) varTypeTable cp dictionary = (varTypeTable, cp, dictionary)
  statementValueToCP _ (TypedStatement(Break, _)) varTypeTable cp dictionary = (varTypeTable, cp, dictionary)
  statementValueToCP _ _ varTypeTable cp dictionary = (varTypeTable, cp, dictionary)


  stmtExprToCP :: Compiler.Ast.Class -> Compiler.Ast.StmtExpr -> VarTypeTable -> [Constant] -> Dictionary -> (VarTypeTable, [Constant], Dictionary)
  stmtExprToCP class_@(Compiler.Ast.Class (Identifier className) _ _) (TypedStmtExpr(Assign _ (Name path (Identifier name)) expression, _)) varTypeTable cp dictionary =
    let
      isField = not $ null path
      fieldType | length path > 1 = varTypeTableLookUp (fullClass class_ (Name (init path) (last path))) varTypeTable
                | length path == 1 = varTypeTableLookUp (translateType class_ (head path)) varTypeTable
                | otherwise = Nothing
      (varTyAfterFieldRef, cpAfterFieldRef, dictAfterFieldRef) = case fieldType of
          Just type_ -> ((\(cpAf, dictAf) -> (varTypeTable, cpAf, dictAf)) (fieldRefToCP className name type_ cp dictionary))
          Nothing    -> (varTypeTable, cp, dictionary)
    in
      (varTyAfterFieldRef, cpAfterFieldRef, dictAfterFieldRef)
  stmtExprToCP class_ (TypedStmtExpr(Instantiation name expressions, _)) varTypeTable cp dictionary =
    let
      (varTyAfterExpression, cpAfterExpression, dictAfterExpression) = foldr (\expression (varTyAcc, cpAcc, dictAcc) -> expressionValueToCP class_ expression varTyAcc cpAcc dictAcc) (varTypeTable, cp, dictionary) (reverse expressions) -- NOTE: Reverse for easy debugging
      (cpAfterClassInMethod, dictAfterClassInMethod) = classInMethodToCP class_ name cpAfterExpression dictAfterExpression
      -- TODO: Add methodref add varTypeTable stuff
      methodName = defaultConstructorMethodName
      expressionTypes = foldr (\(TypedExpression(_, type_)) acc -> type_ : acc) [] expressions
      (cpAfterMethodRef, dictAfterMethodRef) = methodRefToCP (fullClass class_ name) methodName expressionTypes JVoid cpAfterClassInMethod dictAfterClassInMethod
    in
      (varTyAfterExpression, cpAfterMethodRef, dictAfterMethodRef)
  stmtExprToCP class_ (TypedStmtExpr(Apply (TypedExpression(Iden (Name path (Identifier name)), _)) expressions, returnType)) varTypeTable cp dictionary =
    let
      (varTyAfterParams, cpAfterParams, dictAfterParams) = foldr (\expr (varTyAcc, cpAcc, dictAcc) -> expressionValueToCP class_ expr varTyAcc cpAcc dictAcc) (varTypeTable, cp, dictionary) (reverse expressions)
      types = foldr (\(TypedExpression(_, t)) acc -> t : acc) [] expressions
      pathToName p | length p > 1 = Name (init p) (last p)
                   | length p == 1 = Name [] (head p)
                   | length p == 0 = Name [] This
      classType p | length p >= 1 = case varTypeTableLookUp (fullClass class_ (pathToName p)) varTyAfterParams of
          Just (RefType val) -> fullClass class_ val
          Nothing -> fullClass class_ (pathToName p)
                  | otherwise = fullClass class_ (pathToName p)
      (cpAfterMethodRef, dictAfterMethodRef) = methodRefToCP (classType path) name types returnType cpAfterParams dictAfterParams
    in
      (varTypeTable, cpAfterMethodRef, dictAfterMethodRef)
  stmtExprToCP class_ (TypedStmtExpr(SEUnOp _ expression, _)) varTypeTable cp dictionary =
    expressionValueToCP class_ expression varTypeTable cp dictionary
  stmtExprToCP _ _ varTypeTable cp dictionary =
      (varTypeTable, cp, dictionary)


  expressionValueToCP :: AstClass -> Compiler.Ast.Expression -> VarTypeTable -> [Constant] -> Dictionary -> (VarTypeTable, [Constant], Dictionary)
  expressionValueToCP class_ (TypedExpression(TernaryIf cond thenCase elseCase, _)) varTypeTable cp dictionary =
    let
      (varTyAfterCond, cpAfterCond, dictAfterCond) = expressionValueToCP class_ cond varTypeTable cp dictionary
      (varTyAfterThen, cpAfterThen, dictAfterThen) = expressionValueToCP class_ thenCase varTyAfterCond cpAfterCond dictAfterCond
      (varTyAfterElse, cpAfterElse, dictAfterElse) = expressionValueToCP class_ elseCase varTyAfterThen cpAfterThen dictAfterThen
    in
      (varTyAfterElse, cpAfterElse, dictAfterElse)
  expressionValueToCP class_ (TypedExpression(PrimBinOp _ expr0 expr1, _)) varTypeTable cp dictionary =
    let
      (varTyAfterExpr0, cpAfterExpr0, dictAfterExpr0) = expressionValueToCP class_ expr0 varTypeTable cp dictionary
      (varTyAfterExpr1, cpAfterExpr1, dictAfterExpr1) = expressionValueToCP class_ expr1 varTypeTable cpAfterExpr0 dictAfterExpr0
    in
      (varTyAfterExpr1, cpAfterExpr1, dictAfterExpr1)
  expressionValueToCP class_ (TypedExpression(PrimUnOp _ expression, _)) varTypeTable cp dictionary =
    expressionValueToCP class_ expression varTypeTable cp dictionary
  expressionValueToCP class_@(Compiler.Ast.Class (Identifier className) _ _) (TypedExpression(Iden ref@(Name path (Identifier field)), fieldType@(RefType name))) varTypeTable cp dictionary = -- NOTE: Iden has type of class where it is defiened
    let
      pathToName p | length p > 1 = Name (init p) (last p)
                   | length p == 1 = Name [] (head p)
                   | length p == 0 = Name [] This
      classType p | length p >= 1 = case varTypeTableLookUp (fullClass class_ (pathToName p)) varTypeTable of
          Just (RefType val) -> fullClass class_ val
          Nothing -> fullClass class_ (pathToName p)
                  | otherwise = fullClass class_ (pathToName p)
      (cpAfterFieldRef, dictAfterFieldRef) = fieldRefToCP (classType path) field fieldType cp dictionary
    in
      (varTypeTable, cpAfterFieldRef, dictAfterFieldRef)
  expressionValueToCP class_ (TypedExpression(Iden ref, _)) varTypeTable cp dictionary =
    (varTypeTable, cp, dictionary)
  expressionValueToCP _ (TypedExpression(Literal (IntegerL int), (PrimType Int))) varTypeTable cp dictionary =
    let
      (cpAfterInteger, dictAfterInteger) = integerToCP int cp dictionary
    in
      (varTypeTable, cpAfterInteger, dictAfterInteger)
  expressionValueToCP _ (TypedExpression(Literal (StringL str), (RefType (Name [Identifier "java", Identifier "lang"] (Identifier "String"))))) varTypeTable cp dictionary =
    let
      (cpAfterString, dictAfterString) = stringToCP str cp dictionary
    in
      (varTypeTable, cpAfterString, dictAfterString)
  expressionValueToCP _ (TypedExpression(Literal _, _)) varTypeTable cp dictionary =
    (varTypeTable, cp, dictionary)
  expressionValueToCP class_ (TypedExpression(ExprExprStmt stmtExpr, _)) varTypeTable cp dictionary =
    stmtExprToCP class_ stmtExpr varTypeTable cp dictionary
  expressionValueToCP class_ (TypedExpression(Cast _ expression, _)) varTypeTable cp dictionary =
    expressionValueToCP class_ expression varTypeTable cp dictionary
  expressionValueToCP _ _ varTypeTable cp dictionary =
    (varTypeTable, cp, dictionary)


  classInMethodToCP :: Compiler.Ast.Class -> Name -> [Constant] -> Dictionary -> ([Constant], Dictionary)
  classInMethodToCP class_@(
    (Compiler.Ast.Class
      (Identifier classId) _ _))
    name
    cp
    dictionary = (cpAfterClass, dict)
    where classString = fullClass class_ name
          classConst = CONSTANT_Utf8 classString
          (cpAfterUtf8, utf8Index) = updateCP classConst cp
          (cpAfterClass, classIndex) = updateCP (CONSTANT_Class (fromIntegral utf8Index)) cpAfterUtf8
          dict = updateDict ("class " ++ classString, classIndex) dictionary


  translateType :: Compiler.Ast.Class -> Identifier -> String
  translateType _ (Identifier id) = id
  translateType (Compiler.Ast.Class (Identifier name) _ _) This = name
  translateType _ Super = defaultSuperClass


  fullClass :: Compiler.Ast.Class -> Name -> String
  fullClass class_ (Name p n) = (foldr (\id acc -> translateType class_ id ++ "/" ++ acc) "" p) ++ translateType class_ n


  -- | Will add a utf8 string to the constant pool
  utf8ToCP :: String -> [Constant] -> ([Constant], Int)
  utf8ToCP utf8 cp = updateCP utf8Const cp
    where utf8Const = CONSTANT_Utf8 utf8


  -- | Will add a class constant to the constant pool
  classToCP :: String -> [Constant] -> ([Constant], Int)
  classToCP class_ cp = updateCP classConst cpAfterUtf8
    where (cpAfterUtf8, utf8Index) = utf8ToCP class_ cp
          classConst = CONSTANT_Class (fromIntegral utf8Index)


  -- | Will add a name and type index reference to the constant pool
  fieldNameAndtTypeToCP :: String -> Type -> [Constant] -> ([Constant], Int)
  fieldNameAndtTypeToCP name type_ cp = updateCP nameAndtTypeConst cpAfterType
    where (cpAfterName, nameIndex) = utf8ToCP name cp
          (cpAfterType, typeIndex) = fieldDescriptorToCP type_ cpAfterName
          nameAndtTypeConst = CONSTANT_NameAndType (fromIntegral nameIndex) (fromIntegral typeIndex)


  -- | Will add a name and type index reference to the constant pool
  methodNameAndTypeToCP :: String -> [Type] -> Type -> [Constant] -> ([Constant], Int)
  methodNameAndTypeToCP name param returnType cp = updateCP nameAndtTypeConst cpAfterType
    where (cpAfterName, nameIndex) = utf8ToCP name cp
          (cpAfterTypeSign, typeSignIndex) = methodDescriptorToCP param returnType cpAfterName
          (cpAfterType, _) = methodTypeToCP param returnType cpAfterTypeSign -- NOTE: Index not needed, just extra work
          nameAndtTypeConst = CONSTANT_NameAndType (fromIntegral nameIndex) (fromIntegral typeSignIndex)


  -- | Will add a field reference to the constant pool
  fieldRefToCP :: String -> String -> Type -> [Constant] -> Dictionary -> ([Constant], Dictionary)
  fieldRefToCP class_ name descriptor cp dictionary = (cpAfterFieldRef, dictAfterThis)
    where (cpAfterClass, classIndex) = classToCP class_ cp
          (cpAfterNameAndType, nameAndtTypeIndex) = fieldNameAndtTypeToCP name descriptor cpAfterClass
          fieldRefConst = CONSTANT_FieldRef (fromIntegral classIndex) (fromIntegral nameAndtTypeIndex)
          (cpAfterFieldRef, fieldRefIndex) = updateCP fieldRefConst cpAfterNameAndType
          className | cpIndex cp 8 == CONSTANT_Utf8 class_ = Just "this"
                    | otherwise = Nothing
          dict = updateDict ("Field " ++ (class_) ++ "." ++ name, fieldRefIndex) dictionary
          dictAfterThis = case className of
            Just _ -> updateDict ("Field this." ++ name, fieldRefIndex) dict
            Nothing -> dict


  -- | Will add a constructor/method reference and descriptor reference to the constant pool
  methodRefToCP :: String -> String -> [Type] -> Type -> [Constant] -> Dictionary -> ([Constant], Dictionary)
  methodRefToCP class_ name param returnType cp dictionary = (cpAfterMethodType, dictAfterRef)
    where (cpAfterClass, classIndex) = classToCP class_ cp
          (cpAfterNameAndType, nameAndtTypeIndex) = methodNameAndTypeToCP name param returnType cpAfterClass
          methodRefConst = CONSTANT_MethodRef (fromIntegral classIndex) (fromIntegral nameAndtTypeIndex)
          (cpAfterMethodRef, methodRefIndex) = updateCP methodRefConst cpAfterNameAndType
          (cpAfterMethodType, methodTypeIndex) = methodTypeToCP param returnType cpAfterMethodRef
          isThis = classIndex == 2
          dictAfterRef = updateDict ("Method " ++ class_ ++ "." ++ name ++ ":" ++ (methodDescriptorToString param returnType), methodRefIndex) dictionary
          dictAfterThis | isThis = updateDict ("Method this." ++ name ++ ":" ++ (methodDescriptorToString param returnType), methodRefIndex) dictAfterRef
                        | otherwise = dictAfterRef


  stringToCP :: String -> [Constant] -> Dictionary -> ([Constant], Dictionary)
  stringToCP str cp dictionary = (cpAfterString, dict)
    where (cpAfterUtf8, utf8Index) = utf8ToCP str cp
          stringConst = CONSTANT_String (fromIntegral utf8Index)
          (cpAfterString, stringIndex) = updateCP stringConst cpAfterUtf8
          dict | length cpAfterUtf8 == length cpAfterString = dictionary
               | otherwise = updateDict ("String " ++ str, stringIndex) dictionary


  integerToCP :: Integer -> [Constant] -> Dictionary -> ([Constant], Dictionary)
  integerToCP int cp dictionary = (cpAfterInteger, dict)
    where intConst = CONSTANT_Integer (fromIntegral int)
          (cpAfterInteger, integerIndex) | int > 2^15 - 1 || int < (-(2^15)) = updateCP intConst cp
                                         | otherwise = (cp , (-1)) -- NOTE: Is -1 because it should not exist
          dict | integerIndex == (-1) || length cpAfterInteger == length cp = dictionary
               | otherwise = updateDict ("Int " ++ show int, integerIndex) dictionary


  -- | Will be used in method to indicate descriptor
  methodTypeToCP :: [Type] -> Type -> [Constant] -> ([Constant], Int)
  methodTypeToCP param returnType cp = updateCP methodTypeConst cpAfterDescriptor
    where (cpAfterDescriptor, descriptorIndex) = methodDescriptorToCP param returnType cp
          methodTypeConst = CONSTANT_MethodType (fromIntegral descriptorIndex)


  -- | Will add a type to the constant pool
  fieldDescriptorToCP :: Type -> [Constant] -> ([Constant], Int)
  fieldDescriptorToCP descriptor cp = updateCP descriptorConst cp
    where descriptorConst = CONSTANT_Utf8 (typeToString descriptor)


  -- | Will generate a descriptor for constructors/methods for the constant pool
  methodDescriptorToCP :: [Type] -> Type -> [Constant] -> ([Constant], Int)
  methodDescriptorToCP param returnType cp = updateCP descriptorConst cp
    where descriptorConst = CONSTANT_Utf8 $ methodDescriptorToString param returnType


  methodDescriptorToString :: [Type] -> Type -> String
  methodDescriptorToString param returnType = "(" ++ paramT ++ ")" ++ returnT
    where paramT = foldr (\t acc -> typeToString t ++ acc) "" param
          returnT = typeToString returnType


  -- | Generates a string according to the types in the constant pool
  typeToString :: Type -> String
  typeToString (PrimType Boolean) = "Z"
  typeToString (PrimType Int)     = "I"
  typeToString (PrimType Char)    = "C"
  typeToString JVoid              = "V"
  typeToString (RefType name)     = entry name


  -- | Helper which encodes classe types for the constant pool as _L_Class_;_ or _L_ Path _/_ Class _;_
  entry :: Name -> String
  entry (Name [] (Identifier id))   = "L" ++ id ++ ";"
  entry (Name path (Identifier id)) = "L" ++ (foldr (\(Identifier x) acc ->
                                                       x
                                                       ++ "/"
                                                       ++ acc)
                                              ""
                                              path)
                                      ++ id
                                      ++ ";"


  -- | Add an element to the constant pool if it exists, else will add and will return coresponding index
  updateCP :: Constant -> [Constant] -> ([Constant], Int)
  updateCP a cp = (addIn new cp, indexIn new cp)
    where new = a `elemIndex` cp
          addIn (Just _) constantPool = constantPool
          addIn Nothing constantPool  = constantPool ++ [a]
          -- NOTE: CP starts on index 1.
          indexIn (Just index) _       = index + 1
          indexIn Nothing constantPool = length constantPool + 1


  updateDict :: (String, Int) -> Dictionary -> Dictionary
  updateDict a dictionary | a `elem` dictionary = dictionary
                          | otherwise = dictionary ++ [a]


  cpLookUp :: Constant -> [Constant] -> Maybe Int
  cpLookUp key cp = case key `elemIndex` cp of
      Just val -> Just (val + 1)
      Nothing -> Nothing


  -- | Uses constant pool index instead of list index
  cpIndex :: [Constant] -> Int -> Constant
  cpIndex xs i = xs !! (i - 1)


  dictLookUp :: String -> Dictionary -> Maybe Int
  dictLookUp key dictionary | null res = Nothing
                            | otherwise = Just (snd $ head $ res)
    where res = filter (\(k, _) -> k == key) dictionary


  updateVarTypeTable :: String -> Type -> VarTypeTable -> VarTypeTable
  updateVarTypeTable var type_ varTypeTable | (var, type_) `elem` varTypeTable = varTypeTable
                                            | otherwise = varTypeTable ++ [(var, type_)]

  varTypeTableLookUp :: String -> VarTypeTable -> Maybe Type
  varTypeTableLookUp key varTypeTable | null res = Nothing
                                      | otherwise = Just (snd $ head res)
    where res = filter (\(k, _) -> k == key) varTypeTable
