module Compiler.AstToClassFileTranslator.GenerateAbstractClassFile where
    import Compiler.AbstractBytecode
    import Data.Maybe
    import Compiler.Ast
    import Compiler.AstToClassFileTranslator.ConstantPoolGenerator
    import Compiler.AstToClassFileTranslator.Stack
    import Data.Word (Word8, Word16)
    import Compiler.Instructions
    import Compiler.AstToClassFileTranslator.TranslateMethodBody
    import Compiler.AstToClassFileTranslator.MaxStackCalculator
    
    generateAccessFlagClass :: ClassFileAccessFlag
    generateAccessFlagClass = SUPER
    
    generateMagicNumber :: Magic
    generateMagicNumber = Magic
    
    generateMinorVersion :: MinorVersion
    generateMinorVersion = MinorVersion
    
    generateMajorVersion :: MajorVersion
    generateMajorVersion = MajorVersion
    
    getDefaultConstructorEntry :: Instructions
    getDefaultConstructorEntry = [(Aload_0), (Invokespecial 1)] 
    
    translateModClassToClassAccessFlags :: Mod -> ClassFileAccessFlag
    translateModClassToClassAccessFlags Public = PUBLIC
    translateModClassToClassAccessFlags Abstract = ABSTRACT
    
    translateModFieldToFieldAccessFlag :: Mod  -> FieldAccessFlag
    translateModFieldToFieldAccessFlag Public = F_PUBLIC
    translateModFieldToFieldAccessFlag Protected = F_PROTECTED
    translateModFieldToFieldAccessFlag Private = F_PRIVATE
    translateModFieldToFieldAccessFlag Static = F_STATIC
    
    translateModMethodToMethodAccessFlag :: Mod -> MethodAccessFlag
    translateModMethodToMethodAccessFlag Public = M_PUBLIC
    translateModMethodToMethodAccessFlag Protected = M_PROTECTED
    translateModMethodToMethodAccessFlag Private = M_PRIVATE
    translateModMethodToMethodAccessFlag Static = M_STATIC
    translateModMethodToMethodAccessFlag Abstract = M_ABSTRACT
    
    
    translateListOfModClassToListOfClassAccessFlags :: [Mod] -> ClassFileAccessFlags
    translateListOfModClassToListOfClassAccessFlags mds = (map translateModClassToClassAccessFlags mds)
    
    translateListOfModFieldToListOfFieldAccessFlag :: [Mod] -> FieldAccessFlags
    translateListOfModFieldToListOfFieldAccessFlag [] = [(translateModFieldToFieldAccessFlag Private)]
    translateListOfModFieldToListOfFieldAccessFlag mds = (map translateModFieldToFieldAccessFlag mds)
    
    translateListOfModMethodToListOfMethodAccessFlag :: [Mod] -> MethodAccessFlags
    translateListOfModMethodToListOfMethodAccessFlag [] = [(translateModMethodToMethodAccessFlag Private)]
    translateListOfModMethodToListOfMethodAccessFlag mds = (map translateModMethodToMethodAccessFlag mds)
    
    translateThisToClassThisClass :: ThisClass
    translateThisToClassThisClass = (ThisClass (fromIntegral 2))
    
    translateSuperToClassSuperClass :: SuperClass
    translateSuperToClassSuperClass = (SuperClass (fromIntegral 3))
    
    returnConstructorListOfDeclList :: [Decl] -> [Decl] 
    returnConstructorListOfDeclList xs = (filter isConstructor xs)
    
    returnMethodListOfDeclList :: [Decl] -> [Decl] 
    returnMethodListOfDeclList xs = (filter isMethod xs)
    
    returnFieldsOfAstDeclList :: [Decl] -> [Decl] 
    returnFieldsOfAstDeclList xs = (filter isField xs)
    
    isConstructor :: Decl -> Bool
    isConstructor (Constructor _ _ _ _) = True
    isConstructor _  = False
    
    isMethod :: Decl -> Bool
    isMethod (Compiler.Ast.Method _ _ _ _ _) = True
    isMethod _  = False
    
    isField :: Decl -> Bool
    isField (Compiler.Ast.Field _) = True
    isField _ = False
    
    translateMethods :: [Decl] -> Dictionary -> [Constant] -> [Compiler.AbstractBytecode.Method]
    translateMethods [] dic constants = []
    translateMethods (x : []) dic constants = [(translateMethodToAbstractByteCodeMethod x dic constants)]
    translateMethods (x : xs) dic constants = [(translateMethodToAbstractByteCodeMethod x dic constants)] ++ (translateMethods xs dic constants)
    
    translateMethodToAbstractByteCodeMethod :: Decl -> Dictionary -> [Constant] -> Compiler.AbstractBytecode.Method
    translateMethodToAbstractByteCodeMethod (Compiler.Ast.Method (Identifier name) mods retTyp params bdy) dic cp = let
        methodAccessFlags = (translateListOfModMethodToListOfMethodAccessFlag mods)
        methodNameIndex = (fromJust (cpLookUp (CONSTANT_Utf8 name) cp))
        methodDescriptorIndex = (fromJust (cpLookUp  (CONSTANT_Utf8 (methodDescriptorToString (map getTypeOfTypeIdenTuple params) retTyp)) cp))
    
        iniLstVarTypTupel = (getInitialLstVarTypTupelFromPassingParam params 1)
    
        methodAttribute = let
            codeNameIndex = (6 :: Word16) 
            code = let
                translatedBody | (isNothing bdy) = [(Compiler.Instructions.Return)]
                               | otherwise = (getInstructionListOfReturnTupel (translateStatement (fromJust bdy) dic iniLstVarTypTupel emptyStack [])) ++ [(Compiler.Instructions.Return)]
                in (translatedBody)
            maxStack =  (evalInstructionStackImpact code) + 1
            maxLocals = (Prelude.length params) + 1
            exceptionTables = []
            attributes = []
            in [(Code codeNameIndex (fromIntegral maxStack) (fromIntegral maxLocals) code exceptionTables attributes)]
        in (Compiler.AbstractBytecode.Method methodAccessFlags (fromIntegral methodNameIndex) (fromIntegral methodDescriptorIndex) methodAttribute)
    
    
    getMaxLocals :: Instructions -> Word8 -> Word16
    getMaxLocals [] acc = (fromIntegral acc)
    getMaxLocals ((Astore_0): xs) acc = (getMaxLocals xs(acc + 1))
    getMaxLocals ((Astore_1) : xs) acc = (getMaxLocals xs(acc + 1))
    getMaxLocals ((Astore_2) : xs) acc = (getMaxLocals xs(acc + 1))
    getMaxLocals ((Astore_3) : xs) acc = (getMaxLocals xs(acc + 1))
    getMaxLocals ((Astore x) : xs) acc = (getMaxLocals xs(acc + 1))
    getMaxLocals ((Istore_0) : xs) acc = (getMaxLocals xs(acc + 1))
    getMaxLocals ((Istore_1) : xs) acc = (getMaxLocals xs(acc + 1))
    getMaxLocals ((Istore_2) : xs) acc = (getMaxLocals xs(acc + 1))
    getMaxLocals ((Istore_3) : xs) acc = (getMaxLocals xs(acc + 1))
    getMaxLocals ((Istore x) : xs) acc = (getMaxLocals xs(acc + 1))
    getMaxLocals (_ : xs) acc = (getMaxLocals xs acc)
    
    
    translateListOfConstructors :: [Decl] -> [Decl] -> Dictionary -> [Constant] -> [Compiler.AbstractBytecode.Method]
    translateListOfConstructors [] field dictionary  cp = []
    translateListOfConstructors (x : []) field dictionary cp = [(translateConstructorToAbstractByteCodeMethod x field dictionary cp)]
    translateListOfConstructors (x : xs) field dictionary cp = [(translateConstructorToAbstractByteCodeMethod x field dictionary cp)] ++ (translateListOfConstructors xs field dictionary cp)
    
    translateConstructorToAbstractByteCodeMethod :: Decl -> [Decl] -> Dictionary  -> [Constant] -> Compiler.AbstractBytecode.Method
    translateConstructorToAbstractByteCodeMethod (Constructor _ mods params bdy) fields dic cp = let   
        methodAccessFlags = [M_PUBLIC]
        methodNameIndex = (4 :: Word16) 
        methodDescriptorIndex = (fromJust (cpLookUp  (CONSTANT_Utf8 (methodDescriptorToString (map getTypeOfTypeIdenTuple params) JVoid)) cp))
    
    
        iniLstVarTypTupel = (getInitialLstVarTypTupelFromPassingParam params 1)
    
        methodAttribute = let
            codeNameIndex = (6 :: Word16) 
            code = let
                defaultConstructingValue = getDefaultConstructorEntry
                initializeGlobalFields = (getInitForGlobalFields fields dic defaultConstructingValue)
                translatedBody | (isNothing bdy) = initializeGlobalFields ++ [(Compiler.Instructions.Return)]
                               | otherwise = (getInstructionListOfReturnTupel (translateStatement (fromJust bdy) dic iniLstVarTypTupel emptyStack initializeGlobalFields)) ++ [(Compiler.Instructions.Return)]
                in (translatedBody)
            maxStack = (evalInstructionStackImpact code) + 1
            maxLocals = (Prelude.length params) + 1
            exceptionTables = []
            attributes = []
            in [(Code codeNameIndex (fromIntegral maxStack) (fromIntegral maxLocals) code exceptionTables attributes)]
        in (Compiler.AbstractBytecode.Method methodAccessFlags methodNameIndex (fromIntegral methodDescriptorIndex) methodAttribute)
    
    
    
    
    getInitialLstVarTypTupelFromPassingParam :: [(Type, Identifier)] -> Word8 -> [ListVarTypTupel]
    getInitialLstVarTypTupelFromPassingParam [] _ = []
    getInitialLstVarTypTupelFromPassingParam ((typ, (Identifier name)) : []) acc = [(name, acc, typ)]
    getInitialLstVarTypTupelFromPassingParam ((typ, (Identifier name)) : xs) acc =  [(name, acc, typ)] ++ (getInitialLstVarTypTupelFromPassingParam xs (acc + 1))
    
                               
    getParamOfTypeIdenTuple :: (Type, Identifier) -> Identifier
    getParamOfTypeIdenTuple (_, iden ) = iden
    
    getInitForGlobalFields :: [Decl] -> Dictionary -> Instructions -> Instructions
    getInitForGlobalFields decls dic instructionList = (foldr (\d acc -> (translateField d dic acc)) instructionList decls)
    
    translateField :: Decl -> Dictionary -> Instructions -> Instructions
    translateField (Compiler.Ast.Field (VarDecl (Identifier name) _ _ bdy)) dic instructionList = let 
        translatedBody  | (isNothing bdy) = [] 
                        | otherwise = (getInstructionListOfReturnTupel (translateExpression (fromJust bdy) dic [] emptyStack (instructionList ++ [Aload_0])))
        saveInstr | (Prelude.length translatedBody == 0) = []
                  | otherwise =  translatedBody ++ [(Putfield (fromIntegral (fromJust (dictLookUp ("Field this." ++ name) dic))))]
        in saveInstr
    
    
       
    
        
    
    
    getTypeOfTypeIdenTuple :: (Type, Identifier) -> Type
    getTypeOfTypeIdenTuple (typ, _ ) = typ
            
    translateFieldToAbstractClassField :: Decl -> [Constant] -> Compiler.AbstractBytecode.Field
    translateFieldToAbstractClassField (Compiler.Ast.Field (VarDecl (Identifier name) mods typ rhs)) cp = let
        fieldAccessFlags = (translateListOfModFieldToListOfFieldAccessFlag mods)
        fieldNameIndex = (fromJust (cpLookUp (CONSTANT_Utf8 name) cp))
        fieldDescriptorIndex = (fromJust (cpLookUp (CONSTANT_Utf8 (typeToString typ)) cp))
        fieldAttributes = []
        in (Compiler.AbstractBytecode.Field fieldAccessFlags (fromIntegral fieldNameIndex) (fromIntegral fieldDescriptorIndex) fieldAttributes)
    
    checkIfConstructorNoParamsExists :: Decl -> Bool
    checkIfConstructorNoParamsExists (Constructor _ _ params _) = (null params)
    checkIfConstructorNoParamsExists _ = False
    
    doesEmptyConstructorExists :: [Decl] -> Bool
    doesEmptyConstructorExists constList = (not (null (filter checkIfConstructorNoParamsExists constList)))
    
    
    createStandardConstructor :: Decl
    createStandardConstructor = let
        idenList = (Identifier "standard constructor")
        modsList = [Public]
        paramList = []
        bdy = Nothing
        in (Constructor idenList modsList paramList bdy)
    
    translateToAbstractClassFile :: Compiler.Ast.Class -> ClassFile
    translateToAbstractClassFile class_@(Compiler.Ast.Class className accessFlags decls) = let
        magic = generateMagicNumber
        minver = generateMinorVersion
        maxver = generateMajorVersion
        
        cpDictonaryTupel = (generateCP class_) 
        constantPool = (fst cpDictonaryTupel)
        dictionary = (snd cpDictonaryTupel)
        
    
        array_cp = (fst cpDictonaryTupel)
    
        classFileAccesFlags = (translateListOfModClassToListOfClassAccessFlags accessFlags) ++ [generateAccessFlagClass]
        this = translateThisToClassThisClass
        super = translateSuperToClassSuperClass
        array_interface = []
    
    
        globalFieldList = (returnFieldsOfAstDeclList decls)
    
        array_fields = (map (\g -> (translateFieldToAbstractClassField g constantPool)) globalFieldList)
                     
     
    
        listOfConstructors = (returnConstructorListOfDeclList decls)
        listOfMethods  =  (returnMethodListOfDeclList decls)
    
        array_methods | (doesEmptyConstructorExists listOfConstructors) = let
                        methodListConstr = (translateListOfConstructors listOfConstructors globalFieldList dictionary constantPool)
                        methodListNormal = (translateMethods listOfMethods dictionary constantPool)
                        methodList = methodListConstr ++ methodListNormal
                        in methodList
                     | otherwise =  let
                        listOfConstructorsWithEmpty = listOfConstructors ++ [createStandardConstructor]
                        methodListConstr = (translateListOfConstructors listOfConstructorsWithEmpty globalFieldList dictionary constantPool)
                        methodListNormal = (translateMethods listOfMethods dictionary constantPool)
                        methodList = methodListConstr ++ methodListNormal
                        in methodList
                                   
        array_attributes = []
      in (ClassFile magic minver maxver array_cp classFileAccesFlags this super array_interface array_fields array_methods array_attributes)
