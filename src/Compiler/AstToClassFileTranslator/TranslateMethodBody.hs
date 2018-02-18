module Compiler.AstToClassFileTranslator.TranslateMethodBody where
import Compiler.AbstractBytecode
import Compiler.Instructions
import Compiler.Ast
import Compiler.AstToClassFileTranslator.InstructionsToLength
import Data.Char
import Data.Word (Word8, Word16)
import Data.Int (Int16, Int32, Int64)
import Data.Maybe
import Compiler.AstToClassFileTranslator.ConstantPoolGenerator

type ListVarTypTupel = (String, Word8, Type)  
type ReturnValueTupel = ([ListVarTypTupel], Instructions)




-- Identifier = Variable
-- Literal = "test", 1, 's', true, false, ...


                                            -- STATEMENT TRANSLATION --
translateStatement :: Compiler.Ast.Statement -> Dictionary -> [ListVarTypTupel]   -> Instructions -> ReturnValueTupel
translateStatement (TypedStatement ((While cnd bdy), typ)) constantPool lstVarTypTupel   instructionsList = (translateWhileStatement cnd bdy constantPool lstVarTypTupel   instructionsList)
translateStatement (TypedStatement ((If cnd thn els), typ)) constantPool lstVarTypTupel   instructionsList = (translateIfStatement cnd thn els constantPool lstVarTypTupel   instructionsList)
translateStatement (TypedStatement ((Block stmt), typ)) constantPool lstVarTypTupel   instructionsList = (translateBlockStatement stmt constantPool lstVarTypTupel   instructionsList)
translateStatement (TypedStatement ((Compiler.Ast.Return maybeExpr), typ)) constantPool lstVarTypTupel   instructionsList = (translateReturnStatement maybeExpr typ constantPool lstVarTypTupel   instructionsList)
translateStatement (TypedStatement ((Continue), typ)) constantPool lstVarTypTupel   instructionsList = (translateContinueStatement lstVarTypTupel   instructionsList)
translateStatement (TypedStatement ((LocalVar lclVar), typ)) constantPool lstVarTypTupel   instructionsList = (translateLocalVarStatement lclVar constantPool lstVarTypTupel   instructionsList)
translateStatement (TypedStatement ((StmtExprStmt stmtExpr), typ)) constantPool lstVarTypTupel   instructionsList = 
    (translateStatementExpresion stmtExpr constantPool lstVarTypTupel   instructionsList)
                                                                                                                               
translateReturnStatement :: Maybe Expression -> Type -> Dictionary -> [ListVarTypTupel]   -> Instructions -> ReturnValueTupel
translateReturnStatement (Just expr) (PrimType _) constantPool lstVarTypTupel   instructionsList = let
    translatedExpression = (getInstructionListOfReturnTupel (translateExpression expr constantPool lstVarTypTupel   instructionsList))
    newInstructionLstWithReturn = translatedExpression ++ [(Ireturn)]
    in (lstVarTypTupel, newInstructionLstWithReturn)
translateReturnStatement (Just expr) (RefType _) constantPool lstVarTypTupel   instructionsList = let
    translatedExpression = (getInstructionListOfReturnTupel (translateExpression expr constantPool lstVarTypTupel   instructionsList))
    newInstructionLstWithReturn = translatedExpression ++ [(Areturn)]
    in (lstVarTypTupel, newInstructionLstWithReturn)
translateReturnStatement (Just expr) (JVoid) constantPool lstVarTypTupel   instructionsList = let 
    newInstructionLstWithReturn = instructionsList ++ [(Compiler.Instructions.Return)]
    in (lstVarTypTupel, newInstructionLstWithReturn)
translateReturnStatement Nothing _ constantPool lstVarTypTupel   instructionsList = let
    newInstructionLstWithReturn = instructionsList ++ [(Compiler.Instructions.Return)]
    in (lstVarTypTupel, newInstructionLstWithReturn)

translateContinueStatement :: [ListVarTypTupel]  -> Instructions -> ReturnValueTupel
translateContinueStatement lstVarTypTupel   instructionsList = let
    gtoAdr = (-1) * (((getInstructionSize (Ifne 1))  + (sumInstructionSizeInInstructionList instructionsList))) 
    instrListWithContinue = instructionsList ++ [(Goto (fromIntegral gtoAdr))] 
    in (lstVarTypTupel, instrListWithContinue)


translateWhileStatement :: Expression -> Maybe Statement -> Dictionary -> [ListVarTypTupel]  -> Instructions -> ReturnValueTupel
translateWhileStatement cnd (Just bdy) constantPool lstVarTypTupel   instructionsList = let
    translatedCondition = (getInstructionListOfReturnTupel (translateExpression cnd constantPool lstVarTypTupel   []))
    translatedBody = (getInstructionListOfReturnTupel (translateStatement bdy constantPool lstVarTypTupel  translatedCondition)) 
    splittedList = (splitAt (length translatedCondition) translatedBody)
    
    gtoAdr = (-1) * (((getInstructionSize (Ifne 1))  + (sumInstructionSizeInInstructionList translatedBody))) 
    gto = [(Goto (fromIntegral gtoAdr))] 
    
    ifFalseAddress = (sumInstructionSizeInInstructionList (snd splittedList))  + (getInstructionSize (Goto 1)) +  (getInstructionSize (Ifne 1))  

    finishedWhile = instructionsList ++ (fst splittedList) ++ [(Ifeq ifFalseAddress)] ++ (snd splittedList) ++ gto
    in (lstVarTypTupel, finishedWhile)

translateWhileStatement cnd Nothing constantPool lstVarTypTupel   instructionsList = let
    translatedCondition = (getInstructionListOfReturnTupel (translateExpression cnd constantPool lstVarTypTupel   [])) 
    condLength = (sumInstructionSizeInInstructionList translatedCondition) 
    condFinished = instructionsList ++ translatedCondition ++ [(Ifne (-condLength))] 
    in (lstVarTypTupel, condFinished)



translateBlockStatement :: [Statement] -> Dictionary -> [ListVarTypTupel]   -> Instructions -> ReturnValueTupel
translateBlockStatement [] constantPool lstVarTypTupel   instructionsList = (lstVarTypTupel, instructionsList)
translateBlockStatement (x : []) constantPool lstVarTypTupel   instructionsList = (translateStatement x constantPool lstVarTypTupel   instructionsList)
translateBlockStatement (x : xs ) constantPool lstVarTypTupel   instructionsList = let
    translatedFirstStatement = (translateStatement x constantPool lstVarTypTupel  instructionsList)
    translatedRest = (translateBlockStatement xs constantPool (getLocalVarIndexListOfReturnTupel translatedFirstStatement) (getInstructionListOfReturnTupel translatedFirstStatement))
    in (lstVarTypTupel, (getInstructionListOfReturnTupel translatedRest))

translateIfStatement :: Expression -> Maybe Statement -> Maybe Statement -> Dictionary -> [ListVarTypTupel]   -> Instructions -> ReturnValueTupel 
translateIfStatement cnd Nothing Nothing constantPool lstVarTypTupel   instructionsList = (translateExpression cnd constantPool lstVarTypTupel   instructionsList) -- könnte wegoptimiert werden
translateIfStatement cnd (Just thn) Nothing constantPool lstVarTypTupel   instructionsList = let
    translatedConditionInstructionLst = (getInstructionListOfReturnTupel (translateExpression cnd constantPool lstVarTypTupel   [])) 
    thenInstructionList = (getInstructionListOfReturnTupel (translateStatement thn constantPool lstVarTypTupel   []))
    ifFalseAdr = (sumInstructionSizeInInstructionList thenInstructionList) + (getInstructionSize (Ifeq 1)) 
    finishedIfThen = instructionsList ++ translatedConditionInstructionLst ++ [(Ifeq ifFalseAdr)] ++ thenInstructionList 
    in (lstVarTypTupel, finishedIfThen)
translateIfStatement cnd (Just thn) (Just els) constantPool lstVarTypTupel   instructionsList = let
    translatedConditionInstructionLst = (getInstructionListOfReturnTupel (translateExpression cnd constantPool lstVarTypTupel   [])) 
    thenInstructionList = (getInstructionListOfReturnTupel (translateStatement thn constantPool lstVarTypTupel   []))
    translatedElseStatement = (getInstructionListOfReturnTupel (translateStatement els constantPool lstVarTypTupel   []))
    ifFalseAdr =  (getInstructionSize (Ifeq 1)) + (sumInstructionSizeInInstructionList thenInstructionList) + (getInstructionSize (Goto 1)) + (sumInstructionSizeInInstructionList translatedElseStatement) 
    gtoAdr = (sumInstructionSizeInInstructionList translatedElseStatement)  + (getInstructionSize (Goto 1)) 
    finishedIfThenElse = instructionsList ++ translatedConditionInstructionLst ++ [(Ifeq ifFalseAdr)] ++ thenInstructionList ++ [(Goto gtoAdr)] ++ translatedElseStatement 

    in (lstVarTypTupel, finishedIfThenElse)

translateLocalVarStatement :: Compiler.Ast.VarDecl -> Dictionary -> [ListVarTypTupel]   -> Instructions -> ReturnValueTupel
translateLocalVarStatement (VarDecl identifier mods typ (Just expr)) cp lstVarTypTupel   instructionList = let
  rhs = (translateExpression expr cp lstVarTypTupel instructionList)
  number = (Just (getNextFreeIndex lstVarTypTupel 0))
  addedListVatTupel = lstVarTypTupel ++ [((identifierToString [identifier]),(fromIntegral (fromJust number)), typ)]
  updatedList = (getStoreCommandLocal number typ cp addedListVatTupel (getInstructionListOfReturnTupel rhs))
  in updatedList
translateLocalVarStatement (VarDecl identifier mods typ Nothing) cp lstVarTypTupel   instructionList = let
  number = (Just (getNextFreeIndex lstVarTypTupel 0))
  addedListVatTupel = lstVarTypTupel ++ [((identifierToString [identifier]), (fromIntegral (fromJust number)), typ)]
  updatedList = (getStoreCommandLocal number typ cp addedListVatTupel instructionList)
  in updatedList

                                            -- EXPRESSION TRANSLATION --

translateExpression :: Compiler.Ast.Expression -> Dictionary -> [ListVarTypTupel]   -> Instructions -> ReturnValueTupel
translateExpression (TypedExpression ((TernaryIf cond els thn), typ)) constantPool lstVarTypTupel   instructionsList = (translateTernaryIf cond els thn constantPool lstVarTypTupel   instructionsList)
translateExpression (TypedExpression ((PrimBinOp binOp exprL exprR), typ)) constantPool lstVarTypTupel   instructionsList = (translatePrimBinOp binOp exprL exprR constantPool lstVarTypTupel   instructionsList)
translateExpression (TypedExpression ((PrimUnOp unOp expr), typ)) constantPool lstVarTypTupel   instructionsList = (translatePrimUnOp unOp expr constantPool lstVarTypTupel   instructionsList)
translateExpression (TypedExpression ((Iden name), typ)) constantPool lstVarTypTupel   instructionsList = (translateExpressionIden name typ constantPool lstVarTypTupel instructionsList) 
translateExpression (TypedExpression ((Literal lit), typ)) constantPool lstVarTypTupel   instructionsList = (translateExpressionLiteral constantPool lstVarTypTupel instructionsList lit)
translateExpression (TypedExpression ((ExprExprStmt stmtExpr), typ)) constantPool lstVarTypTupel   instructionsList = (translateStatementExpresion stmtExpr constantPool lstVarTypTupel   instructionsList)
translateExpression (TypedExpression ((Cast typ1 expr), typ2)) constantPool lstVarTypTupel   instructionsList = (translateExpressionCast typ1 expr constantPool lstVarTypTupel   instructionsList)


translateExpressionCast :: Compiler.Ast.Type -> Compiler.Ast.Expression -> Dictionary -> [ListVarTypTupel]   -> Instructions -> ReturnValueTupel
translateExpressionCast (PrimType Char) expr cp lstVarTypTupel   instructionsList = let
    e = (translateExpression expr cp lstVarTypTupel   instructionsList)
    in ((getLocalVarIndexListOfReturnTupel e), ((getInstructionListOfReturnTupel e) ++ [I2C]))
translateExpressionCast (PrimType Int) expr cp lstVarTypTupel   instructionsList = let
    e = (translateExpression expr cp lstVarTypTupel   instructionsList)
    in e
translateExpressionCast (RefType nme) (TypedExpression (expr, (PrimType Int))) cp lstVarTypTupel   instructionsList = let
    e = (translateExpression expr cp lstVarTypTupel   instructionsList)
    in ((getLocalVarIndexListOfReturnTupel e),((getInstructionListOfReturnTupel e) ++ [Invokestatic 10])) 

translateExpressionCast (RefType nme) (TypedExpression (expr, (PrimType Char))) cp lstVarTypTupel   instructionsList = let
    e = (translateExpression expr cp lstVarTypTupel   instructionsList)
    in ((getLocalVarIndexListOfReturnTupel e),((getInstructionListOfReturnTupel e) ++ [Invokestatic 11])) 

translateExpressionCast (RefType nme) (TypedExpression (expr, (PrimType Boolean))) cp lstVarTypTupel   instructionsList = let
    e = (translateExpression expr cp lstVarTypTupel   instructionsList)
    in ((getLocalVarIndexListOfReturnTupel e),((getInstructionListOfReturnTupel e) ++ [Invokestatic 12])) 
translateExpressionCast (RefType nme) (TypedExpression (expr, (RefType name))) cp lstVarTypTupel   instructionsList = let
    e = (translateExpression expr cp lstVarTypTupel   instructionsList)
    in e


translateStatementExpresion :: StmtExpr -> Dictionary -> [ListVarTypTupel]   -> Instructions -> ReturnValueTupel
translateStatementExpresion (TypedStmtExpr ((Assign assignOp nme expr), typ)) cp lstVarTypTupel   instructionsList =
    (translateAssignStmtExpr assignOp nme expr typ cp lstVarTypTupel   instructionsList) 
translateStatementExpresion (TypedStmtExpr ((Instantiation nme expr), typ)) cp lstVarTypTupel   instructionsList = 
    (translateInstantiationStmtExpr nme expr cp lstVarTypTupel instructionsList typ)
translateStatementExpresion (TypedStmtExpr ((Apply expr exprsns), typ)) cp lstVarTypTupel   instructionsList = 
    (translateApplyExpression expr exprsns cp lstVarTypTupel  instructionsList typ)  
translateStatementExpresion (TypedStmtExpr ((SEUnOp incrOrDecr expr), typ)) cp lstVarTypTupel   instructionsList = 
    (translateSEUnOpExprssion incrOrDecr expr cp lstVarTypTupel instructionsList)

translateApplyExpression :: Expression -> [Expression] -> Dictionary -> [ListVarTypTupel]  -> Instructions -> Type -> ReturnValueTupel
translateApplyExpression (Iden (Name [This] (Identifier methodName))) passedToList cp lstVarTypTupel instructionsList typ = let    
    translatedPassedToList = (getInstructionListOfReturnTupel (translateListOfExpressions passedToList cp lstVarTypTupel   (instructionsList ++ [Aload_0])))
  
    getCpIndexOfMethod = (fromJust (dictLookUp ("Method " ++ "this." ++  methodName ++ ":" ++ (mapParametersMethodToDiscriptor passedToList typ)) cp))
                                        
    ivokeMethod = translatedPassedToList ++ [(Invokevirtual (fromIntegral getCpIndexOfMethod))]
    
    in (lstVarTypTupel, ivokeMethod)

translateApplyExpression (Iden (Name [idl@(Identifier pth)] (Identifier methodName))) passedToList cp lstVarTypTupel instructionsList typ = let    
    pthPth | (length pth) == 0 = "this" 
           | otherwise = (removeFirstAndListItemOfList (typeToString (fromJust (searchTypInListVarTypTupel idl lstVarTypTupel)))) 
    
    aloadCommand = (getInstructionListOfReturnTupel (getLoadRefTypeFromLocalVar (fromJust (searchIdentInListVarTypTupel idl lstVarTypTupel)) lstVarTypTupel []))
    translatedPassedToList = (getInstructionListOfReturnTupel (translateListOfExpressions passedToList cp lstVarTypTupel   (instructionsList ++ aloadCommand)))

    
    getCpIndexOfMethod = (fromJust (dictLookUp ("Method " ++ pthPth ++ "." ++  methodName ++ ":" ++ (mapParametersMethodToDiscriptor passedToList typ)) cp))
                                        
    ivokeMethod = translatedPassedToList ++ [(Invokevirtual (fromIntegral getCpIndexOfMethod))]
    
    in (lstVarTypTupel, ivokeMethod)

removeFirstAndListItemOfList :: String -> String
removeFirstAndListItemOfList x = (snd (splitAt  1 (init x)))


translateSEUnOpExprssion :: IncrOrDecr -> Expression -> Dictionary -> [ListVarTypTupel]  -> Instructions -> ReturnValueTupel
translateSEUnOpExprssion (PreIncr) (TypedExpression ((Iden (Name [] iden@(Identifier name))), _ )) cp lstVarTypTupel  instructionsList = let   
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel) 
    -- index von wert wenn dieser global gespeichert ist
    indexInConstantPool = (dictLookUp ("Field this." ++ name) cp)  
                -- globale variable  
    preincr     | (isNothing indexOfValueInLstVarTypTupel)  = let
                    incr = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool))), (Iconst_1), (Iadd), (Putfield (fromIntegral (fromJust indexInConstantPool)))]
                    in incr
                -- lokale variable
                | otherwise = let
                    incr = instructionsList ++ [(Iinc (fromJust indexOfValueInLstVarTypTupel) 1)]
                    in incr
    in (lstVarTypTupel, preincr)

translateSEUnOpExprssion (PostIncr) (TypedExpression ((Iden (Name [] iden@(Identifier name))), _ )) cp lstVarTypTupel  instructionsList = let   
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel) 
    -- index von wert wenn dieser global gespeichert ist
    indexInConstantPool = (dictLookUp ("Field this." ++ name) cp)  
                -- globale variable  
    preincr     | (isNothing indexOfValueInLstVarTypTupel)  = let
                    incr = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool))), (Iconst_1), (Iadd), (Putfield (fromIntegral (fromJust indexInConstantPool)))]
                    in incr
                -- lokale variable
                | otherwise = let
                    incr = instructionsList ++ [(Iinc (fromJust indexOfValueInLstVarTypTupel) 1)]
                    in incr
    in (lstVarTypTupel, preincr)
translateSEUnOpExprssion (PreDecr) (TypedExpression ((Iden (Name [] iden@(Identifier name))), _ )) cp lstVarTypTupel  instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel) 
    -- index von wert wenn dieser global gespeichert ist
    indexInConstantPool = (dictLookUp ("Field this." ++ name) cp)  
                -- globale variable  
    preincr     | (isNothing indexOfValueInLstVarTypTupel)  = let
                    incr = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool))), (Iconst_1), (Isub), (Putfield (fromIntegral (fromJust indexInConstantPool)))]
                    in incr
                -- lokale variable
                | otherwise = let
                    incr = instructionsList ++ [(Iinc (fromJust indexOfValueInLstVarTypTupel) (-1))]
                    in incr
    in (lstVarTypTupel, preincr)   
translateSEUnOpExprssion (PostDecr) (TypedExpression ((Iden (Name [] iden@(Identifier name))), _ )) cp lstVarTypTupel  instructionsList = let 
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel) 
    -- index von wert wenn dieser global gespeichert ist
    indexInConstantPool = (dictLookUp ("Field this." ++ name) cp)  
                -- globale variable  
    preincr     | (isNothing indexOfValueInLstVarTypTupel)  = let
                    incr = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool))), (Iconst_1), (Isub), (Putfield (fromIntegral (fromJust indexInConstantPool)))]
                    in incr
                -- lokale variable
                | otherwise = let
                    incr = instructionsList ++ [(Iinc (fromJust indexOfValueInLstVarTypTupel) (-1))]
                    in incr
    in (lstVarTypTupel, preincr)  


translateInstantiationStmtExpr :: Name -> [Expression]  -> Dictionary -> [ListVarTypTupel] -> Instructions -> Type -> ReturnValueTupel
translateInstantiationStmtExpr (Name pth (Identifier iden)) exprsns constantPool lstVarTypTupel  instructionsList name@(RefType (Name pthCls (Identifier clsName))) = let
    -- get the class ref for: "new" - Operation
    classIndexCP_OfName = (fromJust (dictLookUp ("class " ++ clsName) constantPool)) 
    -- get method ref of constructor from class
    getConstructorIndex_OfName = (fromJust (dictLookUp ("Method " ++ clsName ++ ".<init>:" ++ (mapParametersMethodToDiscriptor exprsns JVoid)) constantPool))
    -- create object and dublicate
    newAndDupList = instructionsList ++ [(New (fromIntegral classIndexCP_OfName)), (Dup)]
    -- translate Expression
    translatedExpressions = (getInstructionListOfReturnTupel (translateListOfExpressions exprsns constantPool lstVarTypTupel  []))
    -- contructor call
    contructorTranslated = newAndDupList ++  translatedExpressions ++ [(Invokespecial (fromIntegral getConstructorIndex_OfName))]
    in (lstVarTypTupel, contructorTranslated)

mapParametersMethodToDiscriptor :: [Expression] -> Type -> String
mapParametersMethodToDiscriptor exprs returnType = methodDescriptorToString (map (\(TypedExpression(_, t)) -> t) exprs) returnType

translateListOfExpressions :: [Expression] -> Dictionary -> [ListVarTypTupel]   -> Instructions -> ReturnValueTupel
translateListOfExpressions [] constantPool lstVarTypTupel   instructionsList = (lstVarTypTupel, instructionsList)
translateListOfExpressions (x : []) constantPool lstVarTypTupel   instructionsList = (translateExpression x constantPool lstVarTypTupel   instructionsList)
translateListOfExpressions (x : xs ) constantPool lstVarTypTupel   instructionsList = let
    translatedFirstExpression = (translateExpression x constantPool lstVarTypTupel   instructionsList)
    translatedRest = (translateListOfExpressions xs constantPool lstVarTypTupel   (getInstructionListOfReturnTupel translatedFirstExpression))
    in (lstVarTypTupel, (getInstructionListOfReturnTupel translatedRest))


translateAssignStmtExpr :: AssignOp -> Name -> Expression -> Type -> Dictionary -> [ListVarTypTupel]   -> Instructions -> ReturnValueTupel
translateAssignStmtExpr (NormalAssign) (Name pth iden@(Identifier name)) expr typ cp lstVarTypTupel   instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel) 
    -- index von wert wenn dieser global gespeichert ist
    maybeThis | (null pth) = "this"
              | (isIdenThis (head pth)) = "this"
              | otherwise = (identifierToString  pth)

    indexInConstantPool = (dictLookUp ("Field " ++ maybeThis ++ "." ++ name) cp)    
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel)  = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0)]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadFieldInstrLst))
                           saveInFieldInstr = translatedAssignmentExpression ++ [(Putfield (fromIntegral (fromJust indexInConstantPool)))]      -- is in constantenpool
                           in saveInFieldInstr
                                -- lokale variable
                          | otherwise = let
                            translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   instructionsList))
                            saveToLoacVarInstr = (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel translatedAssignmentExpression))
                            in saveToLoacVarInstr

   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (MultiplyAssign) (Name pth iden@(Identifier name)) expr typ cp lstVarTypTupel   instructionsList = let
     -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
     -- index von wert wenn dieser global gespeichert ist
    maybeThis | (null pth) = "this"
              | (isIdenThis (head pth)) = "this"
              | otherwise = (identifierToString  pth)

    indexInConstantPool = (dictLookUp ("Field " ++ maybeThis ++ "." ++ name) cp)                 
                                 -- globale variable, speichere mit putfield  
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                            loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                            translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadFieldInstrLst))
                            mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Imul), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                            in mulAndSaveInFieldInstr
                                 -- lokale variable
                           | (otherwise) = let
                            loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                            translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadLocalVarInstLst))
                            mulInstruction = translatedAssignmentExpression ++ [(Imul)]
                            saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                            in saveInLocalVarInstr
    in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (DivideAssign) (Name pth iden@(Identifier name)) expr typ cp lstVarTypTupel   instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    maybeThis | (null pth) = "this"
              | (isIdenThis (head pth)) = "this"
              | otherwise = (identifierToString  pth)
    indexInConstantPool = (dictLookUp ("Field " ++ maybeThis ++ "." ++ name) cp)    
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Idiv), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Idiv)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (ModuloAssign) (Name pth iden@(Identifier name)) expr typ cp lstVarTypTupel   instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    maybeThis | (null pth) = "this"
              | (isIdenThis (head pth)) = "this"
              | otherwise = (identifierToString  pth)
    indexInConstantPool = (dictLookUp ("Field " ++ maybeThis ++ "." ++ name) cp)        
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Irem), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Irem)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (PlusAssign) (Name pth iden@(Identifier name)) expr typ cp lstVarTypTupel   instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    maybeThis | (null pth) = "this"
              | (isIdenThis (head pth)) = "this"
              | otherwise = (identifierToString  pth)
    indexInConstantPool = (dictLookUp ("Field " ++ maybeThis ++ "." ++ name) cp)        
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Iadd), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Iadd)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (MinusAssign) (Name pth iden@(Identifier name)) expr typ cp lstVarTypTupel   instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    maybeThis | (null pth) = "this"
              | (isIdenThis (head pth)) = "this"
              | otherwise = (identifierToString  pth)
    indexInConstantPool = (dictLookUp ("Field " ++ maybeThis ++ "." ++ name) cp)        
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Isub), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable,
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Isub)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (LeftShiftAssign) (Name pth iden@(Identifier name)) expr typ cp lstVarTypTupel   instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    maybeThis | (null pth) = "this"
              | (isIdenThis (head pth)) = "this"
              | otherwise = (identifierToString  pth)
    indexInConstantPool = (dictLookUp ("Field " ++ maybeThis ++ "." ++ name) cp)       
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Ishl), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Ishl)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (ShiftRightAssign) (Name pth iden@(Identifier name)) expr typ cp lstVarTypTupel   instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    maybeThis | (null pth) = "this"
              | (isIdenThis (head pth)) = "this"
              | otherwise = (identifierToString  pth)
    indexInConstantPool = (dictLookUp ("Field " ++ maybeThis ++ "." ++ name) cp)     
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Ishr), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Ishr)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (UnsignedShiftRightAssign) (Name pth iden@(Identifier name)) expr typ cp lstVarTypTupel   instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    maybeThis | (null pth) = "this"
              | (isIdenThis (head pth)) = "this"
              | otherwise = (identifierToString  pth)
    indexInConstantPool = (dictLookUp ("Field " ++ maybeThis ++ "." ++ name) cp)     
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Iushr), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Iushr)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (AndAssign) (Name pth iden@(Identifier name)) expr typ cp lstVarTypTupel   instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    maybeThis | (null pth) = "this"
              | (isIdenThis (head pth)) = "this"
              | otherwise = (identifierToString  pth)
    indexInConstantPool = (dictLookUp ("Field " ++ maybeThis ++ "." ++ name) cp)     
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Iand), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Iand)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (BitXOrAssign) (Name pth iden@(Identifier name)) expr typ cp lstVarTypTupel   instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    maybeThis | (null pth) = "this"
              | (isIdenThis (head pth)) = "this"
              | otherwise = (identifierToString  pth)
    indexInConstantPool = (dictLookUp ("Field " ++ maybeThis ++ "." ++ name) cp)     
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Ixor), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable,
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Ixor)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (OrAssign) (Name pth iden@(Identifier name)) expr typ cp lstVarTypTupel   instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    maybeThis | (null pth) = "this"
              | (isIdenThis (head pth)) = "this"
              | otherwise = (identifierToString  pth)
    indexInConstantPool = (dictLookUp ("Field " ++ maybeThis ++ "." ++ name) cp)     
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Ior), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel   loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Ior)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateTernaryIf :: Expression -> Expression -> Expression -> Dictionary -> [ListVarTypTupel]   -> Instructions -> ReturnValueTupel
translateTernaryIf cond els thn cp lstVarTypTupel   instructionsList = let 
    translatedCondition = (getInstructionListOfReturnTupel (translateExpression cond cp lstVarTypTupel   instructionsList))  
    thenInstructionList = (getInstructionListOfReturnTupel (translateExpression thn cp lstVarTypTupel   []))  
    addressToThenStatement = (getNextInstructionIndex thenInstructionList) + (getInstructionSize (Ifne 1)) + (getInstructionSize (Goto 1))  
    conditionFinished = translatedCondition ++ [(Ifne addressToThenStatement)]
    elseInstructionList = (getInstructionListOfReturnTupel (translateExpression els cp lstVarTypTupel   []))
    gotoJumpAddress = (getNextInstructionIndex elseInstructionList) + (getInstructionSize (Goto 1))
    thenFinished = thenInstructionList ++ [(Goto gotoJumpAddress)]
    finishedTranslationStmtLst = conditionFinished ++ thenFinished ++ elseInstructionList        
    in (lstVarTypTupel, finishedTranslationStmtLst)


    -- START OF PRIM BIN OP - EXPRESSION TRANSLATION --
translatePrimBinOp :: BinOp -> Expression -> Expression -> Dictionary -> [ListVarTypTupel]   -> Instructions -> ReturnValueTupel
translatePrimBinOp (And) exprL exprR cp lstVarTypTupel instructionsList = let
    translateLeft = (getInstructionListOfReturnTupel (translateExpression exprL cp lstVarTypTupel []))
    translateRight = (getInstructionListOfReturnTupel (translateExpression exprR cp lstVarTypTupel []))
    ifFirstFalseJumpLength = (sumInstructionSizeInInstructionList translateRight) + (sumInstructionSizeInInstructionList [(Ifeq 1), (Ifeq 1), (Iconst_1), (Goto 1)])
    andFinished = instructionsList ++ translateLeft ++ [(Ifeq ifFirstFalseJumpLength)] ++ translateRight ++ [(Ifeq 7), (Iconst_1), (Goto  ((sumInstructionSizeInInstructionList [(Goto 0), (Iconst_0)]))), (Iconst_0)] 
    in (lstVarTypTupel, andFinished)

translatePrimBinOp (Or) exprL exprR cp lstVarTypTupel instructionsList = let
    translateLeft = (getInstructionListOfReturnTupel (translateExpression exprL cp lstVarTypTupel []))
    translateRight = (getInstructionListOfReturnTupel (translateExpression exprR cp lstVarTypTupel []))
    ifFirstTrueJumpLength = (sumInstructionSizeInInstructionList translateRight) + (sumInstructionSizeInInstructionList [(Ifeq 1), (Ifeq 1), (Iconst_1), (Goto 1)])
    orFinished = instructionsList ++ translateLeft ++ [(Ifne ifFirstTrueJumpLength)] ++ translateRight ++ [(Ifne 7), (Iconst_0), (Goto  ((sumInstructionSizeInInstructionList [(Goto 0), (Iconst_0)]))), (Iconst_1)]  
    in (lstVarTypTupel, orFinished)
translatePrimBinOp (Eq) exprL exprR cp lstVarTypTupel   instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel   instructionsList))
    nextIndexOfInstruction = (getNextInstructionIndex leftAndRightSideTranslated)
    finalInstructionList = leftAndRightSideTranslated ++ [(If_Icmpeq 7), (Iconst_0), (Goto  ((sumInstructionSizeInInstructionList [(Goto 1), (Iconst_0)])) ), (Iconst_1)] 
    in (lstVarTypTupel, finalInstructionList) 

translatePrimBinOp (Less) exprL exprR cp lstVarTypTupel   instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel   instructionsList))
    nextIndexOfInstruction = (getNextInstructionIndex leftAndRightSideTranslated)
    finalInstructionList = leftAndRightSideTranslated ++ [(If_Icmpge 7), (Iconst_1), (Goto  ((sumInstructionSizeInInstructionList [(Goto 1), (Iconst_0)])) ), (Iconst_0)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (LessEq) exprL exprR cp lstVarTypTupel   instructionsList = let 
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel   instructionsList))
    nextIndexOfInstruction = (getNextInstructionIndex leftAndRightSideTranslated)
    finalInstructionList = leftAndRightSideTranslated ++ [(If_Icmpgt 7), (Iconst_1), (Goto  (sumInstructionSizeInInstructionList [(Goto 1), (Iconst_0)])), (Iconst_0)] 
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (Greater) exprL exprR cp lstVarTypTupel   instructionsList = let 
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel   instructionsList))
    nextIndexOfInstruction = (getNextInstructionIndex leftAndRightSideTranslated)
    finalInstructionList = leftAndRightSideTranslated ++ [(If_Icmple 7), (Iconst_1), (Goto  (sumInstructionSizeInInstructionList [(Goto 1), (Iconst_0)])), (Iconst_0)] 
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (GreaterEq) exprL exprR cp lstVarTypTupel   instructionsList = let 
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel   instructionsList))
    nextIndexOfInstruction = (getNextInstructionIndex leftAndRightSideTranslated)
    finalInstructionList = leftAndRightSideTranslated ++ [(If_Icmplt 7), (Iconst_1), (Goto  (sumInstructionSizeInInstructionList [(Goto 1), (Iconst_0)])), (Iconst_0)] 
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (Multiply) exprL exprR cp lstVarTypTupel   instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel   instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Imul)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (Divide) exprL exprR cp lstVarTypTupel   instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel   instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Idiv)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (Add) exprL exprR cp lstVarTypTupel   instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel   instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Iadd)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (Subtract) exprL exprR cp lstVarTypTupel   instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel   instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Isub)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (Modulo) exprL exprR cp lstVarTypTupel   instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel   instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Irem)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (ShiftRight) exprL exprR cp lstVarTypTupel   instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel   instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Ishr)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (ShiftLeft) exprL exprR cp lstVarTypTupel   instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel   instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Ishl)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (UnsignedShiftRight) exprL exprR cp lstVarTypTupel   instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel   instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Iushr)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (BitAnd) exprL exprR cp lstVarTypTupel   instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel   instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Iand)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (BitOr) exprL exprR cp lstVarTypTupel   instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel   instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Ior)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (BitXOr) exprL exprR cp lstVarTypTupel   instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel   instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Ixor)]
    in (lstVarTypTupel, finalInstructionList)


-- Hilfsfunktion für PrimBinOps von Expression Übersetzung
translatePrimBinOpBase :: Expression -> Expression -> Dictionary -> [ListVarTypTupel]   -> Instructions -> ReturnValueTupel
translatePrimBinOpBase exprL exprR constantPool lstVarTypTupel   instructionsList = let
    leftSideOfExpressionTranslated = (getInstructionListOfReturnTupel (translateExpression exprL constantPool lstVarTypTupel   instructionsList))
    rightSideOfExpressionTranslated = (getInstructionListOfReturnTupel (translateExpression exprR constantPool lstVarTypTupel   leftSideOfExpressionTranslated))
    in (lstVarTypTupel, rightSideOfExpressionTranslated)
-- END OF PRIM BIN OP - EXPRESSION TRANSLATION --

translatePrimUnOp :: UnOp -> Expression -> Dictionary -> [ListVarTypTupel]   -> Instructions -> ReturnValueTupel
translatePrimUnOp (Not) expr cp lstVarTypTupel   instructionsList = let
    translatedExpression = (translateExpression expr cp lstVarTypTupel   instructionsList) -- transtalted expression (must be boolean)
    newInstructionLst = (getInstructionListOfReturnTupel translatedExpression)
    finishedIntruction = newInstructionLst ++ [(Ifne 7), (Iconst_1), (Goto  (sumInstructionSizeInInstructionList [(Goto 1), (Iconst_0)])), (Iconst_0)]
    in (lstVarTypTupel, finishedIntruction)

translatePrimUnOp (Neg) expr@(TypedExpression (_, typ)) cp lstVarTypTupel   instructionsList = let
    translatedExpression = (translateExpression expr cp lstVarTypTupel   instructionsList) -- transtalted expression (must be int)
    newInstructionLst = (getInstructionListOfReturnTupel translatedExpression)
    finishedIntruction | (isPrimTypeInt typ) = newInstructionLst ++ [(Iconst_M1), (Ixor), (Iconst_1), (Iadd)] -- two complement
                       | otherwise = newInstructionLst ++ [(Iconst_M1), (Ixor)]
    in (lstVarTypTupel, finishedIntruction)



translateExpressionIden :: Compiler.Ast.Name -> Compiler.Ast.Type  -> Dictionary -> [ListVarTypTupel] -> Instructions -> ReturnValueTupel
translateExpressionIden (Name pth iden@(Identifier name)) typ cp lstVarTypTupel instructionsList = let
    -- get Index in Local variable list
    localVarLstIndex = (searchIdentInListVarTypTupel iden lstVarTypTupel) 
    -- get Index in constant pool
    maybeThis | (null pth) = "this"
              | (isIdenThis (head pth)) = "this"
              | otherwise = (identifierToString  pth)
    constanPoolIndex = (dictLookUp ("Field " ++ maybeThis ++ "." ++ name) cp)    

    loadInstruction | (isNothing localVarLstIndex) = instructionsList ++ [(Aload_0), (Getfield (fromIntegral (fromJust constanPoolIndex)))] -- in cp
                    | otherwise = (getInstructionListOfReturnTupel (getLoadFromLocalVar localVarLstIndex  typ lstVarTypTupel instructionsList)) -- local
    in (lstVarTypTupel, loadInstruction)

isIdenThis :: Identifier -> Bool
isIdenThis (This) = True
isIdenThis _ = False

getLoadFromLocalVar :: Maybe Word8 -> Type -> [ListVarTypTupel] -> Instructions -> ReturnValueTupel
getLoadFromLocalVar (Just index) (PrimType _) lstVarTypTupel instructionsList = (getLoadPrimCommandFromLocalVars index lstVarTypTupel instructionsList)
getLoadFromLocalVar (Just index) (RefType _) lstVarTypTupel instructionsList = (getLoadRefTypeFromLocalVar index lstVarTypTupel instructionsList)

getLoadRefTypeFromLocalVar ::  Word8 -> [ListVarTypTupel] -> Instructions -> ReturnValueTupel
getLoadRefTypeFromLocalVar 0 lstVarTypTupel instructionsList = (lstVarTypTupel, instructionsList ++ [Aload_0])
getLoadRefTypeFromLocalVar 1 lstVarTypTupel instructionsList = (lstVarTypTupel, instructionsList ++ [Aload_1])
getLoadRefTypeFromLocalVar 2 lstVarTypTupel instructionsList = (lstVarTypTupel, instructionsList ++ [Aload_2])
getLoadRefTypeFromLocalVar 3 lstVarTypTupel instructionsList = (lstVarTypTupel, instructionsList ++ [Aload_3])
getLoadRefTypeFromLocalVar index lstVarTypTupel instructionsList = (lstVarTypTupel, instructionsList ++ [(Aload index)])


getLoadPrimCommandFromLocalVars :: Word8 -> [ListVarTypTupel] -> Instructions -> ReturnValueTupel
getLoadPrimCommandFromLocalVars 0 lstVarTypTupel instructionsList = (lstVarTypTupel, instructionsList ++ [Iload_0])
getLoadPrimCommandFromLocalVars 1 lstVarTypTupel instructionsList = (lstVarTypTupel, instructionsList ++ [Iload_1])
getLoadPrimCommandFromLocalVars 2 lstVarTypTupel instructionsList = (lstVarTypTupel, instructionsList ++ [Iload_2])
getLoadPrimCommandFromLocalVars 3 lstVarTypTupel instructionsList = (lstVarTypTupel, instructionsList ++ [Iload_3])
getLoadPrimCommandFromLocalVars index lstVarTypTupel instructionsList = (lstVarTypTupel, instructionsList ++ [(Iload index)])


getStoreCommandLocal :: Maybe Word8 -> Type -> Dictionary -> [ListVarTypTupel] -> Instructions -> ReturnValueTupel
-- CASE IS IN LOCAL LIST PRIM TYPE => local definiert
getStoreCommandLocal (Just index) (PrimType _) cp lstVarTypTupel instructionsList | index == 0 = (lstVarTypTupel, instructionsList ++ [Istore_0])
                                                                                  | index == 1 = (lstVarTypTupel, instructionsList ++ [Istore_1])
                                                                                  | index == 2 = (lstVarTypTupel, instructionsList ++ [Istore_2])
                                                                                  | index == 3 = (lstVarTypTupel, instructionsList ++ [Istore_3])
                                                                                  | otherwise = (lstVarTypTupel, instructionsList ++ [(Istore index)])
-- CASE IN LOCAL LIST REF TYPE
getStoreCommandLocal (Just index) (RefType _) cp lstVarTypTupel instructionsList | index == 0 = (lstVarTypTupel, instructionsList ++ [Astore_0])
                                                                                 | index == 1 = (lstVarTypTupel, instructionsList ++ [Astore_1])
                                                                                 | index == 2 = (lstVarTypTupel, instructionsList ++ [Astore_2])
                                                                                 | index == 3 = (lstVarTypTupel, instructionsList ++ [Astore_3])
                                                                                 | otherwise = (lstVarTypTupel, instructionsList ++ [(Astore index)])


getNextFreeIndex :: [ListVarTypTupel] -> Word8 -> Word8
getNextFreeIndex [] n = n + 1
getNextFreeIndex ((_, v, _) : xs) n | v > n = (getNextFreeIndex xs v)
                                    | otherwise = (getNextFreeIndex xs n)

translateExpressionLiteral :: Dictionary -> [ListVarTypTupel] -> Instructions -> Compiler.Ast.Lit -> ReturnValueTupel
translateExpressionLiteral cp lstVarTypTupel instructionsList (IntegerL iVal) = (loadIntegerToStackInstruction iVal lstVarTypTupel cp instructionsList)
translateExpressionLiteral cp lstVarTypTupel instructionsList (BooleanL bVal) = (loadBooleanToStackInstruction bVal lstVarTypTupel instructionsList)
translateExpressionLiteral cp lstVarTypTupel instructionsList (CharL cVal) = (loadCharacterToStackInstruction cVal lstVarTypTupel instructionsList)
translateExpressionLiteral cp lstVarTypTupel instructionsList (StringL sVal) = (loadStringToStackInstruction sVal lstVarTypTupel cp instructionsList)
translateExpressionLiteral cp lstVarTypTupel instructionsList (Null) = (loadNullToStackInstruction lstVarTypTupel instructionsList)

loadNullToStackInstruction :: [ListVarTypTupel] -> Instructions -> ReturnValueTupel
loadNullToStackInstruction lstVarTypTupel instructionsList  = (lstVarTypTupel, instructionsList ++ [(Aconst_Null)])


loadStringToStackInstruction :: String -> [ListVarTypTupel] -> Dictionary -> Instructions -> ReturnValueTupel
loadStringToStackInstruction sVal lstVarTypTupel cp instructionsList = (lstVarTypTupel, instructionsList ++ [(Ldc (fromIntegral (fromJust (dictLookUp ("String " ++ sVal) cp))))]) 

isPrimTypeInt :: Type -> Bool
isPrimTypeInt (PrimType Int) = True
isPrimTypeInt _ = False

loadCharacterToStackInstruction :: Char -> [ListVarTypTupel] -> Instructions -> ReturnValueTupel
loadCharacterToStackInstruction cVal lstVarTypTupel instructionsList | (ord cVal) < 128 = (lstVarTypTupel, instructionsList ++ [(Bipush (fromIntegral (ord cVal)))])
                                                                     | otherwise = (lstVarTypTupel, instructionsList ++ [(Sipush (fromIntegral (ord cVal)))])



loadBooleanToStackInstruction :: Bool -> [ListVarTypTupel] -> Instructions -> ReturnValueTupel
loadBooleanToStackInstruction bVal lstVarTypTupel instructionsList | bVal = (lstVarTypTupel, instructionsList ++ [(Iconst_1)])
                                                                   | otherwise = (lstVarTypTupel, instructionsList ++ [(Iconst_0)])

loadIntegerToStackInstruction :: Integer -> [ListVarTypTupel] -> Dictionary -> Instructions -> ReturnValueTupel
loadIntegerToStackInstruction iVal lstVarTypTupel cp instructionsList | iVal > 32768 = let
                                                                        indexCP = (fromIntegral (fromJust (dictLookUp ("Int " ++ (show iVal)) cp))) 
                                                                        newInstructionLst | indexCP > 256 = instructionsList ++ [(Ldc_W (fromIntegral indexCP))]
                                                                                          | otherwise = instructionsList ++ [(Ldc (fromIntegral indexCP))]
                                                                        in (lstVarTypTupel, newInstructionLst)
                                                                     | iVal > 127 = (lstVarTypTupel, instructionsList ++ [(Sipush (fromIntegral iVal))])
                                                                     | iVal < (-32769) = let
                                                                        indexCP = (fromIntegral (fromJust (dictLookUp ("Int " ++ (show iVal)) cp))) 
                                                                        newInstructionLst | indexCP > 256 = instructionsList ++ [(Ldc_W  (fromIntegral indexCP))]
                                                                                          | otherwise = instructionsList ++ [(Ldc (fromIntegral indexCP))]
                                                                        in (lstVarTypTupel, newInstructionLst)
                                                                     | iVal < (-128) = (lstVarTypTupel, instructionsList ++ [(Sipush (fromIntegral iVal))])
                                                                     | otherwise = (lstVarTypTupel, instructionsList ++ [(Bipush (fromIntegral iVal))])



getLocalVarIndexListOfReturnTupel :: ReturnValueTupel -> [ListVarTypTupel]
getLocalVarIndexListOfReturnTupel (fst, _) = fst

getInstructionListOfReturnTupel :: ReturnValueTupel -> Instructions
getInstructionListOfReturnTupel (_, snd) = snd


getNextInstructionIndex :: Instructions -> Word16
getNextInstructionIndex = sumInstructionSizeInInstructionList

sumInstructionSizeInInstructionList :: Instructions -> Word16
sumInstructionSizeInInstructionList [] = 0
sumInstructionSizeInInstructionList (currentInstr : []) = (getInstructionSize currentInstr)
sumInstructionSizeInInstructionList (currentInstr : remainingIntrs) = (getInstructionSize currentInstr) + (sumInstructionSizeInInstructionList remainingIntrs)


searchIdentInListVarTypTupel :: Identifier -> [ListVarTypTupel] -> Maybe Word8 
searchIdentInListVarTypTupel (Identifier iden) [] = Nothing
searchIdentInListVarTypTupel (Identifier iden) (x : []) | (getVarNameOfListVarTypTupel x) == iden = Just (getIndexOfListVarTypTupel x)
searchIdentInListVarTypTupel (Identifier iden) (x :xs) | (getVarNameOfListVarTypTupel x) == iden = Just (getIndexOfListVarTypTupel x)
                                                       | otherwise = (searchIdentInListVarTypTupel (Identifier iden) xs)

searchTypInListVarTypTupel :: Identifier -> [ListVarTypTupel] -> Maybe Type 
searchTypInListVarTypTupel (Identifier iden) [] = Nothing
searchTypInListVarTypTupel (Identifier iden) (x : []) | (getVarNameOfListVarTypTupel x) == iden = Just (getTypeOfListVarTypTupel x)
searchTypInListVarTypTupel (Identifier iden) (x :xs) | (getVarNameOfListVarTypTupel x) == iden = Just (getTypeOfListVarTypTupel x)
                                                     | otherwise = (searchTypInListVarTypTupel (Identifier iden) xs)

getVarNameOfListVarTypTupel :: ListVarTypTupel -> String
getVarNameOfListVarTypTupel (s, _ , _) = s

getIndexOfListVarTypTupel :: ListVarTypTupel -> Word8
getIndexOfListVarTypTupel (_, ind, _) = ind

getTypeOfListVarTypTupel :: ListVarTypTupel -> Type
getTypeOfListVarTypTupel (_,_,typ) = typ




identifierToString :: [Identifier] -> String
identifierToString [] = ""
identifierToString ((Identifier s) : []) = s
identifierToString ((Identifier s) : xs) = s ++ (identifierToString xs)





