module Compiler.AstToClassFileTranslator.TranslateMethodBody where
import Compiler.AstToClassFileTranslator.Stack
import Compiler.AbstractBytecode
import Compiler.Instructions
import Compiler.Ast
import Compiler.AstToClassFileTranslator.InstructionsToLength
import Data.Char
import Data.Word (Word8, Word16)
import Data.Int (Int16, Int32, Int64)
import Data.Maybe
import Compiler.AstToClassFileTranslator.ConstantPoolGenerator
-- DEFINITION LISTE IN LOKALE VARIABLEN

type ListVarTypTupel = (String, Word8, Type)  -- wird Zeug eingetragen was mit store gespeichert wurde
type ReturnValueTupel = ([ListVarTypTupel], Instructions)




-- Identifier = Variable
-- Literal = "test", 1, 's', true, false, ...


                                            -- STATEMENT TRANSLATION --
translateStatement :: Compiler.Ast.Statement -> Dictionary -> [ListVarTypTupel] -> Stack Word16 -> Instructions -> ReturnValueTupel
translateStatement (TypedStatement ((While cnd bdy), typ)) constantPool lstVarTypTupel stck instructionsList = (translateWhileStatement cnd bdy constantPool lstVarTypTupel stck instructionsList)
translateStatement (TypedStatement ((If cnd thn els), typ)) constantPool lstVarTypTupel stck instructionsList = (translateIfStatement cnd thn els constantPool lstVarTypTupel stck instructionsList)
translateStatement (TypedStatement ((Block stmt), typ)) constantPool lstVarTypTupel stck instructionsList = (translateBlockStatement stmt constantPool lstVarTypTupel stck instructionsList)
translateStatement (TypedStatement ((Compiler.Ast.Return maybeExpr), typ)) constantPool lstVarTypTupel stck instructionsList = (translateReturnStatement maybeExpr typ constantPool lstVarTypTupel stck instructionsList)
translateStatement (TypedStatement ((Continue), typ)) constantPool lstVarTypTupel stck instructionsList = (translateContinueStatement lstVarTypTupel stck instructionsList)
-- translateStatement (TypedStatement ((Break), typ)) constantPool lstVarTypTupel stck instructionsList = mein job
translateStatement (TypedStatement ((LocalVar lclVar), typ)) constantPool lstVarTypTupel stck instructionsList = (translateLocalVarStatement lclVar constantPool lstVarTypTupel stck instructionsList)
translateStatement (TypedStatement ((StmtExprStmt stmtExpr), typ)) constantPool lstVarTypTupel stck instructionsList = 
    (translateStatementExpresion stmtExpr constantPool lstVarTypTupel stck instructionsList)
                                                                                                                               


--                                              ___ -> typ von rückgabewert
translateReturnStatement :: Maybe Expression -> Type -> Dictionary -> [ListVarTypTupel] -> Stack Word16 -> Instructions -> ReturnValueTupel
translateReturnStatement (Just expr) (PrimType _) constantPool lstVarTypTupel stck instructionsList = let
    translatedExpression = (getInstructionListOfReturnTupel (translateExpression expr constantPool lstVarTypTupel stck instructionsList))
    newInstructionLstWithReturn = translatedExpression ++ [(Ireturn)]
    in (lstVarTypTupel, newInstructionLstWithReturn)
translateReturnStatement (Just expr) (RefType _) constantPool lstVarTypTupel stck instructionsList = let
    translatedExpression = (getInstructionListOfReturnTupel (translateExpression expr constantPool lstVarTypTupel stck instructionsList))
    newInstructionLstWithReturn = translatedExpression ++ [(Areturn)]
    in (lstVarTypTupel, newInstructionLstWithReturn)
translateReturnStatement (Just expr) (JVoid) constantPool lstVarTypTupel stck instructionsList = let 
    newInstructionLstWithReturn = instructionsList ++ [(Compiler.Instructions.Return)]
    in (lstVarTypTupel, newInstructionLstWithReturn)
translateReturnStatement Nothing _ constantPool lstVarTypTupel stck instructionsList = let
    newInstructionLstWithReturn = instructionsList ++ [(Compiler.Instructions.Return)]
    in (lstVarTypTupel, newInstructionLstWithReturn)

translateContinueStatement :: [ListVarTypTupel] -> Stack Word16 -> Instructions -> ReturnValueTupel
translateContinueStatement lstVarTypTupel stck instructionsList = let
    instrListWithContinue = instructionsList ++ [(Goto (fromIntegral (top stck)))] 
    in (lstVarTypTupel, instrListWithContinue)


translateWhileStatement :: Expression -> Maybe Statement -> Dictionary -> [ListVarTypTupel] -> Stack Word16 -> Instructions -> ReturnValueTupel
translateWhileStatement cnd (Just bdy) constantPool lstVarTypTupel stck instructionsList = let
    stckWthGotoAddr =  (push (getNextInstructionIndex instructionsList) stck)
    translatedCondition = (getInstructionListOfReturnTupel (translateExpression cnd constantPool lstVarTypTupel stck instructionsList)) ++ [(Ifne 1)]
    currentListSize = (length translatedCondition)
    translatedBody = (getInstructionListOfReturnTupel (translateStatement bdy constantPool lstVarTypTupel stckWthGotoAddr instructionsList)) ++ [(Goto (top stckWthGotoAddr))]
    ifFalseAddress = (getNextInstructionIndex translatedBody)
    splittedList = (splitAt currentListSize translatedBody)
    replacedIfJumpAddressInstrListFinished = (init (fst splittedList)) ++ [(Ifne ifFalseAddress)] ++ (snd splittedList)
    in (lstVarTypTupel, replacedIfJumpAddressInstrListFinished)
translateWhileStatement cnd Nothing constantPool lstVarTypTupel stck instructionsList = let
    ifIndex =  (getNextInstructionIndex instructionsList)
    translatedCondition = (getInstructionListOfReturnTupel (translateExpression cnd constantPool lstVarTypTupel stck instructionsList)) ++ [(Ifne ifIndex)]  
    in (lstVarTypTupel, translatedCondition)



translateBlockStatement :: [Statement] -> Dictionary -> [ListVarTypTupel] -> Stack Word16 -> Instructions -> ReturnValueTupel
translateBlockStatement [] constantPool lstVarTypTupel stck instructionsList = (lstVarTypTupel, instructionsList)
translateBlockStatement (x : []) constantPool lstVarTypTupel stck instructionsList = (translateStatement x constantPool lstVarTypTupel stck instructionsList)
translateBlockStatement (x : xs ) constantPool lstVarTypTupel stck instructionsList = let
    translatedFirstStatement = (translateStatement x constantPool lstVarTypTupel stck instructionsList)
    translatedRest = (translateBlockStatement xs constantPool (getLocalVarIndexListOfReturnTupel translatedFirstStatement) stck (getInstructionListOfReturnTupel translatedFirstStatement))
    in (lstVarTypTupel, (getInstructionListOfReturnTupel translatedRest))

translateIfStatement :: Expression -> Maybe Statement -> Maybe Statement -> Dictionary -> [ListVarTypTupel] -> Stack Word16 -> Instructions -> ReturnValueTupel
translateIfStatement cnd Nothing Nothing constantPool lstVarTypTupel stck instructionsList = (translateExpression cnd constantPool lstVarTypTupel stck instructionsList) -- könnte wegoptimiert werden
translateIfStatement cnd (Just thn) Nothing constantPool lstVarTypTupel stck instructionsList = let
    translatedConditionInstructionLst = (getInstructionListOfReturnTupel (translateExpression cnd constantPool lstVarTypTupel stck instructionsList)) ++ [(Ifne 1)]
    lengthInstructionFixIf = (length translatedConditionInstructionLst)
    thenInstructionList = (getInstructionListOfReturnTupel (translateStatement thn constantPool lstVarTypTupel stck translatedConditionInstructionLst))
    getNextInstructionIndex = (sumInstructionSizeInInstructionList thenInstructionList)
    splitTupel = (splitAt lengthInstructionFixIf thenInstructionList)
    fixedConditionLst = (init (fst splitTupel)) ++ [(Ifne getNextInstructionIndex)] ++ (snd splitTupel)
    in (lstVarTypTupel, fixedConditionLst)
translateIfStatement cnd (Just thn) (Just els) constantPool lstVarTypTupel stck instructionsList = let
    translatedConditionInstructionLst = (getInstructionListOfReturnTupel (translateExpression cnd constantPool lstVarTypTupel stck instructionsList)) ++ [(Ifne 1)]
    lengthInstructionFixIf = (length translatedConditionInstructionLst)
    thenInstructionList = (getInstructionListOfReturnTupel (translateStatement thn constantPool lstVarTypTupel stck translatedConditionInstructionLst))
    getNextInstructionIndex = (sumInstructionSizeInInstructionList thenInstructionList) + 3 -- um goto für then zu überspringen -> erste zeile else
    splitTupel = (splitAt lengthInstructionFixIf thenInstructionList)
    fixedConditionLstWithGotoPlaceholder = (init (fst splitTupel)) ++ [(Ifne getNextInstructionIndex)] ++ (snd splitTupel) ++ [(Goto 1)]
    splitForGoto = (length fixedConditionLstWithGotoPlaceholder)
    translatedElseStatement = (getInstructionListOfReturnTupel (translateStatement els constantPool lstVarTypTupel stck fixedConditionLstWithGotoPlaceholder))
    afterIfIndex = (sumInstructionSizeInInstructionList translatedElseStatement)
    splitTupelGoto = (splitAt splitForGoto translatedElseStatement)
    finishedLst = (init (fst splitTupelGoto)) ++ [(Goto afterIfIndex)] ++ (snd splitTupelGoto)
    in (lstVarTypTupel, finishedLst)

translateLocalVarStatement :: Compiler.Ast.VarDecl -> Dictionary -> [ListVarTypTupel] -> Stack Word16 -> Instructions -> ReturnValueTupel
translateLocalVarStatement (VarDecl identifier mods typ (Just expr)) cp lstVarTypTupel stck instructionList = let
  rhs = (translateExpression expr cp lstVarTypTupel stck instructionList)
  number = (Just (getNextFreeIndex lstVarTypTupel 0))
  addedListVatTupel = lstVarTypTupel ++ [((identifierToString [identifier]),(fromIntegral (fromJust number)), typ)]
  updatedList = (getStoreCommandLocal number typ cp addedListVatTupel (getInstructionListOfReturnTupel rhs))
  in updatedList
translateLocalVarStatement (VarDecl identifier mods typ Nothing) cp lstVarTypTupel stck instructionList = let
  number = (Just (getNextFreeIndex lstVarTypTupel 0))
  addedListVatTupel = lstVarTypTupel ++ [((identifierToString [identifier]), (fromIntegral (fromJust number)), typ)]
  updatedList = (getStoreCommandLocal number typ cp addedListVatTupel instructionList)
  in updatedList

                                            -- EXPRESSION TRANSLATION --
translateExpression :: Compiler.Ast.Expression -> Dictionary -> [ListVarTypTupel] -> Stack Word16 -> Instructions -> ReturnValueTupel
translateExpression (TypedExpression ((TernaryIf cond els thn), typ)) constantPool lstVarTypTupel stck instructionsList = (translateTernaryIf cond els thn constantPool lstVarTypTupel stck instructionsList)
translateExpression (TypedExpression ((PrimBinOp binOp exprL exprR), typ)) constantPool lstVarTypTupel stck instructionsList = (translatePrimBinOp binOp exprL exprR constantPool lstVarTypTupel stck instructionsList)
translateExpression (TypedExpression ((PrimUnOp unOp expr), typ)) constantPool lstVarTypTupel stck instructionsList = (translatePrimUnOp unOp expr constantPool lstVarTypTupel stck instructionsList)
translateExpression (TypedExpression ((Iden name), typ)) constantPool lstVarTypTupel stck instructionsList = (translateExpressionIden name typ constantPool lstVarTypTupel instructionsList) -- todo fast fertig außer strings
-- translateExpression (TypedExpression ((Select expr ident), typ)) constantPool lstVarTypTupel stck instructionsList = -- TODO was ist das?
translateExpression (TypedExpression ((Literal lit), typ)) constantPool lstVarTypTupel stck instructionsList = (translateExpressionLiteral constantPool lstVarTypTupel instructionsList lit)
translateExpression (TypedExpression ((ExprExprStmt stmtExpr), typ)) constantPool lstVarTypTupel stck instructionsList = (translateStatementExpresion stmtExpr constantPool lstVarTypTupel stck instructionsList)
translateExpression (TypedExpression ((Cast typ1 expr), typ2)) constantPool lstVarTypTupel stck instructionsList = (translateExpressionCast typ1 expr constantPool lstVarTypTupel stck instructionsList)




translateExpressionCast :: Compiler.Ast.Type -> Compiler.Ast.Expression -> Dictionary -> [ListVarTypTupel] -> Stack Word16 -> Instructions -> ReturnValueTupel
translateExpressionCast (PrimType Char) expr cp lstVarTypTupel stck instructionsList = let
    e = (translateExpression expr cp lstVarTypTupel stck instructionsList)
    in ((getLocalVarIndexListOfReturnTupel e), ((getInstructionListOfReturnTupel e) ++ [I2C]))
translateExpressionCast (PrimType Int) expr cp lstVarTypTupel stck instructionsList = let
    e = (translateExpression expr cp lstVarTypTupel stck instructionsList)
    in e
translateExpressionCast (RefType nme) (TypedExpression (expr, (PrimType Int))) cp lstVarTypTupel stck instructionsList = let
    e = (translateExpression expr cp lstVarTypTupel stck instructionsList)
    in ((getLocalVarIndexListOfReturnTupel e),((getInstructionListOfReturnTupel e) ++ [Invokestatic 10])) -- TODO 

translateExpressionCast (RefType nme) (TypedExpression (expr, (PrimType Char))) cp lstVarTypTupel stck instructionsList = let
    e = (translateExpression expr cp lstVarTypTupel stck instructionsList)
    in ((getLocalVarIndexListOfReturnTupel e),((getInstructionListOfReturnTupel e) ++ [Invokestatic 11])) -- TODO

translateExpressionCast (RefType nme) (TypedExpression (expr, (PrimType Boolean))) cp lstVarTypTupel stck instructionsList = let
    e = (translateExpression expr cp lstVarTypTupel stck instructionsList)
    in ((getLocalVarIndexListOfReturnTupel e),((getInstructionListOfReturnTupel e) ++ [Invokestatic 12])) -- TODO
translateExpressionCast (RefType nme) (TypedExpression (expr, (RefType name))) cp lstVarTypTupel stck instructionsList = let
    e = (translateExpression expr cp lstVarTypTupel stck instructionsList)
    in e


translateStatementExpresion :: StmtExpr -> Dictionary -> [ListVarTypTupel] -> Stack Word16 -> Instructions -> ReturnValueTupel
translateStatementExpresion (TypedStmtExpr ((Assign assignOp nme expr), typ)) cp lstVarTypTupel stck instructionsList =
    (translateAssignStmtExpr assignOp nme expr typ cp lstVarTypTupel stck instructionsList)
translateStatementExpresion (TypedStmtExpr ((Instantiation nme expr), typ)) cp lstVarTypTupel stck instructionsList = 
    (translateInstantiationStmtExpr nme expr cp lstVarTypTupel stck instructionsList)
translateStatementExpresion (TypedStmtExpr ((Apply expr exprsns), typ)) cp lstVarTypTupel stck instructionsList = 
    (translateApplyExpression expr exprsns cp lstVarTypTupel stck instructionsList)
-- translateStatementExpresion (TypedStmtExpr ((SEUnOp incrOrDecr expr), typ)) cp lstVarTypTupel stck instructionsList = TODO

translateApplyExpression :: Expression -> [Expression] -> Dictionary -> [ListVarTypTupel] -> Stack Word16 -> Instructions -> ReturnValueTupel
--                                   _________________________________________ > kann ich das so annehmen? TODO
translateApplyExpression (Iden (Name [(Identifier pth)] (Identifier methodName))) passedToList cp lstVarTypTupel stck instructionsList = let
--                       ^^^^^^^^^^^^^^^^^^^^^ einzige Möglichkeit, sonst gibts haue
    
    translatedPassedToList = (getInstructionListOfReturnTupel (translateListOfExpressions passedToList cp lstVarTypTupel stck (instructionsList ++ [Aload_0])))
    --                                                                                                           ^^^^^^^^^ lade derzeitiges objekt, dann übergabeparameter
    pthPth | (length pth) == 0 = "this"
           | otherwise = pth
    getCpIndexOfMethod = (fromJust (dictLookUp ("Method " ++ pthPth ++ "." ++  methodName ++ ":" ++ (mapParametersMethodToDiscriptor passedToList JVoid)) cp))
                                        
    ivokeMethod = translatedPassedToList ++ [(Invokespecial (fromIntegral getCpIndexOfMethod))]
    
    in (lstVarTypTupel, ivokeMethod)




-- in einer anderen klase
-- translateApplyExpression (Iden pth methodName) exprsnList cp lstVarTypTupel stck instructionsList = let

-- da wir eh keine for schleifen unterstüzen ist es eigentlich nutzlos zwischen pre und post zu unterscheiden --> ich glaub mit unserer definiton wird das hier nicht umsetzbar
-- translateSEUnOpExprssion :: IncrOrDecr -> Expression -> Dictionary -> [ListVarTypTupel] -> Stack Word16 -> Instructions -> ReturnValueTupel
-- translateSEUnOpExprssion (PreIncr) expr cp lstVarTypTupel stck instructionsList = let
--     translatedExpression = (translateExpression expr cp lstVarTypTupel stck instructionsList)

-- translateSEUnOpExprssion (PostIncr)
-- translateSEUnOpExprssion (PreDecr)
-- translateSEUnOpExprssion (PostDecr)

translateInstantiationStmtExpr :: Name -> [Expression]  -> Dictionary -> [ListVarTypTupel] -> Stack Word16 -> Instructions -> ReturnValueTupel
translateInstantiationStmtExpr (Name pth (Identifier iden)) exprsns constantPool lstVarTypTupel stck instructionsList = let
    -- get the class ref for: "new" - Operation
    classIndexCP_OfName = (fromJust (dictLookUp ("class " ++ iden) constantPool)) -- TESTEN TODO
    -- get method ref of constructor from class
    getConstructorIndex_OfName = (fromJust (dictLookUp ("Method " ++ iden ++ ".<init>:" ++ (mapParametersMethodToDiscriptor exprsns JVoid)) constantPool))
    -- create object and dublicate
    newAndDupList = instructionsList ++ [(New (fromIntegral classIndexCP_OfName)), (Dup)]
    -- translate Expression
    translatedExpressions = (getInstructionListOfReturnTupel (translateListOfExpressions exprsns constantPool lstVarTypTupel stck instructionsList))
    -- contructor call
    contructorTranslated = translatedExpressions ++ [(Invokespecial (fromIntegral getConstructorIndex_OfName))]
    in (lstVarTypTupel, contructorTranslated)

mapParametersMethodToDiscriptor :: [Expression] -> Type -> String
mapParametersMethodToDiscriptor exprs returnType = methodDescriptorToString (map (\(TypedExpression(_, t)) -> t) exprs) returnType

translateListOfExpressions :: [Expression] -> Dictionary -> [ListVarTypTupel] -> Stack Word16 -> Instructions -> ReturnValueTupel
translateListOfExpressions [] constantPool lstVarTypTupel stck instructionsList = (lstVarTypTupel, instructionsList)
translateListOfExpressions (x : []) constantPool lstVarTypTupel stck instructionsList = (translateExpression x constantPool lstVarTypTupel stck instructionsList)
translateListOfExpressions (x : xs ) constantPool lstVarTypTupel stck instructionsList = let
    translatedFirstExpression = (translateExpression x constantPool lstVarTypTupel stck instructionsList)
    translatedRest = (translateListOfExpressions xs constantPool lstVarTypTupel stck (getInstructionListOfReturnTupel translatedFirstExpression))
    in (lstVarTypTupel, (getInstructionListOfReturnTupel translatedRest))


translateAssignStmtExpr :: AssignOp -> Name -> Expression -> Type -> Dictionary -> [ListVarTypTupel] -> Stack Word16 -> Instructions -> ReturnValueTupel
-- TODO null, I2c evtl noch hinzufügen
-- TODO check pth != " " -> dann nicht in eigenem objekt
translateAssignStmtExpr (NormalAssign) (Name pth iden@(Identifier name)) expr typ cp lstVarTypTupel stck instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel) 
    -- index von wert wenn dieser global gespeichert ist
    indexInConstantPool = (dictLookUp ("Field this." ++ name) cp) 
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel)  = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0)]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadFieldInstrLst))
                           saveInFieldInstr = translatedAssignmentExpression ++ [(Putfield (fromIntegral (fromJust indexInConstantPool)))]      -- is in constantenpool
                           in saveInFieldInstr
                                -- lokale variable
                          | otherwise = let
                            translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck instructionsList))
                            saveToLoacVarInstr = (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel translatedAssignmentExpression))
                            in saveToLoacVarInstr

   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (MultiplyAssign) (Name _ iden@(Identifier name)) expr typ cp lstVarTypTupel stck instructionsList = let
     -- index von wert wenn dieser lokal gespeichert ist
     indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
     -- index von wert wenn dieser global gespeichert ist
     indexInConstantPool = (dictLookUp ("Field this." ++ name) cp)                     
                                 -- globale variable, speichere mit putfield  
     assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                            loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                            translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadFieldInstrLst))
                            mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Imul), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                            in mulAndSaveInFieldInstr
                                 -- lokale variable
                           | (otherwise) = let
                            loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                            translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadLocalVarInstLst))
                            mulInstruction = translatedAssignmentExpression ++ [(Imul)]
                            saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                            in saveInLocalVarInstr
    in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (DivideAssign) (Name _ iden@(Identifier name)) expr typ cp lstVarTypTupel stck instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    indexInConstantPool = (dictLookUp ("Field this." ++ name) cp)    
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Idiv), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Idiv)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (ModuloAssign) (Name _ iden@(Identifier name)) expr typ cp lstVarTypTupel stck instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    indexInConstantPool = (dictLookUp ("Field this." ++ name) cp)    
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Irem), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Irem)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (PlusAssign) (Name _ iden@(Identifier name)) expr typ cp lstVarTypTupel stck instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    indexInConstantPool = (dictLookUp ("Field this." ++ name) cp)    
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Iadd), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Iadd)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (MinusAssign) (Name _ iden@(Identifier name)) expr typ cp lstVarTypTupel stck instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    indexInConstantPool = (dictLookUp ("Field this." ++ name) cp)    
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Isub), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable,
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Isub)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (LeftShiftAssign) (Name _ iden@(Identifier name)) expr typ cp lstVarTypTupel stck instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    indexInConstantPool = (dictLookUp ("Field this." ++ name) cp)   
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Ishl), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Ishl)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (ShiftRightAssign) (Name _ iden@(Identifier name)) expr typ cp lstVarTypTupel stck instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    indexInConstantPool = (dictLookUp ("Field this." ++ name) cp) 
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Ishr), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Ishr)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (UnsignedShiftRightAssign) (Name _ iden@(Identifier name)) expr typ cp lstVarTypTupel stck instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    indexInConstantPool = (dictLookUp ("Field this." ++ name) cp) 
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Iushr), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Iushr)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (AndAssign) (Name _ iden@(Identifier name)) expr typ cp lstVarTypTupel stck instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    indexInConstantPool = (dictLookUp ("Field this." ++ name) cp) 
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Iand), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Iand)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (BitXOrAssign) (Name _ iden@(Identifier name)) expr typ cp lstVarTypTupel stck instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    indexInConstantPool = (dictLookUp ("Field this." ++ name) cp) 
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Ixor), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable,
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Ixor)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateAssignStmtExpr (OrAssign) (Name _ iden@(Identifier name)) expr typ cp lstVarTypTupel stck instructionsList = let
    -- index von wert wenn dieser lokal gespeichert ist
    indexOfValueInLstVarTypTupel = (searchIdentInListVarTypTupel iden lstVarTypTupel)
    -- index von wert wenn dieser global gespeichert ist
    indexInConstantPool = (dictLookUp ("Field this." ++ name) cp) 
                                -- globale variable, speichere mit putfield
    assignmetInstructions | (isNothing indexOfValueInLstVarTypTupel) = let
                           loadFieldInstrLst = instructionsList ++ [(Aload_0), (Dup), (Getfield (fromIntegral (fromJust indexInConstantPool)))]
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadFieldInstrLst))
                           mulAndSaveInFieldInstr = translatedAssignmentExpression ++ [(Ior), (Putfield (fromIntegral (fromJust indexInConstantPool)))] -- is in constantenpool
                           in mulAndSaveInFieldInstr
                                -- lokale variable
                          | (otherwise) = let
                           loadLocalVarInstLst = (getInstructionListOfReturnTupel (getLoadFromLocalVar indexOfValueInLstVarTypTupel typ lstVarTypTupel instructionsList))
                           translatedAssignmentExpression = (getInstructionListOfReturnTupel (translateExpression expr cp lstVarTypTupel stck loadLocalVarInstLst))
                           mulInstruction = translatedAssignmentExpression ++ [(Ior)]
                           saveInLocalVarInstr =  (getInstructionListOfReturnTupel (getStoreCommandLocal indexOfValueInLstVarTypTupel typ cp lstVarTypTupel mulInstruction))
                           in saveInLocalVarInstr
   in (lstVarTypTupel, assignmetInstructions)

translateTernaryIf :: Expression -> Expression -> Expression -> Dictionary -> [ListVarTypTupel] -> Stack Word16 -> Instructions -> ReturnValueTupel
translateTernaryIf cond els thn cp lstVarTypTupel stck instructionsList = let -- warum in ast verkehrt ?!?
    translatedCondition = (translateExpression cond cp lstVarTypTupel stck instructionsList) -- condition übersetzt
    instrLstWithPlaceHolder_If = (getInstructionListOfReturnTupel translatedCondition) ++ [(Ifne 1)] -- instruktionsliste mit platzhalter if
    sizeOfListUntilNow = (length instrLstWithPlaceHolder_If) -- länge der instruktionsliste ink neuem platzhalter if
    thenInstructionList = (getInstructionListOfReturnTupel (translateExpression thn cp lstVarTypTupel stck instrLstWithPlaceHolder_If)) -- intrliste then statement
    --                                                                          ^^^ wirklich ?
    addressToThenStatement = (getNextInstructionIndex thenInstructionList) + 3 -- adresse von else anfang (voerst mit normalem goto größe)
    splittedList = (splitAt sizeOfListUntilNow thenInstructionList) -- splitte : links mit if platzhalter rechts then statment
    listWithCorrectIndex_if = (init (fst splittedList)) ++ [(Ifne addressToThenStatement)] ++ (snd splittedList) ++ [(Goto 1)] -- list mit cond richtigem if jump und then statement + platzhalter goto
    sizeOfListUntilNow_second = (length listWithCorrectIndex_if) -- länge der instruktionsliste ink neuem platzhalter goto
    elseInstructionList = (getInstructionListOfReturnTupel (translateExpression els cp lstVarTypTupel stck instrLstWithPlaceHolder_If)) -- intrliste else statement
    gotoJumpAddress = (getNextInstructionIndex elseInstructionList) -- sprungadresse für then GOTO hinter else
    splittedList_second = (splitAt sizeOfListUntilNow_second elseInstructionList) -- splitte
    finishedTranslationStmtLst =  (init (fst splittedList)) ++ [(Goto gotoJumpAddress)] ++ (snd splittedList) -- ändere goto adresse
    in (lstVarTypTupel, finishedTranslationStmtLst) -- gebe zurück


    -- START OF PRIM BIN OP - EXPRESSION TRANSLATION --
translatePrimBinOp :: BinOp -> Expression -> Expression -> Dictionary -> [ListVarTypTupel] -> Stack Word16 -> Instructions -> ReturnValueTupel
-- TODO
-- translatePrimBinOp (And) exprL exprR cp lstVarTypTupel stck instructionsList = let
-- translatePrimBinOp (Or)
-- translatePrimBinOp (InstaneOf)
translatePrimBinOp (Eq) exprL exprR cp lstVarTypTupel stck instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel stck instructionsList))
    nextIndexOfInstruction = (getNextInstructionIndex leftAndRightSideTranslated)
    finalInstructionList = leftAndRightSideTranslated ++ [(If_Icmpne (nextIndexOfInstruction + 7)), (Iconst_1), (Goto (nextIndexOfInstruction + 8)), (Iconst_0)] -- schreibe wert auf stack (entweder 1 oder 0), abhängig von vergleich
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (Less) exprL exprR cp lstVarTypTupel stck instructionsList = let -- anders herum als im ast, da java anders herum funktioniert
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel stck instructionsList))
    nextIndexOfInstruction = (getNextInstructionIndex leftAndRightSideTranslated)
    finalInstructionList = leftAndRightSideTranslated ++ [(If_Icmpge (nextIndexOfInstruction + 7)), (Iconst_1), (Goto (nextIndexOfInstruction + 8)), (Iconst_0)] -- schreibe wert auf stack (entweder 1 oder 0), abhängig von vergleich
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (LessEq) exprL exprR cp lstVarTypTupel stck instructionsList = let -- anders herum als im ast, da java anders herum funktioniert
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel stck instructionsList))
    nextIndexOfInstruction = (getNextInstructionIndex leftAndRightSideTranslated)
    finalInstructionList = leftAndRightSideTranslated ++ [(If_Icmpgt (nextIndexOfInstruction + 7)), (Iconst_1), (Goto (nextIndexOfInstruction + 8)), (Iconst_0)] -- schreibe wert auf stack (entweder 1 oder 0), abhängig von vergleich
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (Greater) exprL exprR cp lstVarTypTupel stck instructionsList = let -- anders herum als im ast, da java anders herum funktioniert
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel stck instructionsList))
    nextIndexOfInstruction = (getNextInstructionIndex leftAndRightSideTranslated)
    finalInstructionList = leftAndRightSideTranslated ++ [(If_Icmple (nextIndexOfInstruction + 7)), (Iconst_1), (Goto (nextIndexOfInstruction + 8)), (Iconst_0)] -- schreibe wert auf stack (entweder 1 oder 0), abhängig von vergleich
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (GreaterEq) exprL exprR cp lstVarTypTupel stck instructionsList = let -- anders herum als im ast, da java anders herum funktioniert
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel stck instructionsList))
    nextIndexOfInstruction = (getNextInstructionIndex leftAndRightSideTranslated)
    finalInstructionList = leftAndRightSideTranslated ++ [(If_Icmplt (nextIndexOfInstruction + 7)), (Iconst_1), (Goto (nextIndexOfInstruction + 8)), (Iconst_0)] -- schreibe wert auf stack (entweder 1 oder 0), abhängig von vergleich
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (Multiply) exprL exprR cp lstVarTypTupel stck instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel stck instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Imul)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (Divide) exprL exprR cp lstVarTypTupel stck instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel stck instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Idiv)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (Add) exprL exprR cp lstVarTypTupel stck instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel stck instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Iadd)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (Subtract) exprL exprR cp lstVarTypTupel stck instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel stck instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Isub)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (Modulo) exprL exprR cp lstVarTypTupel stck instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel stck instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Irem)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (ShiftRight) exprL exprR cp lstVarTypTupel stck instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel stck instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Ishr)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (ShiftLeft) exprL exprR cp lstVarTypTupel stck instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel stck instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Ishl)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (UnsignedShiftRight) exprL exprR cp lstVarTypTupel stck instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel stck instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Iushr)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (BitAnd) exprL exprR cp lstVarTypTupel stck instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel stck instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Iand)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (BitOr) exprL exprR cp lstVarTypTupel stck instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel stck instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Ior)]
    in (lstVarTypTupel, finalInstructionList)

translatePrimBinOp (BitXOr) exprL exprR cp lstVarTypTupel stck instructionsList = let
    leftAndRightSideTranslated = (getInstructionListOfReturnTupel (translatePrimBinOpBase exprL exprR cp lstVarTypTupel stck instructionsList))
    finalInstructionList = leftAndRightSideTranslated ++ [(Ixor)]
    in (lstVarTypTupel, finalInstructionList)


-- Hilfsfunktion für PrimBinOps von Expression Übersetzung
translatePrimBinOpBase :: Expression -> Expression -> Dictionary -> [ListVarTypTupel] -> Stack Word16 -> Instructions -> ReturnValueTupel
translatePrimBinOpBase exprL exprR constantPool lstVarTypTupel stck instructionsList = let
    leftSideOfExpressionTranslated = (getInstructionListOfReturnTupel (translateExpression exprL constantPool lstVarTypTupel stck instructionsList))
    rightSideOfExpressionTranslated = (getInstructionListOfReturnTupel (translateExpression exprR constantPool lstVarTypTupel stck leftSideOfExpressionTranslated))
    in (lstVarTypTupel, rightSideOfExpressionTranslated)
-- END OF PRIM BIN OP - EXPRESSION TRANSLATION --

translatePrimUnOp :: UnOp -> Expression -> Dictionary -> [ListVarTypTupel] -> Stack Word16 -> Instructions -> ReturnValueTupel
translatePrimUnOp (Not) expr cp lstVarTypTupel stck instructionsList = let
    translatedExpression = (translateExpression expr cp lstVarTypTupel stck instructionsList) -- transtalted expression (must be boolean)
    newInstructionLst = (getInstructionListOfReturnTupel translatedExpression)
    nextIndexOfInstruction = (getNextInstructionIndex newInstructionLst)
    finishedIntruction = newInstructionLst ++ [(Ifne (nextIndexOfInstruction + 7)), (Iconst_1), (Goto (nextIndexOfInstruction + 8)), (Iconst_0)]
    in (lstVarTypTupel, finishedIntruction)

translatePrimUnOp (Neg) expr cp lstVarTypTupel stck instructionsList = let
    translatedExpression = (translateExpression expr cp lstVarTypTupel stck instructionsList) -- transtalted expression (must be int)
    newInstructionLst = (getInstructionListOfReturnTupel translatedExpression)
    finishedIntruction = newInstructionLst ++ [(Iconst_M1), (Ixor)]
    in (lstVarTypTupel, finishedIntruction)
-- translatePrimUnOp (BitCompl) existiert nicht, nur bei assignment oder binary



translateExpressionIden :: Compiler.Ast.Name -> Compiler.Ast.Type  -> Dictionary -> [ListVarTypTupel] -> Instructions -> ReturnValueTupel
-- translateExpressionIden nme (JVoid) cp lstVarTypTupel instructionsList TODO
--                           ____ -> ist in dem derzeitigen objekt
translateExpressionIden (Name [] iden@(Identifier name)) typ cp lstVarTypTupel instructionsList = let
    -- get Index in Local variable list
    localVarLstIndex = (searchIdentInListVarTypTupel iden lstVarTypTupel) 
    -- get Index in constant pool
    constanPoolIndex = (dictLookUp ("Field this." ++ name) cp)    

    loadInstruction | (isNothing localVarLstIndex) = instructionsList ++ [(Aload_0), (Getfield (fromIntegral (fromJust constanPoolIndex)))] -- in cp
                    | otherwise = (getInstructionListOfReturnTupel (getLoadFromLocalVar localVarLstIndex  typ lstVarTypTupel instructionsList)) -- local
    in (lstVarTypTupel, loadInstruction)
--                            ____ -> ist in einem anderen objekt
-- translateExpressionIden (Name pth iden) typ cp lstVarTypTupel instructionsList = let
--      -- get Index in Local variable list
--      localVarLstIndex = (searchIdentInListVarTypTupel (identifierToString pth) lstVarTypTupel)
--      -- get Index in constant pool
--     constanPoolIndex = (dictLookUp (identifierToString (pth ++ [iden])) cp)

--     -- load index of field ref
--     constantPoolInde = (getFieldRefIndex pth iden) -- TODO
--     --


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
translateExpressionLiteral cp lstVarTypTupel instructionsList (Null) = (loadNullToStackInstruction lstVarTypTupel instructionsList)-- eigentlich wird das hier nicht "direkt" benötigt weil auf not null prüft

loadNullToStackInstruction :: [ListVarTypTupel] -> Instructions -> ReturnValueTupel
loadNullToStackInstruction lstVarTypTupel instructionsList  = (lstVarTypTupel, instructionsList ++ [(Aconst_Null)])


loadStringToStackInstruction :: String -> [ListVarTypTupel] -> Dictionary -> Instructions -> ReturnValueTupel
loadStringToStackInstruction sVal lstVarTypTupel cp instructionsList = (lstVarTypTupel, instructionsList ++ [(Ldc (fromIntegral (fromJust (dictLookUp ("String " ++ sVal) cp))))]) -- TODO  anpassen (evtl swtich nach LDC_w oder LDC)



loadCharacterToStackInstruction :: Char -> [ListVarTypTupel] -> Instructions -> ReturnValueTupel
loadCharacterToStackInstruction cVal lstVarTypTupel instructionsList | (ord cVal) < 128 = (lstVarTypTupel, instructionsList ++ [(Bipush (fromIntegral (ord cVal)))])
                                                                     | otherwise = (lstVarTypTupel, instructionsList ++ [(Sipush (fromIntegral (ord cVal)))])



loadBooleanToStackInstruction :: Bool -> [ListVarTypTupel] -> Instructions -> ReturnValueTupel
loadBooleanToStackInstruction bVal lstVarTypTupel instructionsList | bVal = (lstVarTypTupel, instructionsList ++ [(Iconst_1)])
                                                                   | otherwise = (lstVarTypTupel, instructionsList ++ [(Iconst_0)])

loadIntegerToStackInstruction :: Integer -> [ListVarTypTupel] -> Dictionary -> Instructions -> ReturnValueTupel
loadIntegerToStackInstruction iVal lstVarTypTupel cp instructionsList | iVal > 32768 = let
                                                                        indexCP = (fromIntegral (fromJust (dictLookUp ("Int "++ (show iVal)) cp))) -- TODO anpassen constant int im cp
                                                                        newInstructionLst | indexCP > 256 = instructionsList ++ [(Ldc_W (fromIntegral indexCP))]
                                                                                          | otherwise = instructionsList ++ [(Ldc (fromIntegral indexCP))]
                                                                        in (lstVarTypTupel, newInstructionLst)
                                                                     | iVal > 127 = (lstVarTypTupel, instructionsList ++ [(Sipush (fromIntegral iVal))])
                                                                     | iVal < (-32769) = let
                                                                        indexCP = (fromIntegral (fromJust (dictLookUp ("Int "++ (show iVal)) cp))) -- TODO anpassen constant int im cp
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


searchIdentInListVarTypTupel :: Identifier -> [ListVarTypTupel] -> Maybe Word8 -- -> existiert die funktion schon?
searchIdentInListVarTypTupel (Identifier iden) [] = Nothing
searchIdentInListVarTypTupel (Identifier iden) (x : []) | (getVarNameOfListVarTypTupel x) == iden = Just (getIndexOfListVarTypTupel x)
searchIdentInListVarTypTupel (Identifier iden) (x :xs) | (getVarNameOfListVarTypTupel x) == iden = Just (getIndexOfListVarTypTupel x)
                                                       | otherwise = (searchIdentInListVarTypTupel (Identifier iden) xs)


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

                                                       -- (TypedExpression
--  ((TernaryIf
--   (TypedExpression ((PrimBinOp
--                      Less
--                      (TypedExpression ((Literal (IntegerL 4)), (PrimType Int)))
--                      (TypedExpression ((Literal (IntegerL 4)), (PrimType Int)))), (PrimType Boolean)))
--    (TypedExpression ((Literal (IntegerL 4)), (PrimType (Int))))
--    (TypedExpression ((Literal (IntegerL 4)), (PrimType (Int))))), (PrimType Int)))





