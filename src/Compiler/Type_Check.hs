module Compiler.Type_Check where

import Compiler.Ast
import Compiler.Parser 
import Data.List(lookup)
import Data.Tuple(swap)


data Error = TypecheckError String
           | MiscError String
  deriving Show

type Symtab = [(Identifier, Type)]

tcerror = TypecheckError "A typecheck error occurred"




typecheckteststring :: String -> Either Error [Class]
typecheckteststring str = 
  case parseTestString str of
    Nothing  -> Left (MiscError "The test string could not be parsed.")
    Just cls -> (reduceErrors . typecheckprogram) cls 

typecheckprogram :: [Class] -> [Either Error Class]
typecheckprogram cls = map (typecheckclass cls) cls

-- todo check for duplications
typecheckclass :: [Class] -> Class -> Either Error Class
typecheckclass cls (Class ident mods decls) = if notElem Private mods then 
  case typecheckdecls ident cls decls of 
    Left err      -> Left err
    Right typeddecls  -> Right (Class ident mods (snd typeddecls))
  else Left tcerror

typecheckdecls :: Identifier -> [Class] -> [Decl] -> Either Error (Symtab, [Decl])
typecheckdecls ident cls = foldl (tchelper ident cls) (Right ([(This, RefType (Name [] ident))], []))
  where
    tchelper _ _ (Left err) _ = Left err
    tchelper ident cls (Right tuple) decl = typecheckdecl ident cls tuple decl 

typecheckdecl :: Identifier -> [Class] -> (Symtab, [Decl]) -> Decl -> Either Error (Symtab , [Decl]) 

typecheckdecl _ cls (symtab, ast) decl@(Field (VarDecl ident mods typ rhs)) = 
  case rhs of
    Nothing -> Right ((ident, typ) : symtab, ast ++ [decl])
    Just expr -> 
      case typecheckexpr cls symtab expr of
         Left err -> Left err
         Right exp@(TypedExpression(_, exptype)) -> 
           if typ == exptype then
                     Right ((ident, typ) : symtab, ast ++ [(Field (VarDecl ident mods typ (Just exp)))])
           else 
                     Left tcerror

typecheckdecl clident cls (symtab, ast) constr@(Constructor ident mods paramlist body)  
 | clident /= ident = Left tcerror
 | elem Private mods = Left tcerror
 | otherwise = 
  case body of 
    Nothing   -> Right (symtab, ast ++ [constr])
    Just stmt -> if returningstmt stmt then Left tcerror 
      else case typecheckstmt cls ((map swap paramlist) ++ symtab) stmt of
        Left err -> Left err
        Right stmt@(TypedStatement(_, JVoid)) -> Right (symtab, ast ++ [Constructor ident mods paramlist (Just stmt)])
        otherwise -> Left tcerror

typecheckdecl _ cls (symtab, ast) method@(Method ident mods rettype paramlist body) = 
  case body of
    Nothing   -> Right (symtab, ast ++ [method]) 
    Just stmt ->
      case typecheckstmt cls ((map swap paramlist) ++ symtab) stmt of
        Left err -> Left err
        Right stm@(TypedStatement(_, stmtype)) ->
          if rettype == stmtype then
                    Right (symtab, ast ++ [(Method ident mods rettype paramlist (Just stm))])
          else
                    Left tcerror                    








 
typecheckexpr :: [Class] -> Symtab -> Expression -> Either Error Expression 

typecheckexpr cls symtab (TernaryIf cond elseexpr thenexpr) = undefined
  
-- todo adapt to new binops from AST
typecheckexpr cls symtab (PrimBinOp binop expr1 expr2) =
  case (typecheckexpr cls symtab expr1, typecheckexpr cls symtab expr2) of 
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right exp1@(TypedExpression(_, exp1type)), Right exp2@(TypedExpression(_, exp2type))) ->  
       case (exp1type, exp2type) of
         (PrimType Boolean, PrimType Boolean) ->
            if (binop == And || binop == Or || binop == Eq) then
                      Right (TypedExpression(PrimBinOp binop exp1 exp2, PrimType Boolean))
            else 
                      Left tcerror 
         (PrimType Int, PrimType Int) ->
            if (binop == Multiply || binop == Divide || binop == Add || binop == Subtract || binop == Modulo) then
                      Right (TypedExpression(PrimBinOp binop exp1 exp2, PrimType Int))
            else
              if (binop == Eq || binop == Less ||  binop == LessEq || binop == Greater || binop == GreaterEq) then
                      Right (TypedExpression(PrimBinOp binop exp1 exp2, PrimType Boolean))
              else
                      Left tcerror
         (PrimType Char, PrimType Char) ->
            if (binop == Eq) then
                      Right (TypedExpression(PrimBinOp binop exp1 exp2, PrimType Boolean))
            else
                      Left tcerror
         otherwise ->
                      Left tcerror 
   
typecheckexpr cls symtab (PrimUnOp unop expr) =  
  case typecheckexpr cls symtab expr of
    Left err -> Left err
    Right exp@(TypedExpression(_, exptype)) ->
      case (unop, exptype) of
        (Not, PrimType Boolean) -> Right (TypedExpression(PrimUnOp unop exp, PrimType Boolean))
	(Neg, PrimType Int)     -> Right (TypedExpression(PrimUnOp unop exp, PrimType Int))
	(BitCompl, PrimType Int) -> Right (TypedExpression(PrimUnOp unop exp, PrimType Int))
	otherwise -> Left tcerror 
  
typecheckexpr cls symtab expr@(Iden name) = 
  case name of
    (Name [] ident) -> case lookup ident symtab of 
            Nothing    -> Left tcerror
            (Just typ) -> Right (TypedExpression(expr, typ)) 
    (Name (x:xs) ident) -> case lookup x symtab of
            Nothing -> Left tcerror
            (Just (RefType (Name [] c))) -> case findclass c cls of
               Nothing   -> Left tcerror
               (Just cl) -> case lookupnametype cls cl (Name xs ident) of 
                 Left err   -> Left err
                 Right typ  -> Right (TypedExpression(expr, typ)) 

typecheckexpr _ _ expr@(Literal lit) = case lit of 
   (IntegerL _) -> Right (TypedExpression(expr, PrimType Int))
   (BooleanL _) -> Right (TypedExpression(expr, PrimType Boolean))
   (CharL _)    -> Right (TypedExpression(expr, PrimType Char))
   (StringL _)  -> Right (TypedExpression(expr, RefType (Name [] (Identifier "String")))) 
   Null         -> Right (TypedExpression(expr, JVoid))
 

typecheckexpr cls symtab (ExprExprStmt stmtexpr) =
  case typecheckstmtexpr cls symtab stmtexpr of
    Left err -> Left err
    Right (stex@(TypedStmtExpr(_, typ))) -> Right (TypedExpression(ExprExprStmt stex, typ))


typecheckexpr cls symtab (Cast typ expr) = case typecheckexpr cls symtab expr of
  Left err    -> Left err
  Right texpr -> Right (TypedExpression(Cast typ texpr, typ))

typecheckexpr _ _ expr@(TypedExpression(_, _)) = Right expr 


typecheckexpr _ _ _ = undefined









typecheckstmt :: [Class] -> Symtab -> Statement -> Either Error Statement

typecheckstmt cls symtab (While cond body) =
  case typecheckexpr cls symtab cond of 
    Left err    -> Left err
    Right condexp@(TypedExpression(_, PrimType Boolean)) ->
      case body of 
        Nothing -> Right (TypedStatement(While condexp body, JVoid))
        (Just stmt) -> case typecheckstmt cls symtab stmt of 
           Left err -> Left err
           Right stm@(TypedStatement(_, stmtype)) -> Right (TypedStatement(While condexp (Just stm), stmtype))
    otherwise   -> Left tcerror

typecheckstmt cls symtab (If cond ifthen ifelse) =
  case typecheckexpr cls symtab cond of 
    Left err -> Left err
    Right condexp@(TypedExpression(_, PrimType Boolean)) ->
      case (ifthen, ifelse) of 
        (Nothing, Nothing) -> Right (TypedStatement(If condexp ifthen ifelse, JVoid)) 
        (Just stmt1, Nothing) -> case typecheckstmt cls symtab stmt1 of
          Left err -> Left err
          Right stm@(TypedStatement(_, stmtype)) -> Right (TypedStatement(If condexp (Just stm) ifelse, stmtype)) 
        (Nothing, Just stmt2) -> case typecheckstmt cls symtab stmt2 of
          Left err -> Left err
          Right stm@(TypedStatement(_, stmtype)) -> Right (TypedStatement(If condexp ifthen (Just stm), stmtype)) 
        (Just stmt1, Just stmt2) -> case (typecheckstmt cls symtab stmt1, typecheckstmt cls symtab stmt2) of
          (Left err, _) -> Left err
          (_, Left err) -> Left err
          (Right stm1@(TypedStatement(_, stm1type)), Right stm2@(TypedStatement(_, stm2type))) ->
                           Right (TypedStatement(If condexp (Just stm1) (Just stm2), upperbound stm1type stm2type)) 
    otherwise -> Left tcerror 
                        
typecheckstmt cls symtab (Block stmts) = 
  case blockhlp cls symtab stmts of
    Left err -> Left err
    Right (stmtlist, typestmts) -> Right (TypedStatement(Block stmtlist, typestmts)) 

typecheckstmt _ _ stmt@(Return Nothing) = Right (TypedStatement(stmt, JVoid))
typecheckstmt cls symtab (Return (Just expr)) = 
  case typecheckexpr cls symtab expr of
    Left err -> Left err
    Right exp@(TypedExpression(_, exptype)) -> Right (TypedStatement(Return (Just exp), exptype))

typecheckstmt _ _ stmt@(LocalVar (VarDecl _ _ typ Nothing)) = Right (TypedStatement(stmt, typ))
typecheckstmt cls symtab (LocalVar (VarDecl ident mods typ (Just expr))) =
  case typecheckexpr cls symtab expr of
    Left err -> Left err
    Right exp@(TypedExpression(_, exptype)) ->
      if typ == exptype then
        Right (TypedStatement(LocalVar (VarDecl ident mods typ (Just exp)), typ))
      else
        Left tcerror

typecheckstmt cls symtab (StmtExprStmt stmtexpr) = 
  case typecheckstmtexpr cls symtab stmtexpr of
    Left err -> Left err
    Right stmexp@(TypedStmtExpr(_, _)) -> Right (TypedStatement(StmtExprStmt stmexp, JVoid))

typecheckstmt _ _ stmt@(TypedStatement _) = Right stmt


typecheckstmtexpr :: [Class] -> Symtab -> StmtExpr -> Either Error StmtExpr

-- todo correct op assignments? 
typecheckstmtexpr cls symtab (Assign assignop name expr) = 
  case typecheckexpr cls symtab (Iden name) of 
   Left err -> Left err
   Right (TypedExpression(_, vartype)) -> 
     case typecheckexpr cls symtab expr of 
       Left err -> Left err
       Right texpr@(TypedExpression(_, exprtype)) -> 
         case (vartype, exprtype) of
           (PrimType Int, PrimType Int) -> if elem assignop inttoint 
                                           then Right (TypedStmtExpr(Assign assignop name texpr, PrimType Int)) 
                                           else Left tcerror
           (PrimType Boolean, PrimType Boolean) 
                                        -> if elem assignop booltobool
                                           then Right (TypedStmtExpr(Assign assignop name texpr, PrimType Boolean))
                                           else Left tcerror
           otherwise -> Left tcerror
  where
   inttoint = [NormalAssign, MultiplyAssign, DivideAssign, ModuloAssign, PlusAssign, MinusAssign, LeftShiftAssign, ShiftRightAssign, UnsignedShiftRightAssign, BitXOrAssign]
   booltobool = [NormalAssign, AndAssign, BitXOrAssign, OrAssign]


typecheckstmtexpr cls symtab inst@(Instantiation typ@(Name [] c) exprs) = 
  case reduceErrors $ (map (typecheckexpr cls symtab)) exprs of
    Left err -> Left err
    Right exprlist -> 
      case findclass c cls of 
        Nothing -> Left tcerror
        Just cl -> 
          let 
            constrtypes = getconstrtypes cl
            exprtypes = map typeofexpr exprlist
          in 
            if (or $ map ((==) exprtypes) constrtypes) || (constrtypes == [] && exprtypes == [])
	      then Right (TypedStmtExpr(inst, RefType typ)) 
	      else Left tcerror 
	     
typecheckstmtexpr cls symtab (Apply iden@(Iden (Name path ident)) params) = 
  case reduceErrors $ (map (typecheckexpr cls symtab)) params of
   Left err -> Left err
   Right exprlist ->
    case path of
      []     -> typecheckstmtexpr cls symtab (Apply (Iden (Name [This] ident)) params)    
      (p:ps) -> case lookup p symtab of 
                  Nothing -> Left tcerror
                  Just (RefType (Name [] clid)) -> case findclass clid cls of
                    Nothing -> Left tcerror
                    Just cl -> case lookupmethods cls cl (Name ps ident) of
                      [] -> Left tcerror
                      decls@(decl:_) -> if or $ map ((==) (map typeofexpr exprlist)) (map ((map fst) . getParamList) decls)  
                        then Right (TypedStmtExpr(Apply iden exprlist, getReturnType decl))
                        else Left tcerror


typedcheckstmtexpr _ _ stmtexpr@(TypedStmtExpr(_)) = stmtexpr
        




reduceErrors :: [Either Error a] -> Either Error [a]  
reduceErrors = foldl helper (Right [])
  where
    helper (Left err) _ = Left err
    helper _ (Left err) = Left err
    helper (Right ls) (Right x) = Right (ls ++ [x])






emptyOr :: [Bool] -> Bool
emptyOr [] = True
emptyOr ls = or ls

typeofexpr :: Expression -> Type
typeofexpr (TypedExpression(_, typ)) = typ



findclass :: Identifier -> [Class] -> Maybe Class
findclass _ [] = Nothing
findclass ident ((c@(Class id _ _)):cs) = if ident == id then Just c else findclass ident cs 


lookupmethods :: [Class] -> Class -> Name -> [Decl] 
lookupmethods _ (Class _ _ decls) (Name [] ident) =  filter method decls 
  where
    method (Method id _ _ _ _) = id == ident 
    method _ = False
lookupmethods cls c (Name (p:ps) ident) = undefined
         
lookupnametype :: [Class] -> Class -> Name -> Either Error Type
lookupnametype _ c (Name [] ident) = lookupidenttype c ident
lookupnametype cls c (Name (p:ps) ident) = case lookupidenttype c p of
   Left err -> Left err
   Right (RefType (Name [] classname)) -> case findclass classname cls of
     Nothing -> Left tcerror
     (Just cl) -> lookupnametype cls cl (Name ps ident)
   otherwise -> Left tcerror 
                      
lookupidenttype :: Class ->  Identifier -> Either Error Type
lookupidenttype (Class _ mods decls) ident = if elem Private mods 
   then Left tcerror 
   else finddecltype ident decls 

finddecltype :: Identifier -> [Decl] -> Either Error Type 
finddecltype ident [] = Left tcerror
finddecltype ident ((Field (VarDecl id mods typ _)):decls) = if id == ident then
      if elem Private mods 
        then Left tcerror 
        else Right typ 
   else finddecltype ident decls
finddecltype ident (_:decls) = finddecltype ident decls
  
    
getconstrtypes :: Class -> [[Type]]
getconstrtypes (Class _ _ decls) = map extrtypes $ filter isconstr decls
  where
    extrtypes (Constructor _ _ pls _) = map fst pls
    isconstr (Constructor _ _ _ _)    = True
    isconstr _                        = False
     



returningstmt :: Statement -> Bool
returningstmt (Return _) = True
returningstmt (While _ (Just stmt)) = returningstmt stmt
returningstmt (If _ (Just stmt) _) = returningstmt stmt 
returningstmt (If _ _ (Just stmt)) = returningstmt stmt
returningstmt (Block stmts) = or (map returningstmt stmts)
returningstmt (TypedStatement(stmt, _)) = returningstmt stmt
returningstmt _ = False






--todo better Either Error Type, primtypes are no objects
upperbound :: Type -> Type -> Type 
upperbound typ JVoid = typ
upperbound JVoid typ = typ 
upperbound typ1 typ2  = if typ1 == typ2 then typ1 else RefType (Name [] (Identifier "Object"))


blockhlp :: [Class] -> Symtab -> [Statement] -> Either Error ([Statement], Type)
blockhlp _ _ [] = Right ([], JVoid)
blockhlp cls symtab (stmt:stmts) = 
  case typecheckstmt cls symtab stmt of
    Left err -> Left err
    Right (stm@(TypedStatement(LocalVar (VarDecl ident _ typ _), _))) ->
      case blockhlp cls ((ident, typ):symtab) stmts of
        Left err -> Left err
        Right (stmlist, typestmts)  
                 -> Right (stm:stmlist, typestmts)
    Right (stm@(TypedStatement(_, stmtype))) ->
      case blockhlp cls symtab stmts of
        Left err -> Left err
        Right (stmlist, typestmts)
                 -> Right (stm:stmlist, upperbound stmtype typestmts)
                 

