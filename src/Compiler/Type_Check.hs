module Compiler.Type_Check(typecheck, typecheckprogram) where

import Compiler.Ast
import Compiler.Parser 
import Compiler.Utils
import Data.List(lookup)
import Data.Tuple(swap)
import Data.Maybe(fromJust)


type Symtab = [(Identifier, Type)]

throw :: String -> Either Error b
throw s = Left $ SemanticError s

nottyped :: Error
nottyped = InternalError "Typecheck-method returned non typed expression or statement."


typecheck :: [Class] -> Either Error [Class]
typecheck = reduceErrors . typecheckprogram

typecheckteststring :: String -> Either Error [Class]
typecheckteststring str = 
  case parseTestString str of
    Nothing  -> Left (ParseError "The test string could not be parsed.")
    Just cls -> (reduceErrors . typecheckprogram) cls 

typecheckprogram :: [Class] -> [Either Error Class]
typecheckprogram cls = map (typecheckclass cls) cls

typecheckclass :: [Class] -> Class -> Either Error Class
typecheckclass cls (Class ident@(Identifier id) mods decls) 
 | not (validconstructors decls) =  throw ("Invalid Constructors in class " ++ id) 
 | not (validmethods decls) = throw ("Invalid methods in class " ++ id ++ ". Maybe double declarations?")
 | not (modsOk mods) = throw ("Invalid modifier of class " ++ id) 
 | elem Protected mods = throw ("Modifier protected not allowed here. Class " ++ id) 
 | elem Private mods = throw ("Modifier private not allowed here. Class " ++ id) 
 | elem Static mods =  throw ("Modifier static not allowed here. Class " ++ id) 
 | otherwise = case typecheckdecls ident cls decls of 
    Left (SemanticError err)      -> throw ("Class " ++ id ++ " : " ++ err) 
    Left err -> Left err
    Right typeddecls  -> Right (Class ident mods (snd typeddecls))

typecheckdecls :: Identifier -> [Class] -> [Decl] -> Either Error (Symtab, [Decl])
typecheckdecls ident cls = foldl (tchelper ident cls) (Right ([(This, RefType (Name [] ident))], []))
  where
    tchelper _ _ (Left err) _ = Left err
    tchelper ident cls (Right tuple) decl = typecheckdecl ident cls tuple decl 


typecheckdecl :: Identifier -> [Class] -> (Symtab, [Decl]) -> Decl -> Either Error (Symtab , [Decl]) 

typecheckdecl _ cls (symtab, ast) decl@(Field (VarDecl ident@(Identifier id) mods typ rhs)) 
 | not (modsOk mods) = throw ("Invalid modifiers in declaration of field variable " ++ id) 
 | otherwise = 
  case lookup ident symtab of
   (Just _) -> throw ("Variable " ++ id ++ " already defined.") 
   Nothing -> 
    case rhs of
      Nothing -> Right ((ident, typ) : symtab, ast ++ [decl])
      Just expr -> 
        case typecheckexpr cls symtab expr of
         Left (SemanticError err) -> throw ("Field " ++ id ++ " : " ++ err) 
         Left err -> Left err
         Right exp@(TypedExpression(_, exptype)) -> 
           if typ == exptype then
                     Right ((ident, typ) : symtab, ast ++ [(Field (VarDecl ident mods typ (Just exp)))])
           else 
                     throw ("Field variable " ++ id ++ " : Incompatible types, " ++ (show exptype) ++ " cannot be converted to " ++ (show typ))

typecheckdecl clident cls (symtab, ast) constr@(Constructor ident mods paramlist body)  
 | clident /= ident = throw ("Constructor name does not match class.")  
 | not (modsOk mods) = throw ("Invalid modifiers in constructor.") 
 | elem Static mods = throw ("Modifier static not allowed in constructor.") 
 | elem Abstract mods = throw ("Modifier abstract not allowed in constructor.")
 | otherwise = 
  case body of 
    Nothing   -> Right (symtab, ast ++ [constr])
    Just stmt -> 
      case typecheckstmt cls ((map swap paramlist) ++ symtab) stmt of
        Left (SemanticError err) -> throw ("Constructor : " ++ err) 
        Left err -> Left err
        Right stmt@(TypedStatement(_, JVoid)) -> Right (symtab, ast ++ [Constructor ident mods paramlist (Just stmt)])
        Right (TypedStatement(_, _)) -> throw "Unexpected return value in constructor."
        _ -> Left nottyped 

typecheckdecl _ cls (symtab, ast) method@(Method ident@(Identifier id) mods rettype paramlist body) 
 | not (modsOk mods) = throw ("Invalid modifiers in method declaration of " ++ id) 
 | elem Abstract mods = throw ("Modifier abstract not allowed in method declaration of " ++ id) 
 | withdupls (map snd paramlist) = throw ("Double definition of parameters in method " ++ id)
 | otherwise = 
  case body of
    Nothing   -> Right (symtab, ast ++ [method]) 
    Just stmt -> let symt = if elem Static mods then map swap paramlist else ((map swap paramlist) ++ symtab) in
      case typecheckstmt cls symt stmt of
        Left (SemanticError err) -> throw ("Method " ++ id ++ " : " ++ err) 
        Left err -> Left err
        Right stm@(TypedStatement(_, stmtype)) ->
          if rettype == stmtype then
                    Right (symtab, ast ++ [(Method ident mods rettype paramlist (Just stm))])
          else
                   throw ("Method " ++ id ++ " : Incompatible types, " ++ (show stmtype) ++ " cannot be converted to " ++ (show rettype))  








 
typecheckexpr :: [Class] -> Symtab -> Expression -> Either Error Expression 

typecheckexpr cls symtab (TernaryIf cond elseexpr thenexpr) = 
 case typecheckexpr cls symtab cond of
   Left err -> Left err
   Right (condt@(TypedExpression(_, condtype))) -> if not (condtype == PrimType Boolean) 
     then throw ("Condition of ternaryIf must be boolean but has type " ++ (show condtype))
     else case (typecheckexpr cls symtab elseexpr, typecheckexpr cls symtab thenexpr) of
       (Left err, _) -> Left err
       (_, Left err) -> Left err
       (Right telse@(TypedExpression(_, elsetype)), Right tthen@(TypedExpression(_, thentype))) ->
          case upperbound elsetype thentype of
            Nothing  -> throw ("Cannot match type " ++ (show elsetype) ++ " and type " ++ (show thentype))
            Just typ -> Right (TypedExpression(TernaryIf condt telse tthen, typ))
  
typecheckexpr cls symtab (PrimBinOp binop expr1 expr2) =
  case (typecheckexpr cls symtab expr1, typecheckexpr cls symtab expr2) of 
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right exp1@(TypedExpression(_, exp1type)), Right exp2@(TypedExpression(_, exp2type))) ->  
       case (exp1type, exp2type) of
         (PrimType Boolean, PrimType Boolean) ->
            if (binop == And || binop == Or || binop == Eq || binop == BitAnd || binop == BitOr || binop == BitXOr) then
                      Right (TypedExpression(PrimBinOp binop exp1 exp2, PrimType Boolean))
            else 
                     throw ("Binary operator " ++ (show binop) ++ " not applicable to type boolean.") 
         (PrimType Int, PrimType Int) ->
            if (binop == Multiply || binop == Divide || binop == Add || binop == Subtract || binop == Modulo || binop == ShiftLeft || binop == ShiftRight || binop == UnsignedShiftRight || binop == BitAnd || binop == BitOr || binop == BitXOr) then
                      Right (TypedExpression(PrimBinOp binop exp1 exp2, PrimType Int))
            else
              if (binop == Eq || binop == Less ||  binop == LessEq || binop == Greater || binop == GreaterEq) then
                      Right (TypedExpression(PrimBinOp binop exp1 exp2, PrimType Boolean))
              else
                      throw ("Binary operator " ++ (show binop) ++ " not applicable to type int.")
         (PrimType Char, PrimType Char) ->
            if (binop == Eq || binop == Less || binop == LessEq || binop == Greater || binop == GreaterEq) then
                      Right (TypedExpression(PrimBinOp binop exp1 exp2, PrimType Boolean))
            else
                     throw ("Binary operator " ++ (show binop) ++ " not applicable to type char.") 
         (typ1, typ2) ->
                     throw ("Binary operator " ++ (show binop) ++ " not applicable to types " ++ (show typ1) ++ " and " ++ (show typ2)) 
   
typecheckexpr cls symtab (PrimUnOp unop expr) =  
  case typecheckexpr cls symtab expr of
    Left err -> Left err
    Right exp@(TypedExpression(_, exptype)) ->
      case (unop, exptype) of
        (Not, PrimType Boolean) -> Right (TypedExpression(PrimUnOp unop exp, PrimType Boolean))
	(Neg, PrimType Int)     -> Right (TypedExpression(PrimUnOp unop exp, PrimType Int))
	(BitCompl, PrimType Int) -> Right (TypedExpression(PrimUnOp unop exp, PrimType Int))
	otherwise -> throw ("Unary operator " ++ (show unop) ++ " not applicable to type " ++ (show exptype) ++ ".")
  
typecheckexpr cls symtab expr@(Iden name) = 
  case name of
    (Name [] ident) -> case lookup ident symtab of 
            Nothing    -> throw ("Not in scope, " ++ (show ident)) 
            (Just typ) -> Right (TypedExpression(expr, typ)) 
    (Name (x:xs) ident) -> case lookup x symtab of
            Nothing -> throw ("Not in scope, " ++ (show x)) 
            (Just (RefType (Name [] c))) -> case findclass c cls of
               Nothing   -> throw ("Cannot find class " ++ (show c)) 
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
    Right (TypedExpression(_, typ)) -> throw ("Condition of while loop must be of type boolean, but has type " ++ (show typ))
    Right _     -> Left nottyped

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
                 let uppertype = upperbound stm1type stm2type in
                   if uppertype == Nothing 
                   then throw ("then- and else- statements resolve to incompatible types, namely " ++ (show stm1type) ++ " and " ++ (show stm2type))  
                   else Right (TypedStatement(If condexp (Just stm1) (Just stm2), fromJust uppertype)) 
    Right (TypedExpression(_, typ)) -> throw ("If-Condition must be of type boolean, but has type " ++ (show typ))
    Right _ -> Left nottyped 
                        
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
        throw ("Incompatible types, " ++ (show exptype) ++ " cannot be converted to " ++ (show typ) ++ ".")

typecheckstmt cls symtab (StmtExprStmt stmtexpr) = 
  case typecheckstmtexpr cls symtab stmtexpr of
    Left err -> Left err
    Right stmexp@(TypedStmtExpr(_, _)) -> Right (TypedStatement(StmtExprStmt stmexp, JVoid))

typecheckstmt _ _ stmt@(TypedStatement _) = Right stmt

typecheckstmt _ _ Break = Right (TypedStatement(Break, JVoid))
typecheckstmt _ _ Continue = Right (TypedStatement(Continue, JVoid))


typecheckstmtexpr :: [Class] -> Symtab -> StmtExpr -> Either Error StmtExpr

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
                                           else throw ("Operator " ++ (show assignop) ++ " not applicable to type int") 
           (PrimType Boolean, PrimType Boolean) 
                                        -> if elem assignop booltobool
                                           then Right (TypedStmtExpr(Assign assignop name texpr, PrimType Boolean))
                                           else throw ("Operator " ++ (show assignop) ++ " not applicable to type boolean") 
           otherwise                     -> if assignop == NormalAssign 
                                           then if vartype == exprtype 
                                                then Right (TypedStmtExpr(Assign assignop name texpr, vartype))
                                                else throw ("Incompatible types, " ++ (show exprtype) ++ " cannot be converted to " ++ (show vartype) ++ ".")
                                           else throw ("Operator " ++ (show assignop) ++ " not applicable to types " ++ (show vartype) ++ " and " ++ (show exprtype) ++ ".")  
  where
   inttoint = [NormalAssign, MultiplyAssign, DivideAssign, ModuloAssign, PlusAssign, MinusAssign, LeftShiftAssign, ShiftRightAssign, UnsignedShiftRightAssign, BitXOrAssign]
   booltobool = [NormalAssign, AndAssign, BitXOrAssign, OrAssign]


typecheckstmtexpr cls symtab (Instantiation typ@(Name [] c) exprs) = 
  case reduceErrors $ (map (typecheckexpr cls symtab)) exprs of
    Left err -> Left err
    Right exprlist -> 
      case findclass c cls of 
        Nothing -> throw ("Cannot find class " ++ (show c)) 
        Just cl -> 
          let 
            constrtypes = getconstrtypes cl
            exprtypes = map typeofexpr exprlist
          in 
            if (or $ map ((==) exprtypes) constrtypes) || (null constrtypes && null exprtypes)
	      then Right (TypedStmtExpr(Instantiation typ exprlist, RefType typ)) 
	      else throw ("Class " ++ (show c) ++ " does not have a constructor accepting parameters with types " ++ (show exprtypes)) 
	     
typecheckstmtexpr cls symtab (Apply iden@(Iden (Name path ident)) params) = 
  case reduceErrors $ (map (typecheckexpr cls symtab)) params of
   Left err -> Left err
   Right exprlist ->
    case path of
      []     -> typecheckstmtexpr cls symtab (Apply (Iden (Name [This] ident)) params)    
      (p:ps) -> case lookup p symtab of 
                  Nothing -> throw ("Not in scope, " ++ (show p)) 
                  Just (RefType (Name [] clid)) -> case findclass clid cls of
                    Nothing -> throw ("Cannot find class " ++ (show clid)) 
                    Just cl -> case lookupmethods cls cl (Name ps ident) of
                      Left err -> Left err
                      Right [] -> throw ("Cannot find method " ++ (show ident)) 
                      Right (decls@(decl:_)) -> if or $ map ((==) (map typeofexpr exprlist)) (map ((map fst) . getParamList) decls)  
                        then Right (TypedStmtExpr(Apply iden exprlist, getReturnType decl))
                        else throw ("Method " ++ (show ident) ++ " does not accept parameters of type " ++ (show (map typeofexpr exprlist)))


typecheckstmtexpr cls symtab  (SEUnOp op expr) = 
  case typecheckexpr cls symtab expr of
    Left err -> Left err
    Right (texpr@(TypedExpression(_, exprtype))) -> if exprtype == PrimType Int 
      then Right (TypedStmtExpr(SEUnOp op texpr, exprtype))
      else throw ((show op) ++ " not applicable to type " ++ (show exprtype)) 

typecheckstmtexpr _ _ stmtexpr@(TypedStmtExpr(_)) = Right stmtexpr
        




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


lookupmethods :: [Class] -> Class -> Name -> Either Error [Decl] 
lookupmethods _ (Class _ _ decls) (Name [] ident) =  Right $ filter method decls 
  where
    method (Method id _ _ _ _) = id == ident 
    method _ = False
lookupmethods cls c (Name (p:ps) ident) = case lookupidenttype c p of
    Left err -> Left err
    Right (RefType (Name [] classname)) -> case findclass classname cls of
      Nothing -> throw ("Could not find class " ++ (show classname)) 
      Just cl -> lookupmethods cls cl (Name ps ident)  
    Right typ -> throw ((show p) ++ " of type " ++ (show typ) ++ " cannot have a method.") 
         
lookupnametype :: [Class] -> Class -> Name -> Either Error Type
lookupnametype _ c (Name [] ident) = lookupidenttype c ident
lookupnametype cls c (Name (p:ps) ident) = case lookupidenttype c p of
   Left err -> Left err
   Right (RefType (Name [] classname)) -> case findclass classname cls of
     Nothing -> throw ("Could not find class " ++ (show classname)) 
     (Just cl) -> lookupnametype cls cl (Name ps ident)
   Right typ -> throw ((show p) ++ " of type " ++ (show typ) ++ " cannot have a field.") 
                      
lookupidenttype :: Class ->  Identifier -> Either Error Type
lookupidenttype (Class _ mods decls) ident = finddecltype ident decls 

finddecltype :: Identifier -> [Decl] -> Either Error Type 
finddecltype ident [] = throw ("Not in scope, " ++ (show ident)) 
finddecltype ident ((Field (VarDecl id mods typ _)):decls) = if id == ident then
      if elem Private mods 
        then throw ((show id) ++ " is private.") 
        else Right typ 
   else finddecltype ident decls
finddecltype ident (_:decls) = finddecltype ident decls
  
    
getconstrtypes :: Class -> [[Type]]
getconstrtypes (Class _ _ decls) = getconstrtypesfromdecls decls
getconstrtypesfromdecls decls = map extrtypes $ filter isconstr decls
  where
    extrtypes (Constructor _ _ pls _) = map fst pls
    isconstr (Constructor _ _ _ _)    = True
    isconstr _                        = False
     
validconstructors :: [Decl] -> Bool
validconstructors decls = not (withdupllists (getconstrtypesfromdecls decls)) 


validmethods :: [Decl] -> Bool
validmethods = checkmethodshelper []

checkmethodshelper :: [((Identifier, Type), [Type])] -> [Decl] -> Bool
checkmethodshelper _ [] = True
checkmethodshelper ls ((Method ident _ rt pl _):decls) = 
  let 
    idt = (ident, rt)
    mtypes = map fst pl 
    mparams = map snd pl
  in 
    if mok ls idt mtypes then checkmethodshelper ((idt, mtypes):ls) decls
                         else False 
  where
    mok [] _ _ = True
    mok ((tup, mtypes) : ls) idt pl = if idt == tup 
         then if (mtypes == pl) then False else mok ls idt pl 
         else mok ls idt pl 

checkmethodshelper ls (decl:decls) = checkmethodshelper ls decls


withdupls :: Eq a => [a] -> Bool
withdupls [] = False
withdupls (x:xs) = if elem x xs then True else withdupls xs
 
withdupllists :: Eq a => [[a]] -> Bool
withdupllists [] = False
withdupllists (x:xs) = if or (map ((==) x) xs) then True else withdupllists xs




modsOk :: [Mod] -> Bool
modsOk [] = True
modsOk (Static:mods) = if elem Abstract mods || elem Static mods then False else modsOk mods
modsOk (Abstract:mods) = if elem Abstract mods || elem Static mods then False else modsOk mods
modsOk (pubpropri:mods) = if elem Public mods || elem Protected mods || elem Private mods then False else modsOk mods


upperbound :: Type -> Type -> Maybe Type
upperbound typ JVoid = Just typ
upperbound JVoid typ = Just typ
upperbound typ1@(PrimType _) typ2@(PrimType _) = if typ1 == typ2 then Just typ1 else Nothing
upperbound typ1@(RefType _) typ2@(RefType _) = if typ1 == typ2 then Just typ1 else Just (RefType (Name [] (Identifier "Object")))
upperbound _ _ = Nothing


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
                 -> let uppertype = upperbound stmtype typestmts in
                     if uppertype == Nothing 
                     then throw ("Block statements resolve to incompatible types, namely " ++ (show stmtype) ++ " and " ++ (show typestmts)) 
                     else Right (stm:stmlist, fromJust uppertype)
                 

