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




typecheckteststring :: String -> [Either Error Class]
typecheckteststring str = 
  case parseTestString str of
    Nothing  -> [Left (MiscError "The test string could not be parsed.")]
    Just cls -> typecheckprogram cls 

typecheckprogram :: [Class] -> [Either Error Class]
typecheckprogram cls = map (typecheckclass cls) cls

typecheckclass :: [Class] -> Class -> Either Error Class
typecheckclass cls (Class ident mods decls) = 
  case typecheckdecls cls decls of 
    Left err      -> Left err
    Right typeddecls  -> Right (Class ident mods (snd typeddecls))

typecheckdecls :: [Class] -> [Decl] -> Either Error (Symtab, [Decl])
typecheckdecls cls = foldl (tchelper cls) (Right ([], []))
  where
    tchelper _ (Left err) _ = Left err
    tchelper cls (Right tuple) decl = typecheckdecl cls tuple decl 

typecheckdecl :: [Class] -> (Symtab, [Decl]) -> Decl -> Either Error (Symtab , [Decl]) 

typecheckdecl cls (symtab, ast) decl@(Field (VarDecl ident mods typ rhs)) = 
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
    
typecheckdecl cls (symtab, ast) constr@(Constructor _ _ _ _) = undefined

typecheckdecl cls (symtab, ast) method@(Method ident mods rettype paramlist body) = 
  case body of
    Nothing   -> Right ((ident, rettype) : symtab, ast ++ [method]) --is this true?
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
  case lookupname cls symtab name of
    Nothing   -> Left tcerror
    Just texp-> Right (TypedExpression(expr, texp))
      
typecheckexpr _ _ expr@(Literal lit) = case lit of 
   (IntegerL _) -> Right (TypedExpression(expr, PrimType Int))
   (BooleanL _) -> Right (TypedExpression(expr, PrimType Boolean))
   (CharL _)    -> Right (TypedExpression(expr, PrimType Char))
   (StringL _)  -> Right (TypedExpression(expr, RefType (Name [] (Identifier "String")))) 
   Null         -> Right (TypedExpression(expr, JVoid))
 
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
    Right stmexp@(TypedStmtExpr(_, stmexptype)) -> Right (TypedStatement(StmtExprStmt stmexp, stmexptype))

typecheckstmt _ _ stmt@(TypedStatement _) = Right stmt


typecheckstmtexpr :: [Class] -> Symtab -> StmtExpr -> Either Error StmtExpr
typecheckstmtexpr = undefined





-- better Either Error Type ?
upperbound :: Type -> Type -> Type 
upperbound typ JVoid = typ
upperbound JVoid typ = typ 
upperbound typ1 typ2  = if typ1 == typ2 then typ1 else RefType (Name [] (Identifier "Object"))

-- must be implemented + lookup check
lookupname :: [Class] -> Symtab -> Name -> Maybe Type 
lookupname _ symtab (Name [] ident) = lookup ident symtab
lookupname _ _ _ = undefined 



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
                 

