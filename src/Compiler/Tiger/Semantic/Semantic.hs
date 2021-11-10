module Compiler.Tiger.Semantic.Semantic
  ( Exp (..),
    ExpTy (..),
    transExpr,
  )
where

import Compiler.Tiger.AbSyn
import Compiler.Tiger.Lexer (Pos (Pos))
import Compiler.Tiger.Lexer_ (lexer)
import Compiler.Tiger.Parser_ (parser)
import Compiler.Tiger.Semantic.Env
import Compiler.Tiger.Semantic.STable (STable)
import qualified Compiler.Tiger.Semantic.Types as T
import Compiler.Tiger.Symbol
import Compiler.Tiger.Token (Pos)
import Control.Applicative
import Control.Monad.State.Strict
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List

data Exp = Exp deriving (Show)

data ExpTy = ExpTy (Maybe Exp) T.Type deriving (Show)

checkType :: T.Type -> T.Type -> Pos -> State Env Bool
checkType t1 t2 p = do
  actualTy1 <- actualType t1 p
  actualTy2 <- actualType t2 p
  campatible actualTy1 actualTy2
  where
    campatible (T.RecordType _) T.NilType = return True
    campatible T.NilType (T.RecordType _) = return True
    campatible t1 t2
      | t1 == t2 = return True
      | otherwise = error $ "type " ++ show t1 ++ " is not match with " ++ show t2 ++ " at position " ++ show p

--- Find actual Type of symbol
findType :: Sym -> Pos -> State Env T.Type
findType name pos = do
  maybeEntry <- getTenv name
  case maybeEntry of
    Just ty -> actualType ty pos
    _ -> error $ "undefiend type " ++ name ++ " at position " ++ show pos

transExpr :: Expr -> State Env ExpTy
transExpr (VarExpr v) = transVar v
transExpr (NilExpr _) = return $ ExpTy Nothing T.NilType
transExpr (SeqExpr exprs _) =
  if null exprs
    then return $ ExpTy Nothing T.Void
    else do
      expTys <- mapM transExpr exprs
      return $ last expTys
transExpr (IntExpr _ _) = return $ ExpTy Nothing T.IntType
transExpr (StringExpr _ _) = return $ ExpTy Nothing T.StringType
transExpr (UMinus exp p) = do
  ExpTy _ v_ty <- transExpr exp
  case v_ty of
    T.IntType -> return $ ExpTy Nothing v_ty
    _ -> error $ "\"-\" can apply in " ++ show v_ty ++ " type at position " ++ show p
transExpr (Call func_name args p) = do
  fEntry <- getVenv func_name
  case fEntry of
    Just (FuncEntry farmals return_ty) -> do
      argExpTys <- mapM transExpr args
      let argTys = fmap (\(ExpTy _ t) -> t) argExpTys
      sequence_ $ checkType <$> ZipList argTys <*> ZipList farmals <*> ZipList (fmap exprPos args)
      case (compare `on` length) argTys farmals of
        LT -> do
          let ty = farmals !! max 0 (length argTys)
          error $ "excepted " ++ show ty ++ " type arugment at position " ++ show p
        GT -> do
          let ty = argTys !! max 0 (length farmals)
          let arg = args !! max 0 (length farmals)
          error $ "unexcepted " ++ show ty ++ " type argument at position " ++ show (exprPos arg)
        EQ -> return ()
      return $ ExpTy Nothing return_ty
    _ -> error $ "undefined function " ++ func_name ++ " at position " ++ show p
transExpr (OpExpr e1 op e2 p) = do
  ExpTy _ t1 <- transExpr e1
  ExpTy _ t2 <- transExpr e2
  case op of
    PlusOp ->
      if t1 == T.IntType && t2 == T.IntType
        then return $ ExpTy Nothing T.IntType
        else error "\"+\" operator only work with both int type"
    MinusOp ->
      if t1 == T.IntType && t2 == T.IntType
        then return $ ExpTy Nothing T.IntType
        else error "\"+\" operator only work with both int type"
    TimesOp ->
      if t1 == T.IntType && t2 == T.IntType
        then return $ ExpTy Nothing T.IntType
        else error "\"*\" operator only work with both int type"
    DivideOp ->
      if t1 == T.IntType && t2 == T.IntType
        then return $ ExpTy Nothing T.IntType
        else error "\"/\" operator only work with both int type"
    EqOp -> do
      checkType t1 t2 p
      return $ ExpTy Nothing T.IntType
    NeqOp -> do
      checkType t1 t2 p
      return $ ExpTy Nothing T.IntType
    LtOp ->
      if comparable t1 t2
        then return $ ExpTy Nothing T.IntType
        else canNotCompareError t1 t2
    LeOp ->
      if comparable t1 t2
        then return $ ExpTy Nothing T.IntType
        else canNotCompareError t1 t2
    GtOp ->
      if comparable t1 t2
        then return $ ExpTy Nothing T.IntType
        else canNotCompareError t1 t2
    GeOp ->
      if comparable t1 t2
        then return $ ExpTy Nothing T.IntType
        else canNotCompareError t1 t2 p
  where
    comparable T.IntType T.IntType = True
    comparable T.StringType T.StringType = True
    comparable _ _ = False

    canNotCompareError t1 t2 = error $ "can not compare " ++ show t1 ++ " with " ++ show t2 ++ " at position " ++ show p
transExpr (RecordsExpr name fields p) = do
  checkFieldsDuplicate [] fields
  maybeType <- getTenv name
  case maybeType of
    Just ty -> do
      ty' <- actualType ty p
      case ty' of
        rcdTy@(T.RecordType records) -> do
          mapM_
            ( \(Field fieldName value fieldPos) -> do
                case lookup fieldName records of
                  Just ty -> do
                    ExpTy _ fieldTy <- transExpr value
                    checkType fieldTy ty p
                  Nothing -> error $ "unexcepted field " ++ fieldName ++ " at position " ++ show fieldPos
            )
            fields
          mapM_
            ( \(recordName, recordTy) -> do
                fieldTy <- case find ((== recordName) . field_name) fields of
                  Just (Field fieldName value fieldPos) -> do
                    ExpTy _ ty <- transExpr value
                    return ty
                  Nothing -> return T.NilType
                checkType fieldTy recordTy p
            )
            records
          return $ ExpTy Nothing rcdTy
        _ -> notFound
    _ -> notFound
  where
    notFound = error $ "undefined record type " ++ name ++ " at position " ++ show p

    checkFieldsDuplicate :: [Field] -> [Field] -> State Env Bool
    checkFieldsDuplicate xs [] = return True
    checkFieldsDuplicate xs (y : ys) =
      if any ((== field_name y) . field_name) xs
        then error $ "duplicate field " ++ field_name y ++ " at position " ++ show (field_pos y)
        else checkFieldsDuplicate (y : xs) ys
transExpr (ArrayExpr name size element p) = do
  entry <- getTenv name
  case entry of
    Just arrayType@(T.ArrayType elementTy) -> do
      ExpTy _ sizeTy <- transExpr size
      checkType sizeTy T.IntType (exprPos size)
      ExpTy _ elementTy' <- transExpr element
      checkType elementTy' elementTy (exprPos element)
      return $ ExpTy Nothing arrayType
    _ -> error $ "undefiend array type " ++ name
transExpr (AssignExpr lvalue v p) = do
  ExpTy _ lvalueTy <- transVar lvalue
  ExpTy _ vType <- transExpr v
  checkType lvalueTy vType (exprPos v)
  return $ ExpTy Nothing T.Void
transExpr (IFExpr pred succ Nothing p) = do
  ExpTy _ pTy <- transExpr pred
  checkType pTy T.IntType (exprPos pred)
  ExpTy _ sTy <- transExpr succ
  checkType sTy T.Void (exprPos succ)
  return $ ExpTy Nothing T.Void
transExpr (IFExpr pred succ (Just fail) p) = do
  ExpTy _ pTy <- transExpr pred
  checkType pTy T.IntType (exprPos pred)
  ExpTy _ sTy <- transExpr succ
  ExpTy _ fTy <- transExpr fail
  checkType sTy fTy p
  return $ ExpTy Nothing sTy
transExpr (WhileExpr pred body p) = do
  ExpTy _ p_ty <- transExpr pred
  checkType p_ty T.IntType (exprPos pred)
  transExpr body
transExpr (ForExpr v_name from to body _ p) = do
  ExpTy _ f_ty <- transExpr from
  ExpTy _ t_ty <- transExpr to
  checkType f_ty T.IntType (exprPos from)
  checkType t_ty T.IntType (exprPos to)
  scopeBegin
  putVenv v_name (VarEntry T.IntType)
  ExpTy _ bodyTy <- transExpr body
  scopeEnd
  return $ ExpTy Nothing bodyTy
transExpr (BreakExpr p) = return $ ExpTy Nothing T.Void
transExpr (LetExpr decs body p) = do
  scopeBegin
  mapM_ transDecHead decs
  mapM_ transDec decs
  ExpTy _ bodyTy <- transExpr body
  scopeEnd
  return $ ExpTy Nothing bodyTy

transVar :: Var -> State Env ExpTy
transVar (SimpleVar s p) = do
  v <- getVenv s
  case v of
    Just (VarEntry ty) -> return $ ExpTy Nothing ty
    _ -> error $ "undefined variable " ++ s ++ " at position " ++ show p
transVar (FieldVar v s p) = do
  ExpTy _ ty <- transVar v
  case ty of
    t@(T.RecordType records) -> case lookup s records of
      Nothing -> error $ "can not access field \"" ++ s ++ "\" of type " ++ show t ++ " at position " ++ show p
      Just t -> return $ ExpTy Nothing t
    t -> error $ "can not access field \"" ++ s ++ "\" of type " ++ show t ++ " at position " ++ show p
transVar (IndexedVar v exp p) = do
  ExpTy _ i_ty <- transExpr exp
  checkType i_ty T.IntType (exprPos exp)
  ExpTy _ v_ty <- transVar v
  case v_ty of
    T.ArrayType e_ty -> return $ ExpTy Nothing e_ty
    _ -> error $ "indexed is not a type of array at position " ++ show p

transDecHead :: Dec -> State Env ()
transDecHead dec = case dec of
  TypeDec name _ _ ->
    putTenv name $ T.NamedType name Nothing
  FuncDec name parameters Nothing body p ->
    putVenv name $ FuncEntry (fmap recordNamedType parameters) T.Void
  FuncDec name parameters (Just returnTypeName) body p ->
    putVenv name $ FuncEntry (fmap recordNamedType parameters) (T.NamedType returnTypeName Nothing)
  _ -> return ()
  where
    recordNamedType r = T.NamedType (record_type r) Nothing

actualType :: T.Type -> Pos -> State Env T.Type
actualType (T.NamedType name Nothing) p = do
  maybeTy <- getTenv name
  case maybeTy of
    Just ty -> actualType' ty p
    _ -> error $ "undefined type " ++ name ++ " at position " ++ show p
  where
    actualType' (T.NamedType name' Nothing) p
      | name' == name = error $ "unresolved type because loop define of type " ++ name ++ " at position " ++ show p
      | otherwise = do
        maybeTy <- getTenv name'
        case maybeTy of
          Just ty -> actualType' ty p
          _ -> error $ "undefined type " ++ name ++ " at position " ++ show p
    actualType' (T.ArrayType ty) p = actualType' ty p <&> T.ArrayType
    actualType' t _ = return t
actualType (T.NamedType name (Just ty)) p = actualType ty p
actualType (T.ArrayType ty) p = actualType ty p <&> T.ArrayType
actualType t _ = return t

transDec :: Dec -> State Env ExpTy
transDec (TypeDec name ty p) = do
  ExpTy _ ty' <- transType ty
  putTenv name ty'
  return $ ExpTy Nothing T.Void
transDec (VarDec name initial Nothing _ p) = do
  ExpTy _ initialTy <- transExpr initial
  putVenv name (VarEntry initialTy)
  return $ ExpTy Nothing T.Void
transDec (VarDec name initial (Just typeName) _ p) = do
  maybeEntry <- getTenv typeName
  case maybeEntry of
    Just ty -> do
      ExpTy _ initialTy <- transExpr initial
      checkType ty initialTy (exprPos initial)
      putVenv name $ VarEntry ty
      return $ ExpTy Nothing T.Void
    _ -> error $ "undefined type " ++ typeName ++ " at position " ++ show p
transDec (FuncDec name parameters Nothing body p) = do
  formals <- mapM (\r -> findType (record_type r) p) parameters
  putVenv name $ FuncEntry formals T.Void
  scopeBegin
  mapM_ transRecord parameters
  ExpTy _ bodyTy <- transExpr body
  scopeEnd
  return $ ExpTy Nothing T.Void
transDec (FuncDec name parameters (Just returnTypeName) body p) = do
  formals <- mapM (\r -> findType (record_type r) p) parameters
  returnTy <- findType returnTypeName p
  putVenv name $ FuncEntry formals returnTy
  scopeBegin
  mapM_ transRecord parameters
  ExpTy _ bodyTy <- transExpr body
  checkType bodyTy returnTy p
  scopeEnd
  return $ ExpTy Nothing T.Void

transRecord :: Record -> State Env T.Type
transRecord (Record name typeName _ p) = do
  ty <- findType typeName p
  putVenv name (VarEntry ty)
  return ty

transType :: Type -> State Env ExpTy
transType (SimpleType name p) = do
  maybeTy <- getTenv name
  case maybeTy of
    Just ty -> return $ ExpTy Nothing ty
    Nothing -> error $ "undefined type " ++ name ++ " at position " ++ show p
transType (Records records p) = do
  checkRecordsDuplicate [] records
  rcds <- mapM transRecord records
  return $ ExpTy Nothing (T.RecordType rcds)
  where
    transRecord :: Record -> State Env (Sym, T.Type)
    transRecord (Record name typeName _ p) = do
      maybeTy <- getTenv typeName
      case maybeTy of
        Just ty -> return (name, ty)
        Nothing -> error $ "undefined type " ++ typeName ++ " at position " ++ show p

    checkRecordsDuplicate :: [Record] -> [Record] -> State Env Bool
    checkRecordsDuplicate xs [] = return True
    checkRecordsDuplicate xs (y : ys) =
      if any ((== record_name y) . record_name) xs
        then error $ "duplicate field " ++ record_name y ++ " at position " ++ show (record_pos y)
        else checkRecordsDuplicate (y : xs) ys
transType (Array elemName p) = do
  maybeTy <- getTenv elemName
  case maybeTy of
    Just ty -> return $ ExpTy Nothing (T.ArrayType ty)
    Nothing -> error $ "undefined type " ++ elemName ++ " at position " ++ show p

_fatalCurrentEnv :: State Env a
_fatalCurrentEnv = get >>= error . show

_eval :: String -> ExpTy
_eval source = transExpr (parser $ lexer source) `evalState` baseEnv

_run :: String -> (ExpTy, Env)
_run source = transExpr (parser $ lexer source) `runState` baseEnv

main = do
  contents <- readFile "src/Compiler/Tiger/source/MERGE.TIG"
  let (value, state) = runState (transExpr $ parser $ lexer contents) baseEnv
  putStrLn "Result: "
  print value
  putStrLn ""
  putStrLn "Env: "
  print state