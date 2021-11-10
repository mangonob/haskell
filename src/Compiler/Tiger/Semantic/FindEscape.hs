module Compiler.Tiger.Semantic.FindEscape where

import Compiler.Tiger.AbSyn
import Compiler.Tiger.Lexer_ (lexer)
import Compiler.Tiger.Parser_ (parser)
import Compiler.Tiger.Raw (raw)
import Compiler.Tiger.Symbol (Sym)
import Control.Monad.RWS (MonadState (put))
import Control.Monad.State.Strict
import Data.Functor ((<&>))
import Data.Set
import qualified Data.Set as S

type S a = State (Set Sym, Set Sym) a

--- Returns: Declard symbols, Used symbols
findEscape :: Expr -> S Expr
findEscape (VarExpr v) = findEscapeVar v <&> VarExpr
findEscape (Call funcName args p) = do
  args' <- findEscapeExprs args
  return $ Call funcName args' p
findEscape s@(SeqExpr seq p) = do
  seq' <- findEscapeExprs seq
  return $ SeqExpr seq' p
findEscape (OpExpr e1 op e2 p) = do
  (e1', e2') <- findEscape e1 `unionS'` findEscape e2
  return $ OpExpr e1' op e2' p
findEscape (RecordsExpr recordName fields p) = do
  fields' <- findEscapeSeq findEscapeField fields
  return $ RecordsExpr recordName fields' p
  where
    findEscapeField :: Field -> S Field
    findEscapeField (Field fieldName value p) = do
      value' <- findEscape value
      return $ Field fieldName value' p
findEscape (ArrayExpr typeName size element p) = do
  (size', element') <- findEscape size `unionS'` findEscape element
  return $ ArrayExpr typeName size' element' p
findEscape (IFExpr pred succ Nothing p) = do
  (pred', succ') <- findEscape pred `unionS'` findEscape succ
  return $ IFExpr pred' succ' Nothing p
findEscape (IFExpr pred succ (Just failure) p) = do
  ((pred', succ'), failure') <- findEscape pred `unionS'` findEscape succ `unionS'` findEscape failure
  return $ IFExpr pred' succ' (Just failure') p
findEscape (WhileExpr pred body p) = do
  (pred', body') <- findEscape pred `unionS'` findEscape body
  return $ WhileExpr pred' body' p
findEscape (ForExpr var from to body _ p) = do
  (from', to') <- findEscape from `unionS'` findEscape to
  (fl, fr) <- get
  body' <- findEscape body
  (bl, br) <- get
  put (fl `union` bl `delete'` var, fr `union` br `delete'` var)
  return $ ForExpr var from' to' body' (member var br) p
findEscape (LetExpr decs body p) = do
  (body', decs') <- findEscape body `unionS'` findEscapeSeq findEscapeDec decs
  return $ LetExpr decs' body' p
findEscape e = put (empty, empty) >> return e

findEscapeUnion :: (s -> S s) -> [s] -> S [s]
findEscapeUnion f [] = put (empty, empty) >> return []
findEscapeUnion f (s : xs) = do
  s' <- f s
  (l1, r1) <- get
  xs' <- findEscapeUnion f xs
  (l2, r2) <- get
  put (l1 `union` l2, r1 `union` r2)
  return (s' : xs')

findEscapeSeq :: (s -> S s) -> [s] -> S [s]
findEscapeSeq f [] = return []
findEscapeSeq f (s : xs) = do
  xs' <- findEscapeSeq f xs
  s' <- f s
  return (s' : xs')

findEscapeExprs :: [Expr] -> S [Expr]
findEscapeExprs = findEscapeUnion findEscape

findEscapeVar :: Var -> S Var
findEscapeVar v@(SimpleVar varName _) = put (singleton varName, empty) >> return v
findEscapeVar v@(FieldVar var _ _) = findEscapeVar var >> return v
findEscapeVar (IndexedVar var expr p) = do
  (var', expr') <- findEscapeVar var `unionS'` findEscape expr
  return $ IndexedVar var' expr' p

findEscapeDec :: Dec -> S Dec
findEscapeDec d@TypeDec {} = put (empty, empty) >> return d
findEscapeDec (VarDec varName init typeName _ p) = do
  (l1, r1) <- get
  init' <- findEscape init
  (l2, r2) <- get
  put (l1 `union` l2 `delete'` varName, r1 `union` r2 `delete'` varName)
  return $ VarDec varName init' typeName (member varName (r1 `union` r2)) p
findEscapeDec (FuncDec funcName records returnType body p) = do
  (l1, r1) <- get
  body' <- findEscape body
  (l2, r2) <- get
  records' <- mapM findEscapeRecord records
  let recordNames = fromList $ fmap record_name records
  put (l1 `union` l2 \\ recordNames, l1 `union` l2 `union` r2 \\ recordNames)
  return $ FuncDec funcName records' returnType body' p

findEscapeRecord :: Record -> S Record
findEscapeRecord (Record recordName recordType _ p) = do
  (l, r) <- get
  return $ Record recordName recordType (member recordName r) p

unionS' :: S a -> S b -> S (a, b)
unionS' m1 m2 = do
  v1 <- m1
  (l1, r1) <- get
  v2 <- m2
  (l2, r2) <- get
  put (l1 `union` l2, r1 `union` r2)
  return (v1, v2)

delete' :: Ord a => Set a -> a -> Set a
delete' = flip delete

_fatalError :: S a
_fatalError = get >>= error . show

main = do
  contents <- readFile "src/Compiler/Tiger/source/ESCAPE_TEST.TIG"
  let (value, state) = runState (findEscape $ parser $ lexer contents) (empty, empty)
  putStrLn "Result: "
  putStrLn $ raw value
  putStrLn ""
  putStrLn "State: "
  print state