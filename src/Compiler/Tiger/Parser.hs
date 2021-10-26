{-# LANGUAGE ViewPatterns #-}

module Compiler.Tiger.Parser where

import qualified Compiler.Tiger.AbSyn as A
import Compiler.Tiger.Symbol (Sym)
import Compiler.Tiger.Token
import Control.Monad.State (MonadState (get, put), State, evalState, gets, runState)
import Data.Functor.Identity (Identity)

lookAhead :: State [Token] Token
lookAhead = do
  ts <- get
  return $ if null ts then EOF else head ts

-- | Move forward in tokens
eat :: State [Token] ()
eat = get >>= put . tail

match :: Token -> State [Token] Token
match t = do
  la <- lookAhead
  if matched la t
    then eat >> return la
    else error $ show la ++ " is not match " ++ show t ++ " at " ++ prettyPos la

instance MonadFail Identity where
  fail = undefined

-- | Match the unresolved field in Token
__ :: a
__ = undefined

prog :: State [Token] A.Expr
prog = do
  epr <- expr
  la <- lookAhead
  case la of
    EOF -> return epr
    t -> unexceptedToken t

expr :: State [Token] A.Expr
expr = logic >>= expr'

expr' :: A.Expr -> State [Token] A.Expr
expr' epr = do
  l <- lookAhead
  case l of
    And p -> do
      eat
      lgc <- logic
      expr' A.IFExpr {A.predicate = epr, A.success = lgc, A.failure = Just A.zero, A.if_pos = p}
    Or p -> do
      eat
      lgc <- logic
      expr' A.IFExpr {A.predicate = epr, A.success = A.one, A.failure = Just lgc, A.if_pos = p}
    _ -> return epr

logic :: State [Token] A.Expr
logic = comp >>= logic'

logic' :: A.Expr -> State [Token] A.Expr
logic' epr = do
  la <- lookAhead
  case la of
    Eq p -> do
      eat
      cmp <- comp
      logic' $ A.OpExpr epr A.EqOp cmp p
    Gt p -> do
      eat
      cmp <- comp
      logic' $ A.OpExpr epr A.GtOp cmp p
    Ge p -> do
      eat
      cmp <- comp
      logic' $ A.OpExpr epr A.GeOp cmp p
    Lt p -> do
      eat
      cmp <- comp
      logic' $ A.OpExpr epr A.LtOp cmp p
    Le p -> do
      eat
      cmp <- comp
      logic' $ A.OpExpr epr A.LeOp cmp p
    NotEq p -> do
      eat
      cmp <- comp
      logic' $ A.OpExpr epr A.NeqOp cmp p
    _ -> return epr

comp :: State [Token] A.Expr
comp = term >>= comp'

comp' :: A.Expr -> State [Token] A.Expr
comp' epr = do
  la <- lookAhead
  case la of
    Plus p -> do
      match (Plus p)
      trm <- term
      comp' $ A.OpExpr epr A.PlusOp trm p
    Minus p -> do
      match (Minus p)
      trm <- term
      comp' $ A.OpExpr epr A.MinusOp trm p
    _ -> return epr

term :: State [Token] A.Expr
term = factor >>= term'

term' :: A.Expr -> State [Token] A.Expr
term' epr = do
  la <- lookAhead
  case la of
    Times p -> do
      eat
      fct <- factor
      term' $ A.OpExpr epr A.PlusOp fct p
    Divide p -> do
      eat
      fct <- term
      term' $ A.OpExpr epr A.DivideOp fct p
    _ -> return epr

factor :: State [Token] A.Expr
factor = do
  la <- lookAhead
  case la of
    Int i p -> eat >> return (A.IntExpr i p)
    Nil p -> eat >> return (A.NilExpr p)
    String s p -> eat >> return (A.StringExpr s p)
    Minus p -> do
      eat
      e <- expr
      return $ A.UMinus e p
    ID id p -> do
      eat
      la <- lookAhead
      case la of
        LeftParen _ -> do
          eat
          eprs <- parameters
          match (RightParen __)
          return A.Call {A.call_name = id, A.args = eprs, A.call_pos = p}
        LeftBracket lbp -> do
          eat
          epr <- expr
          match (RightBracket __)
          la <- lookAhead
          case la of
            -- common prefix
            -- 1. array literal
            Of _ -> do
              eat
              value <- expr
              return $ A.ArrayExpr {A.array_type = id, A.size = epr, A.element_value = value, A.array_pos = p}
            -- 2. index access
            _ -> do
              v <- var' (A.IndexedVar (A.SimpleVar id p) epr lbp)
              varOrAssign v
        LeftBrace _ -> do
          eat
          flds <- fields
          match (RightBrace __)
          return $ A.RecordsExpr {A.records_type = id, A.records = flds, A.records_pos = p}
        _ -> do
          v <- var' (A.SimpleVar id p)
          varOrAssign v
    Let p -> do
      eat
      dcs <- decs
      match (In __)
      eprs <- seqExpr
      match (End __)
      return $ A.LetExpr dcs eprs p
    If p -> do
      eat
      pre <- expr
      match (Then __)
      suc <- expr
      la <- lookAhead
      case la of
        Else _ -> do
          eat
          fai <- expr
          return $ A.IFExpr {A.predicate = pre, A.success = suc, A.failure = Just fai, A.if_pos = p}
        _ -> return $ A.IFExpr {A.predicate = pre, A.success = suc, A.failure = Nothing, A.if_pos = p}
    While p -> do
      eat
      pre <- expr
      match (Do __)
      body <- expr
      return $ A.WhileExpr {A.predicate = pre, A.body = body, A.while_pos = p}
    For p -> do
      eat
      ID v _ <- match (ID __ __)
      match (Assign __)
      from <- expr
      match (To __)
      to <- expr
      match (Do __)
      body <- expr
      return $ A.ForExpr {A.for_var = v, A.from = from, A.to = to, A.body = body, A.for_pos = p}
    Break p -> return (A.BreakExpr p)
    LeftParen p -> do
      eat
      epr <- seqExpr
      match (RightParen __)
      return epr
    t -> unexceptedToken t

-- | Parameters of function call
parameters :: State [Token] [A.Expr]
parameters = do
  la <- lookAhead
  case la of
    RightParen _ -> return []
    _ -> do
      epr <- expr
      parameters' [epr]

parameters' :: [A.Expr] -> State [Token] [A.Expr]
parameters' eprs = do
  la <- lookAhead
  case la of
    Comma _ -> do
      eat
      epr <- expr
      parameters' (eprs ++ [epr])
    _ -> return eprs

var' :: A.Var -> State [Token] A.Var
var' var = do
  la <- lookAhead
  case la of
    Dot p -> do
      sym <- eat >> lookAhead
      case sym of
        ID s _ -> eat >> var' (A.FieldVar var s p)
        t -> error $ "bad field access " ++ show t ++ " at " ++ prettyPos t
    LeftBracket p -> do
      eat
      epr <- expr
      match (RightBracket __)
      var' (A.IndexedVar var epr p)
    _ -> return var

varOrAssign :: A.Var -> State [Token] A.Expr
varOrAssign v = do
  la <- lookAhead
  case la of
    Assign assign_p -> do
      eat
      epr <- expr
      return $ A.AssignExpr v epr assign_p
    _ -> return $ A.VarExpr v

decs :: State [Token] [A.Dec]
decs = do
  la <- lookAhead
  case la of
    Var _ -> decs' []
    Type _ -> decs' []
    Function _ -> decs' []
    _ -> return []

decs' :: [A.Dec] -> State [Token] [A.Dec]
decs' dcs = do
  d <- dec
  case d of
    Just dc -> decs' (dcs ++ [dc])
    Nothing -> return dcs

dec :: State [Token] (Maybe A.Dec)
dec = do
  la <- lookAhead
  case la of
    Var _ -> Just <$> varDec
    Type _ -> Just <$> typeDec
    Function _ -> Just <$> funcDec
    _ -> return Nothing

varDec :: State [Token] A.Dec
varDec = do
  la <- lookAhead
  case la of
    Var p -> do
      eat
      la <- lookAhead
      case la of
        ID id _ -> do
          eat
          la <- lookAhead
          case la of
            Assign _ -> do
              eat
              value <- expr
              return A.VarDec {A.var_name = id, A.var_init = value, A.var_type = Nothing, A.var_pos = p}
            Colon _ -> do
              eat
              la <- lookAhead
              case la of
                ID type_id _ -> do
                  match (Assign __)
                  value <- expr
                  return A.VarDec {A.var_name = id, A.var_init = value, A.var_type = Just type_id, A.var_pos = p}
                t -> badToken "bad type id" t
            t -> unexceptedToken t
        t -> unexceptedToken t
    t -> unexceptedToken t

typeDec :: State [Token] A.Dec
typeDec = do
  match (Type __)
  la <- lookAhead
  case la of
    ID type_id p -> do
      eat
      match (Eq __)
      t <- type_
      return $ A.TypeDec {A.type_name = type_id, A.alias = t, A.type_pos = p}
    t -> badToken "bad type id" t

type_ :: State [Token] A.Type
type_ = do
  la <- lookAhead
  case la of
    ID type_id p -> do
      eat
      return $ A.SimpleType type_id p
    LeftBrace p -> do
      eat
      rcds <- records
      match (RightBrace __)
      return $ A.Records rcds p
    Array p -> do
      eat
      match (Of __)
      la <- lookAhead
      case la of
        ID type_id _ -> do
          eat
          return $ A.Array type_id p
        t -> badToken "bad type id" t
    t -> unexceptedToken t

funcDec :: State [Token] A.Dec
funcDec = do
  Function p <- match (Function __)
  la <- lookAhead
  case la of
    ID id _ -> do
      eat
      match (LeftParen __)
      rcds <- records
      match (RightParen __)
      la <- lookAhead
      case la of
        Eq _ -> do
          eat
          epr <- expr
          return $
            A.FuncDec
              { A.func_name = id,
                A.parameters = rcds,
                A.returnType = Nothing,
                A.func_body = epr,
                A.func_pos = p
              }
        Colon _ -> do
          eat
          la <- lookAhead
          case la of
            ID type_id _ -> do
              eat
              match (Eq __)
              epr <- expr
              return $
                A.FuncDec
                  { A.func_name = id,
                    A.parameters = rcds,
                    A.returnType = Just type_id,
                    A.func_body = epr,
                    A.func_pos = p
                  }
            t -> badToken "bad function return type" t
        t -> unexceptedToken t
    t -> badToken "function id not found" t

-- | Field records in function or record type declare
records :: State [Token] [A.Record]
records = do
  la <- lookAhead
  case la of
    ID _ _ -> do
      rcd <- record
      records' [rcd]
    _ -> records' []

record :: State [Token] A.Record
record = do
  la <- lookAhead
  case la of
    ID id p -> do
      eat
      match (Colon __)
      la <- lookAhead
      case la of
        ID type_id _ -> eat >> return (A.Record id type_id p)
        t -> unexceptedToken t
    t -> unexceptedToken t

records' :: [A.Record] -> State [Token] [A.Record]
records' rcds = do
  la <- lookAhead
  case la of
    Comma _ -> do
      eat
      rcd <- record
      return $ rcds ++ [rcd]
    _ -> return rcds

fields :: State [Token] [A.Field]
fields = do
  la <- lookAhead
  case la of
    ID _ _ -> do
      fld <- field
      fields' [fld]
    _ -> fields' []

fields' :: [A.Field] -> State [Token] [A.Field]
fields' flds = do
  la <- lookAhead
  case la of
    Comma _ -> do
      eat
      fld <- field
      fields' (flds ++ [fld])
    _ -> return flds

field :: State [Token] A.Field
field = do
  ID type_id p <- match (ID __ __)
  match (Eq __)
  epr <- expr
  return $ A.Field type_id epr p

seqExpr :: State [Token] A.Expr
seqExpr = do
  la <- lookAhead
  case la of
    End p -> return (A.SeqExpr [] p)
    RightParen p -> return (A.SeqExpr [] p)
    _ -> do
      epr <- expr
      eprs <- seqExpr' [epr]
      A.SeqExpr eprs . pos <$> lookAhead

seqExpr' :: [A.Expr] -> State [Token] [A.Expr]
seqExpr' eprs = do
  la <- lookAhead
  case la of
    Semicolon _ -> do
      eat
      epr <- expr
      seqExpr' (eprs ++ [epr])
    _ -> return eprs

prettyPos :: Token -> String
prettyPos EOF = "end of file"
prettyPos (pos -> Pos _ r c) = "row: " ++ show r ++ " column: " ++ show c

badToken :: String -> Token -> a
badToken msg t = error $ msg ++ ' ' : show t ++ " at " ++ prettyPos t

unexceptedToken :: Token -> a
unexceptedToken = badToken "unexcepted token"

parser :: [Token] -> A.Expr
parser = evalState prog