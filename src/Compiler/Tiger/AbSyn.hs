module Compiler.Tiger.AbSyn where

import Compiler.Tiger.Lexer (Pos (..))
import Compiler.Tiger.Symbol (Sym)

data Expr
  = VarExpr Var
  | NilExpr Pos
  | SeqExpr [Expr]
  | IntExp Int Pos
  | StringExp String Pos
  | UMinus Expr Pos
  | Call {call_name :: Sym, args :: [Expr], call_pos :: Pos}
  | OpExpr Expr Operator Expr Pos
  | RecordsExpr {record_type :: Sym, fileds :: [Field], record_pos :: Pos}
  | ArrayExpr {array_type :: Sym, size :: Expr, init :: Expr, array_pos :: Pos}
  | AssignExpr Var Expr Pos
  | IFExpr {predicate :: Expr, success :: Expr, failure :: (Maybe Expr), if_pos :: Pos}
  | WhileExpr {predicate :: Expr, body :: Expr, while_pos :: Pos}
  | ForExpr {from :: Expr, to :: Expr, body :: Expr, for_pos :: Pos}
  | BreakExpr Pos
  | LetExpr {decs :: [Dec], body :: Expr, let_pos :: Pos}

data Field = FieldAssign {record_field :: Sym, field_value :: Expr, field_assign_pos :: Pos}

data Dec
  = TypeDec {type_name :: Sym, alias :: Type, type_pos :: Pos}
  | VarDec {var_name :: Sym, expr :: Expr, varType :: Maybe Type, var_pos :: Pos}
  | FuncDec {func_name :: Sym, parameters :: [Record], returnType :: Maybe Type, func_pos :: Pos}

data Type
  = Type Sym Pos
  | Records [Record] Pos
  | Array Sym Pos

data Record = Field {field_name :: Sym, field_type :: Sym, field_pos :: Pos}

data Operator
  = PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp

-- | Left value
data Var
  = SimpleVar Sym Pos
  | FieldVar Var Sym Pos
  | IndexedVar Var Expr Pos