module Compiler.Tiger.AbSyn where

import Compiler.Tiger.Symbol (Sym)
import Compiler.Tiger.Token (Pos (Pos))

{- -}

data Expr
  = VarExpr Var
  | NilExpr Pos
  | SeqExpr [Expr]
  | IntExpr Int Pos
  | StringExpr String Pos
  | UMinus Expr Pos
  | Call {call_name :: Sym, args :: [Expr], call_pos :: Pos}
  | OpExpr Expr Operator Expr Pos
  | RecordsExpr {records_type :: Sym, records :: [Field], records_pos :: Pos}
  | ArrayExpr {array_type :: Sym, size :: Expr, element_value :: Expr, array_pos :: Pos}
  | AssignExpr Var Expr Pos
  | IFExpr {predicate :: Expr, success :: Expr, failure :: Maybe Expr, if_pos :: Pos}
  | WhileExpr {predicate :: Expr, body :: Expr, while_pos :: Pos}
  | ForExpr {for_var :: Sym, from :: Expr, to :: Expr, body :: Expr, for_pos :: Pos}
  | BreakExpr Pos
  | LetExpr {decs :: [Dec], body :: Expr, let_pos :: Pos}
  deriving (Show)

data Field = Field {field_name :: Sym, field_value :: Expr, field_pos :: Pos} deriving (Show)

data Dec
  = TypeDec {type_name :: Sym, alias :: Type, type_pos :: Pos}
  | VarDec {var_name :: Sym, var_init :: Expr, var_type :: Maybe Sym, var_pos :: Pos}
  | FuncDec {func_name :: Sym, parameters :: [Record], returnType :: Maybe Sym, func_body :: Expr, func_pos :: Pos}
  deriving (Show)

data Type
  = SimpleType Sym Pos
  | Records [Record] Pos
  | Array Sym Pos
  deriving (Show)

data Record = Record {record_name :: Sym, record_type :: Sym, record_pos :: Pos} deriving (Show)

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
  deriving (Show)

-- | Left value
data Var
  = SimpleVar Sym Pos
  | FieldVar Var Sym Pos
  | IndexedVar Var Expr Pos
  deriving (Show)

zero :: Expr
zero = IntExpr 0 (Pos 0 0 0)

one :: Expr
one = IntExpr 1 (Pos 0 0 0)