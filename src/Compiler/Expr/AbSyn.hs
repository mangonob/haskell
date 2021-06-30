module Compiler.Expr.AbSyn where

import Control.Monad (join)
import Data.List (intersperse)

data Expr
  = Parented Expr
  | OpExpr Expr Op Expr
  | IntExpr Int
  | VarExpr String
  deriving (Eq)

instance Show Expr where
  show (IntExpr i) = show i
  show (VarExpr v) = v
  show (Parented e) = show e
  show (OpExpr e1 op e2) = join $ intersperse " " [show op, show e1, show e2]

data Op
  = Plus
  | Minus
  | Times
  | Div
  deriving (Eq)

instance Show Op where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Div = "/"