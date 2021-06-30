module Compiler.Expr.Token where

data Token
  = Let
  | In
  | Int Int
  | Var String
  | Eq
  | Plus
  | Minus
  | Times
  | Div
  | OB
  | CB
  deriving (Show, Eq)