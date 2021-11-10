module Compiler.Tiger.Semantic.Tree where

import Prelude hiding (EQ, GT, LT, not)

data Stm
  = SEQ Stm Stm
  | LEBEL String
  | JUMP Exp [String]
  | CJUMP
      { op :: RelOp,
        left :: Exp,
        right :: Exp,
        true :: String,
        false :: String
      }
  | MOVE Exp Exp
  | EXP Exp
  deriving (Show, Eq)

data Exp
  = BINOP BinOp Exp Exp
  | MEM Exp
  | TEMP String
  | ESEQ Stm Exp
  | NAME String
  | CONST Int
  | CALL Exp [Exp]
  deriving (Show, Eq)

data BinOp
  = PLUS
  | MINUS
  | MUL
  | DIV
  | AND
  | OR
  | LSHIFT
  | RSHIFT
  | ARSHIFT
  | XOR
  deriving (Show, Eq)

data RelOp
  = EQ
  | NE
  | LT
  | LE
  | GT
  | GE
  | ULT
  | ULE
  | UGT
  | UGE
  deriving (Show, Eq)

not :: RelOp -> RelOp
not EQ = NE
not NE = EQ
not LT = GE
not LE = GT
not GT = LE
not GE = LT
not ULT = UGE
not ULE = UGT
not UGT = ULE
not UGE = ULT