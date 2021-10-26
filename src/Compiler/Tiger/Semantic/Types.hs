module Compiler.Tiger.Semantic.Types where

import Compiler.Expr.Token (Token)
import Compiler.Tiger.Symbol (Sym)
import Control.Monad (join)
import Data.List (intersperse)

data Type
  = Void
  | NilType
  | IntType
  | StringType
  | ArrayType Type
  | RecordType [(Sym, Type)]
  | NamedType Sym (Maybe Type)
  deriving (Eq)

instance Show Type where
  show (NamedType s _) = s ++ "'"
  show NilType = "nil"
  show IntType = "int"
  show Void = "void"
  show StringType = "string"
  show (ArrayType t) = "[" ++ show t ++ "]"
  show (RecordType xs) =
    "{" ++ join (intersperse ", " (map showRecord xs)) ++ "}"
    where
      showRecord (s, t) = s ++ ": " ++ show t