module Compiler.Tiger.Semantic.Types where

import Compiler.Tiger.Symbol (Sym)
import Control.Monad (join)
import Data.List (intersperse)

data Type
  = NilType
  | IntType
  | StringType
  | ArrayType Type
  | RecordType [(Sym, Type)]
  | NamedType Sym (Maybe Type)

instance Show Type where
  show t = ':' : pretty t
    where
      pretty (NamedType s _) = s
      pretty NilType = "nil"
      pretty IntType = "int"
      pretty StringType = "string"
      pretty (ArrayType t) = "[" ++ pretty t ++ "]"
      pretty (RecordType xs) =
        "{" ++ join (intersperse ", " (map showRecord xs)) ++ "}"

      showRecord (s, t) = s ++ ": " ++ pretty t

data Eventry = Var Type | Func [Type] Type