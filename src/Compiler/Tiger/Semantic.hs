module Compiler.Tiger.Semantic where

import Control.Monad (join)
import Data.List (intersperse)
import Data.Map (Map, empty)

type Sym = String

data Type
  = NilType
  | IntType
  | StringType
  | ArrayType Type
  | RecordType [(Sym, Type)]
  | Named Sym (Maybe Type)

instance Show Type where
  show t = ':' : pretty t
    where
      pretty (Named s _) = s
      pretty NilType = "nil"
      pretty IntType = "int"
      pretty StringType = "string"
      pretty (ArrayType t) = "[" ++ pretty t ++ "]"
      pretty (RecordType xs) =
        if null xs
          then "{}"
          else "{ " ++ (join (intersperse ", " (map showRecord xs))) ++ " }"

      showRecord (s, t) = s ++ ": " ++ pretty t

data Eventry = Var Type | Func [Type] Type