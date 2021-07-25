module Compiler.Tiger.Semantic.Types where

import Compiler.Tiger.Symbol (Sym)

-- | Type in semantic
data Ty
  = IntTy
  | StringTy
  | NilTy
  | VoidTy
  | RecordTy [(Sym, Ty)]
  | ArrayTy Ty
  | NamedTy Sym (Maybe Ty)