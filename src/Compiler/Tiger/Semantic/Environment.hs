module Compiler.Tiger.Semantic.Environment where

data Environment = Environment
  { -- | Type Environment
    typeEnv :: Int,
    -- | Variable Environment
    varEnv :: Int
  }