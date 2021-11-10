module Compiler.Tiger.Semantic.Frame where

data Frame

data Temp = Temp deriving (Show, Eq)

data Access = InFrame Int | InReg Temp deriving (Show, Eq)