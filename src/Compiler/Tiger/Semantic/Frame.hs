module Compiler.Tiger.Semantic.Frame
  ( Frame,
    newFrame,
    formals,
    allocLocal,
    Access (..),
  )
where

import qualified Compiler.Tiger.Semantic.Temp as Temp

data Access = InFrame Int | InReg Int deriving (Show, Eq)

class Frame f where
  newFrame :: Temp.Label -> [Bool] -> Temp.S f
  formals :: f -> [Access]
  allocLocal :: f -> Bool -> Temp.S (Access, f)