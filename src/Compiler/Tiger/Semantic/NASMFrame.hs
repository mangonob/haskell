module Compiler.Tiger.Semantic.NASMFrame where

import Compiler.Tiger.Semantic.Frame
import qualified Compiler.Tiger.Semantic.Temp as Temp

data NASMFrame = NASMFrame
  { name :: Temp.Label,
    formals :: [Access],
    locals :: [Access],
    fp :: Int,
    sp :: Int
  }
  deriving (Show)

instance Frame NASMFrame where
  newFrame = undefined
  formals = undefined
  allocLocal = undefined