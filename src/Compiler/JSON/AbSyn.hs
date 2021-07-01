module Compiler.JSON.AbSyn where

import Compiler.JSON.Token (Pos)

data JSON
  = Null Pos
  | Bool Bool Pos
  | String String Pos
  | Number Double Pos
  | Array [JSON] Pos
  | Object [(String, JSON)] Pos
  deriving (Show, Eq)