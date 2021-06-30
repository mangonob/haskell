module Compiler.JSON.AbSyn where

import Compiler.JSON.Token (Pos)

data JSON
  = Null
  | Bool Bool Pos
  | String String Pos
  | Number Double Pos
  | Array [JSON] Pos
  | Object [(String, JSON)] Pos