module Compiler.JSON.Token where

data Pos = Pos {position :: !Int, row :: !Int, col :: !Int} deriving (Eq)

instance Show Pos where
  show (Pos _ r c) = show (r, c)

data Token
  = Bool {bool_value :: Bool, pos :: Pos}
  | Null {pos :: Pos}
  | LeftBracket {pos :: Pos}
  | RightBracket {pos :: Pos}
  | LeftBrace {pos :: Pos}
  | RightBrace {pos :: Pos}
  | Colon {pos :: Pos}
  | Comma {pos :: Pos}
  | String {string_value :: String, pos :: Pos}
  | Number {number_value :: Double, pos :: Pos}
  deriving (Show, Eq)