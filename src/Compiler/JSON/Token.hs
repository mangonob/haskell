module Compiler.JSON.Token where

data Pos = Pos {position :: !Int, row :: !Int, col :: !Int} deriving (Eq)

instance Show Pos where
  show (Pos _ r c) = show (r, c)

data Token
  = Bool Bool Pos
  | Null Pos
  | LeftParen Pos
  | RightParen Pos
  | LeftBracket Pos
  | RightBracket Pos
  | LeftBrace Pos
  | RightBrace Pos
  | Colon Pos
  | Comma Pos
  | String String Pos
  | Number Double Pos
  deriving (Show, Eq)