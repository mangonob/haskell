module Compiler.Tiger.Token where

data Pos = Pos {position :: !Int, row :: !Int, column :: !Int} deriving (Eq)

instance Show Pos where
  show (Pos pos row col) = show (row, col)

data Token
  = While {pos :: Pos}
  | For {pos :: Pos}
  | To {pos :: Pos}
  | Break {pos :: Pos}
  | Let {pos :: Pos}
  | In {pos :: Pos}
  | End {pos :: Pos}
  | Function {pos :: Pos}
  | Var {pos :: Pos}
  | Type {pos :: Pos}
  | Array {pos :: Pos}
  | If {pos :: Pos}
  | Then {pos :: Pos}
  | Else {pos :: Pos}
  | Do {pos :: Pos}
  | Of {pos :: Pos}
  | Nil {pos :: Pos}
  | Comma {pos :: Pos}
  | Colon {pos :: Pos}
  | Semicolon {pos :: Pos}
  | LeftParen {pos :: Pos}
  | RightParen {pos :: Pos}
  | LeftBrace {pos :: Pos}
  | RightBrace {pos :: Pos}
  | LeftBracket {pos :: Pos}
  | RightBracket {pos :: Pos}
  | Dot {pos :: Pos}
  | Plus {pos :: Pos}
  | Minus {pos :: Pos}
  | Times {pos :: Pos}
  | Divide {pos :: Pos}
  | Eq {pos :: Pos}
  | NotEq {pos :: Pos}
  | Lt {pos :: Pos}
  | Le {pos :: Pos}
  | Gt {pos :: Pos}
  | Ge {pos :: Pos}
  | And {pos :: Pos}
  | Or {pos :: Pos}
  | Assign {pos :: Pos}
  | String {s_value :: String, pos :: Pos}
  | Int {i_value :: Int, pos :: Pos}
  | ID {id_value :: String, pos :: Pos}
  | EOF
  deriving (Show, Eq)

matched :: Token -> Token -> Bool
matched (While _) (While _) = True
matched (For _) (For _) = True
matched (To _) (To _) = True
matched (Break _) (Break _) = True
matched (Let _) (Let _) = True
matched (In _) (In _) = True
matched (End _) (End _) = True
matched (Function _) (Function _) = True
matched (Var _) (Var _) = True
matched (Type _) (Type _) = True
matched (Array _) (Array _) = True
matched (If _) (If _) = True
matched (Then _) (Then _) = True
matched (Else _) (Else _) = True
matched (Do _) (Do _) = True
matched (Of _) (Of _) = True
matched (Nil _) (Nil _) = True
matched (Comma _) (Comma _) = True
matched (Colon _) (Colon _) = True
matched (Semicolon _) (Semicolon _) = True
matched (LeftParen _) (LeftParen _) = True
matched (RightParen _) (RightParen _) = True
matched (LeftBrace _) (LeftBrace _) = True
matched (RightBrace _) (RightBrace _) = True
matched (LeftBracket _) (LeftBracket _) = True
matched (RightBracket _) (RightBracket _) = True
matched (Dot _) (Dot _) = True
matched (Plus _) (Plus _) = True
matched (Minus _) (Minus _) = True
matched (Times _) (Times _) = True
matched (Divide _) (Divide _) = True
matched (Eq _) (Eq _) = True
matched (NotEq _) (NotEq _) = True
matched (Lt _) (Lt _) = True
matched (Le _) (Le _) = True
matched (Gt _) (Gt _) = True
matched (Ge _) (Ge _) = True
matched (And _) (And _) = True
matched (Or _) (Or _) = True
matched (Assign _) (Assign _) = True
matched (String _ _) (String _ _) = True
matched (Int _ _) (Int _ _) = True
matched (ID _ _) (ID _ _) = True
matched EOF EOF = True
matched _ _ = False