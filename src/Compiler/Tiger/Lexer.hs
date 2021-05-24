{-# LANGUAGE ViewPatterns #-}

module Compiler.Tiger.Lexer where

import Data.Char

data Token
  = -- Reverse
    While
  | For
  | To
  | Break
  | Let
  | In
  | End
  | Function
  | Var
  | Type
  | Array
  | If
  | Then
  | Else
  | Do
  | Of
  | Nil
  | -- Punctuation symbol
    Comma
  | Colon
  | Semicolon
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Dot
  | Plus
  | Minus
  | Times
  | Divide
  | Eq
  | NotEq
  | LTToken
  | LEToken
  | GTToken
  | GEToken
  | And
  | Or
  | Assign
  deriving (Show, Eq)

lexer :: [Char] -> [Token]
lexer [] = []
lexer ((isSpace -> True) : xs) = lexer xs
-- Reverse
lexer (startWith "while" -> Just xs) = While : lexer xs
lexer (startWith "for" -> Just xs) = For : lexer xs
lexer (startWith "to" -> Just xs) = To : lexer xs
lexer (startWith "break" -> Just xs) = Break : lexer xs
lexer (startWith "let" -> Just xs) = Let : lexer xs
lexer (startWith "in" -> Just xs) = In : lexer xs
lexer (startWith "end" -> Just xs) = End : lexer xs
lexer (startWith "function" -> Just xs) = Function : lexer xs
lexer (startWith "var" -> Just xs) = Var : lexer xs
lexer (startWith "type" -> Just xs) = Type : lexer xs
lexer (startWith "array" -> Just xs) = Array : lexer xs
lexer (startWith "if" -> Just xs) = If : lexer xs
lexer (startWith "then" -> Just xs) = Then : lexer xs
lexer (startWith "else" -> Just xs) = Else : lexer xs
lexer (startWith "do" -> Just xs) = Do : lexer xs
lexer (startWith "of" -> Just xs) = Of : lexer xs
lexer (startWith "nil" -> Just xs) = Nil : lexer xs
-- Punctuation symbol
lexer (startWith "," -> Just xs) = Comma : lexer xs
lexer (startWith ":" -> Just xs) = Colon : lexer xs
lexer (startWith ";" -> Just xs) = Semicolon : lexer xs
lexer (startWith "(" -> Just xs) = LeftParen : lexer xs
lexer (startWith ")" -> Just xs) = RightParen : lexer xs
lexer (startWith "[" -> Just xs) = LeftBrace : lexer xs
lexer (startWith "]" -> Just xs) = RightBrace : lexer xs
lexer (startWith "." -> Just xs) = Dot : lexer xs
lexer (startWith "+" -> Just xs) = Plus : lexer xs
lexer (startWith "-" -> Just xs) = Minus : lexer xs
lexer (startWith "*" -> Just xs) = Times : lexer xs
lexer (startWith "/" -> Just xs) = Divide : lexer xs
lexer (startWith "=" -> Just xs) = Eq : lexer xs
lexer (startWith "<>" -> Just xs) = NotEq : lexer xs
lexer (startWith "<" -> Just xs) = LTToken : lexer xs
lexer (startWith "<=" -> Just xs) = LEToken : lexer xs
lexer (startWith ">" -> Just xs) = GTToken : lexer xs
lexer (startWith ">=" -> Just xs) = GEToken : lexer xs
lexer (startWith "&" -> Just xs) = And : lexer xs
lexer (startWith "|" -> Just xs) = Or : lexer xs
lexer (startWith ":=" -> Just xs) = Assign : lexer xs

startWith :: [Char] -> [Char] -> Maybe [Char]
startWith "" x = Just x
startWith _ "" = Nothing
startWith (x : xs) (y : ys)
  | x == y = startWith xs ys
  | otherwise = Nothing