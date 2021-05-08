{-# LANGUAGE ViewPatterns #-}

module Compiler.StraightLine () where

import Data.Char

data Token
  = Plus
  | Minus
  | Times
  | Div
  | Semicolon
  | Print
  | Identity String
  | Number Int
  | Common
  | Assign
  | ParenthesesLeft
  | ParenthesesRight
  deriving (Show)

startWith :: [Char] -> [Char] -> Maybe [Char]
startWith "" [] = Just []
startWith "" ((isSpace -> True) : xs) = Just xs
startWith "" _ = Nothing
startWith _ "" = Nothing
startWith (x : xs) (y : ys)
  | x == y = startWith xs ys
  | otherwise = Nothing

startBy :: (a -> Bool) -> [a] -> Maybe ([a], [a])
startBy f (span f -> ([], _)) = Nothing
startBy f (span f -> (h, l)) = Just (h, l)

lexer :: String -> [Token]
lexer "" = []
lexer ((isSpace -> True) : xs) = lexer xs
lexer ('+' : xs) = Plus : lexer xs
lexer ('-' : xs) = Minus : lexer xs
lexer ('*' : xs) = Times : lexer xs
lexer ('/' : xs) = Div : lexer xs
lexer (';' : xs) = Semicolon : lexer xs
lexer (',' : xs) = Common : lexer xs
lexer ('(' : xs) = ParenthesesLeft : lexer xs
lexer (')' : xs) = ParenthesesRight : lexer xs
lexer (startWith ":=" -> Just xs) = Assign : lexer xs
lexer (startWith "print" -> Just xs) = Print : lexer xs
lexer (startBy isNumber -> Just (xh, xs)) = Number (read xh :: Int) : lexer xs
lexer (startBy isAlpha -> Just (xh, xs)) = Identity xh : lexer xs
