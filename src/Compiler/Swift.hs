{-# LANGUAGE ViewPatterns #-}

module Compiler.Swift () where

import Data.Char
import Data.List

data Token
  = Func
  | Let
  | Var
  | Return
  | Colon
  | Common
  | Assign
  | Plus
  | Minus
  | Times
  | Division
  | Number Int
  | Identity String
  | ParenthesesLeft -- (
  | ParenthesesRight -- )
  | BracesLeft -- {
  | BracesRight -- }
  | BracketsLeft -- [
  | BracketsRight -- ]
  | ChevronsLeft -- <
  | ChevronsRight -- >
  deriving (Show)

startWith :: [Char] -> [Char] -> Maybe [Char]
startWith "" x = Just x
startWith _ "" = Nothing
startWith (x : xs) (y : ys)
  | x == y = startWith xs ys
  | otherwise = Nothing

fOr :: (t -> Bool) -> (t -> Bool) -> t -> Bool
fOr f g = \x -> (f x) || (g x)

isIdPrefix :: Char -> Bool
isIdPrefix = fOr (== '_') isAlpha

isId :: Char -> Bool
isId = fOr (== '_') isAlphaNum

lexer :: String -> [Token]
lexer "" = []
lexer ((isSpace -> True) : xs) = lexer xs
lexer (';' : xs) = lexer xs
lexer ('(' : xs) = ParenthesesLeft : lexer xs
lexer (')' : xs) = ParenthesesRight : lexer xs
lexer ('{' : xs) = BracesLeft : lexer xs
lexer ('}' : xs) = BracesRight : lexer xs
lexer ('[' : xs) = BracketsLeft : lexer xs
lexer (']' : xs) = BracketsRight : lexer xs
lexer ('<' : xs) = ChevronsLeft : lexer xs
lexer ('>' : xs) = ChevronsRight : lexer xs
lexer ('=' : xs) = Assign : lexer xs
lexer ('+' : xs) = Plus : lexer xs
lexer ('-' : xs) = Minus : lexer xs
lexer ('*' : xs) = Times : lexer xs
lexer ('/' : xs) = Division : lexer xs
lexer (',' : xs) = Common : lexer xs
lexer (':' : xs) = Colon : lexer xs
lexer x@((isNumber -> True) : _) = let (xh, xs) = span isNumber x in Number (read xh :: Int) : lexer xs
lexer (startWith "func" -> Just xs) = Func : lexer xs
lexer (startWith "let" -> Just xs) = Let : lexer xs
lexer (startWith "var" -> Just xs) = Var : lexer xs
lexer (startWith "return" -> Just xs) = Return : lexer xs
lexer (x@(isIdPrefix -> True) : xs) = let (yh, ys) = span isId xs in Identity (x : yh) : lexer ys