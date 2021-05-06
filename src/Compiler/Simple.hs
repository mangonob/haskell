{-# LANGUAGE ViewPatterns #-}

module Compiler.Simple () where

import Data.Char

data Token = Plus | Times | Assign | Identity String | Number Int deriving (Show)

lexer :: String -> [Token]
lexer "" = []
lexer (' ' : xs) = lexer xs
lexer ('+' : xs) = Plus : lexer xs
lexer ('*' : xs) = Times : lexer xs
lexer ('=' : xs) = Assign : lexer xs
lexer x@((isDigit -> True) : _) = let (y, xs) = span isDigit x in Number (read y :: Int) : lexer xs
lexer x@((isAlpha -> True) : _) = let (y, xs) = span isAlpha x in Identity y : lexer xs