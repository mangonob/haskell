{-# LANGUAGE ViewPatterns #-}

module Compiler.Tiger.Lexer
  ( Token (..),
    Pos (..),
    lexer,
  )
where

import Compiler.Tiger.Token
import Control.Monad (guard)
import Control.Monad.State (MonadState (put), State, evalState, get)
import Data.Char (isAlpha, isAlphaNum, isNumber, isSpace)
import Data.Maybe (fromMaybe)

newLine :: State Pos ()
newLine = get >>= \(Pos p r c) -> put (Pos (p + 1) (r + 1) 1)

forward :: Int -> State Pos ()
forward n = get >>= \(Pos p r c) -> put (Pos (p + n) r (c + n))

next :: State Pos ()
next = forward 1

go :: (Pos -> Token) -> State Pos [Token] -> State Pos [Token]
go f s = do
  pos <- get
  next
  ts <- s
  return (f pos : ts)

jump :: Int -> (Pos -> Token) -> State Pos [Token] -> State Pos [Token]
jump n f s = do
  pos <- get
  forward n
  ts <- s
  return (f pos : ts)

lexer_ :: [Char] -> State Pos [Token]
lexer_ [] = return []
lexer_ (((== '\n') -> True) : xs) = newLine >> lexer_ xs
lexer_ ((isSpace -> True) : xs) = next >> lexer_ xs
-- Reverse
lexer_ (keyword "while" -> Just xs) = jump 5 While $ lexer_ xs
lexer_ (keyword "for" -> Just xs) = jump 3 For $ lexer_ xs
lexer_ (keyword "to" -> Just xs) = jump 2 To $ lexer_ xs
lexer_ (keyword "break" -> Just xs) = jump 5 Break $ lexer_ xs
lexer_ (keyword "let" -> Just xs) = jump 3 Let $ lexer_ xs
lexer_ (keyword "in" -> Just xs) = jump 2 In $ lexer_ xs
lexer_ (keyword "end" -> Just xs) = jump 3 End $ lexer_ xs
lexer_ (keyword "function" -> Just xs) = jump 7 Function $ lexer_ xs
lexer_ (keyword "var" -> Just xs) = jump 3 Var $ lexer_ xs
lexer_ (keyword "type" -> Just xs) = jump 4 Type $ lexer_ xs
lexer_ (keyword "array" -> Just xs) = jump 5 Array $ lexer_ xs
lexer_ (keyword "if" -> Just xs) = jump 2 If $ lexer_ xs
lexer_ (keyword "then" -> Just xs) = jump 4 Then $ lexer_ xs
lexer_ (keyword "else" -> Just xs) = jump 4 Else $ lexer_ xs
lexer_ (keyword "do" -> Just xs) = jump 2 Do $ lexer_ xs
lexer_ (keyword "of" -> Just xs) = jump 2 Of $ lexer_ xs
lexer_ (keyword "nil" -> Just xs) = jump 3 Nil $ lexer_ xs
-- Punctuation symbol
lexer_ (startWith ":=" -> Just xs) = jump 2 Assign $ lexer_ xs
lexer_ (startWith "," -> Just xs) = go Comma $lexer_ xs
lexer_ (startWith ":" -> Just xs) = go Colon $ lexer_ xs
lexer_ (startWith ";" -> Just xs) = go Semicolon $ lexer_ xs
lexer_ (startWith "(" -> Just xs) = go LeftParen $ lexer_ xs
lexer_ (startWith ")" -> Just xs) = go RightParen $ lexer_ xs
lexer_ (startWith "{" -> Just xs) = go LeftBrace $ lexer_ xs
lexer_ (startWith "}" -> Just xs) = go RightBrace $ lexer_ xs
lexer_ (startWith "[" -> Just xs) = go LeftBracket $ lexer_ xs
lexer_ (startWith "]" -> Just xs) = go RightBracket $ lexer_ xs
lexer_ (startWith "." -> Just xs) = go Dot $ lexer_ xs
lexer_ (startWith "+" -> Just xs) = go Plus $ lexer_ xs
lexer_ (startWith "-" -> Just xs) = go Minus $ lexer_ xs
lexer_ (startWith "*" -> Just xs) = go Times $ lexer_ xs
lexer_ (startWith "/*" -> Just xs) = do
  forward 2
  ys <- takeComments xs
  lexer_ ys
lexer_ (startWith "/" -> Just xs) = go Divide $ lexer_ xs
lexer_ (startWith "=" -> Just xs) = go Eq $ lexer_ xs
lexer_ (startWith "<>" -> Just xs) = jump 2 NotEq $ lexer_ xs
lexer_ (startWith "<=" -> Just xs) = jump 2 Le $ lexer_ xs
lexer_ (startWith "<" -> Just xs) = go Lt $ lexer_ xs
lexer_ (startWith ">=" -> Just xs) = jump 2 Ge $ lexer_ xs
lexer_ (startWith ">" -> Just xs) = go Gt $ lexer_ xs
lexer_ (startWith "&" -> Just xs) = go And $ lexer_ xs
lexer_ (startWith "|" -> Just xs) = go Or $ lexer_ xs
lexer_ (startWith "\"" -> Just xs) = do
  pos <- get
  next
  (str, ys) <- takeString xs
  oth <- lexer_ ys
  return (String str pos : oth)
lexer_ (x@(isAlpha -> True) : xs) = let (tl, ys) = span isAlphaNum_ xs in jump (length tl + 1) (ID (x : tl)) $ lexer_ ys
  where
    isAlphaNum_ c = isAlphaNum c || c == '_'
lexer_ (x@(isNumber -> True) : xs) = let (tl, ys) = span isNumber xs in jump (length tl + 1) (Int (read (x : tl) :: Int)) $ lexer_ ys
lexer_ xs = get >>= let (w, ws) = break isSpace xs in bad w

lexer :: [Char] -> [Token]
lexer xs = evalState (lexer_ xs) (Pos 0 1 1)

startWith :: [Char] -> [Char] -> Maybe [Char]
startWith "" x = Just x
startWith _ "" = Nothing
startWith (x : xs) (y : ys)
  | x == y = startWith xs ys
  | otherwise = Nothing

keyword :: [Char] -> [Char] -> Maybe [Char]
keyword k s = do
  result <- startWith k s
  guard $ null result || (isSpace) (head result)
  return result

bad :: Show a => a -> Pos -> b
bad a (Pos _ r c) = error $ "bad character(s) " ++ show a ++ " at row: " ++ show r ++ " column: " ++ show c

takeComments :: String -> State Pos String
takeComments "" = return ""
takeComments (startWith "/*" -> Just xs) = do
  forward 2
  ys <- takeComments xs
  takeComments ys
takeComments (startWith "*/" -> Just xs) = forward 2 >> return xs
takeComments ('\n' : xs) = newLine >> takeComments xs
takeComments (_ : xs) = next >> takeComments xs

takeString :: String -> State Pos (String, String)
takeString "" = return ("", "")
takeString ('\"' : xs) = next >> return ("", xs)
takeString ('\\' : x : xs) = do
  next
  (s, ys) <- takeString xs
  pos <- get
  let ch = fromMaybe (bad x pos) (escape x)
  next
  return (ch : s, ys)
  where
    escape c = lookup c $ zip "abfnrtv\\'\"0" "\a\b\f\n\r\t\v\\\'\"\0"
takeString (x : xs) = do
  next
  (s, ys) <- takeString xs
  return (x : s, ys)