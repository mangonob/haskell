{-# LANGUAGE ViewPatterns #-}

module Compiler.Regex where

import Control.Monad.State
  ( MonadState (get, put),
    State,
    runState,
  )
import Data.Char (isDigit)

data Token
  = CharTk Char
  | ParenthesLeft
  | ParenthesRight
  | BracketLeft
  | BracketRight
  | BraceLeft
  | BraceRight
  | Comma
  | Minus
  | Caret
  | Star
  | Plus
  | Question
  | Bar
  | Num Int
  | End
  deriving (Show, Eq)

startWith :: [Char] -> [Char] -> Maybe [Char]
startWith "" x = Just x
startWith _ "" = Nothing
startWith (x : xs) (y : ys)
  | x == y = startWith xs ys
  | otherwise = Nothing

lexer :: String -> [Token]
lexer "" = []
lexer ('(' : xs) = ParenthesLeft : lexer xs
lexer (')' : xs) = ParenthesRight : lexer xs
lexer ('{' : xs) =
  let (ys, tks) = snd $ runState (takeNum >> takeChar ',' >> takeNum) (xs, [])
   in BracketLeft : tks ++ lexer ys
lexer ('}' : xs) = BraceRight : lexer xs
lexer ('[' : xs) =
  let (cs, ys) = break (== ']') xs
      contents = case cs of
        ('^' : xs) -> Caret : csTokens xs
        xs -> csTokens xs
   in BracketLeft : contents ++ lexer ys
  where
    -- Tokens in char set
    csTokens [] = []
    csTokens ('-' : xs) = Minus : csTokens xs
    csTokens (x : xs) = CharTk x : csTokens xs
lexer (']' : xs) = BracketRight : lexer xs
lexer (',' : xs) = Comma : lexer xs
lexer ('|' : xs) = Bar : lexer xs
lexer ('*' : xs) = Star : lexer xs
lexer ('+' : xs) = Plus : lexer xs
lexer ('?' : xs) = Question : lexer xs
lexer ('\\' : x : xs) =
  if isExcape x
    then CharTk x : lexer xs
    else lexer (x : xs)
  where
    isExcape = (`elem` "(){}[],-^|*+?")
lexer (x : xs) = CharTk x : lexer xs

takeNum :: State (String, [Token]) ()
takeNum = do
  (xs, tokens) <- get
  let (m, ys) = span isDigit xs
  if (not . null) m
    then do
      put (ys, tokens ++ [Num (read m :: Int)])
    else return ()

takeChar :: Char -> State (String, [Token]) ()
takeChar ch = do
  (xs, toknes) <- get
  case startWith [ch] xs of
    Just ys -> do
      put (ys, toknes ++ [CharTk ch])
    Nothing -> return ()

data Reg = Con Reg RegItem | Item RegItem

data RegItem = Char Char | Reg Reg | CharSet

data CharSet = Include CSItems | Exclude CSItems

data CSItems = CSItems CSItem

data CSItem = Single Char | Range Char Char

lookAhead :: State [Token] Token
lookAhead = do
  ts <- get
  if null ts then return End else return (head ts)

eat :: State [Token] ()
eat = get >>= put . tail

match :: Token -> State [Token] ()
match t = do
  la <- lookAhead
  if la == t
    then eat
    else error $ show la ++ " is not match " ++ show t

csItem :: State [Token] CSItem
csItem = undefined

-- Reg -> Reg | Con
-- Reg -> Con
-- Con -> Con Closure
-- Con -> Closure
-- Closure -> Term *
-- Closure -> Term +
-- Closure -> Term ?
-- Closure -> Term
-- Term -> char
-- Term -> [ charset ]
-- Term -> [ ^ charset ]
-- Term -> ( Reg )