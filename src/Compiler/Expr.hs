{-# LANGUAGE ViewPatterns #-}

module Compiler.Expr where

import Control.Monad.State
  ( MonadState (get, put),
    State,
    runState,
  )
import Data.Char (isAlpha, isDigit, isSpace)

-- Lexer
data Token
  = Plus
  | Minus
  | Times
  | DivTk
  | ParenthesLeft
  | ParenthesRight
  | NumTk Double
  | IntTk Int
  | End
  deriving (Show, Eq)

takeT :: Read a => String -> Maybe (a, String)
takeT xs =
  let result = readsPrec 0 xs
   in if null result
        then Nothing
        else Just (head result)

lexer :: String -> [Token]
lexer "" = []
lexer ((isSpace -> True) : xs) = lexer xs
lexer ('-' : xs) = Minus : lexer xs
lexer ('+' : xs) = Plus : lexer xs
lexer ('*' : xs) = Times : lexer xs
lexer ('/' : xs) = DivTk : lexer xs
lexer ('(' : xs) = ParenthesLeft : lexer xs
lexer (')' : xs) = ParenthesRight : lexer xs
lexer x@(takeT -> Just (i, xs)) = IntTk i : lexer xs
lexer x@(takeT -> Just (num, xs)) = NumTk num : lexer xs
lexer (x : xs) = error $ "bad token " ++ show x

-- Parser
data Factor = Num Double | Int Int | Expr Expr | UMinus Factor deriving (Show)

data Term = Mul Term Factor | Div Term Factor | Factor Factor deriving (Show)

data Expr = Add Expr Term | Sub Expr Term | Term Term deriving (Show)

class Eval a where
  eval :: a -> Double

instance Eval Expr where
  eval (Add e t) = eval e + eval t
  eval (Sub e t) = eval e - eval t
  eval (Term t) = eval t

instance Eval Term where
  eval (Mul t f) = eval t * eval f
  eval (Div t f) = eval t / eval f
  eval (Factor f) = eval f

instance Eval Factor where
  eval (Num num) = num
  eval (Int i) = fromIntegral i
  eval (Expr e) = eval e
  eval (UMinus e) = - (eval e)

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

expr :: State [Token] Expr
expr = term >>= expr' . Term

expr' :: Expr -> State [Token] Expr
expr' ep = do
  la <- lookAhead
  case la of
    Plus -> do
      eat
      t <- term
      expr' (Add ep t)
    Minus -> do
      eat
      t <- term
      expr' (Sub ep t)
    _ -> return ep

term :: State [Token] Term
term = factor >>= term' . Factor

term' :: Term -> State [Token] Term
term' trm = do
  la <- lookAhead
  case la of
    Times -> do
      eat
      f <- factor
      term' (Mul trm f)
    DivTk -> do
      eat
      f <- factor
      term' (Div trm f)
    _ -> return trm

factor :: State [Token] Factor
factor = do
  la <- lookAhead
  case la of
    NumTk num -> eat >> return (Num num)
    IntTk i -> eat >> return (Int i)
    Minus -> eat >> factor >>= return . UMinus
    ParenthesLeft -> do
      eat
      ep <- expr
      match ParenthesRight
      return (Expr ep)

evaluate :: String -> Double
evaluate xs = eval . fst . runState expr $ lexer xs