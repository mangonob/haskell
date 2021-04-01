module Parser
  ( Expr (..),
    Term (..),
    Factor (..),
    expr,
    evaluate,
  )
where

data Token
  = Plus
  | Minus
  | Times
  | Div
  | ParenthesOpen
  | ParenthesClose
  | Value Int
  deriving (Show, Eq)

data Expr
  = Term Term
  | AddExpr Expr Term
  | SubExpr Expr Term
  | ResetExpr Term Expr'
  deriving (Show)

data Expr' = EmptyExpr' | AddExpr' Term Expr' | SubExpr' Term Expr' deriving (Show)

data Term
  = Factor Factor
  | MultTerm Term Factor
  | DivTerm Term Factor
  | RestTerm Factor Term'
  deriving (Show)

data Term' = EmptyTerm' | MultTerm' Factor Term' | DivTerm' Factor Term' deriving (Show)

data Factor = Parenthes Expr | Number Int deriving (Show)

class Evalable a where
  eval :: a -> Double

instance Evalable Expr where
  eval (Term t) = eval t
  eval (AddExpr e t) = eval e + eval t
  eval (SubExpr e t) = eval e - eval t
  eval e = error $ "bad expr " ++ show e

instance Evalable Term where
  eval (Factor f) = eval f
  eval (MultTerm t f) = eval t * eval f
  eval (DivTerm t f) = eval t / eval f
  eval t = error $ "bad term " ++ show t

instance Evalable Factor where
  eval (Parenthes e) = eval e
  eval (Number v) = fromIntegral v

lexer :: String -> [Token]
lexer "" = []
lexer ('+' : xs) = Plus : lexer xs
lexer ('-' : xs) = Minus : lexer xs
lexer ('*' : xs) = Times : lexer xs
lexer ('/' : xs) = Div : lexer xs
lexer ('(' : xs) = ParenthesOpen : lexer xs
lexer (')' : xs) = ParenthesClose : lexer xs
lexer (' ' : xs) = lexer xs
lexer all@(x : _) | isDigit x = Value num : lexer xs
  where
    (numStr, xs) = break (not . isDigit) all
    num = read numStr :: Int
lexer (x : _) = error $ "unexcepted " ++ [x]

isDigit :: Char -> Bool
isDigit x = elem x ['0' .. '9']

expr :: [Token] -> (Expr, [Token])
expr xs = (toExpr (Term t) e', zs)
  where
    (t, ys) = term xs
    (e', zs) = expr' ys

    toExpr :: Expr -> Expr' -> Expr
    toExpr e EmptyExpr' = e
    toExpr e (AddExpr' t e') = toExpr (AddExpr e t) e'
    toExpr e (SubExpr' t e') = toExpr (SubExpr e t) e'

expr' :: [Token] -> (Expr', [Token])
expr' (Plus : xs) = (AddExpr' t e, zs)
  where
    (t, ys) = term xs
    (e, zs) = expr' ys
expr' (Minus : xs) = (SubExpr' t e, zs)
  where
    (t, ys) = term xs
    (e, zs) = expr' ys
expr' xs = (EmptyExpr', xs)

term :: [Token] -> (Term, [Token])
term xs = (toTerm (Factor f) t', zs)
  where
    (f, ys) = factor xs
    (t', zs) = term' ys

    toTerm :: Term -> Term' -> Term
    toTerm t EmptyTerm' = t
    toTerm t (MultTerm' f t') = toTerm (MultTerm t f) t'
    toTerm t (DivTerm' f t') = toTerm (DivTerm t f) t'

term' :: [Token] -> (Term', [Token])
term' (Times : xs) = (MultTerm' f t, zs)
  where
    (f, ys) = factor xs
    (t, zs) = term' ys
term' (Div : xs) = (DivTerm' f t, zs)
  where
    (f, ys) = factor xs
    (t, zs) = term' ys
term' xs = (EmptyTerm', xs)

factor :: [Token] -> (Factor, [Token])
factor (ParenthesOpen : xs) = (Parenthes e, ts)
  where
    (e, (ParenthesClose : ts)) = expr xs
factor (Value v : xs) = (Number v, xs)

evaluate :: String -> Double
evaluate s = eval . fst . expr $ lexer s