module Main where

main :: IO ()
main = do
  line <- fmap reverse getLine
  putStrLn line

data Wrapper a = Wrapper a deriving (Show)

instance Functor Wrapper where
  fmap f (Wrapper x) = Wrapper $ f x

data ReversedList a = ReversedList {getList :: [a]} deriving (Show)

instance Functor ReversedList where
  fmap _ (ReversedList []) = ReversedList []
  fmap f (ReversedList (x : xs)) = ReversedList $ (getList $ fmap f $ ReversedList xs) ++ [f x]
