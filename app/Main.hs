module Main where

import Control.Applicative

main :: IO ()
main = do
  line <- fmap reverse getLine
  putStrLn line

data Action a = Pop a | Top a | Bottom a deriving (Show)

solve :: (Ord a, Num a) => [a] -> [Action a]
solve [] = []
solve xs
  | sum l < sum r = (map (\x -> Bottom x) l) ++ [Pop m] ++ solve (t ++ l)
  | otherwise = (map (\x -> Top x) (reverse r)) ++ [Pop m] ++ solve (t ++ l)
  where
    m = minimum xs
    (l, r) = break (== m) xs
    t = tail r

data Optional a = Nil | Some a deriving (Show)

instance Functor Optional where
  fmap _ Nil = Nil
  fmap f (Some x) = Some $ f x

instance Applicative Optional where
  pure x = Some x
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  Some f <*> Some x = Some $ f x

instance Monad Optional where
  return x = Some x
  Nil >>= _ = Nil
  Some x >>= f = f x

landLeft :: (Ord b, Num b) => b -> (b, b) -> Maybe (b, b)
landLeft x (left, right)
  | abs (left + x - right) < 4 = Just (left + x, right)
  | otherwise = Nothing

landRight :: (Ord b, Num b) => b -> (b, b) -> Maybe (b, b)
landRight x (left, right)
  | abs (right + x - left) < 4 = Just (left, right + x)
  | otherwise = Nothing

foo :: (Ord a, Num a) => Maybe (a, a)
foo = do
  a <- return (0, 0)
  b <- landLeft 3 a
  landRight 3 b