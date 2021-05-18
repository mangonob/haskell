{-# LANGUAGE ViewPatterns #-}

module RealWorldHaskell.CH03.ListADT (List (..)) where

data List a = Nil | Cons a (List a) deriving (Show)

fromList :: [a] -> List a
fromList [] = Nil
fromList (x : xs) = Cons x (fromList xs)

toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

nth :: (Ord t, Num t) => t -> List a -> Maybe a
nth n xs | n < 0 = Nothing
nth n Nil = Nothing
nth 0 (Cons x xs) = Just x
nth n (Cons x xs) = nth (n - 1) xs