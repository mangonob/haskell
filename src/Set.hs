{-# LANGUAGE ViewPatterns #-}

module Set () where

import Data.List

data Set a = Set [a] deriving (Show)

has :: Eq a => a -> Set a -> Maybe (Set a)
has x (Set xs)
  | elem x xs = Just $ Set (delete x xs)
  | otherwise = Nothing

delete' :: Eq a => a -> Set a -> Set a
delete' x (has x -> Just s) = s
delete' _ s = s

insert' :: Eq a => a -> Set a -> Set a
insert' x s@(has x -> Just _) = s
insert' x (Set xs) = Set (x : xs)