module DiffList () where

import Data.Monoid ()

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  f <> g = mappend f g

instance Monoid (DiffList a) where
  mempty = DiffList ([] ++)
  (DiffList f) `mappend` (DiffList g) = DiffList (f . g)

listStringAppend :: (Num a, Ord a, Show a) => a -> [Char]
listStringAppend 0 = ""
listStringAppend x
  | x < 0 = error "x must greater then 0"
  | otherwise = listStringAppend (x - 1) ++ show x

diffListStringAppend :: (Num a, Ord a, Show a) => a -> DiffList Char
diffListStringAppend 0 = toDiffList ""
diffListStringAppend x
  | x < 0 = error "x must greater than 0"
  | otherwise = diffListStringAppend (x - 1) `mappend` toDiffList (show x)

-- Slow append
-- > length $ listStringAppend 10000

-- Fast append
-- > length $ fromDiffList $ diffListStringAppend 10000