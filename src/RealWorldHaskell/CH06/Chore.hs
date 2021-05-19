{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module RealWorldHaskell.CH06.Chore where

import Control.Arrow

class BaseEq a where
  isEqual :: a -> a -> Bool
  isEqual a b = not $ notEqual a b

  notEqual :: a -> a -> Bool
  notEqual a b = not $ isEqual a b

instance BaseEq Int where
  isEqual = (==)

instance BaseEq Integer where
  isEqual = (==)

instance BaseEq Char where
  isEqual = (==)

instance BaseEq Bool where
  isEqual True True = True
  isEqual False False = True
  isEqual _ _ = False

instance BaseEq a => BaseEq ([] a) where
  isEqual [] [] = True
  isEqual xs [] = False
  isEqual [] ys = False
  isEqual (x : xs) (y : ys) = isEqual x y && isEqual xs ys

data Color = Red | Green | Blue

instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

startWith :: Eq a => [a] -> [a] -> Maybe [a]
startWith [] xs = Just xs
startWith ys [] = Nothing
startWith (x : xs) (((== x) -> True) : ys) = startWith xs ys
startWith _ _ = Nothing

instance Read Color where
  readsPrec i (startWith "Red" -> Just xs) = [(Red, xs)]
  readsPrec i (startWith "Green" -> Just xs) = [(Green, xs)]
  readsPrec i (startWith "Blue" -> Just xs) = [(Blue, xs)]
  readsPrec _ _ = []

-- 覆盖实例
class Borked a where
  bork :: a -> String

instance Borked Int where
  bork = show

instance {-# OVERLAPPABLE #-} Borked (Int, Int) where
  bork (a, b) = bork a ++ ", " ++ bork b

instance {-# OVERLAPPABLE #-} (Borked a, Borked b) => Borked (a, b) where
  bork (a, b) = ">>" ++ bork a ++ " " ++ bork b ++ "<<"

--
newtype NewString = NewString String

instance Borked NewString where
  bork (NewString s) = show s

-- Control.Arrow
foo :: Int -> String
foo n = "Foo: " ++ show n

test :: (String, [Char])
test = (first foo) (1, "second") -- Output: ("Foo: 1", "second")