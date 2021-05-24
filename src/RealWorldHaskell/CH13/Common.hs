{-# LANGUAGE ViewPatterns #-}

module RealWorldHaskell.CH13.Common where

startWith :: Eq a => [a] -> [a] -> Maybe [a]
startWith [] ys = Just ys
startWith xs [] = Nothing
startWith (x : xs) (((== x) -> True) : ys) = startWith xs ys
startWith _ _ = Nothing

split :: Eq a => [a] -> [a] -> [[a]]
split t [] = []
split [] (x : xs) = [x] : split [] xs
split t (startWith t -> Just ys) = [] : split t ys
split t (x : (split t -> [])) = [[x]]
split t (x : (split t -> ys)) = (x : head ys) : tail ys