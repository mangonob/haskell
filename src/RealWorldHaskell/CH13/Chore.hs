{-# LANGUAGE ViewPatterns #-}

module RealWorldHaskell.CH13.Chore where

-- lookup
al :: [(Integer, [Char])]
al = [(1, "one"), (2, "two"), (3, "three"), (4, "four")]

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' k [] = Nothing
lookup' k (((== k) -> True, v) : xs) = Just v
lookup' k (_ : xs) = lookup' k xs