{-# LANGUAGE ViewPatterns #-}

module RealWorldHaskell.CH04.SplitLines () where

splitLines :: [Char] -> [[Char]]
splitLines [] = []
splitLines (break isLineTerminator -> (xs, ys)) = xs : splitLines (trim ys)

trim :: [Char] -> [Char]
trim ('\r' : '\n' : xs) = xs
trim ('\r' : xs) = xs
trim ('\n' : xs) = xs
trim xs = xs

isLineTerminator :: Char -> Bool
isLineTerminator c = elem c ['\r', '\n']

fixLine :: [Char] -> String
fixLine = unlines . splitLines