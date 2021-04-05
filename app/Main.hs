module Main where

import Control.Applicative

main :: IO ()
main = putStrLn "42"

sqA :: Applicative f => [f a] -> f [a]
sqA [] = pure []
sqA (x : xs) = (:) <$> x <*> sqA xs

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) =
  filter
    onBoard
    [ (c + 2, r + 1),
      (c + 2, r - 1),
      (c - 2, r + 1),
      (c - 2, r - 1),
      (c + 1, r + 2),
      (c + 1, r - 2),
      (c - 1, r + 2),
      (c - 1, r - 2)
    ]
  where
    onBoard (c, r) = (elem c [1 .. 8]) && (elem r [1 .. 8])

in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight