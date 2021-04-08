module Main where

main :: IO ()
main = putStrLn "Hello world!"

a :: Double -> Double
a = do
  a <- (* 10)
  b <- (/ 3)
  return (a + b)