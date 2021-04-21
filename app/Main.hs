module Main where

main :: IO ()
main = do
  putStrLn $ show 42

fibo :: Int -> Integer
fibo = (map memo [1 ..] !!)
  where
    memo 1 = 1
    memo 2 = 1
    memo x = fibo (x - 1) + fibo (x - 2)