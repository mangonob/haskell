module Main where

main :: IO ()
main = do
  line <- getLine
  let num = read line :: Int
  if num == 42
    then return ()
    else do
      putStrLn (show num)
      main