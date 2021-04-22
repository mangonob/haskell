module Main where

import Data.Char

main :: IO ()
main = do
  str <- getLine
  putStrLn $ title str
  main

title :: [Char] -> [Char]
title [] = []
title (x : xs) = toUpper x : xs