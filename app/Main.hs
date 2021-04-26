module Main where

main :: IO ()
main = interact $ unlines . filter ((< 10) . length) . lines
