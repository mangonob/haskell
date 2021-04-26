module Main where

main :: IO ()
main = interact respondPalindromes

respondPalindromes :: String -> String
respondPalindromes = unlines . map (\x -> if x == reverse x then "palindrome" else "not a palindrome") . lines