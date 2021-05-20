module RealWorldHaskell.CH07.Chore where

import System.IO

str2action :: String -> IO ()
str2action input = putStrLn $ "Data: " ++ input

list2actions :: [String] -> [IO ()]
list2actions = map str2action

numbers :: [Integer]
numbers = [1 .. 10]

strings :: [String]
strings = map show numbers

actions :: [IO ()]
actions = list2actions strings

runall :: [IO ()] -> IO ()
runall [] = return ()
runall (x : xs) = x >>= \_ -> runall xs

printitall :: IO ()
printitall = runall actions

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  echo
  where
    echo = getChar >>= putChar >>= return echo