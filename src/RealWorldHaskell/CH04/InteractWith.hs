module RealWorldHaskell.CH04.InteractWith () where

import System.Environment

interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith f input output = do
  input <- readFile input
  writeFile output $ f input

main :: IO ()
main = mainWith myFunction
  where
    mainWith f = do
      [input, output] <- getArgs
      interactWith f input output
    myFunction = id