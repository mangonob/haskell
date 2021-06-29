module Compiler.JSON.Main where

import Compiler.JSON.Lexer (lexer)
import System.Environment (getArgs)

main :: IO ()
main = do
  (filename : _) <- getArgs
  contents <- readFile filename
  putStrLn $ show $ lexer contents