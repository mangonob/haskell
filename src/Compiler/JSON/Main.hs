module Compiler.JSON.Main where

import Compiler.JSON.Lexer (lexer)
import Compiler.JSON.Parser (parser)
import System.Environment (getArgs)

main :: IO ()
main = do
  (filename : _) <- getArgs
  contents <- readFile filename
  print $ parser $ lexer contents