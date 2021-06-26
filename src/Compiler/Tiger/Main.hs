module Compiler.Tiger.Main where

import Compiler.Tiger.Lexer (lexer)
import Compiler.Tiger.Parser (parser)
import Compiler.Tiger.Raw (raw)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("-r" : filename : _) -> do
      contents <- readFile filename
      putStrLn $ raw . parser $ lexer contents
    (filename : _) -> do
      contents <- readFile filename
      print . parser $ lexer contents
    _ ->
      putStrLn $
        unlines
          [ "usage: command_name [-r|-h] [file_name]",
            "    -r   Raw text file.",
            "    -h   Show help message."
          ]