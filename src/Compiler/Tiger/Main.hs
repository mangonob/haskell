module Compiler.Tiger.Main where

import Compiler.Tiger.Lexer_ (lexer)
import Compiler.Tiger.Parser_ (parser)
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
      print $ lexer contents
    _ ->
      putStrLn $
        unlines
          [ "usage: command_name [-r|-h] [file_name]",
            "    -r   Raw text file.",
            "    -h   Show help message."
          ]