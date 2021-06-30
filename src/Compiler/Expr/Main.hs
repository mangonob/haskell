import Compiler.Expr.Lexer (lexer)
import Compiler.Expr.Parser (parser)
import Utils (readLine)

main :: IO ()
main = do
  mline <- readLine ""
  case mline of
    Just line -> do
      print $ parser $ lexer line
    Nothing -> return ()
  main