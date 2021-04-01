module Parser () where

data Token = Plus | Minus | Times | Div | ParenthesOpen | ParenthesClose | Value Int deriving (Show)

lexer :: String -> [Token]
lexer "" = []
lexer ('+' : xs) = Plus : lexer xs
lexer ('-' : xs) = Minus : lexer xs
lexer ('*' : xs) = Times : lexer xs
lexer ('/' : xs) = Div : lexer xs
lexer ('(' : xs) = ParenthesOpen : lexer xs
lexer (')' : xs) = ParenthesClose : lexer xs
lexer (' ' : xs) = lexer xs
lexer all@(x : _) | isDigit x = Value num : lexer xs
  where
    (numStr, xs) = break (not . isDigit) all
    num = read numStr :: Int
lexer (x : _) = error $ "unexcepted " ++ [x]

isDigit :: Char -> Bool
isDigit x = elem x ['0' .. '9']
