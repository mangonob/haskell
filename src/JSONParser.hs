module JSONParser () where

import Text.Regex

data Token
  = BracesOpen
  | BracesClose
  | BracketsOpen
  | BracketsClose
  | Comma
  | Semicolon
  | StringTk String
  | Identify String
  | BoolTk Bool
  | IntegerTk Int
  | FloatTk Double
  | UndefinedTk
  | NullTk
  deriving (Show, Eq)

lexer :: String -> [Token]
lexer "" = []
lexer ('{' : xs) = BracesOpen : lexer xs
lexer ('}' : xs) = BracesClose : lexer xs
lexer ('[' : xs) = BracketsOpen : lexer xs
lexer (']' : xs) = BracketsClose : lexer xs
lexer ('"' : xs) = StringTk str : lexer ys
  where
    (str, (_ : ys)) = break (== '"') xs
lexer ('\'' : xs) = StringTk str : lexer ys
  where
    (str, (_ : ys)) = break (== '\'') xs
lexer (':' : xs) = Semicolon : lexer xs
lexer (',' : xs) = Comma : lexer xs
lexer all@(x : xs)
  | isBlank x = lexer xs
  | isNumPrefix x = let (t, xs) = takeNum all in t : lexer xs
  | isAlpha x = case id of
    "true" -> BoolTk True : lexers
    "false" -> BoolTk False : lexers
    "undefined" -> UndefinedTk : lexers
    "null" -> NullTk : lexers
    otherwise -> Identify (x : idTail) : lexers
  | otherwise = error $ "unexcepted " ++ show x
  where
    (idTail, ys) = break (\x -> (not . isAlpha) x && (not . isNum) x) xs
    id = x : idTail
    lexers = lexer ys
    isNum x = elem x ['0' .. '9']
    isBlank x = elem x ['\t', '\n', ' ']
    isAlpha x = elem x $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']
    isNumPrefix x = isNum x || elem x ['-', '.']
    takeNum s = case matchRegexAll floatRegex s of
      Just (_, x, xs, _) -> (FloatTk (read x :: Double), xs)
      Nothing -> case matchRegexAll intRegex s of
        Just (_, x, xs, _) -> (IntegerTk (read x :: Int), xs)
        Nothing -> let (x : xs) = s in error $ "unexcepted " ++ show x
      where
        intRegex = mkRegex "^-?[0-9]+"
        floatRegex = mkRegex "^-?[0-9]+((\\.[0-9]+[eE]-?[0-9]+)|(\\.[0-9]+)|([eE]-?[0-9]+))"

data Number = Integer Int | Float Double deriving (Show)

data JSON = Undefiend | Null | Boolean Bool | Number Number | String String | Array [JSON] | Object [(String, JSON)] deriving (Show)

json :: [Token] -> (JSON, [Token])
json (UndefinedTk : xs) = (Undefiend, xs)
json (NullTk : xs) = (Null, xs)
json (BoolTk b : xs) = (Boolean b, xs)
json (IntegerTk i : xs) = (Number $ Integer i, xs)
json (FloatTk f : xs) = (Number $ Float f, xs)
json (StringTk s : xs) = (String s, xs)
json all@(BracesOpen : _) = let (p, xs) = object all in (Object p, xs)
json all@(BracketsOpen : _) = let (e, xs) = array all in (Array e, xs)
json xs = error $ "bad json " ++ show xs

object :: [Token] -> ([(String, JSON)], [Token])
object (BracesOpen : xs) = case ys of
  (BracesClose : zs) -> (pair, zs)
  otherwise -> error $ "excepted }"
  where
    (pair, ys) = contents xs

    contents :: [Token] -> ([(String, JSON)], [Token])
    contents (StringTk str : Semicolon : xs) =
      let (j, ys) = json xs
          (pair, zs) = oTail ys
       in ((str, j) : pair, zs)
    contents (Identify str : Semicolon : xs) =
      let (j, ys) = json xs
          (pair, zs) = oTail ys
       in ((str, j) : pair, zs)
    contents ts = ([], ts)

    oTail :: [Token] -> ([(String, JSON)], [Token])
    oTail (Comma : StringTk str : Semicolon : xs) =
      let (j, ys) = json xs
          (pair, zs) = oTail ys
       in ((str, j) : pair, zs)
    oTail (Comma : Identify str : Semicolon : xs) =
      let (j, ys) = json xs
          (pair, zs) = oTail ys
       in ((str, j) : pair, zs)
    oTail (xs) = ([], xs)

array :: [Token] -> ([JSON], [Token])
array (BracketsOpen : xs) = case ys of
  (BracketsClose : zs) -> (cts, zs)
  otherwise -> error $ "excepted ]"
  where
    (cts, ys) = contents xs

    contents :: [Token] -> ([JSON], [Token])
    contents [] = ([], [])
    contents (Comma : xs) =
      let (t, ys) = aTail xs
       in (Null : t, ys)
    contents all@(BracketsClose : _) = ([], all)
    contents xs =
      let (j, ys) = json xs
          (t, zs) = aTail ys
       in (j : t, zs)

    aTail :: [Token] -> ([JSON], [Token])
    aTail [] = ([], [])
    aTail all@(BracketsClose : _) = ([], all)
    aTail (Comma : BracketsClose : xs) = ([Null], BracketsClose : xs)
    aTail (Comma : Comma : xs) =
      let (t, ys) = aTail $ Comma : xs
       in (Null : t, ys)
    aTail (Comma : xs) =
      let (j, ys) = json xs
          (t, zs) = aTail ys
       in (j : t, zs)