{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Compiler.Production where

import Control.Monad.Writer
import Data.Char (isLower, isSpace, isUpper)
import Data.List (intersperse, nub)
import qualified Data.Map as M
import qualified Data.Set as S
import Prelude hiding (head)

data Symbol
  = Empty
  | -- 终结符号 (eg: id, num, 7)
    Term String
  | -- 非终结符号 (eg: A, Expr)
    NoTerm String
  deriving (Eq, Ord)

-- 空符号
epsilon :: Symbol
epsilon = Empty

isTerm :: Symbol -> Bool
isTerm (Term _) = True
isTerm _ = False

isNoTerm :: Symbol -> Bool
isNoTerm (NoTerm _) = True
isNoTerm _ = False

instance Show Symbol where
  show Empty = "ε"
  show (Term xs) = xs
  show (NoTerm xs) = xs

instance Read Symbol where
  readsPrec _ "" = []
  readsPrec _ str =
    let (word, tail) = takeWord str
     in [(symbol word, tail)]
    where
      takeWord = break isSpace . snd . span isSpace

      isUppers (x : _) = isUpper x
      isLowers (x : _) = isLower x

      symbol "" = error "no symbol"
      symbol "ε" = Empty
      symbol "_e" = Empty
      symbol xs@((isUpper -> True) : _) = NoTerm xs
      symbol xs = Term xs

data Production = Production {head :: Symbol, body :: [Symbol]} deriving (Eq)

instance Show Production where
  show (Production hs bodySyms) = show hs ++ " -> " ++ (concat . intersperse " " . map show) bodySyms

instance Read Production where
  readsPrec _ "" = []
  readsPrec _ str =
    let ([head], (_ : body)) = (break (== "->") . words) str
        hs = read head :: Symbol
        bodySyms =
          if null body
            then [Empty]
            else map read body :: [Symbol]
     in [(Production hs bodySyms, "")]

createProductions :: String -> [Production]
createProductions = map read . lines

main :: IO ()
main = do
  input <- getContents
  putStrLn $ show $ createProductions input

ps1 :: [Production]
ps1 =
  map
    read
    [ "Z -> d",
      "Z -> X Y Z",
      "Y ->",
      "Y -> c",
      "X -> Y",
      "X -> a"
    ]

psE :: [Production]
psE =
  map
    read
    [ "E -> E + T",
      "E -> E - T",
      "E -> T",
      "T -> T * F",
      "T -> T / F",
      "T -> F",
      "F -> ( E )",
      "F -> id",
      "F -> num"
    ]

psL :: [Production]
psL = map read ["E -> T", "T -> E"]

sym :: String -> Symbol
sym = read

(&&&) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
f &&& g = \x -> f x && g x

nullables :: [Production] -> [(Symbol, Bool)]
nullables ps = M.toList $ _nullables initial
  where
    initial = M.fromList $ map (,False) $ nub $ map head ps

    _nullables m =
      let newM = foldl steper m ps
       in if newM == m
            then newM
            else _nullables newM

    steper m (Production h bs)
      | all isNullable bs = M.insert h True m
      | otherwise = m
      where
        isNullable Empty = True
        isNullable (Term x) = False
        isNullable noTermS = M.lookup noTermS m == Just True

firsts :: [Production] -> [(Symbol, [Symbol])]
firsts ps = map (\(k, v) -> (k, S.toList v)) $ M.toList $ _firsts initial
  where
    initial = M.fromList $ map (,S.empty) $ nub $ map head ps

    _firsts m = initial

follows :: [Production] -> [(Symbol, [Symbol])]
follows ps = map (\(k, v) -> (k, S.toList v)) $ M.toList $ _follows initial
  where
    initial = M.fromList $ map (,S.empty) $ nub $ map head ps

    _follows m = initial