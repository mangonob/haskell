{-# LANGUAGE ViewPatterns #-}

module Compiler.Production where

import Data.Char (isLower, isSpace, isUpper)
import Data.List (intersperse)
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

productionsExample :: [Production]
productionsExample =
  map
    read
    [ "Z -> d",
      "Z -> X Y Z",
      "Y ->",
      "Y -> c",
      "X -> Y",
      "X -> a"
    ]

mapForProductions :: [Production] -> M.Map Symbol [Production]
mapForProductions [] = M.empty
mapForProductions (p@(Production hs bs) : xs) =
  let m = mapForProductions xs
   in case M.lookup hs m of
        Just ps -> M.insert hs (p : ps) m
        Nothing -> M.insert hs [p] m

just :: Maybe p -> p
just (Just a) = a
just Nothing = error "no value"

(&&&) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
f &&& g = \x -> f x && g x

nullable :: [Production] -> Symbol -> Bool
nullable _ Empty = True
nullable _ (Term _) = False
nullable ps s =
  any (all ((/= s) &&& nullable ps)) $ map body $ just $ M.lookup s pMaps
  where
    pMaps = mapForProductions ps

first :: [Production] -> Symbol -> [Symbol]
first _ Empty = []
first _ (Term s) = [Term s]
first (p : ps) (NoTerm s) = undefined

follow :: [Production] -> Symbol -> [Symbol]
follow = undefined