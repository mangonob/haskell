{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Compiler.Production
  ( Production,
    Symbol,
    SymbolSets,
    head,
    body,
    epsilon,
    firsts,
    follows,
    isNoTerm,
    isTerm,
    nullables,
    sym,
  )
where

import Data.Char (isLower, isSpace, isUpper)
import Data.List (insert, intersperse, nub)
import qualified Data.Map as M
import qualified Data.Set as S
import Utils (just, (??))
import Prelude hiding (head)

data Symbol
  = Empty
  | -- | Terminal symbol (eg: id, num, 7)
    Term String
  | -- | No terminal symbol (eg: A, Expr)
    NoTerm String
  deriving (Eq, Ord)

-- | Empty symbol.
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

-- | Create a 'Symbol'.
--
-- >  sym "A" == NoTerm "A"
-- >  sym "num" == Term "num"
sym ::
  -- | Symbol text
  String ->
  -- | Symbol
  Symbol
sym = read

-- | 'nullables' use to judge Term-Symbol in a series of productions is nullable.
nullables ::
  -- | Productions
  [Production] ->
  -- | Nullable set (key: Symbol, value: Symbol is nullable(?))
  M.Map Symbol Bool
nullables ps = _nullables initial
  where
    initial = M.fromList . map (,False) . nub $ map head ps

    _nullables m =
      let newM = foldl stp m ps
       in if newM == m
            then newM
            else _nullables newM

    stp m (Production h bs)
      | all isNullable bs = M.insert h True m
      | otherwise = m
      where
        isNullable Empty = True
        isNullable (Term x) = False
        isNullable noTermS = M.lookup noTermS m == Just True

untilSame :: (Eq t1, Foldable t2) => (t1 -> a -> t1) -> t1 -> t2 a -> t1
untilSame step v xs =
  let v' = foldl step v xs
   in if v' == v
        then v'
        else untilSame step v' xs

prefixWhen :: (a -> Bool) -> [a] -> [a]
prefixWhen f [] = []
prefixWhen f (x : xs) =
  if f x
    then x : prefixWhen f xs
    else [x]

type SymbolSets = M.Map Symbol (S.Set Symbol)

firstIn :: SymbolSets -> Symbol -> S.Set Symbol
firstIn m Empty = S.empty
firstIn m (Term x) = S.singleton (Term x)
firstIn m noTerm = just $ (M.lookup noTerm m) ?? Just S.empty

-- | FIRST set of a series of productions.
firsts :: [Production] -> SymbolSets
firsts ps = untilSame step M.empty ps
  where
    prefixes = prefixWhen (\s -> M.lookup s (nullables ps) == Just True)

    step m (Production h bs) =
      let s = foldl S.union S.empty $ map (firstIn m) $ prefixes bs
       in merge m $ M.singleton h s

follows :: [Production] -> SymbolSets
follows ps = untilSame step M.empty ps
  where
    firstOf = firstIn $ firsts ps
    prefixes = prefixWhen (\s -> M.lookup s (nullables ps) == Just True)
    postfixes = prefixes . reverse

    step m (Production h bs) = merge m $ merge m' $ asRight bs
      where
        m' = foldl step M.empty (postfixes bs)
          where
            step m sym = merge m $ M.singleton sym (followOf h)

        asRight [] = M.empty
        asRight (x : xs) = merge (foldl step M.empty (prefixes xs)) (asRight xs)
          where
            step m sym = merge m $ M.singleton x (firstOf sym)

        followOf s = just $ M.lookup s m ?? Just S.empty

-- | Merge two 'SymbolSet'.
merge :: SymbolSets -> SymbolSets -> SymbolSets
merge m1 m2 = M.mergeWithKey (\k s1 s2 -> Just $ S.union s1 s2) id id m1 m2
