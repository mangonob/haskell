{-# LANGUAGE NoImplicitPrelude #-}

module Compiler.Tiger.Semantic.STable where

import Compiler.Tiger.Semantic.Types
import Compiler.Tiger.Symbol (Sym)
import qualified Data.Map as Map
import Data.Maybe (Maybe (Just, Nothing))
import Prelude (Show, error, ($))

type Table a = Map.Map Sym a

data STable a = STable {prev :: Maybe (STable a), table :: Table a} deriving (Show)

empty :: STable a
empty = STable {prev = Nothing, table = Map.empty}

singleton :: Sym -> a -> STable a
singleton s t = STable {prev = Nothing, table = Map.singleton s t}

insert :: Sym -> a -> STable a -> STable a
insert s t STable {prev = a, table = b} = STable {prev = a, table = Map.insert s t b}

enter :: STable a -> STable a
enter st = STable {prev = Just st, table = Map.empty}

exit :: STable a -> STable a
exit STable {prev = Nothing, table = _} = error "can not exit"
exit STable {prev = Just st, table = b} = st

lookup :: Sym -> STable a -> Maybe a
lookup s STable {prev = a, table = b} = case Map.lookup s b of
  Just t -> Just t
  Nothing -> case a of
    Just st -> lookup s st
    Nothing -> Nothing