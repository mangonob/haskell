{-# LANGUAGE NoImplicitPrelude #-}

module Compiler.Tiger.Semantic.STable where

import Compiler.Tiger.Semantic.Types
import Compiler.Tiger.Symbol (Sym)
import qualified Data.Map as Map
import Data.Maybe (Maybe (Just, Nothing))
import Prelude (Show, error)

type Table = Map.Map Sym Type

data STable = STable {prev :: Maybe STable, table :: Table} deriving (Show)

empty :: STable
empty = STable {prev = Nothing, table = Map.empty}

singleton :: Sym -> Type -> STable
singleton s t = STable {prev = Nothing, table = Map.singleton s t}

insert :: Sym -> Type -> STable -> STable
insert s t STable {prev = a, table = b} = STable {prev = a, table = Map.insert s t b}

enter :: STable -> STable
enter st = STable {prev = Just st, table = Map.empty}

exit :: STable -> STable
exit STable {prev = Nothing, table = _} = error "can not exit"
exit STable {prev = Just st, table = b} = st

lookup :: Sym -> STable -> Maybe Type
lookup s STable {prev = a, table = b} = case Map.lookup s b of
  Just t -> Just t
  Nothing -> case a of
    Just st -> lookup s st
    Nothing -> Nothing