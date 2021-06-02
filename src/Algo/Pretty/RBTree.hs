{-# LANGUAGE OverloadedStrings #-}

module Algo.Pretty.RBTree (PrettyRBTree (..)) where

import Algo.Data.RBTree (Color (..), RBTree (Empty, Node))
import Data.Aeson
  ( KeyValue ((.=)),
    ToJSON (toJSON),
    Value (Null),
    encode,
    object,
  )
import Data.ByteString.Lazy.Char8 (unpack)

newtype PrettyRBTree a = PrettyRBTree {getRBTree :: RBTree a}

instance ToJSON Color where
  toJSON Red = "Red"
  toJSON Black = "Black"

instance ToJSON a => ToJSON (PrettyRBTree a) where
  toJSON (PrettyRBTree Empty) = Null
  toJSON (PrettyRBTree (Node c v l r)) =
    object
      [ "color" .= c,
        "value" .= v,
        "left" .= PrettyRBTree l,
        "right" .= PrettyRBTree r
      ]

instance ToJSON a => Show (PrettyRBTree a) where
  show p = unpack $ encode p