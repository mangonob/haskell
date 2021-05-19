module RealWorldHaskell.CH05.PrettyJSON (renderJValue) where

import RealWorldHaskell.CH05.Prettify
import RealWorldHaskell.CH05.SimpleJSON (JValue (..))
import Prelude hiding ((<>))

renderJValue :: JValue -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber n) = double n
renderJValue (JString s) = string s
renderJValue (JArray a) = series '[' ']' renderJValue a
renderJValue (JObject o) = series '{' '}' field o
  where
    field (name, val) =
      string name
        <> text ": "
        <> renderJValue val
