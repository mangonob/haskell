module RealWorldHaskell.CH05.SimpleJSON
  ( JValue (..),
    getString,
    getInt,
    getDouble,
    getObject,
    getArray,
    isNull,
  )
where

import Data.List

-- JSON 数据结构定义
data JValue
  = JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JObject [(String, JValue)]
  | JArray [JValue]
  deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getInt :: Integral a => JValue -> Maybe a
getInt (JNumber n) = Just $ truncate n
getInt _ = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _ = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject pair) = Just pair
getObject _ = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray a) = Just a
getArray _ = Nothing

isNull :: JValue -> Bool
isNull = (== JNull)

renderJValue :: JValue -> String
renderJValue (JString s) = show s
renderJValue (JNumber n) = show n
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue JNull = "null"
renderJValue (JObject pairs) = "{" ++ renderPairs pairs ++ "}"
  where
    renderPairs xs = intercalate ", " $ map renderPair xs
    renderPair (s, json) = show s ++ ": " ++ renderJValue json
renderJValue (JArray elements) = "[" ++ renderElements elements ++ "]"
  where
    renderElements xs = intercalate ", " $ map renderJValue xs

putJValue :: JValue -> IO ()
putJValue json = putStrLn $ renderJValue json