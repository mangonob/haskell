module Crypt.CaesarCipher () where

import Data.Char

translate :: Int -> Char -> Char
translate x c = chr' $ mod (x + ord' c) 26
  where
    ord' x = ord (toLower x) - ord 'a'
    chr' x = chr $ x + ord 'a'

encrypt :: Int -> String -> String
encrypt _ [] = []
encrypt k (x : xs) = translate k x : encrypt k xs

decrypt :: Int -> String -> String
decrypt _ [] = []
decrypt k (x : xs) = translate (- k) x : decrypt k xs

decryptMap :: String -> [String]
-- keyspace [0..26]
decryptMap x = decrypt <$> [0 .. 26] <*> [x]
