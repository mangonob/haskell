{-# LANGUAGE ViewPatterns #-}

module RealWorldHaskell.CH08.GlobRegex where

import Text.Regex.Posix

globToRegex :: String -> String
globToRegex = ('^' :) . (++ "$") . globToRegex'

globToRegex' :: String -> String
globToRegex' "" = ""
globToRegex' ('*' : cs) = ".*" ++ globToRegex' cs
globToRegex' ('?' : cs) = "." ++ globToRegex' cs
globToRegex' ('[' : '!' : c : cs) = "[^" ++ c : charClass cs
globToRegex' ('[' : c : cs) = '[' : c : charClass cs
globToRegex' (c : cs) = escape c ++ globToRegex' cs

escape :: Char -> String
escape c@((`elem` "\\+()^$.{}]|") -> True) = '\\' : [c]
escape c = [c]

charClass :: String -> String
charClass [] = error "unterminated character class"
charClass (']' : cs) = ']' : globToRegex' cs
charClass (c : cs) = c : charClass cs

matchesGlob :: FilePath -> String -> Bool
matchesGlob name pat = name =~ globToRegex pat :: Bool