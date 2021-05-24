{-# LANGUAGE ViewPatterns #-}

module RealWorldHaskell.CH13.Passwd where

import Data.List
import RealWorldHaskell.CH13.Common
import System.Environment

-- 读取文件并根据UID查找对应的用户名
main :: IO ()
main = do
  (filename : uidS : xs) <- getArgs
  let uid = (read uidS) :: Int
  contents <- readFile filename
  let userName = lookup uid $ uidNames $ lines contents
  case userName of
    Just name -> putStrLn $ "user name " ++ show name
    _ -> putStrLn $ "uid " ++ show uid ++ " not found"

uidNames :: [String] -> [(Int, String)]
uidNames [] = []
uidNames ((isPrefixOf "#" -> True) : xs) = uidNames xs
uidNames (x : xs) =
  let vs = split ":" x
   in if smallThen 3 vs
        then uidNames xs
        else
          let uid = read (vs !! 2) :: Int
              useName = head vs
           in (uid, useName) : uidNames xs

smallThen :: (Ord t, Num t) => t -> [a] -> Bool
smallThen ((<= 0) -> True) xs = False
smallThen n [] = True
smallThen n (x : xs) = smallThen (n - 1) xs