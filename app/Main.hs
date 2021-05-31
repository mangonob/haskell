module Main where

import Compiler.Production (Production (body), SymbolSets, firsts, follows, isTerm)
import qualified Compiler.Production as P
import Data.List (intersperse, nub, sort, sortOn)
import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment (getArgs)
import Utils (readLine)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("-h" : _) -> help
    _ -> do
      ps <- getProductions
      putStrLn "FIRST"
      putStrLn $ generateContents ps firsts
      putStrLn "FOLLOW"
      putStrLn $ generateContents ps follows

help :: IO ()
help =
  putStrLn
    "Output FIRST and FOLLOW set (format .csv)\n\
    \\n\
    \    -h:    help"

generateContents :: [Production] -> ([Production] -> SymbolSets) -> String
generateContents ps f =
  let header = concat $ intersperse sep $ "Symbol/Term" : map show terms
      lns = map genLine . sortOn fst $ M.toList $ f ps
   in unlines (header : lns)
  where
    terms = sort . nub . (filter isTerm) . concat $ map body ps
    sep = ","

    genLine (x, xs) =
      concat $ intersperse sep $ show x : (map (\s -> if S.member s xs then "+" else "-") terms)

getProductions :: IO [Production]
getProductions = do
  contents <- getContents
  let lns = lines contents
  return $ (map read lns :: [Production])