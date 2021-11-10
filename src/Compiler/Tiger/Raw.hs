module Compiler.Tiger.Raw where

import Compiler.Tiger.AbSyn
import Control.Monad (join)
import Data.List (intersperse, isSuffixOf)

class Raw r where
  raw :: r -> String

instance Raw Expr where
  raw (VarExpr v) = raw v
  raw (NilExpr _) = "nil"
  raw (SeqExpr eprs _) =
    let seqs = joined "; \n" $ map raw eprs
     in if length eprs == 1
          then raw (head eprs)
          else combine' (\c -> ["(", identIfNeeded c, ")"]) seqs
  raw (IntExpr i _) = show i
  raw (StringExpr s _) = show s
  raw (UMinus e _) = '-' : raw e
  raw (Call n args _) = n ++ "(" ++ joined ", " (map raw args) ++ ")"
  raw (OpExpr e1 op e2 _) = raw e1 ++ ' ' : raw op ++ ' ' : raw e2
  raw (RecordsExpr t rcds _) = t ++ "{" ++ joined ", " (map raw rcds) ++ "}"
  raw (ArrayExpr t size v _) = t ++ "[" ++ raw size ++ "] of " ++ raw v
  raw (AssignExpr n v _) = raw n ++ " := " ++ raw v
  raw (IFExpr p suc Nothing _) =
    let headStr = combine (\c -> ["if", identIfNeeded c, "then"]) (raw p)
     in combine (\c -> [headStr, identIfNeeded c]) (raw suc)
  raw (IFExpr p suc (Just fai) _) =
    let headStr = combine (\c -> ["if", identIfNeeded c, "then"]) (raw p)
        thenStr = combine (\c -> [headStr, identIfNeeded c, "else"]) (raw suc)
     in combine (\c -> [thenStr, identIfNeeded c]) (raw fai)
  raw (WhileExpr p body _) = "while " ++ raw p ++ " do " ++ raw body
  raw (ForExpr v from to body escape _) = "for " ++ escaped escape v ++ " := " ++ raw from ++ " to " ++ raw to ++ " do " ++ raw body
  raw (BreakExpr _) = "break"
  raw (LetExpr decs body _) =
    let headStr = combine (\c -> ["let", identIfNeeded c, "in"]) (unlines (map raw decs))
     in combine (\c -> [headStr, identIfNeeded c, "end"]) (raw body)

escaped :: Bool -> String -> String
escaped True s = "<escaped> " ++ s
escaped False s = s

joined :: String -> [String] -> String
joined sep xs = join $ intersperse sep xs

identation :: String -> String
identation = unlines . map ("|   " ++) . lines

isMultiLine :: String -> Bool
isMultiLine = (> 1) . length . lines

unlines' :: [String] -> String
unlines' = unlines . map lined

lined :: [Char] -> [Char]
lined s = if "\n" `isSuffixOf` s then init s else s

spaced' :: [String] -> String
spaced' = joined " " . map lined

concat' :: [String] -> String
concat' = concatMap lined

width :: String -> Int
width = (\x -> if null x then 0 else maximum x) . map length . lines

shouldChangeLine :: [Char] -> Bool
shouldChangeLine s = width s > 42 || isMultiLine s

identIfNeeded :: [Char] -> String
identIfNeeded s = if shouldChangeLine s then identation s else s

combine :: (String -> [String]) -> String -> String
combine cts_f s =
  let f = if shouldChangeLine s then unlines' else spaced'
      cts = cts_f s
   in f cts

combine' :: (String -> [String]) -> String -> String
combine' cts_f s =
  let f = if shouldChangeLine s then unlines' else concat'
      cts = cts_f s
   in f cts

instance Raw Field where
  raw (Field n v _) = n ++ " = " ++ raw v

instance Raw Dec where
  raw (TypeDec n t _) = "type " ++ n ++ " = " ++ raw t
  raw (VarDec n v Nothing escape _) = "var " ++ escaped escape n ++ " := " ++ raw v
  raw (VarDec n v (Just t) escape _) = "var " ++ escaped escape n ++ ": " ++ t ++ " := " ++ raw v
  raw (FuncDec n args Nothing bd _) =
    let argsStr = join (intersperse ", " (map raw args))
        headStr = combine' (\c -> ["function " ++ n ++ "(", c, ") ="]) argsStr
     in combine (\c -> [headStr, c]) (raw bd)
  raw (FuncDec n args (Just rt) bd _) =
    let argsStr = join (intersperse ", " (map raw args))
        headStr = combine' (\c -> ["function " ++ n ++ "(", c, ") : " ++ rt ++ " ="]) argsStr
     in combine (\c -> [headStr, c]) (raw bd)

instance Raw Type where
  raw (SimpleType s _) = s
  raw (Records rds _) = "{" ++ join (intersperse ", " (map raw rds)) ++ "}"
  raw (Array s _) = "array of" ++ s

instance Raw Record where
  raw (Record s t escape _) = escaped escape s ++ ": " ++ t

instance Raw Operator where
  raw PlusOp = "+"
  raw MinusOp = "-"
  raw TimesOp = "*"
  raw DivideOp = "/"
  raw EqOp = "="
  raw NeqOp = "<>"
  raw LtOp = "<"
  raw LeOp = "<="
  raw GtOp = ">"
  raw GeOp = ">="

instance Raw Var where
  raw (SimpleVar s _) = s
  raw (FieldVar v s _) = raw v ++ "." ++ s
  raw (IndexedVar v e _) = raw v ++ "[" ++ raw e ++ "]"