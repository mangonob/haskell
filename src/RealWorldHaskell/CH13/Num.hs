module RealWorldHaskell.CH13.Num where

import Data.List
import Utils

data Op
  = Plus
  | Minus
  | Mul
  | Div
  | Pow
  deriving (Eq)

instance Show Op where
  show Plus = "+"
  show Minus = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "**"

data SymbolicManip a
  = Number a
  | Symbol String
  | BinaryArith Op (SymbolicManip a) (SymbolicManip a)
  | UnaryArith String (SymbolicManip a)
  deriving (Eq)

instance (Num a, Show a) => Show (SymbolicManip a) where
  show = prettyShow

instance Num a => Num (SymbolicManip a) where
  a + b = BinaryArith Plus a b
  a - b = BinaryArith Minus a b
  a * b = BinaryArith Mul a b
  negate a = UnaryArith "-" a
  abs a = UnaryArith "abs" a
  signum _ = noImplemented
  fromInteger i = Number (fromInteger i)

instance (Fractional a) => Fractional (SymbolicManip a) where
  a / b = BinaryArith Div a b
  recip a = BinaryArith Div (Number 1) a
  fromRational r = Number (fromRational r)

instance (Floating a) => Floating (SymbolicManip a) where
  pi = Symbol "pi"
  exp a = UnaryArith "exp" a
  log a = UnaryArith "log" a
  sqrt a = UnaryArith "sqrt" a
  a ** b = BinaryArith Pow a b
  sin a = UnaryArith "sin" a
  cos a = UnaryArith "cos" a
  tan a = UnaryArith "tan" a
  asin a = UnaryArith "asin" a
  acos a = UnaryArith "acos" a
  atan a = UnaryArith "atan" a
  sinh a = UnaryArith "sinh" a
  cosh a = UnaryArith "cosh" a
  tanh a = UnaryArith "tanh" a
  asinh a = UnaryArith "asinh" a
  acosh a = UnaryArith "acosh" a
  atanh a = UnaryArith "atanh" a

prettyShow :: (Show a, Num a) => SymbolicManip a -> String
prettyShow (Number x) = show x
prettyShow (Symbol x) = x
prettyShow (BinaryArith op a b) =
  let pa = simpleParen a
      pb = simpleParen b
   in pa ++ show op ++ pb
prettyShow (UnaryArith "-" a) = "-" ++ show a
prettyShow (UnaryArith op a) = op ++ "(" ++ show a ++ ")"

simpleParen :: (Show a, Num a) => SymbolicManip a -> String
simpleParen x@(BinaryArith _ _ _) = "(" ++ prettyShow x ++ ")"
simpleParen x = prettyShow x

rpnShow :: (Show a, Num a) => SymbolicManip a -> String
rpnShow = join " " . toList
  where
    toList (Number x) = [show x]
    toList (Symbol x) = [x]
    toList (BinaryArith op x y) = toList x ++ toList y ++ [show op]
    toList (UnaryArith op x) = toList x ++ [op]

    join det l = concat (intersperse det l)

simplify :: (Eq a, Num a) => SymbolicManip a -> SymbolicManip a
simplify (BinaryArith op a b) =
  let sa = simplify a
      sb = simplify b
   in case (op, sa, sb) of
        (Mul, Number 1, b) -> b
        (Mul, a, Number 1) -> a
        (Mul, Number 0, b) -> Number 0
        (Mul, a, Number 0) -> Number 0
        (Div, a, Number 1) -> a
        (Plus, a, Number 0) -> a
        (Plus, Number 0, b) -> b
        (Minus, a, Number 0) -> a
        _ -> BinaryArith op sa sb
simplify (UnaryArith op a) = UnaryArith op (simplify a)
simplify x = x

data Units a = Units a (SymbolicManip a) deriving (Eq)

instance (Num a, Show a) => Show (Units a) where
  show (Units x u) = show x ++ " " ++ show u

instance (Eq a, Num a) => Num (Units a) where
  (Units xa ua) + (Units xb ub)
    | ua == ub = Units (xa + xb) ua
    | otherwise = error "Unmatched units"
  (Units xa ua) - (Units xb ub)
    | ua == ub = Units (xa - xb) ua
    | otherwise = error "Unmatched units"
  (Units xa ua) * (Units xb ub) =
    Units (xa * xb) (ua * ub)
  negate (Units xa ua) = Units (negate xa) ua
  abs (Units xa ua) = Units (abs xa) ua
  signum (Units xa ua) = Units (signum xa) (Number 1)
  fromInteger i = Units (fromInteger i) (Number 1)

instance (Eq a, Fractional a) => Fractional (Units a) where
  (Units xa ua) / (Units xb ub) = Units (xa / xb) (ua / ub)
  recip a = 1 / a
  fromRational r = Units (fromRational r) (Number 1)

instance (Eq a, Floating a) => Floating (Units a) where
  pi = (Units pi (Number 1))
  exp _ = error "exp not yet implemented in Units"
  log _ = error "log not yet implemented in Units"
  (Units xa ua) ** (Units xb ub)
    | ub == Number 1 = Units (xa ** xb) (ua ** Number xb)
    | otherwise = error "units for RHS of ** not supported"
  sqrt (Units xa ua) = Units (sqrt xa) (sqrt ua)
  sin (Units xa ua)
    | ua == Symbol "rad" = Units (sin xa) (Number 1)
    | ua == Symbol "deg" = Units (sin (deg2rad xa)) (Number 1)
    | otherwise = error "Units for sin must be deg or rad"
  cos (Units xa ua)
    | ua == Symbol "rad" = Units (cos xa) (Number 1)
    | ua == Symbol "deg" = Units (cos (deg2rad xa)) (Number 1)
    | otherwise = error "Units for cos must be deg or rad"
  tan (Units xa ua)
    | ua == Symbol "rad" = Units (tan xa) (Number 1)
    | ua == Symbol "deg" = Units (tan (deg2rad xa)) (Number 1)
    | otherwise = error "Units for tan must be deg or rad"
  asin (Units xa ua)
    | ua == Number 1 = Units (rad2deg $ asin xa) (Symbol "deg")
    | otherwise = error "Units for asin must be empty"
  acos (Units xa ua)
    | ua == Number 1 = Units (rad2deg $ acos xa) (Symbol "deg")
    | otherwise = error "Units for acos must be empty"
  atan (Units xa ua)
    | ua == Number 1 = Units (rad2deg $ atan xa) (Symbol "deg")
    | otherwise = error "Units for atan must be empty"
  sinh = error "sinh not yet implemented in Units"
  cosh = error "cosh not yet implemented in Units"
  tanh = error "tanh not yet implemented in Units"
  asinh = error "asinh not yet implemented in Units"
  acosh = error "acosh not yet implemented in Units"
  atanh = error "atanh not yet implemented in Units"

{- A simple function that takes a number and a String and returns an
appropriate Units type to represent the number and its unit of measure -}
units :: (Num z) => z -> String -> Units z
units a b = Units a (Symbol b)

{- Extract the number only out of a Units type -}
dropUnits :: (Num z) => Units z -> z
dropUnits (Units x _) = x

{- Utilities for the Unit implementation -}
deg2rad :: Floating a => a -> a
deg2rad x = 2 * pi * x / 360

rad2deg :: Floating a => a -> a
rad2deg x = 360 * x / (2 * pi)