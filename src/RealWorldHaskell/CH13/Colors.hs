module RealWorldHaskell.CH13.Colors where

data CustomColor = CustomColor
  { red :: Int,
    green :: Int,
    blue :: Int
  }
  deriving (Eq, Show, Read)

data FuncRec = FuncRec
  { name :: String,
    colorCalc :: Int -> (CustomColor, Int)
  }

plus5Func :: Num b => a -> b -> (a, b)
plus5Func color x = (color, x + 5)

purple :: CustomColor
purple = CustomColor 0xff 0 0xff

plus5 :: FuncRec
plus5 = FuncRec {name = "plus5", colorCalc = plus5Func purple}

always0 = FuncRec {name = "always0", colorCalc = \x -> (purple, 0)}