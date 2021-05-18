module RealWorldHaskell.CH03.Point () where

import Data.List

data Point = Point Double Double deriving (Eq, Show)

data Vector = Vector Double Double deriving (Eq, Show)

data Direction = Clockwise | Counterclockwise | Collinear deriving (Eq, Show)

vector :: Point -> Point -> Vector
vector (Point x1 y1) (Point x2 y2) = Vector (x2 - x1) (y2 - y1)

direction :: Point -> Point -> Point -> Direction
direction p1 p2 p3 =
  let (Vector dx1 dy1) = vector p1 p2
      (Vector dx2 dy2) = vector p2 p3
      area = dy2 * dx1 - dx2 * dy1
   in if area > 0
        then Counterclockwise
        else
          if area == 0
            then Collinear
            else Clockwise

directions :: [Point] -> [Direction]
directions (x : y : []) = []
directions (x : y : z : zs) = direction x y z : directions (y : z : zs)
directions _ = error "have no enough point"

fromList :: [Double] -> [Point]
fromList ([]) = []
fromList (x : []) = [Point x 0]
fromList (x : y : xs) = Point x y : fromList xs

-- 计算凸包（Example: fromList [0, 8, 1, 3, 2, 1, 2 ,5, 4, 0, 4, 6, 5, 4, 6, 0, 6, 7, 7, 5, 9, 2, 9, 5]）
graham :: [Point] -> [Point]
graham [] = []
graham xs =
  let (p0@(Point x0 y0) : ys) = sortOn (\(Point x y) -> (x, y)) xs
      (stack, tail) = splitAt 3 $ p0 : sortOn (\(Point x y) -> atan ((y - y0) / (x - x0))) ys
   in reverse $ graham' (reverse stack) tail

graham' :: [Point] -> [Point] -> [Point]
graham' ss [] = ss
graham' (x : y : ys) (z : zs)
  | direction y x z == Counterclockwise = graham' (z : x : y : ys) zs
  | otherwise = graham' (z : y : ys) zs