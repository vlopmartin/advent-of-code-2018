module Rect where

import Data.List.Split (splitOn)
import Control.Monad ((>=>))

data Point = Point Int Int deriving Show
data Rect = Rect Point Point deriving Show

le :: Point -> Point -> Bool
le (Point x y) (Point x' y') = x <= x' && y <= y'

ge :: Point -> Point -> Bool
ge (Point x y) (Point x' y') = x >= x' && y >= y'

isIn :: Rect -> Point -> Bool
isIn (Rect p1 p2) p = p `ge` p1 && p `le` p2

isAbove :: Rect -> Rect -> Bool
isAbove (Rect _ (Point _ y)) (Rect (Point _ y') _) = y < y'

isLeft :: Rect -> Rect -> Bool
isLeft (Rect _ (Point x _)) (Rect (Point x' _) _) = x < x'

overlaps :: Rect -> Rect -> Bool
overlaps r1 r2 = not (isAbove r1 r2 || isAbove r2 r1 || isLeft r1 r2 || isLeft r2 r1)

toRect :: String -> Rect
toRect = listToRect . numbers
    where listToRect (a:b:c:d:_) = Rect (Point a b) (Point (a+c-1) (b+d-1))

numbers :: String -> [Int]
numbers = map read . splitAll . tail . dropWhile (/= '@')
    where splitAll = splitOn "," >=> splitOn ": " >=> splitOn "x"
