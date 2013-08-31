module World.Geometry where

import Linear

type Radius = Double
type Point = V3 Int

data Bound = Circle { circleCenter :: Point
                    , circleRadius ::  Radius }
--           | Rectangle { rectangleLeftUp :: Point
--                       , rectangleRightDown :: Point }
           | Pointed { pointPosition :: Point }
  deriving (Show, Read, Eq)
type Bounds = [Bound]

class Bounded i where
    bounds :: i -> Point -> Bound

toDoubleV3 :: V3 Int -> V3 Double
toDoubleV3 (V3 x1 x2 x3) = V3 (fromIntegral x1) (fromIntegral x2) (fromIntegral x3)

normIntV3 :: Point -> Double
normIntV3  = norm . toDoubleV3
 
-- Inspired by https://github.com/ocharles/netwire-classics/blob/master/asteroids/Asteroids.hs
intersecting :: Bound -> Bound -> Bool
intersecting (Circle c1 r1) (Circle c2 r2) = normIntV3 (c1 - c2) < (r1 + r2)
intersecting c@(Circle _ _) (Pointed p) = intersecting c (Circle p 0)
intersecting p@(Pointed _) c@(Circle _ _) = intersecting c p
intersecting (Pointed _) (Pointed _) = False

inBounds :: Point -> Bounds -> Bool
inBounds p = any (intersecting (Pointed p)) 

type Direction = Point
type Directions = [Direction]
type Shift = Point -> Point
type Shifts = [Shift]

point :: Int -> Int -> Int -> V3 Int
point = V3
zeroPoint = V3 0 0 0 :: V3 Int

leftUp    = V3 (-1) (-1) 0 :: V3 Int
leftDown  = V3 (-1) 1 0 :: V3 Int
rightUp   = V3 1 (-1) 0 :: V3 Int
rightDown = V3 1 1 0 :: V3 Int
left  = V3 (-1) 0 0 :: V3 Int
right = V3 1 0 0 :: V3 Int
up    = V3 0 (-1) 0 :: V3 Int
down  = V3 0 1 0 :: V3 Int

shiftNone      = (^+^) zero
shiftLeft      = (^+^) left
shiftRight     = (^+^) right
shiftUp        = (^+^) up
shiftDown      = (^+^) down
shiftLeftUp    = (^+^) leftUp
shiftLeftDown  = (^+^) leftDown
shiftRightUp   = (^+^) rightUp
shiftRightDown = (^+^) rightDown

relativeCorners = [leftUp, leftDown, rightUp, rightDown]
relativeSides = [left, right, up, down]

isCornerShift, isSideShift, isSingleShift :: Shift -> Bool
isCornerShift sh = sh zero `elem` relativeCorners
isSideShift sh = sh zero `elem` relativeSides
isSingleShift = isSideShift

subShift1, subShift2 :: Shift -> Shift
subShift1 sh = let (V3 x1 _ x3) = sh zero
               in  (^+^) $ V3 x1 0 x3
subShift2 sh = let (V3 _ x2 x3) = sh zero
               in  (^+^) $ V3 0 x2 x3

direction :: Shift -> Direction
direction sh = sh zeroPoint

ringSquareShifts :: Shifts
ringSquareShifts = [shiftLeft, shiftRight, shiftUp, shiftDown
                   , shiftLeftUp, shiftLeftDown, shiftRightUp, shiftRightDown]

ringSquareFiller, fullSquareFiller :: Point -> [Point]
ringSquareFiller point = map ($ point) ringSquareShifts
fullSquareFiller point = map ($ point) (shiftNone : ringSquareShifts)

nextDirection dir | dir == left = up
                  | dir == up = right
                  | dir == right = down
                  | dir == down = left
                  | otherwise = zero
                  
movePoint :: Point -> Direction -> Point
movePoint = (^+^)