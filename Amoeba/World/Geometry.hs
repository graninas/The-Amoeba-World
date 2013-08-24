module World.Geometry where

import Linear

type Radius = V3 Double
type Point = V3 Int

type Position = (Int, Int)

data Bound = BoundCircle { circleCenter :: Point
                         , circleRadius ::  Radius }
           | BoundRectangle { rectangleLeftUp :: Point
                            , rectangleRightDown :: Point }
           | BoundPoint { pointPosition :: Point }
  deriving (Show, Read, Eq)


class Bounded i where
    bounds :: i -> Point -> Bound


type Direction = Point
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
