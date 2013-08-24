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


point = V3

type Shift = Point -> Point
type Shifts = [Shift]

shiftNone      = (^+^) $ V3 0 0 0
shiftLeft      = (^+^) $ V3 (-1) 0 0
shiftRight     = (^+^) $ V3 1    0 0
shiftUp        = (^+^) $ V3 0 (-1) 0
shiftDown      = (^+^) $ V3 0 1 0
shiftLeftUp    = (^+^) $ V3 (-1) (-1) 0
shiftLeftDown  = (^+^) $ V3 (-1) 1 0
shiftRightUp   = (^+^) $ V3 1 (-1) 0
shiftRightDown = (^+^) $ V3 1 1 0

ringSquareShifts :: Shifts
ringSquareShifts = [shiftLeft, shiftRight, shiftUp, shiftDown
                   , shiftLeftUp, shiftLeftDown, shiftRightUp, shiftRightDown]

ringSquareFiller, fullSquareFiller :: Point -> [Point]
ringSquareFiller point = map ($ point) ringSquareShifts
fullSquareFiller point = map ($ point) (shiftNone : ringSquareShifts)


