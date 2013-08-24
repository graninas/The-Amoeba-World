module World.Geometry where

import Linear

type Center = V3 Int
type Radius = V3 Double
type Point = V3 Int

type Position = (Int, Int)

point = V3


shiftNone      = (^+^) $ V3 0 0 0
shiftLeft      = (^+^) $ V3 (-1) 0 0
shiftRight     = (^+^) $ V3 1    0 0
shiftUp        = (^+^) $ V3 0 (-1) 0
shiftDown      = (^+^) $ V3 0 1 0
shiftLeftUp    = (^+^) $ V3 (-1) (-1) 0
shiftLeftDown  = (^+^) $ V3 (-1) 1 0
shiftRightUp   = (^+^) $ V3 1 (-1) 0
shiftRightDown = (^+^) $ V3 1 1 0

ringSquareShifters = [shiftLeft, shiftRight, shiftUp, shiftDown,
                      shiftLeftUp, shiftLeftDown, shiftRightUp, shiftRightDown]

ringSquareFiller, fullSquareFiller :: Point -> [Point]
ringSquareFiller point = map ($ point) ringSquareShifters
fullSquareFiller point = map ($ point) (shiftNone : ringSquareShifters)


