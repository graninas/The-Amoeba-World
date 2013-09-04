module World.Geometry where

-- Inspired by https://github.com/ocharles/netwire-classics/blob/master/asteroids/Asteroids.hs
import qualified Linear as L

class Bounded i where
    bounds :: i -> Point -> Bound

data Bound = Circled { circleCenter :: Point
                    , circleRadius ::  Radius }
           | Rectangled { rectangleLeftUp :: Point
                       , rectangleRightDown :: Point }
           | Pointed { pointPosition :: Point }
  deriving (Show, Read, Eq)
type Bounds = [Bound]

type Radius = Double
type Point = L.V3 Int    
type Direction = Point
type Directions = [Direction]
type Shift = Point -> Point
type Shifts = [Shift]

toDoubleV3 :: L.V3 Int -> L.V3 Double
toDoubleV3 (L.V3 x1 x2 x3) = L.V3 (fromIntegral x1) (fromIntegral x2) (fromIntegral x3)

normIntV3 :: Point -> Double
normIntV3  = L.norm . toDoubleV3

inSegment :: (Int, Int) -> Int -> Bool
inSegment (x, y) z = (z <= max x y) && (z >= min x y)

minMaxPoint (x1, y1) (x2, y2) = point (min x1 x2) (max y1 y2) 0
maxMinPoint (x1, y1) (x2, y2) = point (max x1 x2) (min y1 y2) 0

rectBound :: (Int, Int) -> (Int, Int) -> Bound
rectBound c1 c2 = Rectangled (minMaxPoint c1 c2) (maxMinPoint c1 c2)
pointBound :: (Int, Int) -> Bound
pointBound (x1, y1) = Pointed (point x1 y1 0)
circleBound :: Point -> Radius -> Bound
circleBound = Circled

intersecting :: Bound -> Bound -> Bool
intersecting (Circled c1 r1) (Circled c2 r2) = normIntV3 (c1 - c2) < (r1 + r2)
intersecting c@(Circled _ _) (Pointed p) = intersecting c (Circled p 0)
intersecting p@(Pointed _) c@(Circled _ _) = intersecting c p
intersecting (Pointed _) (Pointed _) = False
intersecting (Rectangled lu@(L.V3 x1 x2 _) rd@(L.V3 y1 y2 _)) (Pointed p@(L.V3 p1 p2 _)) =
    inSegment (x1, y1) p1 && inSegment (x2, y2) p2
intersecting p@(Pointed {}) r@(Rectangled {}) = intersecting r p
intersecting b1 b2 = error $ "Intersecting not implemented for " ++ show b1 ++ " and " ++ show b2


inBounds :: Point -> Bounds -> Bool
inBounds p = any (intersecting (Pointed p)) 

point :: Int -> Int -> Int -> L.V3 Int
point = L.V3
zeroPoint = L.V3 0 0 0 :: L.V3 Int

leftUp    = L.V3 (-1) (-1) 0 :: L.V3 Int
leftDown  = L.V3 (-1) 1 0 :: L.V3 Int
rightUp   = L.V3 1 (-1) 0 :: L.V3 Int
rightDown = L.V3 1 1 0 :: L.V3 Int
left  = L.V3 (-1) 0 0 :: L.V3 Int
right = L.V3 1 0 0 :: L.V3 Int
up    = L.V3 0 (-1) 0 :: L.V3 Int
down  = L.V3 0 1 0 :: L.V3 Int

shiftNone      = (L.^+^) L.zero
shiftLeft      = (L.^+^) left
shiftRight     = (L.^+^) right
shiftUp        = (L.^+^) up
shiftDown      = (L.^+^) down
shiftLeftUp    = (L.^+^) leftUp
shiftLeftDown  = (L.^+^) leftDown
shiftRightUp   = (L.^+^) rightUp
shiftRightDown = (L.^+^) rightDown

relativeCorners = [leftUp, leftDown, rightUp, rightDown]
relativeSides = [left, right, up, down]

isCornerShift, isSideShift, isSingleShift :: Shift -> Bool
isCornerShift sh = sh L.zero `elem` relativeCorners
isSideShift sh = sh L.zero `elem` relativeSides
isSingleShift = isSideShift

subShift1, subShift2 :: Shift -> Shift
subShift1 sh = let (L.V3 x1 _ x3) = sh L.zero
               in  (L.^+^) $ L.V3 x1 0 x3
subShift2 sh = let (L.V3 _ x2 x3) = sh L.zero
               in  (L.^+^) $ L.V3 0 x2 x3

direction :: Shift -> Direction
direction sh = sh zeroPoint

subDirection1 = direction . subShift1
subDirection2 = direction . subShift2

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
                  | otherwise = L.zero
                  
movePoint :: Point -> Direction -> Point
movePoint = (L.^+^)

literateDirection dir | dir == left = "Left"
                      | dir == up = "Up"
                      | dir == right = "Right"
                      | dir == down = "Down"
                      | dir == leftUp = "Left Up"
                      | dir == leftDown = "Left Down"
                      | dir == rightUp = "Right Up"
                      | dir == rightDown = "Right Down"
                      | otherwise = "<Unknown direction>"