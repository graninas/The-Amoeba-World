module Amoeba.View.Output.Utils where

-- TODO
scale = 10 :: Int
cellSide = 7 :: Int


-- TODO: replace this specific function by linear.
(+!) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
(-!) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

{-
toSdlRect :: ViewPoint -> Point -> SDL.Rect
toSdlRect (planeX, planeY) point = SDL.Rect x' y' (x' + cellSide) (y' + cellSide)
  where
    x = pointX point
    y = pointY point
    x' = x * scale + planeX
    y' = y * scale + planeY
    
-}