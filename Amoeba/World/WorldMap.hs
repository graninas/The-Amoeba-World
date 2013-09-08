module World.WorldMap where

import World.Geometry

import qualified Data.Map as Map

-- TODO: provide clean module interface

data WorldMap i = WorldMap { wmMap :: Map.Map Point i
                           , wmBound :: Bound }
                           
worldMapFromList :: [(Point, i)] -> WorldMap i
worldMapFromList list = WorldMap wm b
  where
    wm = Map.fromList list
    b = occupiedArea (map fst list)