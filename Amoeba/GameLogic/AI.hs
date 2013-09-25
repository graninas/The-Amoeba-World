module GameLogic.AI where

import GameLogic.Geometry
import GameLogic.Object

move p (StraightMoving s d) = moveStraight s p d
path p (StraightMoving s d) = scanl advance p (replicate s d)
