module GameLogic.World.Scenario where

import GameLogic.World.Geometry
import GameLogic.World.World
import GameLogic.World.Properties
import GameLogic.World.Objects

import Control.Lens

move p (StraightMoving s d) = moveStraight s p d

path p (StraightMoving s d) = scanl advance p (replicate s d)