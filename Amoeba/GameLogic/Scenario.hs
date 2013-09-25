module GameLogic.Scenario where

import GameLogic.Geometry
import GameLogic.World
import GameLogic.Properties
import GameLogic.Objects

import Control.Lens

move p (StraightMoving s d) = moveStraight s p d

path p (StraightMoving s d) = scanl advance p (replicate s d)