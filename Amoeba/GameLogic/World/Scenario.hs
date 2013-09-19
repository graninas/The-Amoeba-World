module GameLogic.World.Scenario where

import GameLogic.World.Geometry
import GameLogic.World.World
import GameLogic.World.Properties
import GameLogic.World.Objects

import Control.Lens

objects :: Traversal' Game Properties
objects = world.worldMap.traversed




