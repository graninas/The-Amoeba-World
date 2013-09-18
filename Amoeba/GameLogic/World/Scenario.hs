module GameLogic.World.Scenario where

import GameLogic.World.Geometry
import GameLogic.World.World
import GameLogic.World.Properties
import GameLogic.World.Objects

import qualified Control.Lens as L

objects :: Traversal' Game Properties
objects = game.world.worldMap.traversed




