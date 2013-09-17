module GameLogic.World.World where

import qualified Control.Lens as L
import Data.Maybe
import Data.Monoid
import System.Random

import qualified GameLogic.World.GenericWorld as GW
import GameLogic.World.Geometry
import GameLogic.World.Properties
import GameLogic.World.Objects
import GameLogic.World.Player

type World = GW.CelledWorld Properties
type PropertiesMap = GW.GenericMap Properties

data Game = Game { _world :: World
                 , _rndGen :: StdGen }

instance GW.GenericCell Properties where
    empty = emptyProperties
    merge = mergeProperties

worldMap :: L.Lens' World PropertiesMap
worldMap = L.lens GW.worldMap GW.resetWorldMap

bound :: L.Getter World Bound
bound = L.to GW.worldBound

world :: L.Lens' Game World
world = L.lens _world (\game w -> game { _world = w })

rndGen :: L.Lens' Game StdGen
rndGen = L.lens _rndGen (\game g -> game { _rndGen = g})

initialGame seed = Game GW.emptyWorld (mkStdGen seed)

