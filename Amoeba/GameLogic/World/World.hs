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
type WorldMap = GW.GenericMap Properties

data Game = Game { _world :: World
                 , _rndGen :: StdGen }
  deriving (Show)

instance Eq Game where
    (Game w1 g1) == (Game w2 g2) = (w1 == w2) && (show g1 == show g2)

instance GW.GenericCell Properties where
    empty = emptyProperties
    merge = mergeProperties

emptyWorld = GW.emptyWorld
initialGame seed = Game emptyWorld (mkStdGen seed)
refreshWorldBound = GW.refreshWorldBound

emptyCell = emptyProperties
alterCell = GW.alterCell
deleteCell p w = alterCell w (p, emptyCell)
insertCell p c w = alterCell w (p, c)

worldMap :: L.Lens' World WorldMap
worldMap = L.lens GW.worldMap GW.resetWorldMap

bound :: L.Getter World Bound
bound = L.to GW.worldBound

world :: L.Lens' Game World
world = L.lens _world (\game w -> game { _world = w })

objects :: L.Lens' Game WorldMap
objects = world.worldMap

rndGen :: L.Lens' Game StdGen
rndGen = L.lens _rndGen (\game g -> game { _rndGen = g})


