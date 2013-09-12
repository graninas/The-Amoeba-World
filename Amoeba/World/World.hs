module World.World where

import qualified Control.Lens as L
import System.Random

import World.GenericWorld

type WorldMap = GW.PointMap Int
type World = GenericWorld WorldMap

data Game = Game { _world :: World
                 , _rndGen :: StdGen }
                 

map :: L.Lens' World WorldMap
map = L.lens GW.worldMap (\w wm -> w { GW.worldMap = wm } )

bound :: L.Getter World Bound
bound = L.to GW.worldBound

-- Tip: use Control.Lens.At for Map-like structures.

world :: L.Lens' Game World
world = L.lens _world (\game w -> game { _world = w })

rndGen :: L.Lens' Game StdGen
rndGen = L.lens _rndGen (\game g -> game { _rndGen = g})

