module World.World where

import qualified Control.Lens as L
import System.Random

import World.GenericWorld


type World = GenericWorld Cell



data Game = Game { _world :: World
                 , _rndGen :: StdGen }

objects.traversed.pos :: Traversal' World Point
objects.traversed.properties :: Traversal' World Spec

objects :: L.Lens' World Cells
objects = L.lens GW.cells GW.alterWorld

world :: L.Lens' Game World
world = L.lens _world (\game w -> game { _world = w })

rndGen :: L.Lens' Game StdGen
rndGen = L.lens _rndGen (\game g -> game { _rndGen = g})

