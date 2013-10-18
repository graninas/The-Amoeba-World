module GameLogic.World where

import qualified Control.Lens as L

import qualified GameLogic.GenericWorld as GW
import GameLogic.Object as O
import GameLogic.ObjectCell
import GameLogic.Geometry

type World = GW.CelledWorld Object
type WorldMap = GW.GenericMap Object


emptyCell = O.empty
alterCell = GW.alterCell
deleteCell p w = alterCell w p emptyCell
insertCell p c w = alterCell w p c

emptyWorld = GW.emptyWorld
refreshWorldBound = GW.refreshWorldBound

worldMap :: L.Lens' World WorldMap
worldMap = L.lens GW.worldMap GW.resetWorldMap

bound :: L.Getter World Bound
bound = L.to GW.worldBound


