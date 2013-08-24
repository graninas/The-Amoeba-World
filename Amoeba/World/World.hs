{-# LANGUAGE ExistentialQuantification, NoMonomorphismRestriction #-}

module World.World where

import Prelude hiding (Bounded)
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import System.Random

import World.Geometry
import World.Player

class Active i where
  --activate :: item -> item position -> previous mutator -> previous word -> new mutator
    ownedBy :: i -> Player
    activate :: i -> Point -> WorldMutator -> World -> WorldMutator


data World = World { worldMap :: WorldMap }


data Action = forall i. Active i => AddSingleActive Point i
            | forall i. Active i => AddSingleConflicted Point Player i
            | forall i. Active i => DeleteActive Point i
  
type Actions = [Action]

data WorldMutator = WorldMutator { worldMutatorActions :: Actions
                                 , worldMutatorRndGen :: StdGen }

data Items = forall i. Active i => Items [i]
                                 | NoItems
data WorldMap = WorldMap (Map.Map Point Items)

noItems :: Items
noItems = NoItems

makeItems :: forall i. Active i => [i] -> Items
makeItems = Items

worldMapFromList :: [(Point, Items)] -> WorldMap
worldMapFromList l = WorldMap (Map.fromList l)

inactive :: Point -> WorldMutator -> World -> WorldMutator
inactive _ wm _ = wm


takeWorldItems :: Point -> World -> Items
takeWorldItems p (World (WorldMap wolrdMap)) = Maybe.fromMaybe noItems $ Map.lookup p wolrdMap

isOnePlayerHere :: Player -> Items -> Bool
isOnePlayerHere _ NoItems = False
isOnePlayerHere pl (Items actives) = all ((pl ==) . ownedBy) actives

isObstacle :: Items -> Bool
isObstacle NoItems = False
isObstacle (Items actives) = any ( (`elem` obstaclePlayers) . ownedBy) actives

isEmptyCell :: Point -> World -> Bool
isEmptyCell p w = case takeWorldItems p w of
    NoItems -> False
    _ -> True

emptyCellChecker p w =
    if isEmptyCell p w
    then Right ()
    else Left "Cell not empty"

createWorldMutator :: StdGen -> Actions -> WorldMutator
createWorldMutator = flip WorldMutator

addSingleActive :: Active i => Point -> i -> Action
addSingleActive = AddSingleActive

addSingleConflicted :: Active i => Point -> Player -> i -> Action
addSingleConflicted = AddSingleConflicted