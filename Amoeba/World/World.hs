{-# LANGUAGE ExistentialQuantification, NoMonomorphismRestriction #-}

module World.World where

import Prelude hiding (Bounded)
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import System.Random

import World.Geometry
import World.Player

type ItemId = Int

class Id i where
    getId :: i -> ItemId

class Id i => Active i where
    ownedBy :: i -> Player
    activate :: World -> Point -> i -> WorldMutator -> WorldMutator

data World = World { worldMap :: WorldMap }


data Action = forall i. Active i => AddSingleActive { actionPoint :: Point
                                                    , actionItemConstructor :: ItemId -> i }
            | forall i. Active i => AddSingleConflicted { actionPoint :: Point
                                                        , actionItemConstructor :: ItemId -> i
                                                        , actionPlayer :: Player } 
            | forall i. Active i => Modify { actionPoint :: Point
                                           , actionItem :: i
                                           , actionModificatorId :: Int
                                           , actionModificator :: WorldMap -> WorldMap }
            | forall i. Active i => DeleteActive { actionPoint :: Point
                                                 , actionItem :: i }

type Actions = [Action]

data WorldMutator = WorldMutator { worldMutatorActions :: Actions
                                 , worldMutatorRndGen :: StdGen }

data Items = forall i. Active i => Items [i]
                                 | NoItems
data WorldMap = WorldMap (Map.Map Point Items)

invalidId = -1

instance Id Items where
  getId _ = invalidId
  
instance Active Items where
  ownedBy _ = dummyPlayer
  activate _ _ NoItems wm = wm
  activate w p (Items its) wm = foldr (activate w p) wm its

noItems :: Items
noItems = NoItems

makeItems :: forall i. Active i => [i] -> Items
makeItems = Items

worldMapFromList :: [(Point, Items)] -> WorldMap
worldMapFromList l = WorldMap (Map.fromList l)

worldFromList :: [(Point, Items)] -> World
worldFromList = World . worldMapFromList

inactive :: World -> Point -> WorldMutator -> WorldMutator
inactive _ _ wm = wm


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

emptyWorldMutator = WorldMutator []

addSingleActive :: Active i => Point -> (ItemId -> i) -> Action
addSingleActive = AddSingleActive

addSingleConflicted :: Active i => Point -> (ItemId -> i) -> Player -> Action
addSingleConflicted = AddSingleConflicted

getModificatorActions :: ItemId -> Int -> WorldMutator -> Actions
getModificatorActions itemId mId (WorldMutator acts _) = filter (isMutatorAction itemId mId) acts

isMutatorAction :: ItemId -> Int -> Action -> Bool
isMutatorAction itemId mId (Modify _ i mId' _) = (mId == mId') && (itemId == getId i)
isMutatorAction _ _ _ = False

activateWorld :: StdGen -> World -> (World, WorldMutator) -- TODO
activateWorld g w@(World (WorldMap worldMap)) = let
    emptyWm = emptyWorldMutator g
    wm = Map.foldrWithKey (activate w) emptyWm worldMap 
    in (w, wm)
    