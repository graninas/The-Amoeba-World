{-# LANGUAGE ExistentialQuantification, NoMonomorphismRestriction #-}

module World.World where

import Prelude hiding (Bounded)
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.List as L
import System.Random

import World.Types
import World.Geometry
import World.Player
import World.Constants

class Id i where
    getId :: i -> ItemId

class Id i => Active i where
    ownedBy :: i -> Player
    activate :: World -> Point -> i -> WorldMutator -> WorldMutator

type WorldMap = Map.Map Point Items
data World = World { worldMap :: WorldMap
                   , worldLastItemId :: ItemId }

data Action = forall i. Active i => AddSingleActive { actionPoint :: Point
                                                    , actionItemConstructor :: ItemId -> i }
            | forall i. Active i => AddSingleConflicted { actionPoint :: Point
                                                        , actionItemConstructor :: ItemId -> i
                                                        , actionConflictedPlayers :: Players } 
            | forall i. Active i => ModifyItem { actionPoint :: Point
                                               , actionItem :: i
                                               , actionModificatorId :: Int
                                               , actionModificator :: i -> i }
            | forall i. Active i => DeleteActive { actionPoint :: Point
                                                 , actionItem :: i }

type Actions = [Action]

showAction (ModifyItem p i modId _) = "Modify item: " ++ (show . getId $ i)
                                   ++ " Point: " ++ show p
                                   ++ " " ++ (show . ownedBy $ i)
                                   ++ " ModId: " ++ show modId
showAction (AddSingleActive p _) = "Add single active at: " ++ show p
showAction (AddSingleConflicted p _ pls) = "Single conflicted active at: " ++ show p ++ " for players " ++ show pls
showAction _ = error "Show action not implemented." -- TODO

data WorldMutator = WorldMutator { worldMutatorActions :: Actions
                                 , worldMutatorRndGen :: StdGen }

data Items = forall i. Active i => Items [i]
                                 | NoItems

instance Id Items where
  getId _ = invalidId

isIdsEqual :: Id i => i -> i -> Bool
isIdsEqual item1 item2 = getId item1 == getId item2

instance Active Items where
  ownedBy _ = dummyPlayer
  activate _ _ NoItems wm = wm
  activate w p (Items its) wm = foldr (activate w p) wm its

noItems :: Items
noItems = NoItems

--makeItems :: forall i. Active i => [i] -> Items
makeItems = Items

{-
addActive item NoItems = makeItems [item]
addActive item (Items its) = Items $ item : its
-}

worldMapFromList :: [(Point, Items)] -> WorldMap
worldMapFromList = Map.fromList

createWorld :: ItemId -> [(Point, Items)] -> World
createWorld startId l = World (worldMapFromList l) startId

inactive :: World -> Point -> WorldMutator -> WorldMutator
inactive _ _ wm = wm

takeWorldItems :: Point -> World -> Items
takeWorldItems p (World wm _) = Maybe.fromMaybe noItems $ Map.lookup p wm

isOnePlayerHere :: Player -> Items -> Bool
isOnePlayerHere _ NoItems = False
isOnePlayerHere pl (Items actives) = all ((pl ==) . ownedBy) actives

isObstacleItem :: Items -> Bool
isObstacleItem NoItems = False
isObstacleItem (Items actives) = any (isObstaclePlayer . ownedBy) actives

isEmptyCell :: Point -> World -> Bool
isEmptyCell p w = case takeWorldItems p w of
    NoItems -> True
    Items [] -> True
    _ -> False

getPlayers :: Items -> Players
getPlayers NoItems = []
getPlayers (Items its) = map ownedBy $ filter (isOrdinaryPlayer . ownedBy) its

emptyCellChecker p w =
    if isEmptyCell p w
    then Right ()
    else Left "Cell not empty"

createWorldMutator :: StdGen -> Actions -> WorldMutator
createWorldMutator = flip WorldMutator

emptyWorldMutator = WorldMutator []

--addSingleActiveAction :: forall i. Active i => Point -> (ItemId -> i) -> Action
addSingleActiveAction = AddSingleActive

--addSingleConflictedAction :: forall i. Active i => Point -> (ItemId -> i) -> Players -> Action
addSingleConflictedAction = AddSingleConflicted

--modifyItemAction :: forall i. Active i => Point -> i -> Int -> (i -> i) -> Action
modifyItemAction = ModifyItem

getModificatorActions :: ItemId -> Int -> WorldMutator -> Actions
getModificatorActions itemId mId (WorldMutator acts _) = filter (isMutatorAction itemId mId) acts

isMutatorAction :: ItemId -> Int -> Action -> Bool
isMutatorAction itemId mId (ModifyItem _ i mId' _) = (mId == mId') && (itemId == getId i)
isMutatorAction _ _ _ = False

activateWorld :: StdGen -> World -> (World, WorldMutator)
activateWorld g w = let
    emptyWm = emptyWorldMutator g
    wm = Map.foldrWithKey (activate w) emptyWm (worldMap w) 
    in (w, wm)

{-
--addItem :: forall i. Active i => Point -> World -> (ItemId -> i) -> World
addItem p (World wm lId) itemConstr = let
    itemId = lId + 1
    item = itemConstr itemId
    addItemF :: Maybe Items -> Maybe Items
    addItemF Nothing = Just (Items [item])
    addItemF mbIts = fmap (addActive item) mbIts
    in World (Map.alter addItemF p wm) itemId

--modifyItem :: forall i. Active i => Point -> World -> i -> (i -> i) -> World -- TODO: (i -> i) can be (Maybe i -> Maybe i) for Map.alter
modifyItem p (World wm lId) item mod = let
    modifyItemF :: Maybe Items -> Maybe Items
    modifyItemF Nothing = Nothing
    modifyItemF (Just NoItems) = Nothing
    modifyItemF t@(Just (Items its)) = 
        case L.find (isIdsEqual item) its of
            Nothing -> t -- TODO: log message: can't find item to modify it
            Just it -> let
                itemId = getId item
                itsWithoutIt = L.delete it its
                modifiedIt = mod it
                newIts = modifiedIt : itsWithoutIt 
                in Just . Items $ newIts
    in World (Map.alter modifyItemF p wm) lId

--deleteItem :: forall i. Active i => Point -> World -> i -> World 
deleteItem p (World wm lId) item = let
    deleteItemF :: Maybe Items -> Maybe Items
    deleteItemF Nothing = Nothing
    deleteItemF (Just NoItems) = Nothing
    deleteItemF t@(Just (Items its)) = 
        case L.find (isIdsEqual item) its of
            Nothing -> t -- TODO: log message: can't find item to delete it
            Just it -> let
                itemId = getId item
                itsWithoutIt = L.delete it its
                in Just . Items $ itsWithoutIt
    in World (Map.alter deleteItemF p wm) lId
-}

mutateWolrd :: World -> WorldMutator -> World
mutateWolrd w (WorldMutator acts _) = foldr evalAction w acts

evalAction :: Action -> World -> World
evalAction (AddSingleActive p itemConstr) w = undefined -- addItem p w itemConstr
evalAction (AddSingleConflicted p itemConstr _) w = undefined -- addItem p w itemConstr
evalAction (ModifyItem p item _ mod) w = undefined -- modifyItem p w item mod
evalAction (DeleteActive p item) w = undefined -- deleteItem p w item
