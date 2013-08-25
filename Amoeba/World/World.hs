{-# LANGUAGE ExistentialQuantification, ExistentialQuantification, NoMonomorphismRestriction, FunctionalDependencies, MultiParamTypeClasses #-} 

module World.World where

import Prelude hiding (Bounded)
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.List as L
import qualified Control.Arrow as Arr (second)
import System.Random

import World.Types
import World.Geometry
import World.Player
import World.Constants
import World.Id

{- Description -}

class Descripted a where
    description :: a -> String

{- WorldMap Mutation -}

class (Descripted a, Id a) => Mutable a where
    mutate :: a -> Point -> WorldMap -> WorldMap

data WorldMapMutation = forall mut . Mutable mut => MkWorldMapMutation mut

instance Id WorldMapMutation where
    getId (MkWorldMapMutation mut) = getId mut

instance Mutable WorldMapMutation where
    mutate (MkWorldMapMutation mut) = mutate mut

instance Descripted WorldMapMutation where
    description (MkWorldMapMutation mut) = description mut

packWorldMapMutation :: Mutable a => a -> WorldMapMutation
packWorldMapMutation = MkWorldMapMutation

immutable = id

{- Active, ActiveItem -}

class (Id i, Descripted i) => Active i mod | i -> mod where
    ownedBy :: i -> Player
    activate :: World -> Point -> i -> WorldMutator -> WorldMutator
    modify :: i -> mod -> i
    
                -- mutate :: mutable -> mutator -> mutable
            -- mutate (MkActiveItem i) (MkActiveItemMutator mut) = mutate mut i

data ActiveItem = forall i mod. Active i mod => MkActiveItem i
type ActiveItems = [ActiveItem]

instance Id ActiveItem where
    getId (MkActiveItem i) = getId i

instance Active ActiveItem where
    ownedBy (MkActiveItem i) = ownedBy i
    activate w p (MkActiveItem i) = activate w p i

instance Descripted ActiveItem where
    description (MkActiveItem i) = "ActiveItem: " ++ description i

instance Eq ActiveItem where
    it1 == it2 = getId it1 == getId it2

packItem :: Active i mod => i -> ActiveItem
packItem = MkActiveItem

packItems :: Active i mod => [i] -> ActiveItems
packItems = map packItem

appendActiveItem :: Active i mod => i -> ActiveItems -> ActiveItems
appendActiveItem item its = packItem item : its

{- World, WorldMutator -}

type WorldMap = Map.Map Point ActiveItems
data World = World { worldMap :: WorldMap
                   , worldLastItemId :: ItemId }

data WorldMutator = WorldMutator { worldMutatorActions :: Actions
                                 , worldMutatorRndGen :: StdGen }

worldMapFromList :: Active i mod => [(Point, [i])] -> WorldMap
worldMapFromList = Map.fromList . map (Arr.second packItems)

createWorld :: Active i mod => ItemId -> [(Point, [i])] -> World
createWorld startId l = World (worldMapFromList l) startId

takeWorldItems :: Point -> World -> ActiveItems
takeWorldItems p (World wm _) = Maybe.fromMaybe [] $ Map.lookup p wm

isEmptyCell :: Point -> World -> Bool
isEmptyCell p w = null $ takeWorldItems p w

inactive :: World -> Point -> WorldMutator -> WorldMutator
inactive _ _ wm = wm

isOnePlayerHere :: Player -> ActiveItems -> Bool
isOnePlayerHere _ [] = False
isOnePlayerHere pl activeIts = all ((pl ==) . ownedBy) activeIts

isNonePlayersHere :: Player -> ActiveItems -> Bool
isNonePlayersHere _ [] = True
isNonePlayersHere pl activeIts = all (not . isOrdinaryPlayer . ownedBy) activeIts

isObstacleItem :: ActiveItems -> Bool
isObstacleItem [] = False
isObstacleItem activeIts = any (isObstaclePlayer . ownedBy) activeIts

getPlayers :: ActiveItems -> Players
getPlayers activeIts = map ownedBy $ filter (isOrdinaryPlayer . ownedBy) activeIts

createWorldMutator :: StdGen -> Actions -> WorldMutator
createWorldMutator = flip WorldMutator

emptyWorldMutator = WorldMutator []

activateWorld :: StdGen -> World -> (World, WorldMutator)
activateWorld g w = let
    emptyWm = emptyWorldMutator g
    wm'' = Map.foldrWithKey (activateItems w) emptyWm (worldMap w)
    in (w, wm'')

activateItems w p activeIts wm = foldr (activate w p) wm activeIts

mutateWolrd :: World -> WorldMutator -> World
mutateWolrd w (WorldMutator acts _) = foldr evalAction w acts


{- Actions -}

data Action = AddSingleActive { actionPoint :: Point
                              , actionItemConstructor :: ItemId -> ActiveItem }
            | AddSingleConflicted { actionPoint :: Point
                                  , actionItemConstructor :: ItemId -> ActiveItem
                                  , actionConflictedPlayers :: Players } 
            | MutateWorldMap { actionPoint :: Point   -- who is requested mutation
                             , actionItem :: ActiveItem -- who is requested mutation
                             , actionWorldMutation :: WorldMapMutation }
            | DeleteActive { actionPoint :: Point
                           , actionItem :: ActiveItem }

type Actions = [Action]

addSingleActiveAction = AddSingleActive
addSingleConflictedAction = AddSingleConflicted
mutateWorldMapAction = MutateWorldMap
deleteActiveAction = DeleteActive

showAction (MutateWorldMap p i mut) = "Mutate world map requested by: " ++ (show . getId $ i)
                                   ++ " \n  from point: " ++ show p
                                   ++ " \n  description: " ++ (show . description $ mut) 
showAction (AddSingleActive p _) = "Add single active at: " ++ show p
showAction (AddSingleConflicted p _ pls) = "Single conflicted active at: " ++ show p ++ " for players " ++ show pls
showAction _ = error "Show action not implemented." -- TODO

getModificatorActions :: ItemId -> Int -> WorldMutator -> Actions
getModificatorActions itemId mId (WorldMutator acts _) = filter (isMutateWorldMapAction itemId mId) acts

isMutateWorldMapAction :: ItemId -> ItemId -> Action -> Bool
isMutateWorldMapAction itemId mId (MutateWorldMap _ i mut) = (mId == getId mut) && (itemId == getId i)
isMutateWorldMapAction _ _ _ = False

updateItem :: ActiveItem -> Point -> WorldMap -> WorldMap
updateItem newIt = Map.alter updateItemF
  where
    updateItemF :: Maybe ActiveItems -> Maybe ActiveItems
    updateItemF Nothing = Nothing
    updateItemF t@(Just its) = 
        case L.find (isIdsEqual newIt) its of
            Nothing -> t -- TODO: log message: can't find item to update it
            Just activeItem -> let
                itsWithoutIt = L.delete activeItem its
                in Just $ newIt : itsWithoutIt

addItem :: Point -> World -> (ItemId -> ActiveItem) -> World
addItem p (World wm lId) itemConstr = let
    itemId = lId + 1
    item = itemConstr itemId
    in World (Map.insertWith (++) p [item] wm) itemId

{- Just test:
mutateItem p iId w@(World wm lId) mut = let
  where
    updateItemF :: Maybe ActiveItems -> Maybe ActiveItems
    updateItemF Nothing = Nothing
    updateItemF t@(Just its) = 
        case L.find (isIdsEqual newIt) its of
            Nothing -> t -- TODO: log message: can't find item to update it
            Just activeItem -> let
                mutatedIt = mutate activeItem mut
                itsWithoutIt = L.delete activeItem its
                in Just $ newIt : itsWithoutIt
  -}   


mutateWorldMap :: Point -> World -> WorldMapMutation -> World
mutateWorldMap p w@(World wm lId) mut = let
    wm' = mutate mut p wm
    in World wm' lId

{-
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

evalAction :: Action -> World -> World
evalAction (AddSingleActive p itemConstr) w = addItem p w itemConstr
evalAction (AddSingleConflicted p itemConstr _) w = addItem p w itemConstr
evalAction (MutateWorldMap p _ mut) w = mutateWorldMap p w mut
evalAction (DeleteActive p item) w = undefined -- deleteItem p w item
