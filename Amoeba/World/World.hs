{-# LANGUAGE ExistentialQuantification, ExistentialQuantification, NoMonomorphismRestriction, FunctionalDependencies, MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-} 

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

{- Active, ActiveItem -}

class (Id i, Descripted i) => Active i where
    ownedBy :: i -> Player
    activate :: Point  -> i -> World -> (World, Annotations)
    name :: i -> String

data ActiveItem = forall i. Active i => MkActiveItem i
type ActiveItems = [ActiveItem]

instance Id ActiveItem where
    getId (MkActiveItem i) = getId i

instance Active ActiveItem where
    ownedBy (MkActiveItem i) = ownedBy i
    activate p (MkActiveItem i) = activate p i
    name (MkActiveItem i) = name i

instance Descripted ActiveItem where
    description (MkActiveItem i) = description i

instance Eq ActiveItem where
    ai1 == ai2 = getId ai1 == getId ai2

packItem :: Active i => i -> ActiveItem
packItem = MkActiveItem

packItems :: Active i => [i] -> ActiveItems
packItems = map packItem

appendActiveItem :: Active i => i -> ActiveItems -> ActiveItems
appendActiveItem item its = packItem item : its

packObject :: Active i => (Point, i) -> (Point, ActiveItem)
packObject (p, i) = (p, packItem i)

(|>|) :: Active i => [(Point, i)] -> [(Point, ActiveItem)] -> [(Point, ActiveItem)]
objects |>| items = map packObject objects ++ items

(|>||) :: (Active i1, Active i2) => [(Point, i1)] -> [(Point, i2)] -> [(Point, ActiveItem)]
objects1 |>|| objects2 = objects1 |>| (objects2 |>| [])

infixr 5 |>|
infixr 5 |>||
{- World -}

type WorldMap = Map.Map Point ActiveItems
data World = World { worldMap :: WorldMap
                   , worldLastItemId :: ItemId
                   , worldStdGen :: StdGen }
                   
data Annotation = Annotation { annotationMessage :: String }
type Annotations = [Annotation]

itemToList i = [i]
worldMapFromList :: [(Point, ActiveItem)] -> WorldMap
worldMapFromList = Map.fromList . map (Arr.second itemToList)

newWorld :: WorldMap -> ItemId -> StdGen -> World
newWorld = World

stepWorld :: World -> (World, Annotations)
stepWorld world@(World wm _ _) = Map.foldrWithKey activateItems (world, []) wm

activateItem :: Point -> ActiveItem -> (World, Annotations) -> (World, Annotations)
activateItem p i (w, an) = let (w', an') = activate p i w
                           in (w', an ++ an')

activateItems :: Point -> ActiveItems -> (World, Annotations) -> (World, Annotations)
activateItems p items activationData = foldr (activateItem p) activationData items

inactive :: Point -> i -> World -> (World, Annotations)
inactive _ _ w = (w, [])

annotation :: String -> Annotation
annotation = Annotation

showPointAndPlayer :: Point -> Player -> String
showPointAndPlayer p pl = "[" ++ show p ++ ", " ++ show pl ++ "]"

showPoint :: Point -> String
showPoint p = "[" ++ show p ++ "]"

activationAnnotation p i = annotation $ showPointAndPlayer p (ownedBy i) ++ " " ++ name i ++ ": activated"

{- World operations -}

takeWorldItems :: Point -> World -> ActiveItems
takeWorldItems p (World wm _ _) = Maybe.fromMaybe [] $ Map.lookup p wm

isEmptyCell :: Point -> World -> Bool
isEmptyCell p w = null $ takeWorldItems p w

getPlayers :: ActiveItems -> Players
getPlayers activeIts = map ownedBy $ filter (isOrdinaryPlayer . ownedBy) activeIts

itemsCount :: World -> Int
itemsCount (World wm _ _) = Map.fold f 0 wm
  where
    f items n = n + length items

{- WorldMap Updater -}

class WorldMapUpdater a where
    updateFunc :: a -> Maybe ActiveItems -> Maybe ActiveItems
    itemPoint :: a -> Point

--                    Incoming items   Existing items
type ActiveItemsMerge = ActiveItems -> ActiveItems -> ActiveItems

data WorldMapFunction = WorldMapFunction 
                        { worldMapFunctionP :: Point 
                        , worldMapFunctionF :: Maybe ActiveItems -> Maybe ActiveItems }

type WorldMapFunctions = [WorldMapFunction]

instance WorldMapUpdater WorldMapFunction where
    updateFunc = worldMapFunctionF
    itemPoint = worldMapFunctionP

worldMapFunction :: Point -> ActiveItem -> ActiveItemsMerge -> WorldMapFunction
worldMapFunction p i mergeF = let wmFunc = Just . maybe [i] (mergeF [i])
                              in WorldMapFunction p wmFunc

addItemFunc :: Active i => (Point, i) -> WorldMapFunction
addItemFunc (p, i) = worldMapFunction p (packItem i) simpleMerge

addItemsFunc :: Active i => [(Point, i)] -> WorldMapFunctions
addItemsFunc = map addItemFunc
    
replaceItemFunc :: Active i => (Point, i) -> WorldMapFunction
replaceItemFunc (p, i) = worldMapFunction p (packItem i) replaceMerge

alterItem :: WorldMapFunction -> WorldMap -> WorldMap
alterItem wmFunc = Map.alter (updateFunc wmFunc) (itemPoint wmFunc)

updateWorldMap :: WorldMapFunctions -> WorldMap -> WorldMap
updateWorldMap wmFuncs wm = foldr alterItem wm wmFuncs

simpleMerge :: ActiveItemsMerge
simpleMerge = (++)

replaceMerge :: ActiveItemsMerge
replaceMerge newIts oldIts = newIts ++ (oldIts L.\\ newIts) 