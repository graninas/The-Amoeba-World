{-# LANGUAGE ExistentialQuantification #-}

module World.World where

import Prelude hiding (Bounded)
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.List as L
import qualified Control.Arrow as Arr (second, (***))
import System.Random

import World.WorldMap as WM
import World.Types
import World.Geometry
import World.Player
import World.Constants
import World.Descripted
import World.Id

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
    
instance Ord ActiveItem where
    ai1 <= ai2 = getId ai1 <= getId ai2

packItem :: Active i => i -> ActiveItem
packItem = MkActiveItem

packItems :: Active i => [i] -> ActiveItems
packItems = map packItem

appendActiveItem :: Active i => i -> ActiveItems -> ActiveItems
appendActiveItem item its = packItem item : its

packObject :: Active i => (Point, i) -> (Point, ActiveItem)
packObject (p, i) = (p, packItem i)

packObjects :: Active i => [(Point, i)] -> [(Point, ActiveItem)]
packObjects = map packObject

(|>|) :: Active i => [(Point, i)] -> [(Point, ActiveItem)] -> [(Point, ActiveItem)]
objects |>| items = map packObject objects ++ items

(|>||) :: (Active i1, Active i2) => [(Point, i1)] -> [(Point, i2)] -> [(Point, ActiveItem)]
objects1 |>|| objects2 = objects1 |>| (objects2 |>| [])

infixr 5 |>|
infixr 5 |>||

{- World -}

type GameMap = WorldMap ActiveItems
data World = World { worldMap :: GameMap
                   , worldLastItemId :: ItemId
                   , worldStdGen :: StdGen }

data Annotation = Annotation { annotationMessage :: String }
type Annotations = [Annotation]
   
newWorld :: GameMap -> ItemId -> StdGen -> World
newWorld = World

worldFromList l = let
    itemToList i = [i]
    newList = map (Arr.second itemToList) l
    in newWorld (WM.fromList newList)

stepWorld :: World -> (World, Annotations)
stepWorld world@(World wm _ _) = Map.foldrWithKey activateItems (world, []) (wmMap wm)

activateItem :: Point -> ActiveItem -> (World, Annotations) -> (World, Annotations)
activateItem p i (w, an) = let (w', an') = activate p i w
                           in (w', an ++ an')

activateItems :: Point -> ActiveItems -> (World, Annotations) -> (World, Annotations)
activateItems p items activationData = foldr (activateItem p) activationData items

inactive :: Point -> i -> World -> (World, Annotations)
inactive _ _ w = (w, [])

{- World operations -}

annotation :: String -> Annotation
annotation = Annotation

showPointAndPlayer :: Point -> Player -> String
showPointAndPlayer p pl = "[" ++ show p ++ ", " ++ show pl ++ "]"

showPoint :: Point -> String
showPoint p = "[" ++ show p ++ "]"

activationAnnotation p i = annotation $ showPointAndPlayer p (ownedBy i) ++ " " ++ name i ++ ": activated"

itemsAt :: Point -> World -> ActiveItems
itemsAt p (World wm _ _) = Maybe.fromMaybe [] $ Map.lookup p (wmMap wm)

worldItems :: World -> ActiveItems
worldItems (World wm _ _) = Map.foldr (++) [] (wmMap wm)

isEmptyCell :: Point -> World -> Bool
isEmptyCell p = null . itemsAt p

getPlayers :: ActiveItems -> Players
getPlayers activeIts = map ownedBy $ filter (isOrdinaryPlayer . ownedBy) activeIts

itemsCount :: World -> Int
itemsCount (World wm _ _) = Map.fold f 0 (wmMap wm)
  where
    f items n = n + length items