module World.GameMapUpdater where

import World.World
import World.WorldMap
import World.Types
import World.Geometry

import qualified Data.Map as Map
import qualified Data.List as L

-- TODO: seems unnecessary, remove it.
class GameMapUpdater a where
    updateFunc :: a -> Maybe ActiveItems -> Maybe ActiveItems
    itemPoint :: a -> Point

--                    Incoming items   Existing items
type ActiveItemsMerge = ActiveItems -> ActiveItems -> ActiveItems

data GameMapFunction = GameMapFunction 
                        { worldMapFunctionP :: Point 
                        , worldMapFunctionF :: Maybe ActiveItems -> Maybe ActiveItems }

type GameMapFunctions = [GameMapFunction]

instance GameMapUpdater GameMapFunction where
    updateFunc = worldMapFunctionF
    itemPoint = worldMapFunctionP

-- | Updates wordlmap with a functions.    
alterGameMap :: GameMapFunctions -> GameMap -> GameMap
alterGameMap wmFuncs wm = foldr alterItem wm wmFuncs

alterItem :: GameMapFunction -> GameMap -> GameMap
alterItem wmFunc (WorldMap wm b) = WorldMap (f wm) b'
  where
    f = Map.alter (updateFunc wmFunc) (itemPoint wmFunc)
    b' = updateRectBound (worldMapFunctionP wmFunc) b

-- | Altering functions can be used in alterGameMap.
addItemFunc :: Active i => (Point, i) -> GameMapFunction
addItemFunc (p, i) = worldMapFunction p (packItem i) simpleMerge

addItemsFunc :: Active i => [(Point, i)] -> GameMapFunctions
addItemsFunc = map addItemFunc
    
replaceItemFunc :: Active i => (Point, i) -> GameMapFunction
replaceItemFunc (p, i) = worldMapFunction p (packItem i) replaceMerge
    

-- | Generic world map updating function with merge strategy.
worldMapFunction :: Point -> ActiveItem -> ActiveItemsMerge -> GameMapFunction
worldMapFunction p i mergeF = let wmFunc = Just . maybe [i] (mergeF [i])
                              in GameMapFunction p wmFunc

-- | Merge functions
simpleMerge :: ActiveItemsMerge
simpleMerge = (++)

replaceMerge :: ActiveItemsMerge
replaceMerge newIts oldIts = newIts ++ (oldIts L.\\ newIts) 
