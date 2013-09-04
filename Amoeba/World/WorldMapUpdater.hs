module World.WorldMapUpdater where

import World.World
import World.Types
import World.Geometry

import qualified Data.Map as Map
import qualified Data.List as L

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

-- | Updates wordlmap with a functions.    
alterWorldMap :: WorldMapFunctions -> WorldMap -> WorldMap
alterWorldMap wmFuncs wm = foldr alterItem wm wmFuncs

alterItem :: WorldMapFunction -> WorldMap -> WorldMap
alterItem wmFunc (WorldMap wm b) = WorldMap (f wm) b'
  where
    f = Map.alter (updateFunc wmFunc) (itemPoint wmFunc)
    b' = updateRectBound (worldMapFunctionP wmFunc) b

-- | Altering functions can be used in alterWorldMap.
addItemFunc :: Active i => (Point, i) -> WorldMapFunction
addItemFunc (p, i) = worldMapFunction p (packItem i) simpleMerge

addItemsFunc :: Active i => [(Point, i)] -> WorldMapFunctions
addItemsFunc = map addItemFunc
    
replaceItemFunc :: Active i => (Point, i) -> WorldMapFunction
replaceItemFunc (p, i) = worldMapFunction p (packItem i) replaceMerge
    

-- | Generic world map updating function with merge strategy.
worldMapFunction :: Point -> ActiveItem -> ActiveItemsMerge -> WorldMapFunction
worldMapFunction p i mergeF = let wmFunc = Just . maybe [i] (mergeF [i])
                              in WorldMapFunction p wmFunc

-- | Merge functions
simpleMerge :: ActiveItemsMerge
simpleMerge = (++)

replaceMerge :: ActiveItemsMerge
replaceMerge newIts oldIts = newIts ++ (oldIts L.\\ newIts) 
