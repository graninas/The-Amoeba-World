module GameLogic.Data.World where

import qualified Control.Lens as L
import qualified Data.Map as M
import Prelude hiding (null, lookup)

import GameLogic.Base.Geometry
import GameLogic.Data.Object

data Effect = Effect
  deriving (Show, Read, Eq)
type Effects = [Effect]
type EffectMap = M.Map Int Effects

data Action = Action
  deriving (Show, Read, Eq)

type WorldMap = M.Map Point Object
data World = World { worldMap :: WorldMap
                   , effectMap :: EffectMap
                   , width :: Int
                   , height :: Int
                   , defaultCell :: Maybe Object
                   }
  deriving (Show, Read, Eq)

emptyWorld = World M.empty M.empty 0 0 Nothing

insertObject point object w@(World wm _ _ _ _) = w { worldMap = M.insert point object wm }

{-
fromList :: [(Point, Object)] -> World
fromList list = World wm b
  where
    wm = M.fromList list
    b = occupiedArea (map fst list)


resetWorldMap :: World -> WorldMap -> World
resetWorldMap w wm = w { worldMap = wm
                       , worldBound = worldMapBound wm }

worldMapBound wm = foldr updateRectBound NoBound (M.keys wm)
refreshWorldBound w = w { worldBound = worldMapBound $ worldMap w }

lookup :: Point -> WorldMap -> Maybe Object
lookup = M.lookup
emptyMap = M.empty

-- Lenses
makeLenses ''World


alterMap :: WorldMap -> Point -> Object -> WorldMap
alterMap m p c = f m
  where
    f = M.alter alteringFunc p
    alteringFunc oldCell | empty == c = Nothing
                         | otherwise = Just . maybe c (merge c) $ oldCell
                         
alterCell :: GenericCell c => CelledWorld c -> Point -> c -> CelledWorld c
alterCell (GenericWorld m b) p c = GenericWorld newMap b'
  where
    newMap = alterMapCell m p c
    b' = if M.null newMap then NoBound
                            else updateRectBound p b

alterWorld :: GenericCell c => CelledWorld c -> [(Point, c)] -> CelledWorld c
alterWorld = foldl alterCell'
  where
    alterCell' w (p, c) = alterCell w p c
-}

