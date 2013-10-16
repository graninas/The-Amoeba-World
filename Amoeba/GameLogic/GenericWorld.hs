module GameLogic.GenericWorld where

import qualified Data.Map as M
import Prelude hiding (null, lookup)

import GameLogic.Geometry

type GenericMap c = M.Map Point c
data GenericWorld mp = GenericWorld { worldMap :: mp
                                    , worldBound :: Bound }
    deriving (Eq)

type CelledWorld c = GenericWorld (GenericMap c)

instance (Show mp) => Show (GenericWorld mp) where
    show (GenericWorld wm wb) = "World { worldMap = " ++ show wm
                             ++ ", worldBound = " ++ show wb ++ " } "

fromList :: [(Point, c)] -> CelledWorld c
fromList list = GenericWorld wm b
  where
    wm = M.fromList list
    b = occupiedArea (map fst list)

resetWorldMap :: GenericCell c => CelledWorld c -> GenericMap c -> CelledWorld c
resetWorldMap w wm = w { worldMap = wm
                       , worldBound = worldMapBound wm }

lookup :: GenericCell c => Point -> GenericMap c -> Maybe c
lookup = M.lookup
emptyMap = M.empty
emptyWorld = GenericWorld emptyMap noBound


class Eq c => GenericCell c where
    empty :: c
    merge :: c -> c -> c

alterMapCell :: GenericCell c => GenericMap c -> (Point, c) -> GenericMap c
alterMapCell m (p, c) = f m
  where
    f = M.alter alteringFunc p
    alteringFunc oldCell | empty == c = Nothing
                         | otherwise = Just . maybe c (merge c) $ oldCell
                         
alterCell :: GenericCell c => CelledWorld c -> (Point, c) -> CelledWorld c
alterCell (GenericWorld m b) objDef@(p, c) = GenericWorld newMap b'
  where
    newMap = alterMapCell m objDef
    b' = if M.null newMap then NoBound
                            else updateRectBound p b

alterWorld :: GenericCell c => CelledWorld c -> [(Point, c)] -> CelledWorld c
alterWorld = foldl alterCell

worldMapBound wm = foldr updateRectBound NoBound (M.keys wm)

refreshWorldBound w = w { worldBound = worldMapBound $ worldMap w }
