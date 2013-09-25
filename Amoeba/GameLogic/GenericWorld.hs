module GameLogic.GenericWorld where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Graph.AStar as AStar
import Prelude hiding (null)
import Data.Maybe (fromMaybe, maybe)

import GameLogic.Geometry

type GenericMap c = Map.Map Point c
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
    wm = Map.fromList list
    b = occupiedArea (map fst list)

resetWorldMap :: GenericCell c => CelledWorld c -> GenericMap c -> CelledWorld c
resetWorldMap w wm = w { worldMap = wm }

emptyWorld = GenericWorld Map.empty noBound

class Eq c => GenericCell c where
    empty :: c
    merge :: c -> c -> c

alterWorld :: GenericCell c => CelledWorld c -> [(Point, c)] -> CelledWorld c
alterWorld = foldl alterCell

alterCell :: GenericCell c => CelledWorld c -> (Point, c) -> CelledWorld c
alterCell (GenericWorld m b) (p, c) = GenericWorld newMap b'
  where
    alteringFunc oldCell | empty == c = Nothing
                         | otherwise = Just . maybe c (merge c) $ oldCell
    f = Map.alter alteringFunc p
    newMap = f m
    b' = if Map.null newMap then NoBound
                            else updateRectBound p b

refreshWorldBound w = let wmKeys = Map.keys . worldMap $ w
                          newBound = foldr updateRectBound NoBound wmKeys
                      in w { worldBound = newBound }

-- Graph

data Node c = Node Point c
type NodeSet c = Set.Set (Node c)

instance Ord (Node c) where
    (Node p1 _) <= (Node p2 _) = p1 <= p2
    
instance Eq (Node c) where
    (Node p1 _) == (Node p2 _) = p1 == p2

lookupNode :: GenericCell c => CelledWorld c -> Point -> Node c
lookupNode (GenericWorld wm _) p = Node p (fromMaybe empty $ Map.lookup p wm)

graph :: GenericCell c => CelledWorld c -> Point -> NodeSet c
graph wm p = let
    ps = neighbours p
    nodes = map (lookupNode wm) ps
    in Set.fromList nodes

aStar :: (GenericCell c, Ord d, Num d)
      => (Node c -> NodeSet c)
      -> (Node c -> Node c -> d)
      -> (Node c -> d)
      -> (Node c -> Bool)
      -> Node c
      -> Maybe [Node c]
aStar = AStar.aStar

{-
-- From here: http://hackage.haskell.org/packages/archive/astar/0.1/doc/html/src/Data-Graph-AStar.html#aStar
aStar
:: (Ord a, Ord c, Num c)     
=> (a -> Set a)    The graph we are searching through, given as a function from vertices to their neighbours.
-> (a -> a -> c)   Distance function between neighbouring vertices of the graph. This will never be applied to vertices that are not neighbours, so may be undefined on pairs that are not neighbours in the graph.
-> (a -> c)        Heuristic distance to the (nearest) goal. This should never overestimate the distance, or else the path found may not be minimal.
-> (a -> Bool)     The goal, specified as a boolean predicate on vertices.
-> a               The vertex to start searching from.
-> Maybe [a]       An optimal path, if any path exists. This excludes the starting vertex. 
-}