module World.GenericWorld where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Graph.AStar as AStar
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid

import World.Geometry

type GenericMap c = Map.Map Point c
data GenericWorld mp = GenericWorld { _worldMap :: mp
                                    , _worldBound :: Bound }

type CelledWorld c = GenericWorld (GenericMap c)

fromList :: [(Point, c)] -> CelledWorld c
fromList list = GenericWorld wm b
  where
    wm = Map.fromList list
    b = occupiedArea (map fst list)

worldMap :: (GenericCell c, Monoid c) => CelledWorld c -> GenericMap c
worldMap = _worldMap

worldBound :: (GenericCell c, Monoid c) => CelledWorld c -> Bound
worldBound = _worldBound

resetWorldMap :: (GenericCell c, Monoid c) => CelledWorld c -> GenericMap c -> CelledWorld c
resetWorldMap w wm = w { _worldMap = wm }

class GenericCell c where
    null :: c -> Bool
    pos :: c -> Point

alterWorld :: (GenericCell c, Monoid c) => CelledWorld c -> [c] -> CelledWorld c
alterWorld = foldl alterCell

alterCell :: (GenericCell c, Monoid c) => CelledWorld c -> c -> CelledWorld c
alterCell (GenericWorld m b) c = GenericWorld (f m) b'
  where
    cellPos = pos c
    alteringFunc = Just . maybe c (mappend c)
    f = Map.alter alteringFunc cellPos
    b' = updateRectBound cellPos b

-- Graph

data Node c = Node Point c
type NodeSet c = Set.Set (Node c)

instance Ord (Node c) where
    (Node p1 _) <= (Node p2 _) = p1 <= p2
    
instance Eq (Node c) where
    (Node p1 _) == (Node p2 _) = p1 == p2

lookupNode :: Monoid c => CelledWorld c -> Point -> Node c
lookupNode (GenericWorld wm _) p = Node p (fromMaybe mempty $ Map.lookup p wm)

graph :: (Monoid c, GenericCell c) => CelledWorld c -> Point -> NodeSet c
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