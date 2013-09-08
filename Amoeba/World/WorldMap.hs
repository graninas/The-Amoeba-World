module World.WorldMap where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Graph.AStar as AStar
import Data.Maybe (fromMaybe)

import World.Geometry

data WorldMap i = WorldMap { wmMap :: Map.Map Point i
                           , wmBound :: Bound }

fromList :: [(Point, i)] -> WorldMap i
fromList list = WorldMap wm b
  where
    wm = Map.fromList list
    b = occupiedArea (map fst list)

-- Graph

class Cell c where
    empty :: c

data Node c = Node Point c
type NodeSet c = Set.Set (Node c)

instance Ord (Node c) where
    (Node p1 _) <= (Node p2 _) = p1 <= p2
    
instance Eq (Node c) where
    (Node p1 _) == (Node p2 _) = p1 == p2

lookupNode :: Cell c => WorldMap c -> Point -> Node c
lookupNode (WorldMap wm _) p = Node p (fromMaybe empty $ Map.lookup p wm)

graph :: Cell c => WorldMap c -> Point -> NodeSet c
graph wm p = let
    ps = neighbours p
    nodes = map (lookupNode wm) ps
    in Set.fromList nodes

aStar :: (Cell c, Ord d, Num d)
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