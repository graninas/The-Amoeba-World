module GameLogic.AI.GenericAI where

import qualified Data.Set as Set
import qualified Data.Graph.AStar as AStar
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

import GameLogic.Geometry
import GameLogic.GenericWorld as GW

data Node c = Node Point c
type NodeNeighbours c = Set.Set (Node c)
data NodeSet c = NodeSet (NodeNeighbours c)

type Graph c = Node c -> NodeNeighbours c

instance Ord (Node c) where
    (Node p1 _) <= (Node p2 _) = p1 <= p2
    
instance Eq (Node c) where
    (Node p1 _) == (Node p2 _) = p1 == p2

isNodeEmpty (Node _ c) = GW.isCellEmpty c
node = Node

fromNode (Node p c) = (p, c)

lookupNode :: GenericCell c => Point -> CelledWorld c -> Node c
lookupNode p (GenericWorld wm _) = Node p (fromMaybe GW.empty $ GW.lookup p wm)

graph :: GenericCell c => CelledWorld c -> NeighboursFunc -> Graph c
graph w neighboursFunc n@(Node p c) = let
    ps = neighboursFunc p
    nodes = map (`lookupNode` w) ps
    ns = Set.fromList nodes
    in ns

aStar :: (GenericCell c, Ord d, Num d)
      => Graph c
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