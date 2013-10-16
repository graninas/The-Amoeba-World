module GameLogic.AI where

import qualified Data.Set as Set
import qualified Data.Graph.AStar as AStar
import Data.Maybe (fromMaybe)

import GameLogic.Geometry
import GameLogic.Object
import GameLogic.GenericWorld as GW

data PathStrategy = WithLast
type TrackAction = Object -> Point -> Object

-- For some Object, track points with action on the object.
track :: Path -> PathStrategy -> TrackAction -> Object -> Object
track ps strategy act obj = foldl act obj (enrollPathStrategy strategy ps) 

withLast :: PathStrategy
withLast = WithLast

enrollPathStrategy :: PathStrategy -> Points -> Points
enrollPathStrategy WithLast [] = []
enrollPathStrategy WithLast ps = [last ps]

move p (StraightMoving s d) = moveStraight s p d
path p (StraightMoving s d) = scanl advance p (replicate s d)


-- Graph

data Node c = Node Point c
type NodeSet c = Set.Set (Node c)
type Graph c = Point -> NodeSet c

instance Ord (Node c) where
    (Node p1 _) <= (Node p2 _) = p1 <= p2
    
instance Eq (Node c) where
    (Node p1 _) == (Node p2 _) = p1 == p2

toNode (p, c) = Node p c

lookupNode :: GenericCell c => CelledWorld c -> Point -> Node c
lookupNode (GenericWorld wm _) p = Node p (fromMaybe GW.empty $ GW.lookup p wm)

graph :: GenericCell c => CelledWorld c -> Graph c
graph w p = let
    ps = neighbours p
    nodes = map (lookupNode w) ps
    in Set.fromList nodes

aStar :: (GenericCell c, Ord d, Num d)
      => (Node c -> NodeSet c)
      -> (Node c -> Node c -> d)
      -> (Node c -> d)
      -> (Node c -> Bool)
      -> Node c
      -> Maybe [Node c]
aStar = AStar.aStar

-- Path finding & search

nearestEmpty p = undefined

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