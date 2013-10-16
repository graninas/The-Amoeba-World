module GameLogic.AI where

import GameLogic.Geometry
import GameLogic.Object
import GameLogic.GenericAI as GAI

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

type ObjectGraph = GAI.Graph Object
type ObjectNode = GAI.Node Object
type ObjectNodeSet = GAI.NodeSet Object

-- Path finding & search

passableNodesDist = 1
notPassableNodesDist = 1000
nodesDist n1@(Node p1 o1) n2@(Node p2 o2) =
    if passable o1 o2
    then passableNodesDist
    else notPassableNodesDist

nearestEmpty p g = GAI.aStar g 

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