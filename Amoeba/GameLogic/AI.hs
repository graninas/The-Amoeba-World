module GameLogic.AI where

import Control.Lens ((^.), singular)

import GameLogic.Geometry
import GameLogic.Object as O
import GameLogic.ObjectCell
import GameLogic.GenericAI as GAI
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

type ObjectGraph = GAI.Graph Object
type ObjectNode = GAI.Node Object
type ObjectNodeSet = GAI.NodeSet Object

-- Path finding & search

passableNodesDist = 1
notPassableNodesDist = 1000
nodesDist l n1@(Node p1 o1) n2@(Node p2 o2) =
    if isPathExist o1 o2 l
    then passableNodesDist
    else notPassableNodesDist

-- TODO: heuristic distance
heurDist _ = 2

nearestEmpty :: Layer -> Object -> ObjectGraph -> Maybe [ObjectNode]
nearestEmpty l obj gr = GAI.aStar gr (nodesDist l) heurDist GAI.isNodeEmpty startNode
  where
    startNode = node (obj ^. singular objectDislocation) obj
    
    