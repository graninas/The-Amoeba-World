module GameLogic.AI.AI where

import Control.Lens

import GameLogic.Geometry
import GameLogic.Player
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

nodePoint = fst . GAI.fromNode

-- Path finding & search

pathAllowed mbPl l obj1 obj2 = pass && playersEqual && playerAllowed mbObj1Pl
    where
        mbObj1Pl = obj1 ^? ownership
        mbObj2Pl = obj2 ^? ownership
        pass = isPassable l obj1 && isPassable l obj2
        playersEqual = mbObj1Pl == mbObj2Pl
        playerAllowed Nothing = True
        playerAllowed mbPl' = mbPl' == mbPl

passableNodesDist = 1
notPassableNodesDist = 1000
nodesDist pl l n1@(Node p1 o1) n2@(Node p2 o2) =
    if pathAllowed pl l o1 o2
    then passableNodesDist
    else notPassableNodesDist


-- TODO: heuristic distance
heurDist _ = 2

nearestEmpty :: Maybe Player -> Layer -> Object -> ObjectGraph -> Maybe [ObjectNode]
nearestEmpty mbPl l obj gr = GAI.aStar gr (nodesDist mbPl l) heurDist GAI.isNodeEmpty startNode
  where
    startNode = node (obj ^. singular objectDislocation) obj
    
    