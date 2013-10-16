module GameLogic.GenericAI where

import qualified Data.Set as Set
import qualified Data.Graph.AStar as AStar
import Data.Maybe (fromMaybe)

import GameLogic.Geometry
import GameLogic.GenericWorld as GW

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