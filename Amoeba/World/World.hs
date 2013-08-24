{-# LANGUAGE ExistentialQuantification #-}

module World.World where

import Prelude hiding (Bounded)
import qualified Data.Map as Map
import System.Random

import World.Geometry

-- Inspired by Asteroids: https://github.com/ocharles/netwire-classics/blob/master/asteroids/Asteroids.hs
class Bounded i where
    bounds :: i -> Bound

data Bound = BoundCircle { circleCenter :: Center
                         , circleRadius ::  Radius }
           | BoundRectangle { rectangleLeftUp :: Point
                            , rectangleRightDown :: Point }
           | BoundPoint { pointPosition :: Point }
  deriving (Show, Read, Eq)



class Active i where
    activate :: i -> Point -> World -> Activator 


data World = World { worldMap :: WorldMap
                   , worldStdGen :: StdGen }


data Action = forall i. Active i => Add Point i
            | forall i. Active i => Delete Point i
  
type Actions = [Action]

data Activator = Activator { activatorActions :: Actions
                           , activatorRndGen :: StdGen }


type CoordinateMap = Map.Map Point
data WorldMap = forall i. Active i => WorldMap (CoordinateMap i)


worldMapFromList :: Active i => [(Point, i)] -> WorldMap
worldMapFromList l = WorldMap (Map.fromList l)


 