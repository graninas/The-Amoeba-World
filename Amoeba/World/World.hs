{-# LANGUAGE ExistentialQuantification #-}

module World.World where

import Prelude hiding (Bounded)
import qualified Data.Map as Map
import System.Random

import World.Geometry

class Active i where
  --activate :: item -> item position -> previous activators -> previous word -> new activators
    activate :: i -> Point -> Activators -> World -> Activators


data World = World { worldMap :: WorldMap
                   , worldStdGen :: StdGen }


data Action = forall i. Active i => Add Point i
            | forall i. Active i => Delete Point i
  
type Actions = [Action]

data Activators = Activators { activatorActions :: Actions
                             , currentRndGen :: StdGen }


type CoordinateMap = Map.Map Point
data WorldMap = forall i. Active i => WorldMap (CoordinateMap i)


worldMapFromList :: Active i => [(Point, i)] -> WorldMap
worldMapFromList l = WorldMap (Map.fromList l)


inactive :: Active i => i -> Point -> Activators -> World -> Activators
inactive _ _ acts _ = acts