{-# LANGUAGE ExistentialQuantification #-}

module World.World where

import Prelude hiding (Bounded)
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import System.Random

import World.Geometry

class Active i where
  --activate :: item -> item position -> previous activators -> previous word -> new activators
    activate :: i -> Point -> Activators -> World -> Activators


data World = World { worldMap :: WorldMap }


data Action = forall i. Active i => Add Point (Point -> i)
            | forall i. Active i => Delete Point i
  
type Actions = [Action]

data WorldMutator = WorldMutator { worldMutatorActions :: Actions
                                 , worldMutatorRndGen :: StdGen }

data WorldMap = forall i. Active i => WorldMap (Map.Map Point [i])


worldMapFromList :: Active i => [(Point, [i])] -> WorldMap
worldMapFromList l = WorldMap (Map.fromList l)

inactive :: Active i => i -> Point -> WorldMutator -> World -> WorldMutator
inactive _ _ acts _ = acts


takeWorldItems p (World wm) = Maybe.fromMaybe [] $ Map.lookup p wm
isEmptyCell p (World wm) = null $ takeWorldItems p wm

emptyCellChecker p w =
    if isEmptyCell p w
    then Right ()
    else Left "Cell not empty"

addActive = Add