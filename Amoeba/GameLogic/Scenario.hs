{-# LANGUAGE NoMonomorphismRestriction #-}

module GameLogic.Scenario where

import Control.Lens
import Control.Monad.State

import GameLogic.Evaluation
import GameLogic.Geometry
import GameLogic.Object
import GameLogic.AI

data ScenarioResult = ScenarioResult

energyPosted :: Collision -> Bool
energyPosted = undefined
selfDestruct :: Collision -> Bool
selfDestruct = undefined

saveEnergy :: a
saveEnergy = undefined

remove :: a
remove = undefined
save :: a
save = undefined

p1 = undefined
p2 = undefined

example = do
    rndNum <- nextRndNum
    obj1 <- objectAt p1
    obj2 <- objectAt p2
    trans obj1 energyPosted saveEnergy remove
    trans obj2 selfDestruct remove save

produce = undefined
mv = undefined

run :: Eval ScenarioResult
run = do
    with fabric produce
    with moving mv

    example







