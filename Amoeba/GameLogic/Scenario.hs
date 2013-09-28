{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}

module GameLogic.Scenario where

import Control.Lens
import Control.Monad.State
import Prelude hiding (read)

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
    (Just obj1) <- objectAt p1
    (Just obj2) <- objectAt p2
    transact obj1 energyPosted saveEnergy remove
    transact obj2 selfDestruct remove save

query q = undefined

produce :: ObjectedEval ()
produce = do
    f  <- read fabric
    pl <- read ownership
    k1 <- query ()
    k2 <- query $ ownership `is` pl
    k3 <- query (ownership `is` pl ~&~ battery `suchThat` charged)
    return ()

run :: Eval ScenarioResult
run = do
    with fabric produce
    f <- read fabric
    example







