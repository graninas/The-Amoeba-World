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
    f  <- read fabric
    pl <- read ownership
    k1 <- find $ battery `suchThat` charged
    k2 <- find $ ownership `is` pl
    k3 <- find (ownership `is` pl ~&~ battery `suchThat` charged)
    transact obj1 energyPosted saveEnergy remove
    transact obj2 selfDestruct remove save

produce :: ObjectedEval ()
produce = do
    fabricProp <- read fabric
    playerProp <- read ownership
    karyonObj  <- find $ ownership `is` playerProp
    let eCost = fabricProp ^. energyCost
--    transact karyonObj (withdrawEnergy eCost)
--    transact thisObject (constructObject (fabricProp ^. production)
    return ()

run :: Eval ScenarioResult
run = do
    with fabric produce
    f <- read fabric
    example







