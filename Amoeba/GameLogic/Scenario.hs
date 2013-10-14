{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}

module GameLogic.Scenario where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Prelude hiding (read)

import GameLogic.Evaluation
import GameLogic.Geometry
import GameLogic.Object
import GameLogic.AI

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

{-
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
-}

withdrawEnergy pl cnt = do
    mbK <- lift $ find $ ownership `is` pl ~&~ batteryCharge `suchThat` (>= cnt)
    
    return mbK 


constructObject = undefined

createProduct eCost sch = do
    pl <- read ownership
    withdrawEnergy pl eCost
    return ()
    
placeProduct prod plAlg = undefined

produce = do
    f <- read fabric
    when (f ^. producing) $ do
        prodObj <- createProduct (f ^. energyCost) (f ^. scheme)
        placeProduct prodObj (f ^. placementAlg)

mainScenario :: Eval ()
mainScenario = do
    withProperty fabric produce
    return ()

