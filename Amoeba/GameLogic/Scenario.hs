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
{-
withdrawEnergy obj cnt = forObject obj $ do
    mbCh <- batteryCharge `whenIt` (>= cnt)
    case mbCh of
        Nothing -> return Nothing
        Just ch -> return . Just 
-}

withdrawEnergy = undefined
constructObject = undefined

createProduct eCost sch = undefined
placeProduct prod plAlg = undefined

--produce :: Object -> Eval ()
produce obj = do
    f <- read obj fabric
    when (f ^. producing) $ do
        prodObj <- createProduct (f ^. energyCost) (f ^. scheme)
        placeProduct prodObj (f ^. placementAlg)

mainScenario :: Eval ()
mainScenario = do
    withProperty fabric produce
    return ()

