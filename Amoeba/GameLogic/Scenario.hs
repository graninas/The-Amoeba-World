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
import GameLogic.Types
import GameLogic.Player

withdrawEnergy :: Player -> Energy -> Eval ()
withdrawEnergy pl cnt = do
    k <- single $ ownership `is` pl ~&~ batteryCharge `suchThat` (>= cnt)
    let ch = k ^. singular batteryCharge
    let newK = batteryCharge .~ (ch - cnt) $ k
    save newK

createProduct :: Energy -> Object -> Eval Object
createProduct eCost sch = do
    pl <- read ownership
    d <- read dislocation
    withdrawEnergy pl eCost
    let p1 = ownership .~ pl $ sch
    let p2 = dislocation .~ d $ p1
    return p2

placeProduct prod plAlg = do
    l <- getProperty layer ground prod
    obj <- getActedObject
    targetP <- evaluatePlacementAlg plAlg l obj
    let p1 = objectDislocation .~ targetP $ prod
    save p1

produce :: Eval String
produce = do
    f <- read fabric
    when (f ^. producing) $ do
        prodObj <- createProduct (f ^. energyCost) (f ^. scheme)
        placeProduct prodObj (f ^. placementAlg)
    return "Successfully produced."

mainScenario :: Eval ()
mainScenario = do
    withProperty fabric produce
    return ()

