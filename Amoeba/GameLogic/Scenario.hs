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

--modifyResource :: (Ord a, ZeroOrd a) => prop -> Player -> a -> Eval ()
modifyResource prop pl cnt | zeroCompare cnt == EQ = return ()
                           | zeroCompare cnt == LT = modifyResource' prop ltQ cnt
                           | zeroCompare cnt == GT = modifyResource' prop gtQ cnt
  where
    ltQ = ownership `is` pl
    gtQ = ownership `is` pl ~&~ (prop.current) `suchThat` (>= cnt)
    modifyResource' prop q cnt = do
        obj <- single q
        res <- getProperty prop obj
        let newRes = (prop.current) .~ modifyResourceStock res cnt $ obj
        save newRes
    
withdrawEnergy pl eCost = modifyResource battery pl (negate eCost)

createProduct :: Energy -> Object -> Eval Object
createProduct eCost sch = do
    pl <- read ownership
    d  <- read dislocation
    withdrawEnergy pl eCost
    return $ adjust sch [ownership .~ pl, dislocation .~ d]

placeProduct prod plAlg = do
    l   <- withDefault ground $ getProperty layer prod
    obj <- getActedObject
    p   <- evaluatePlacementAlg plAlg l obj
    save $ objectDislocation .~ p $ prod

produce f = do
    prodObj <- createProduct (f ^. energyCost) (f ^. scheme)
    placeProduct prodObj (f ^. placementAlg)
    return "Successfully produced."

producingScenario :: Eval String
producingScenario = do
    f <- read fabric
    if f ^. producing
        then produce f
        else return "Producing paused."

mainScenario :: Eval ()
mainScenario = do
    forProperty fabric producingScenario
    return ()

