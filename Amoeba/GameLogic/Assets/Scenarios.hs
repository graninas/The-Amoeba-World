module GameLogic.Assets.Scenarios where
{-

import Control.Lens
import Control.Monad
import Control.Monad.State
import Prelude hiding (read)

import GameLogic.Evaluation
import GameLogic.Geometry
import GameLogic.Object
import GameLogic.Types
import GameLogic.Player
import Misc.Descriptions

-- TODO: make it safe
modifyResourceStock res@(Resource cur (Just cap)) cnt
    | zeroCompare (cur + cnt) == LT = error $ "Resource exhausted: " ++ show res ++ ", cnt = " ++ show cnt
    | cur + cnt >= cap = cap
    | otherwise = cur + cnt
modifyResourceStock res@(Resource cur Nothing) cnt
    | zeroCompare (cur + cnt) == LT = error $ "Resource exhausted: " ++ show res ++ ", cnt = " ++ show cnt
    | otherwise = cur + cnt 

withdrawEnergy pl cnt = do
    obj <- singleActual $ named `is` karyonName ~&~ ownership `is` pl ~&~ batteryCharge `suchThat` (>= cnt)
    batRes <- getProperty battery obj
    save $ batteryCharge .~ modifyResourceStock batRes cnt $ obj

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

-- TODO: moving with obstacles
move m = do
    obj <- getActedObject
    p <- evaluateMovingAlg m obj
    replace obj $ objectDislocation .~ p $ obj
    return "Successfully moved."

movingScenario :: Eval String
movingScenario = do
    m <- read moving
    move m


mainScenario :: Eval ()
mainScenario = do
    forProperty fabric producingScenario
    forProperty moving movingScenario
    return ()

-}