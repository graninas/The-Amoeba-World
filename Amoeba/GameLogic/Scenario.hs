{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module GameLogic.Scenario where

import Control.Lens
import Control.Monad.State

import GameLogic.Geometry
import GameLogic.Object
import GameLogic.AI

data EvaluationContext = EvaluationContext { _ctxNextRndNum :: State EvaluationContext Int
                                           , _ctxObjectAt :: Point -> State  EvaluationContext Object }
data EvaluationResult = EvaluationResult

makeLenses ''EvaluationContext

trans :: Object -> (Collision -> Bool) -> a -> a -> b
trans = undefined

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


nextRndNum :: State EvaluationContext Int
nextRndNum = get >>= _ctxNextRndNum

objectAt :: Point -> State EvaluationContext Object
objectAt p = get >>= flip _ctxObjectAt p

run :: State EvaluationContext EvaluationResult
run = do
    rndNum <- nextRndNum
    obj1 <- objectAt p1
    obj2 <- objectAt p2
    trans obj1 energyPosted saveEnergy remove
    trans obj2 selfDestruct remove save

