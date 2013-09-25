{-# LANGUAGE NoMonomorphismRestriction #-}

module GameLogic.Scenario where

import GameLogic.Geometry
import GameLogic.Object
import GameLogic.AI

import Control.Lens

data Collision = Collision Objects

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
nextRndNum :: m Int
nextRndNum = undefined

objectAt :: Point -> m Object
objectAt = undefined

p1 = undefined
p2 = undefined

run = do
    rndNum <- nextRndNum
    obj1 <- objectAt p1
    obj2 <- objectAt p2
    trans obj1 energyPosted saveEnergy remove
    trans obj2 selfDestruct remove save
