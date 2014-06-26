module Application.Assets.TestWorld where

import GameLogic.Facade

import qualified Data.Map as M

res1 = toResource (0, 1000)
res2 = toResource (100, 100)
res3 = toResource (300, 2000)

p1 = point 10 10 0
p2 = point 20 20 0

objectId1 = 1

testWorldMap = M.fromList [(p1, Object objectId1 1 humanPlayer res1 res2 res3)]
testEffectMap = M.empty


testWorld = World testWorldMap testEffectMap 1 1 Nothing