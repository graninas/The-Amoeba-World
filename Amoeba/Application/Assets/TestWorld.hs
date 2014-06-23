module Application.Assets.TestWorld where

import GameLogic.Facade

import qualified Data.Map as M

res1 = toResource (0, 1000)
res2 = toResource (100, 100)
res3 = toResource (300, 2000)

objectId1 = 1

testWorldMap = M.fromList [(point 10 10 0, Object objectId1 1 player1 res1 res2 res3)]
testEffectMap = M.empty

target1 = TargetArea (rectBound p1 p2)

p1 = point 10 10 0
p2 = point 20 20 0
testTasksMap = M.fromList [(objectId1, [Task target1 capture Nothing])]

testWorld = World testWorldMap testEffectMap testTasksMap 1 1 Nothing