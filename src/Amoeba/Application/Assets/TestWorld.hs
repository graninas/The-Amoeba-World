module Amoeba.Application.Assets.TestWorld (testWorld) where

import Amoeba.GameLogic.Facade

import qualified Data.Map as M

res1, res2, res3 :: Resource Int
res1 = toResource (0, 1000)
res2 = toResource (100, 100)
res3 = toResource (300, 2000)

p1, p2 :: Point
p1 = point 10 10 0
p2 = point 20 20 0

objectId1, objectId2 :: Int
objectId1 = 1
objectId2 = 2

testWorldMap :: M.Map Point Object
testWorldMap = M.fromList [ (p1, Object objectId1 1 humanPlayer res1 res2 res3)
                          , (p2, Object objectId2 1 ai1Player   res1 res2 res3)]
testEffectMap :: EffectMap
testEffectMap = M.empty

testWorld :: World
testWorld = World testWorldMap testEffectMap 1 1 Nothing