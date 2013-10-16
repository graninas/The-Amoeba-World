module Test.Utils.TestGameData where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Lens ((&), over, (^.))
import System.Random

import GameLogic.World
import GameLogic.Player
import GameLogic.Geometry
import GameLogic.Objects
import GameLogic.Object
import qualified GameLogic.GenericWorld as GW
import GameLogic.Game

import Test.Utils.Data

customKaryon p pl = putObject p $ karyon pl

plasma1       = putObject point1 $ plasma player1
plasma2       = putObject point2 $ plasma player1
soundWave1    = putObject point3 $ soundWave player1 left 10
laserBeam1    = putObject point4 $ laserBeam player2 up 200
karyon1       = customKaryon point5 player2


testGame seed = testGame'
  where
    g = initialGame seed & karyon1 & plasma1 & plasma2 & soundWave1 & laserBeam1
    w = g ^. world
    testGame' = over world refreshWorldBound g

testGame1 = let
    g1 = Game {_world = GW.fromList [(point 1 1 1,Object {_propertyMap = M.fromList [(1,PDurability {__durability = Resource {_current = 4, _capacity = Just 4}}),(5,PDislocation {__dislocation = Dislocation {_dislocationPoint = point 1 1 1}})]})], _rndGen = mkStdGen 1}
    g2 = g1 & customKaryon (point 1 1 1) player1
    in g2