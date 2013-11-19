{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Monoid
import Data.Default
import Data.Maybe
import Data.Char
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Control.Monad.State

import Test.QuickCheck
import Test.QuickCheck.All

import Test.Utils.GeometryData
import Test.Utils.TestGameData
import Test.Utils.GeometryArbitraries
import Test.Utils.ObjectArbitraries
import Test.Utils.EvaluationUtils

import GameLogic.World
import GameLogic.Player
import GameLogic.Geometry
import GameLogic.Objects
import GameLogic.Object
import GameLogic.Scenario
import GameLogic.Evaluation
import GameLogic.Game
import GameLogic.AI as AI
import Misc.Descriptions
import qualified GameLogic.GenericWorld as GW
import qualified GameLogic.GenericAI as GAI

{-
prop_producingScenario = let
    ctx = testContext testGame1
    newContext = execute producingScenario ctx
    in ctx == newContext
-}

tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = do
    runTests
    
    let ctx = testContext $ initialGame 1
    let placeProductResult = execute (placeProduct (plasma player1 (point 1 1 1)) PlaceToNearestEmptyCell) ctx
    print "Place product result transaction map:"
    print $ placeProductResult ^. ctxTransactionMap

    let f = soundWaveFabric player1 up (point 1 1 1)
    let produceResult = execute (produce f) ctx
    print "Produce result transaction map:"
    print $ produceResult ^. ctxTransactionMap

    let ctx = testContext gameWithFabric    
    let newContext = execute producingScenario ctx
    let oldTransMap = ctx ^. ctxTransactionMap
    let newTransMap = newContext ^. ctxTransactionMap
    print "Game:"
    print gameWithFabric
    print "Old transaction map:"
    print oldTransMap
    print "New transaction map:"
    print newTransMap