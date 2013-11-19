{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Monoid
import Data.Maybe
import System.Random
import Control.Lens
import Control.Monad.State
import Control.Monad

import Test.QuickCheck
import Test.QuickCheck.All

import Test.Utils.GeometryData
import Test.Utils.TestGameData
import Test.Utils.GeometryArbitraries
import Test.Utils.ObjectArbitraries
import Test.Utils.GameArbitraries
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

prop_objectAt1 p seed = obj1 == obj2
  where
    (game, ctx) = testGameAndContext seed
    obj1 = evaluate (getObjectAt p) ctx
    obj2 = Right $ game ^. objects . at p

prop_objectAt2 seed = (obj1 == obj2) && isJust (obj1 ^. _Right)
  where
    (game, ctx) = testGameAndContext seed
    obj1 = evaluate (getObjectAt point1) ctx
    obj2 = Right $ game ^. objects . at point1

prop_query1 seed = evaluatedObject == expectedSoundWave
  where
    (game, ctx) = testGameAndContext seed
    q = find $ layer `is` sky ~&~ named `is` soundWaveName
    evaluatedObject = evaluate q ctx
    expectedSoundWave = Right . Just $ soundWave player1 left 10 point3

prop_query2 name l seed = (length queriedObjects == wmSize) && (not . null $ queriedObjects)
  where
    (game, ctx) = testGameAndContext seed
    wmSize = M.size (game ^. world.worldMap)
    evaluatedObjects = evaluate (query justAll) ctx
    queriedObjects = evaluatedObjects ^. _Right


prop_mandatoryDislocation game = ( classify isNothingFound  "nothing found"
                                 . classify isSingleFound   "single found"
                                 . classify isMultipleFound "multiple found") test
  where
    q = has objectDislocation
    ctx = testContext game
    evaluatedObject = evaluate (single q) ctx
    manyObjects     = evaluate (query  q) ctx
    isSingleFound   = has _Right evaluatedObject && (not . isGameEmpty $ game)
    isMultipleFound = has _Left  evaluatedObject && (length (manyObjects ^. _Right) > 1)
    isNothingFound  = has _Left  evaluatedObject && isGameEmpty game
    test = isNothingFound || isSingleFound || isMultipleFound
    
testSingleProperty game prop = ( classify isSingleFound "single found"
                               . classify isQueryError1 "ENotFound error"
                               . classify isQueryError2 "EOverlappedObjects error") test
  where
    q = has prop
    ctx = testContext game
    evaluatedObject = evaluate (single q) ctx
    singularError   = evaluatedObject ^. singular _Left
    hasLeft         = has _Left  evaluatedObject
    hasRight        = has _Right evaluatedObject
    isSingleFound   = hasRight
    isQueryError1   = hasLeft && isENotFound singularError
    isQueryError2   = hasLeft && isEOverlappedObjects singularError
    test = isSingleFound || isQueryError1 || isQueryError2

prop_singleNamed       game = testSingleProperty game named
prop_singleDurability  game = testSingleProperty game durability    
prop_singleBattery     game = testSingleProperty game battery
prop_singleOwnership   game = testSingleProperty game ownership
prop_singleDislocation game = testSingleProperty game dislocation
prop_singlePassRestr   game = testSingleProperty game passRestriction
prop_singleAge         game = testSingleProperty game age
prop_singleDirected    game = testSingleProperty game directed
prop_singleFabric      game = testSingleProperty game fabric
prop_singleSelfDestr   game = testSingleProperty game selfDestructable
prop_singleMoving      game = testSingleProperty game moving
prop_singleLayer       game = testSingleProperty game layer
prop_singleCollision   game = testSingleProperty game collision

transactionMapFromList :: [(Point, Transaction)] -> TransactionMap
transactionMapFromList = M.fromList

prop_save obj = ( classify hasDislocation "object with dislocation"
                . classify (not hasDislocation) "object without dislocation") test
  where
    ctx = testContext defaultGame
    objPoint = obj ^. singular objectDislocation
    expectedTransMap = transactionMapFromList [(objPoint, actuatedTransaction obj)]
    resultState = execute (save obj) ctx
    actualTransMap = resultState ^. ctxTransactionMap
    hasDislocation = has objectDislocation obj
    test = not hasDislocation || (expectedTransMap == actualTransMap)

rolloutTransaction (Transaction _ (Just obj))  = obj
rolloutTransaction (Transaction (Just obj) _) = obj
rolloutTransaction _ = error "rolloutTransaction impossible" 

rolloutTransactions = map rolloutTransaction

-- TODO: "forProperty fabric" seems like overkill because producingScenario already has fabric dealing.
-- Under construction
{-
prop_forProperty game = test
  where
    ctx = testContext game
    evaluatedState = execute (forProperty fabric producingScenario) ctx
    evaluatedObjects = rolloutTransactions $ fromMap $ evaluatedState ^. ctxTransactionMap
    sourceObjects = fromMap $ game ^. world.worldMap
    test = evaluatedObjects /= sourceObjects
-}

tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = do
    runTests

    print actualTransMap

  where
    obj = Object {_propertyMap = M.fromList [(5,PDislocation {__dislocation = Dislocation {_dislocationPoint = point 6 (-4) (-2)}})]}
    ctx = testContext defaultGame
    objPoint = obj ^. singular objectDislocation
    expectedTransMap = transactionMapFromList [(objPoint, actuatedTransaction obj)]
    resultState = execute (save obj) ctx
    actualTransMap = resultState ^. ctxTransactionMap
    hasDislocation = has objectDislocation obj
    test = not hasDislocation || (expectedTransMap == actualTransMap)

{-
    print sourceObjects
    print evaluatedObjects
    print res
  where
    ctx = testContext testGame1
    (res, evaluatedState) = run (forProperty fabric producingScenario) ctx
    evaluatedObjects = rolloutTransactions $ fromMap $ evaluatedState ^. ctxTransactionMap
    sourceObjects = fromMap $ testGame1 ^. world.worldMap
    test = evaluatedObjects == sourceObjects
-}