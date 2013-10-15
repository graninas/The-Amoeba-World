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

import Test.Utils.Data
import Test.Utils.Arbitraries

import GameLogic.World
import GameLogic.Player
import GameLogic.Geometry
import GameLogic.Objects
import GameLogic.Object
import GameLogic.Scenario
import GameLogic.Evaluation hiding (objects)
import qualified GameLogic.GenericWorld as GW
import GameLogic.Game
import GameLogic.AI
import Misc.Descriptions

plasma1       = putObject point1 $ plasma player1
plasma2       = putObject point2 $ plasma player1
soundWave1    = putObject point3 $ soundWave player1 left 10
laserBeam1    = putObject point4 $ laserBeam player2 up 200
karyon1       = putObject point5 $ karyon player2
testGame seed = testGame'
  where
    g = initialGame seed & karyon1 & plasma1 & plasma2 & soundWave1 & laserBeam1
    w = g ^. world
    testGame' = over world refreshWorldBound g

testContext :: Game -> EvaluationContext
testContext game = context rndF objectAtF objectsF
  where
    rndF = nextRnd game
    objectAtF = getObjectAt game
    objectsF = getObjects game

testGameAndContext seed = let
    game = testGame seed
    ctx = testContext game
    in (game, ctx)

nextRnd :: Game -> Eval Int
nextRnd game = let (r, g) = random (game ^. rndGen)
                   newGame = rndGen .~ g $ game
               in do
                   ctx <- get
                   put $ ctx { _ctxNextRndNum = nextRnd newGame }
                   return r

getObjectAt :: Game -> Point -> Eval (Maybe Object)
getObjectAt game p = return $ game ^? objects . ix p

getObjects :: Game -> Eval Objects
getObjects game = return $ (game ^. objects) ^.. folding id


prop_objectAt1 p seed = obj1 == obj2
  where
    (game, ctx) = testGameAndContext seed
    obj1 = evaluate (objectAt p) ctx
    obj2 = Right $ game ^. objects . at p

prop_objectAt2 seed = (obj1 == obj2) && isJust (obj1 ^. _Right)
  where
    (game, ctx) = testGameAndContext seed
    obj1 = evaluate (objectAt point1) ctx
    obj2 = Right $ game ^. objects . at point1

prop_query1 seed = evaluatedObject == expectedSoundWave
  where
    (game, ctx) = testGameAndContext seed
    q = find $ layer `is` sky ~&~ named `is` toNamed "SoundWave"
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

tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = runTests
    