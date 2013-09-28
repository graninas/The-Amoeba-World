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
import GameLogic.Evaluation
import GameLogic.Game
import GameLogic.AI

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
    game = testGame seed
    ctx = testContext game
    obj1 = evalState (objectAt p) ctx
    obj2 = game ^. objects . at p
    
prop_objectAt2 seed = (obj1 == obj2) && isJust obj1
  where
    game = testGame seed
    ctx = testContext game
    obj1 = evalState (objectAt point1) ctx
    obj2 = game ^. objects . at point1


tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = do
    runTests
    
    let game = testGame 1
    let ctx = testContext game
    print $ evalState nextRndNum ctx