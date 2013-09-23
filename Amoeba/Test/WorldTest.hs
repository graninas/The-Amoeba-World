{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable (foldMap)
import Data.Monoid
import Control.Lens
import Control.Monad.State
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Test.QuickCheck
import Test.QuickCheck.All

import Test.Utils.Data
import Test.Utils.Arbitraries

import GameLogic.World.World
import GameLogic.World.Player
import GameLogic.World.Geometry
import GameLogic.World.Objects
import GameLogic.World.Properties

instance Monoid r => Monoid (Accessor r a) where
  mempty = Accessor mempty
  mappend (Accessor a) (Accessor b) = Accessor $ a <> b

instance Monoid Properties where
    mempty  = emptyProperties
    mappend = mergeProperties

blankGame = initialGame 1

plasma1    = objects . at point1 ?~ plasma player1 point1
plasma2    = objects . at point2 ?~ plasma player1 point2
soundWave1 = objects . at point3 ?~ soundWave player1 left 10 point3
laserBeam1 = objects . at point4 ?~ laserBeam player2 up 200 point4
testGame   = testGame'
  where
    g = blankGame & plasma1 & plasma2 & soundWave1 & laserBeam1
    w = g ^. world
    testGame' = over world refreshWorldBound g

deleteObject p       = world %= deleteCell p
insertObject p props = world %= insertCell p props

getP obj p = obj ^. singular p

deleteObject' obj = deleteObject (getP obj dislocation)
insertObject' obj = insertObject (getP obj dislocation) obj

moveObject' obj = let
    d = getP obj dislocation
    mv = getP obj moving
    newPoint = move d mv
    in dislocation .~ newPoint $ obj

moveObject :: Object -> State Game (Either Game Game)
moveObject obj | hasn't moving obj = liftM Left get
moveObject obj = do
        let newObj = moveObject' obj
        deleteObject' obj
        insertObject' newObj
        g <- get
        return $ Right g

selectScenario accessor | accessor == movingA = Just moveObject
                        | otherwise           = Nothing

evalScenario Nothing _ g = Left g
evalScenario (Just scenario) obj g = evalState (scenario obj) g

tryRun accessor obj g = let scenario = selectScenario accessor
                        in evalScenario scenario obj g

applyRunResult :: Either Game Game -> Game
applyRunResult (Right g) = g
applyRunResult (Left g)  = g

runScenarios :: Point -> Object -> Game -> Game
runScenarios k obj g = let runResult = tryRun movingA obj g
                       in applyRunResult runResult

evalScenarios :: Game -> Game
evalScenarios g = M.foldrWithKey runScenarios g (g ^. world.worldMap)

insertOnly :: Game -> Game
insertOnly = execState insert'
  where
    insert' = insertObject point5 (plasma player1 point5)

insertAndDelete :: Game -> Game
insertAndDelete = execState insertAndDelete'
  where
    insertAndDelete' = do
            insertObject point5 (plasma player1 point5)
            deleteObject point5

moveSingleObject :: Point -> Game -> Game
moveSingleObject p g = flip execState g $ do
    obj <- use $ objects . ix p
    when (has moving obj) $ do
        res <- moveObject obj
        return ()

prop_testGame = testGame /= blankGame
prop_world1 p pl seed = game /= initialGame seed
    where
        game = objects . at p ?~ ps $ initialGame seed
        ps = plasma pl p

prop_insertOnly1       = insertOnly blankGame /= blankGame
prop_insertOnly2       = insertOnly testGame  /= testGame
prop_insertAndDelete1  = insertAndDelete blankGame == blankGame
prop_insertAndDelete2  = insertAndDelete testGame == testGame
prop_moveSingleObject1 = moveSingleObject point3 blankGame == blankGame
prop_moveSingleObject2 = moveSingleObject point3 testGame `notElem` [testGame, blankGame]

prop_evalScenarios = evalScenarios testGame == foldr moveSingleObject testGame [point3, point4]

tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

printTestData = do
    print "Blank game:"
    print blankGame
    putStrLn ""
    print "Test game:"
    print testGame
    putStrLn ""
    print "InsertAndDelete blankGame:"
    print $ insertAndDelete blankGame
    putStrLn ""
    print "InsertAndDelete testGame:"
    print $ insertAndDelete testGame
    putStrLn ""
    print "MoveSingleObject blankGame:"
    print $ moveSingleObject point3 blankGame
    putStrLn ""
    print "MoveSingleObject testGame:"
    print $ moveSingleObject point3 testGame

main :: IO ()
main = do
    runTests
    printTestData
    putStrLn "\nMoving all movable objects:"
    print $ evalScenarios testGame