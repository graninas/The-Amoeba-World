{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable (foldMap)
import Data.Monoid
import System.Random
import Control.Lens
import Control.Monad.State
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
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
import GameLogic.Game
import GameLogic.AI

instance Monoid r => Monoid (Accessor r a) where
  mempty = Accessor mempty
  mappend (Accessor a) (Accessor b) = Accessor $ a <> b

blankGame = initialGame 1

putObject p objConstr = objects . at p ?~ objConstr p

plasma1    = putObject point1 $ plasma player1
plasma2    = putObject point2 $ plasma player1
soundWave1 = putObject point3 $ soundWave player1 left 10
laserBeam1 = putObject point4 $ laserBeam player2 up 200
karyon1    = putObject point5 $ karyon player2
testGame   = testGame'
  where
    g = blankGame & plasma1 & plasma2 & soundWave1 & laserBeam1
    w = g ^. world
    testGame' = over world refreshWorldBound g

deleteObject p       = world %= deleteCell p
insertObject p props = world %= insertCell p props

-- this function can be optimized by saving property information anywhere.
getP obj p = obj ^. singular p

deleteObject' obj = deleteObject (getP obj dislocation)
insertObject' obj = insertObject (getP obj dislocation) obj

moveObject' obj = let
    d = getP obj dislocation
    mv = getP obj moving
    newPoint = move d mv
    in dislocation .~ newPoint $ obj

moveObject'' :: Object -> Point -> Object
moveObject'' obj p = dislocation .~ p $ obj

trackMovingPath obj = let
    startPoint = getP obj dislocation
    mv = getP obj moving
    points = path startPoint mv
    in track points withLast moveObject'' obj

moveObject :: Object -> State Game (Either Game Game)
moveObject obj | hasn't moving obj = liftM Left get
moveObject obj | hasn't named obj = liftM Left get
moveObject obj = do
        let newObj = trackMovingPath obj
        deleteObject' obj
        insertObject' newObj
        g <- get
        return $ Right g

selectScenario accessor | accessor == movingA = Just moveObject
                        | otherwise           = Nothing

evalScenario Nothing _ g = Left g
evalScenario (Just scenario) obj g = evalState (scenario obj) g

activate prop obj g = let scenario = selectScenario prop
                      in evalScenario scenario obj g

applyRunResult :: Either Game Game -> Game
applyRunResult (Right g) = g
applyRunResult (Left g)  = g

runScenarios :: Point -> Object -> Game -> Game
runScenarios k obj g = let runResult1 = activate movingA obj g
                       in applyRunResult runResult1

step :: Game -> Game
step g = M.foldrWithKey runScenarios g (g ^. world.worldMap)


newRndNum :: State StdGen Int
newRndNum = do
    (newNum, newG) <- liftM random get
    put newG
    return newNum

data Logic = Logic

logic = undefined

apply lr g = lr
eval l rnd objs = undefined

evaluate :: Logic -> Game -> Game
evaluate l g = let logicResult = eval l (g ^. rndGen) (g ^. objects)
               in apply logicResult g

step' :: Game -> Game
step' g = evaluate (g ^. logic) g



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

prop_evalScenarios = step testGame == foldr moveSingleObject testGame [point3, point4]


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
    print $ step testGame