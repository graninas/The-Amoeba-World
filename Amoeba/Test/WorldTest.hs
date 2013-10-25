{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable (foldMap)
import Data.Monoid
import System.Random
import Control.Lens
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Either as E
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Test.QuickCheck
import Test.QuickCheck.All

import Test.Utils.GeometryData
import Test.Utils.GeometryArbitraries
import Test.Utils.ObjectArbitraries
import Test.Utils.GameArbitraries

import GameLogic.World
import GameLogic.Player
import GameLogic.Geometry as G
import GameLogic.Objects
import GameLogic.Object
import GameLogic.Scenario
import GameLogic.Game
import GameLogic.AI

blankGame = initialGame 1

plasma1    = putObject point1 $ plasma player1
plasma2    = putObject point2 $ plasma player1
soundWave1 = putObject point3 $ soundWave player1 G.left 10
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

deleteObject' obj = deleteObject (getP obj objectDislocation)
insertObject' obj = insertObject (getP obj objectDislocation) obj

moveObject' obj = let
    d = getP obj objectDislocation
    mv = getP obj moving
    newPoint = move d mv
    in objectDislocation .~ newPoint $ obj

moveObject'' :: Object -> Point -> Object
moveObject'' obj p = objectDislocation .~ p $ obj

trackMovingPath obj = let
    startPoint = getP obj objectDislocation
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

newRndNum :: State StdGen Int
newRndNum = do
    (newNum, newG) <- liftM random get
    put newG
    return newNum

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

--------

hasDislObj = Object {_propertyMap = M.fromList [(5,PDislocation {__dislocation = Dislocation {_dislocationPoint = G.point (-1) (-1) 1}}),(4,PAge {__age = Resource {_stock = 2, _capacity = Nothing}})]}
prop_hasDislocationTest = has dislocation hasDislObj

prop_hasDislocation game = classify (isGameEmpty game) "trivial" test
  where
    test = all hasDislocation objs
    objs = M.toList $ game ^. objects
    hasDislocation (_, obj) = has dislocation obj
    types1 = game :: Game

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
main = runTests