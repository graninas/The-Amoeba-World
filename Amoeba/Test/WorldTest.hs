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

moveObject :: Object -> State Game ()
moveObject obj = do
        let newObj = moveObject' obj
        deleteObject' obj
        insertObject' newObj


moveObjects = do
    obj <- use $ objects . ix point3
    when (has moving obj) (moveObject obj)



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

moveSingleObject :: Game -> Game
moveSingleObject = execState moveObjects

prop_testGame = testGame /= blankGame
prop_world1 p pl seed = game /= initialGame seed
    where
        game = objects . at p ?~ ps $ initialGame seed
        ps = plasma pl p

prop_insertOnly1       = insertOnly blankGame /= blankGame
prop_insertOnly2       = insertOnly testGame  /= testGame
prop_insertAndDelete1  = insertAndDelete blankGame == blankGame
prop_insertAndDelete2  = insertAndDelete testGame == testGame
prop_moveSingleObject1 = moveSingleObject blankGame == blankGame
prop_moveSingleObject2 = moveSingleObject testGame `notElem` [testGame, blankGame]

tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = do
    runTests
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
    print $ moveSingleObject blankGame
    putStrLn ""
    print "MoveSingleObject testGame:"
    print $ moveSingleObject testGame
    putStrLn ""