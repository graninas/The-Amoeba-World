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
soundWave1 = objects . at point3 ?~ soundWave player1 left (moveStraight 10 point3 left) point3
testGame   = testGame'
  where
    g = blankGame & plasma1 & plasma2 & soundWave1
    w = g ^. world
    testGame' = over world refreshWorldBound g

-- foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
{-
dereference :: Point -> State Game Point
dereference p = do
    <- 
-}

deleteObject p = world %= deleteCell p
insertObject p props = world %= insertCell p props

moveObject :: Point -> State Game ()
moveObject p = do
    props <- use $ objects . ix p
    let f = move p :: Moving -> Point
    let mv = props ^. singular moving :: Moving
    when (has moving props) $ do
        let newPoint = f mv
        let newProps = dislocation .~ newPoint $ props
        deleteObject p
        insertObject newPoint newProps
    return ()

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
moveSingleObject = execState (moveObject point3)

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