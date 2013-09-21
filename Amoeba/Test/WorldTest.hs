{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable (foldMap)
import Data.Monoid
import Control.Lens
import Control.Monad.State
import Control.Applicative ((<$>), (<*>))
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

plasma1 = objects . at point1 ?~ plasma player1 point1
plasma2 = objects . at point2 ?~ plasma player1 point2
soundWave1 = objects . at point3 ?~ soundWave player1 left (movePoint 10 point3 left) point3
testGame = blankGame & plasma1 & plasma2 & soundWave1

prop_testGame = testGame /= blankGame
prop_world1 p pl seed = game /= initialGame seed
    where
        game = objects . at p ?~ ps $ initialGame seed
        ps = plasma pl p

--prop_moveObjects = movedObjectsGame /= blankGame
--  where
--    movedObjectsGame = execState moveObjects testGame

tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = do
    runTests
