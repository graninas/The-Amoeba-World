{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Monoid
import Data.Default
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Control.Monad.State
import Test.QuickCheck
import Test.QuickCheck.All

import GameLogic.World.Player
import GameLogic.World.Properties
import GameLogic.World.Objects
import GameLogic.World.Scenario
import GameLogic.World.World
import GameLogic.World.Geometry

import Test.Utils.Data
import Test.Utils.Arbitraries





tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = do
    runTests
    
    print $ soundWaveFabric player1 left point1