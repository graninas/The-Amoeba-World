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
import GameLogic.World.Geometry

import Test.Utils.Data
import Test.Utils.Arbitraries

{-
world1:
karyon point1 player1 0

step world1 -> world2:
karyon point1 player1
plasma (around point1) player1

step world2 -> world3:
karyon point1 player1
plasma (around point1) player1
plasma (around (around point1)) player1
-}



runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = runTests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."