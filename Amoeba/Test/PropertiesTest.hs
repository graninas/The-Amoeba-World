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

prop_durability d = ps == expected
    where
        durKey = key durabilityA
        expected = Properties $ Map.fromList [(durKey, constr durabilityA d)]
        st = durabilityA |= d
        ps = execState st emptyProperties

prop_battery b = ps == expected
    where
        batKey = key batteryA
        expected = Properties $ Map.fromList [(batKey, constr batteryA b)]
        st = batteryA |= b
        ps = execState st emptyProperties

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = runTests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."