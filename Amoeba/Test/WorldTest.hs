{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Control.Lens as L
import qualified Data.Map as Map
import Test.QuickCheck
import Test.QuickCheck.All

import World.World
import World.Geometry
import Test.Data

w :: PropertiesMap
w = Map.fromList [(point1, PropertyLens 1)]

prop_cellTest = True
  where
    t = cellTest point2

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = runTests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."