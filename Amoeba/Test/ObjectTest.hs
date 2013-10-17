{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.All

import Test.Utils.GeometryData
import Test.Utils.GeometryArbitraries
import Test.Utils.ObjectArbitraries

import GameLogic.Object

prop_passable obj l = classify passable "passable"
                    . classify (not passable) "not passable" $ passable || not passable
  where
    passable = isPassable obj l

tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = runTests