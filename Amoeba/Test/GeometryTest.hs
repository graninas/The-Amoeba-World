{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad

import qualified Linear as L
import World.Geometry

instance Arbitrary (L.V3 Int) where
    arbitrary = liftM3 point arbitrary arbitrary arbitrary

prop_inSegmentBounded _ = inSegment (minBound :: Int, maxBound :: Int)
prop_inSegmentSwap (x1, x2) y = inSegment (x1, x2) y == inSegment (x2, x1) y

prop_inBoundsEmpty p = not (inBounds p [])
prop_inBoundsSelf p _ = inBounds p [pointBound p]

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = runTests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."
                                         

tests = [ prop_inSegmentBounded
        , prop_inSegmentSwap
        ]