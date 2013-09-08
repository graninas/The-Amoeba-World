{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad

import Test.Arbitraries
import Test.Data

import World.Geometry

prop_inSegmentBounded _ = inSegment (minBound :: Int, maxBound :: Int)
prop_inSegmentSwap (x1, x2) y = inSegment (x1, x2) y == inSegment (x2, x1) y

prop_inBoundsEmpty p = not (inBounds p [])
prop_inBoundsSelf1 p _ = inBounds p [pointBound p]
prop_inBoundsSelf2 p bs = inBounds p $ pointBound p : bs 
prop_inBoundsNoBounds p bs = inBounds p $ noBound : bs

prop_rectBound1 = rectBound point1 point2 == Rectangled point1 point2
prop_rectBound2 = rectBound point3 point4 == Rectangled point5 point6
prop_rectInRect = inRect rect1 rect2

prop_updateRectBound ps = rect1 `inRect` newRect
  where
    newRect = foldr updateRectBound rect1 ps

prop_occupiedArea1 p ps = occupiedArea (p:ps) == foldr updateRectBound (rectBound p p) ps
prop_occupiedArea2 ps = let
    area = occupiedArea ps
    in all (\p -> inBounds p [area]) ps

prop_shift1 = shiftNone zeroPoint == zeroPoint

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = runTests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."