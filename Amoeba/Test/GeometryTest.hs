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
    
instance Arbitrary Bound where
    arbitrary = oneof [ liftM pointBound arbitrary
                      , liftM2 rectBound arbitrary arbitrary
                      , liftM2 circleBound arbitrary arbitrary
                      , return noBound ]

prop_inSegmentBounded _ = inSegment (minBound :: Int, maxBound :: Int)
prop_inSegmentSwap (x1, x2) y = inSegment (x1, x2) y == inSegment (x2, x1) y

prop_inBoundsEmpty p = not (inBounds p [])
prop_inBoundsSelf1 p _ = inBounds p [pointBound p]
prop_inBoundsSelf2 p bs = inBounds p $ pointBound p : bs 
prop_inBoundsNoBounds p bs = inBounds p $ noBound : bs

point1 = point 0 (-10) 0
point2 = point 3 (-3) 0
point3 = point (-10) 0 0
point4 = point 5 (-3) 0
point5 = point (-10) (-3) 0
point6 = point 5 0 0

rect1 = rectBound point2 point6
rect2 = rectBound point5 point6

prop_rectBound1 = rectBound point1 point2 == Rectangled point1 point2
prop_rectBound2 = rectBound point3 point4 == Rectangled point5 point6
prop_rectInRect = inRect rect1 rect2

prop_updateRectBound ps = rect1 `inRect` newRect
  where
    newRect = foldr updateRectBound rect1 ps

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = runTests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."