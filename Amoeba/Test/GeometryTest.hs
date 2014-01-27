{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.All
import Control.Monad

import Test.Utils.GeometryArbitraries
import Test.Utils.GeometryData

import GameLogic.Base.Geometry

prop_inSegmentBounded _ = inSegment (minBound :: Int, maxBound :: Int)
prop_inSegmentSwap (x1, x2) y = inSegment (x1, x2) y == inSegment (x2, x1) y

prop_inBoundsEmpty p = not (inBounds p [])
prop_inBoundsSelf1 p _ = inBounds p [pointBound p]
prop_inBoundsSelf2 p bs = inBounds p $ pointBound p : bs 
prop_inBoundsNoBounds p bs = inBounds p $ noBound : bs

prop_rectBound1 = rectBound point1 point2 == Rectangled point1 point2
prop_rectBound2 = rectBound point3 point4 == Rectangled point5 point6
prop_rectInRect = inRect rect1 rect2

prop_validateDirections = all (==zeroPoint) mappedDirPairs
    where
        mappedDirPairs = map (\(a, b) -> addPoint (toVector a) (toVector b)) oppositeDirections

prop_updateRectBound ps = rect1 `inRect` newRect
  where
    newRect = foldr updateRectBound rect1 ps

prop_occupiedArea1 p ps = occupiedArea (p:ps) == foldr updateRectBound (rectBound p p) ps
prop_occupiedArea2 ps = let
    area = occupiedArea ps
    in all (\p -> inBounds p [area]) ps

prop_movePoint1 n p dir = classify isTrivial "trivial" res
    where moved1 = moveStraight n p dir
          moved2 = moveStraight n moved1 (opposite dir)
          isTrivial = n == 0
          res = if isTrivial then (moved1 == p) && (moved2 == p)
                             else (moved1 /= p) && (moved2 == p)

prop_movePoint2 n p dir = classify isTrivial "trivial" res
    where moved1 = moveStraight n p dir
          moved2 = moveStraight (negate n) moved1 dir
          isTrivial = n == 0
          res = if isTrivial then (moved1 == p) && (moved2 == p)
                             else (moved1 /= p) && (moved2 == p)

prop_neighbours1 p1 dir = testNeighbours dir && testNeighbours (opposite dir)
  where
    testNeighbours d = advance p1 d `elem` neighbours p1

prop_neighbours2 p1 dir (NonZero dist) = ( classify far "not neighbours"
                                         . classify near  "neighbours") (far || near)
  where
    far = (abs dist > 1) && not test
    near = (abs dist <= 1) && test
    test = testNeighbours dir && testNeighbours (opposite dir)
    testNeighbours d = moveStraight (abs dist) p1 d `elem` neighbours p1

prop_areNeighbours p1 p2 = ( classify (not ns) "not neighbours"
                           . classify ns "neighbours"
                           . classify same "same point") (not ns || ns || same)
  where
    same = p1 == p2
    ns = areNeighbours p1 p2
    

tests :: IO Bool
tests = $quickCheckAll

runTests = tests >>= \passed -> putStrLn $
  if passed then "All tests passed."
            else "Some tests failed."

main :: IO ()
main = runTests