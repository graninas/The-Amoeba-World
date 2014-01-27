{-# LANGUAGE FlexibleInstances #-}
module Test.Utils.GeometryArbitraries where

import Test.QuickCheck
import Control.Monad
import System.Random
import qualified Linear as L

import GameLogic.Base.Geometry

instance Arbitrary (L.V3 Int) where
    arbitrary = liftM3 point arbitrary arbitrary arbitrary

instance Arbitrary Direction where
    arbitrary = oneof $ map return directions

instance Arbitrary Bound where
    arbitrary = oneof [ liftM  pointBound arbitrary
                      , liftM2 rectBound arbitrary arbitrary
                      , liftM2 circleBound arbitrary arbitrary
                      , return noBound ]
