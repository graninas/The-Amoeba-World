{-# LANGUAGE FlexibleInstances #-}

module Test.Arbitraries where

import Test.QuickCheck
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
                      
instance Arbitrary Direction where
    arbitrary = oneof $ map return [ left, right, up, down,
                                     leftUp, rightDown, leftDown, rightUp ]