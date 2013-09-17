{-# LANGUAGE FlexibleInstances #-}

module Test.Utils.Arbitraries where

import Test.QuickCheck
import Control.Monad
import qualified Linear as L
import qualified Data.List as List
import qualified Data.Sequence as Seq

import World.Geometry
import World.Player
import World.Properties as P

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
                                     
instance Arbitrary Player where
    arbitrary = liftM Player arbitrary

instance Arbitrary PassRestriction where
    arbitrary = oneof $ map return passRestrictions

instance Arbitrary (Seq.Seq PassRestriction) where
    arbitrary = oneof $ map (return . Seq.fromList) s
        where
            s = List.subsequences passRestrictions

instance Arbitrary P.Property where
    arbitrary = oneof [ liftM PDurability arbitrary
                      , liftM PBattery arbitrary
                      , liftM POwnership arbitrary
                      , liftM PDislocation arbitrary
                      , liftM PPassRestriction arbitrary ]
