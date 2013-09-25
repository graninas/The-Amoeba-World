{-# LANGUAGE FlexibleInstances #-}

module Test.Utils.Arbitraries where

import Test.QuickCheck
import Control.Monad
import qualified Linear as L
import qualified Data.List as List
import qualified Data.Sequence as Seq

import GameLogic.Geometry
import GameLogic.Player
import GameLogic.Object as O

instance Arbitrary (L.V3 Int) where
    arbitrary = liftM3 point arbitrary arbitrary arbitrary

instance Arbitrary Bound where
    arbitrary = oneof [ liftM  pointBound arbitrary
                      , liftM2 rectBound arbitrary arbitrary
                      , liftM2 circleBound arbitrary arbitrary
                      , return noBound ]

instance Arbitrary Direction where
    arbitrary = oneof $ map return directions

instance Arbitrary Player where
    arbitrary = liftM Player arbitrary

instance Arbitrary PassRestriction where
    arbitrary = oneof $ map return passRestrictions

instance Arbitrary (Seq.Seq PassRestriction) where
    arbitrary = oneof $ map (return . Seq.fromList) s
        where
            s = List.subsequences passRestrictions

instance Arbitrary Fabric where
    arbitrary = liftM2 Fabric arbitrary arbitrary

instance Arbitrary Moving where
    arbitrary = liftM2 StraightMoving arbitrary arbitrary

instance Arbitrary SelfDestructable where
    arbitrary = liftM SelfDestructOnTarget arbitrary

instance Arbitrary Layer where
    arbitrary = oneof $ map return layers

-- TODO: add another properties
propertiesCount = 12
instance Arbitrary O.Property where
    arbitrary = oneof [ liftM PNamed (arbitrary `suchThat` (not.null))
                      , liftM PDurability (arbitrary `suchThat` isBoundedValid)
                      , liftM PBattery  (arbitrary `suchThat` isBoundedValid)
                      , liftM POwnership arbitrary
                      , liftM PDislocation arbitrary
                      , liftM PPassRestriction arbitrary
                      , liftM PAge (arbitrary `suchThat` isBoundedValid)
                      , liftM PDirected arbitrary
                      , liftM PFabric arbitrary
                      , liftM PSelfDestructable arbitrary
                      , liftM PMoving arbitrary
                      , liftM PLayer arbitrary
                      ]

instance Arbitrary O.PropertyMap where
    arbitrary = sized pm
      where
            pm 0 = return emptyPropertyMap
            pm n | n > 0 = let propArbitraries = replicate n arbitrary
                               em = return emptyPropertyMap
                               k = choose (0, propertiesCount)
                           in foldr (liftM3 insertProperty k) em propArbitraries

instance Arbitrary O.Object where
    arbitrary = liftM O.Object arbitrary