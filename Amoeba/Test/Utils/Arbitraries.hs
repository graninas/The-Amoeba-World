{-# LANGUAGE FlexibleInstances #-}

module Test.Utils.Arbitraries where

import Test.QuickCheck
import Control.Monad
import qualified Linear as L
import qualified Data.List as List
import qualified Data.Sequence as Seq

import GameLogic.World.Geometry
import GameLogic.World.Player
import GameLogic.World.Properties as P

instance Arbitrary (L.V3 Int) where
    arbitrary = liftM3 point arbitrary arbitrary arbitrary
    
instance Arbitrary Bound where
    arbitrary = oneof [ liftM  pointBound arbitrary
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

-- TODO: add another properties
propertiesCount = 11
instance Arbitrary P.Property where
    arbitrary = oneof [ liftM PDurability (arbitrary `suchThat` isBoundedValid)
                      , liftM PBattery  (arbitrary `suchThat` isBoundedValid)
                      , liftM POwnership arbitrary
                      , liftM PDislocation arbitrary
                      , liftM PPassRestriction arbitrary ]

instance Arbitrary P.PropertyMap where
    arbitrary = sized pm
      where
            pm 0 = return emptyPropertiesMap
            pm n | n > 0 = let propArbitraries = replicate n arbitrary
                               em = return emptyPropertiesMap
                               k = choose (0, propertiesCount)
                           in foldr (liftM3 insertProperty k) em propArbitraries

instance Arbitrary P.Properties where
    arbitrary = sized props
      where
            props 0 = return emptyProperties
            props n | n > 0 = oneof [ liftM Properties arbitrary
                                    , liftM2 LayeredProperties subprops subprops ]
              where
                subprops = props (n `div` 2)
