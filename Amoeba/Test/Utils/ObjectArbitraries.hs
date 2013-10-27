{-# LANGUAGE FlexibleInstances #-}
module Test.Utils.ObjectArbitraries where

import Test.QuickCheck
import Control.Monad
import System.Random
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.Foldable (foldlM)
import Control.Lens ((^?), (^.))

import GameLogic.Geometry
import GameLogic.Types
import GameLogic.Player
import GameLogic.Object as O

import Test.Utils.GeometryArbitraries

instance Arbitrary Player where
    arbitrary = liftM Player arbitrary

instance Arbitrary Layer where
    arbitrary = oneof $ map return layers

instance Arbitrary PassRestriction where
    arbitrary = liftM PassRestriction arbitrary

instance Arbitrary (Set.Set Layer) where
    arbitrary = oneof $ map (return . Set.fromList) s
        where
            s = List.subsequences layers

instance Arbitrary Named where
    arbitrary = liftM Named arbitrary

instance Arbitrary PlacementAlg where
    arbitrary = oneof [ return PlaceToNearestEmptyCell
                      , liftM PlaceToPoint arbitrary ]

instance Arbitrary Fabric where
    arbitrary = liftM4 Fabric arbitrary arbitrary arbitrary arbitrary

instance Arbitrary Moving where
    arbitrary = liftM2 StraightMoving arbitrary arbitrary

instance Arbitrary SelfDestructable where
    arbitrary = liftM SelfDestructOnTarget arbitrary

instance Arbitrary (Resource Int) where
    arbitrary = liftM2 Resource arbitrary arbitrary

instance Arbitrary Dislocation where
    arbitrary = liftM Dislocation arbitrary

-- Enumeration of properties. Order should be corresponded with property keys.
-- TODO: add another properties
propertyArbitraries = [ liftM PNamed (arbitrary `suchThat` isNamedValid)
                      , liftM PDurability (arbitrary `suchThat` isResourceValid)
                      , liftM PBattery  (arbitrary `suchThat` isResourceValid)
                      , liftM POwnership arbitrary
                      , liftM PPassRestriction arbitrary
                      , liftM PDislocation arbitrary
                      , liftM PAge (arbitrary `suchThat` isResourceValid)
                      , liftM PDirected arbitrary
                      , liftM PFabric arbitrary
                      , liftM PSelfDestructable arbitrary
                      , liftM PMoving arbitrary
                      , liftM PLayer arbitrary
                      --, liftM Collision arbitrary
                      ]

instance Arbitrary O.Property where
    arbitrary = oneof propertyArbitraries

data ValidProperty = ValidProperty O.PropertyKey O.Property

-- Validated property key.
instance Arbitrary ValidProperty where
    arbitrary = sized vp
      where
            propIdx k = k `mod` length propertyArbitraries
            propArb k = propertyArbitraries !! propIdx k
            vp n = liftM2 ValidProperty (return $ propIdx n) (propArb n)

instance Arbitrary O.PropertyMap where
    arbitrary = sized pm
      where
            pm n = let validProps = replicate n arbitrary :: [Gen ValidProperty]
                       insertProperty' gVP mM = do
                            (ValidProperty k p) <- gVP
                            m <- mM
                            return $ insertProperty k p m
                   in foldr insertProperty' (return emptyPropertyMap) validProps

instance Arbitrary O.Object where
    arbitrary = liftM O.Object arbitrary

