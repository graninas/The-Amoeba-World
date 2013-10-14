{-# LANGUAGE FlexibleInstances #-}

module Test.Utils.Arbitraries where

import Test.QuickCheck
import Control.Monad
import System.Random
import qualified Linear as L
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.Foldable (foldlM)
import Control.Lens ((^?))

import GameLogic.Geometry
import GameLogic.Player
import GameLogic.Game
import GameLogic.World
import GameLogic.Object as O
import qualified GameLogic.GenericWorld as GW
import GameLogic.Evaluation hiding (suchThat)

instance Arbitrary (L.V3 Int) where
    arbitrary = liftM3 point arbitrary arbitrary arbitrary

instance Arbitrary Bound where
    arbitrary = oneof [ liftM  pointBound arbitrary
                      , liftM2 rectBound arbitrary arbitrary
                      , liftM2 circleBound arbitrary arbitrary
                      , return noBound ]

instance Arbitrary Layer where
    arbitrary = oneof $ map return layers
    
instance Arbitrary Direction where
    arbitrary = oneof $ map return directions

instance Arbitrary Player where
    arbitrary = liftM Player arbitrary

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
    
-- TODO: add another properties
propertiesCount = 11
instance Arbitrary O.Property where
    arbitrary = oneof [ liftM PNamed (arbitrary `suchThat` isNamedValid)
                      , liftM PDurability (arbitrary `suchThat` isResourceValid)
                      , liftM PBattery  (arbitrary `suchThat` isResourceValid)
                      , liftM POwnership arbitrary
                      --, liftM PDislocation arbitrary
                      , liftM PPassRestriction arbitrary
                      , liftM PAge (arbitrary `suchThat` isResourceValid)
                      , liftM PDirected arbitrary
                      , liftM PFabric arbitrary
                      , liftM PSelfDestructable arbitrary
                      , liftM PMoving arbitrary
                      , liftM PLayer arbitrary
                      --, liftM Collision arbitrary
                      ]

instance Arbitrary O.PropertyMap where
    arbitrary = sized pm
      where
            pm n | n <= 0 = return emptyPropertyMap
            pm n | n > 0  = let propArbitraries = replicate n arbitrary
                                dislocated = liftM PDislocation arbitrary : propArbitraries
                                em = return emptyPropertyMap
                                k = choose (0, propertiesCount)
                            in foldr (liftM3 insertProperty k) em dislocated

instance Arbitrary O.Object where
    arbitrary = liftM O.Object arbitrary

instance Arbitrary (GW.GenericMap Object) where
    arbitrary = sized gm
      where
        gm n | n <= 0 = return GW.emptyMap
        gm n = foldl (liftM2 GW.alterMapCell) (return GW.emptyMap) (objList n)

        objList :: Int -> [Gen (Point, Object)]
        objList 0 = []
        objList n = i : objList (n - 1)
        i = do obj <- arbitrary :: Gen Object
               let p = fromJust $ obj ^? dislocation . dislocationPoint
               return (p, obj)

            

instance Arbitrary World where
    arbitrary = liftM2 GW.GenericWorld wmGen b
      where
        wmGen = arbitrary :: Gen (GW.GenericMap O.Object)
        b = do wm <- wmGen
               return $ GW.worldMapBound wm
    
instance Arbitrary Game where
    arbitrary = sized g
      where
        g s = liftM2 Game arbitrary (return $ mkStdGen s)