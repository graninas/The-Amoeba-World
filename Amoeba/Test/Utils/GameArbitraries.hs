{-# LANGUAGE FlexibleInstances #-}

module Test.Utils.GameArbitraries where

import Test.QuickCheck
import Control.Monad
import System.Random
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.Foldable (foldlM)
import Control.Lens ((^?), (^.))

import GameLogic.Geometry
import GameLogic.Player
import GameLogic.Game
import GameLogic.World
import GameLogic.Object as O
import qualified GameLogic.GenericWorld as GW
import GameLogic.Evaluation hiding (suchThat)

import Test.Utils.GeometryArbitraries
import Test.Utils.ObjectArbitraries

-- | All objects will have dislocation property.
instance Arbitrary (GW.GenericMap Object) where
    arbitrary = sized gm
      where
        gm n | n <= 0 = return GW.emptyMap
        gm n = foldl (liftM2 GW.alterMapCell) (return GW.emptyMap) (objList n)
        objList :: Int -> [Gen (Point, Object)]
        objList 0 = []
        objList n = i : objList (n - 1)
        i = do obj <- arbitrary :: Gen Object
               rndP <- arbitrary :: Gen Point
               let rndDislK = key dislocationA
               let dislProp = PDislocation $ Dislocation rndP
               let dislocatedPropMap = insertProperty rndDislK dislProp (obj ^. propertyMap)
               let (p, obj') = case obj ^? objectDislocation of
                        Just dp -> (dp, obj)
                        Nothing -> (rndP, Object dislocatedPropMap)
               return (p, obj')

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