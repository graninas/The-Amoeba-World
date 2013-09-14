{-# LANGUAGE TemplateHaskell #-}

module World.Properties where

import Data.Monoid
import Control.Lens
import qualified Data.Map as Map

import World.Geometry
import World.Player

type Target = Point
type Speed = Int
type Energy = Int
type Capacity = Energy
type Durability = Int
data Passability = AbleToFly | AbleToCreep | AbleToUndermine
  deriving (Show, Read, Eq)
type Passabilities = [Passability]

data Property 
            = PDurability { __durability :: (Durability, Durability) }
            | PPassabilities { __passabilities :: Passabilities }
            | PBattery { __battery :: (Capacity, Energy) }
            | POwnership { __ownership :: Player }
            | PDislocation { __dislocation :: Point }
  deriving (Show, Read, Eq)

type PropertyMap = Map.Map Int Property
data Properties = Properties { _propertyMap :: PropertyMap }
  deriving (Show, Read, Eq)

makeLenses ''Property
makeLenses ''Properties

  
mergeProperties (Properties ps1) (Properties ps2) = Properties $ Map.union ps1 ps2
noProperty = Properties Map.empty

kDurability = 1 :: Int
 
pPassabilities = 2
pBattery = 3
pOwnership = 4

{-
pDislocation :: Point -> Property
moving :: Direction -> Property

-- Can we add not Energy but generic resource, and 'energy' - one of resources?

consuming :: Energy -> Property
producing :: Energy -> Property

-- Property == Target?
transmission :: Property -> Property

permissibility :: Properties -> Property
interaction :: Property -> Property -> Property

-- (Properties -> Bool) == Condition
pursuit :: (Properties -> Bool) -> Target -> Speed -> Property

type ProductionAlg = Properties -> Property
type PlacementAlg = Property -> Properties -> Point -> Bool 

fabric :: ProductionAlg -> PlacementAlg -> Property
-}