{-# LANGUAGE TemplateHaskell #-}

module World.Properties where

import World.Geometry
import World.Player

import qualified Data.Map as Map
import Control.Lens

type Target = Point
type Speed = Int
type Energy = Int
type Capacity = Energy
type Durability = Int
data Passability = AbleToFly | AbleToCreep | AbleToUndermine
type Passabilities = [Passability]

data PropertyDef = PDurability { __durability :: (Durability, Durability) }
                 | PPassabilities { __passabilities :: Passabilities }
                 | PBattery { __battery :: (Capacity, Energy) }
                 | POwnership { __ownership :: Player }
                 | PDislocation { __dislocation :: Point }

makeLenses ''PropertyDef

type Properties = Map.Map String PropertyDef

pDurability = "durability"
pPassabilities = "passability"
pBattery = "battery"
pOwnership = "ownership"

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