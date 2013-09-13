module World.Properties where

import World.Geometry

data Property = PEmpty
              | PDurability Durability Structure
              | PPassability 
              | PDislocation Point
              | PBattery Capacity Energy
              | POwnership Player

type Properties = [Property]

type Target = Point
type Speed = Int
type Energy = Int
type Capacity = Energy
type Durability = Int
type Structure = Durability

data Passability = AbleToFly | AbleToCreep | AbleToDrill
type Passabilities = [Passability]

pDurability :: Durability -> Structure -> Property
pPassability :: Passability -> Property
pBattery :: Capacity -> Energy -> Property
pOwnership :: Player -> Property
pEmpty :: Property


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
