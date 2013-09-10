module World.Properties where

import World.Geometry

data Property = Moving
              | Energetics

type Properties = [Property]

type Target = Point
type Speed = Int
type Energy = Int
type Capacity = Energy
type Durability = Int

data Passability = AbleToFly | AbleToCreep | AbleToDrill

passability :: Passability -> Property
dislocation :: Point -> Property
battery :: Capacity -> Energy -> Property
ownership :: Player -> Property
durability :: Durability -> Property



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
