module World.Properties where

import World.Geometry

data Property = Moving
              | Energetics

type Properties = [Property]

type Target = Point
type Speed = Int
type Energy = Int
type Durability = Int

dislocation :: Point -> Property
moving :: Direction -> Property

-- (Properties -> Bool) == Condition
pursuit :: (Properties -> Bool) -> Target -> Speed -> Property

energetics :: Energy -> Property

ownership :: Player -> Property
interaction :: Property -> Property -> Property

type ProductionAlg = Properties -> Property
type PlacementAlg = Property -> Properties -> Point -> Bool 

durability :: Durability -> Property
fabric :: ProductionAlg -> PlacementAlg -> Property
