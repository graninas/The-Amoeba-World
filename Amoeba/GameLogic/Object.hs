{-# LANGUAGE TemplateHaskell #-}
module GameLogic.Object where

import Control.Lens

import GameLogic.Player

{-
data PlacementAlg = PlaceToNearestEmptyCell
                  | PlaceToPoint Point
  deriving (Show, Read, Eq)

data Fabric = Fabric { _energyCost :: Energy
                     , _scheme :: Object
                     , _producing :: Bool
                     , _placementAlg :: PlacementAlg }
  deriving (Show, Read, Eq)

data Moving = StraightMoving { _speed :: Speed
                             , _dir :: Direction }
  deriving (Show, Read, Eq)
-}

data Resource a = Resource { _stock :: a
                           , _capacity :: Maybe a }
  deriving (Show, Read, Eq)

  
-- data Property = PNamed { __named :: Named }                                     -- < improper property. Use 'flyweight' pattern.
--              | PCollision { __collision :: Collision }                         -- < ???
--              | PObjectId { __objectId :: Int }
--              | POwnership { __ownership :: Player }
--              | PDislocation { __dislocation :: Dislocation }                   
              -- | PDirected { __directed :: Direction }                           -- < improper property. Should be used in other properties (moving, energy conducting etc.)
              -- | PLayer { __layer :: Layer }                                     -- < can be replaced by dislocation's Z coordinate
--              | PAge { __age :: Resource Age }
--              | PBattery { __battery :: Resource Energy }                       -- < (interaction with) energy consumers and producers (i.e. conductors)
--              | PDurability { __durability :: Resource Durability }             -- < (interaction with) damage resistance
              -- | PPassRestriction { __passRestriction :: PassRestriction }       -- < (interaction with) obstacle for movables
              -- | PSelfDestructable { __selfDestructable :: SelfDestructable } -- replaced by Decay (распад)
--              | PFabric { __fabric :: Fabric }                                  -- < interaction with map, dislocation. Energy consumer, object producer

           -- | PMoving { __moving :: Moving }                                  -- < (e) interaction with dislocation, time
           -- | PEnergyLeak { __energyLeak :: EnergyLeak }                      -- < (e) interaction with energy, time
           -- | PDecay { __decay :: Decay }                                     -- < (e) interaction with durability, time
           -- | PIndestructable {}                                              -- < (e) interaction with effects? durability, time
              
           -- | PTreason { __treason :: Treason }                               -- < interaction with ownership, age (lifebounds) / dislocation / time
           -- | PTeleport { __teleport :: Teleport }                            -- < interaction with dislocation
           -- | PTimeMachine { __timeMachine :: TimeMachine }                   -- < interaction with age (lifebounds)
           -- | PConduct { __conduct :: Conduct }                               -- < interaction with energy, map (neighbour conductors), time
           
--  deriving (Show, Read, Eq)


data Object = Object {
                        -- Properties:
                         _objectId :: Int               -- static property
                       , _objectType :: Int             -- predefined property
                       
                       -- Runtime properties, resources:
                       , _ownership :: Player           -- runtime property... or can be effect!

                       , _lifebound  :: Resource Int    -- runtime property
                       , _durability :: Resource Int    -- runtime property
                       , _energy     :: Resource Int    -- runtime property
                       
                       

                       -- , __effects :: Effects'
                       -- , __actions :: Actions' 
                       } -- Эффекты и действия не обязательно должны быть здесь. Они могут находиться и во вне, например, в списке эффектов/действий для объекта или куска карты. Буквально, на объект "навешаны" эффекты.
  deriving (Show, Read, Eq)

type Objects = [Object]

-- Lenses
makeLenses ''Object
makeLenses ''Resource

--makeLenses ''PlacementAlg
--makeLenses ''Fabric
--makeLenses ''Moving

-- Properties itself

isResourceValid (Resource c (Just m)) = (c >= 0) && (c <= m)
isResourceValid (Resource c Nothing)  = c >= 0
resourceValidator r | isResourceValid r = r
                    | otherwise         = error $ "Invalid resource property: " ++ show r
toResource (c, mbM) = resourceValidator $ Resource c mbM

charded :: Resource Energy -> Bool
charged (Resource c _) = c > 0

batteryCharge = battery.stock

placeToNearestEmptyCell = PlaceToNearestEmptyCell
placeToPoint = PlaceToPoint

straightMoving = StraightMoving

-- System funcs
-- adjust = foldr ($) -- ??? is it important or not?

    