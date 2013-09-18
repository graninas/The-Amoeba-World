{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExistentialQuantification #-}

module GameLogic.World.Properties where

import Data.Default
import Data.Monoid
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import GameLogic.World.Types
import GameLogic.World.Geometry
import GameLogic.World.Player

type Target = Point

data PassRestriction = NoFly | NoWalk | NoUndermine
  deriving (Show, Read, Eq)
  
data Fabric = Fabric { _energyCost :: Energy
                     , _production :: Properties }
  deriving (Show, Read, Eq)

data Property = PDurability { __durability :: (Durability, Durability) }
              | PBattery { __battery :: (Capacity, Energy) }
              | POwnership { __ownership :: Player }
              | PDislocation { __dislocation :: Point }
              | PPassRestriction { __passRestriction :: Seq.Seq PassRestriction }
              | PAge { __age :: Age }
              | PFabric { __fabric :: Fabric }
  deriving (Show, Read, Eq)

type PropertyKey = Int
type PropertyMap = Map.Map PropertyKey Property
data Properties = Properties { _propertyMap :: PropertyMap }
  deriving (Show, Read, Eq)

data PAccessor a = PAccessor { key :: PropertyKey
                             , constr :: a -> Property }

(|=) accessor v = do
    props <- get
    let oldPropMap = _propertyMap props
    let newPropMap = Map.insert (key accessor) (constr accessor v) oldPropMap
    put $ props { _propertyMap = newPropMap }

setProperty :: PAccessor a -> a -> State Properties ()
setProperty = (|=)

emptyProperties = Properties Map.empty
mergeProperties (Properties pm1) (Properties pm2) = Properties $ Map.union pm1 pm2

-- Lenses
makeLenses ''Properties
makeLenses ''Property
makeLenses ''Fabric

property k l = propertyMap . at k . traverse . l

-- Properties itself

durabilityA      = PAccessor 1 PDurability
batteryA         = PAccessor 2 PBattery
ownershipA       = PAccessor 3 POwnership
passRestrictionA = PAccessor 4 PPassRestriction
dislocationA     = PAccessor 5 PDislocation
ageA             = PAccessor 6 PAge
fabricA          = PAccessor 7 PFabric

durability      = property (key durabilityA)      _durability
battery         = property (key batteryA)         _battery
ownership       = property (key ownershipA)       _ownership
passRestriction = property (key passRestrictionA) _passRestriction
dislocation     = property (key dislocationA)     _dislocation
age             = property (key ageA)             _age
fabric          = property (key fabricA)          _fabric

passRestrictions = [NoFly, NoWalk, NoUndermine]

baseFabric :: Fabric
baseFabric = Fabric 0 def

instance Default Properties where
    def = emptyProperties

instance Default Fabric where
    def = baseFabric


{-
passRestriction = property $ PAccessor 5 _passRestriction
noFly = passRestriction . ix flyIndex .~ NoFly
noWalk = passRestriction . ix walkIndex .~ NoWalk
noUndermine = passRestriction . ix undermineIndex .~ NoUndermine
-}
{-
noFly = undefined
noWalk = undefined
noUndermine = undefined
noPass :: State Properties ()
noPass = do
    noFly
    noWalk
    noUndermine

fullPassable :: State Properties ()
fullPassable = do
    ableToFly .= True
    ableToWalk .= True
    ableToUndermine .= True
-}
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