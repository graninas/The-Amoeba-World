{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExistentialQuantification #-}

module GameLogic.Object where

import Data.Default
import Data.Monoid
import Control.Lens
import Control.Monad.State
import Prelude
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import GameLogic.Types
import GameLogic.Geometry
import GameLogic.Player

type Target = Point

data PassRestriction = NoFly | NoWalk | NoUndermine
  deriving (Show, Read, Eq)

data Fabric = Fabric { _energyCost :: Energy
                     , _production :: Object }
  deriving (Show, Read, Eq)

data SelfDestructable = SelfDestructOnTarget TargetPoint
  deriving (Show, Read, Eq)

data Moving = StraightMoving { _speed :: Speed
                             , _dir :: Direction }
  deriving (Show, Read, Eq)

data Layer = Underground | Ground | Sky
  deriving (Show, Read, Eq)

data Collision = Collision { _collidings :: Objects }
  deriving (Show, Read, Eq)

data Property = PNamed { __named :: String }
              | PDurability { __durability :: (Durability, Maybe MaxDurability) }
              | PBattery { __battery :: (Energy, Maybe EnergyCapacity) }
              | POwnership { __ownership :: Player }
              | PDislocation { __dislocation :: Point }
              | PPassRestriction { __passRestriction :: Seq.Seq PassRestriction }
              | PAge { __age :: (Age, Maybe Age) }
              | PDirected { __directed :: Direction }
              | PFabric { __fabric :: Fabric }
              | PSelfDestructable { __selfDestructable :: SelfDestructable }
              | PMoving { __moving :: Moving }
              | PLayer { __layer :: Layer }
              | PCollision { __collision :: Collision }
  deriving (Show, Read, Eq)

type PropertyKey = Int
type PropertyMap = Map.Map PropertyKey Property
data Object = Object { _propertyMap :: PropertyMap }
  deriving (Show, Read, Eq)
type Objects = [Object]

data PAccessor a = PAccessor { key :: PropertyKey
                             , constr :: a -> Property
                             }

insertProperty = Map.insert
emptyPropertyMap = Map.empty
empty = Object emptyPropertyMap
merge (Object pm1) (Object pm2) = Object $ Map.union pm1 pm2

-- | Access to base layer. Use it to setup base properties in layer 0.
baseLayer = 0
(|=) accessor v = do
    props <- get
    let oldPropMap = _propertyMap props
    let newPropMap = insertProperty (key accessor) (constr accessor v) oldPropMap
    put $ props { _propertyMap = newPropMap }

setProperty :: PAccessor a -> a -> State Object ()
setProperty = (|=)

-- Lenses
makeLenses ''Object
makeLenses ''Property
makeLenses ''Fabric
makeLenses ''SelfDestructable
makeLenses ''Moving
makeLenses ''Layer
makeLenses ''Collision

property k l = propertyMap . at k . traverse . l

-- Properties itself

isBoundedValid (a, Just b) = (a >= 0) && (a <= b)
isBoundedValid _ = True
boundedValidator r | isBoundedValid r = r
                   | otherwise        = error $ "Invalid bounded property: " ++ show r

notNullValidator s | null s = error "This property can't be null."
                   | otherwise = s

-- TODO: remove boilerplate with TH
namedA            = PAccessor 0    $ PNamed            .notNullValidator
durabilityA       = PAccessor 1    $ PDurability       .boundedValidator
batteryA          = PAccessor 2    $ PBattery          .boundedValidator
ownershipA        = PAccessor 3    $ POwnership        .id
passRestrictionA  = PAccessor 4    $ PPassRestriction  .id
dislocationA      = PAccessor 5    $ PDislocation      .id
ageA              = PAccessor 6    $ PAge              .boundedValidator
directedA         = PAccessor 7    $ PDirected         .id
fabricA           = PAccessor 8    $ PFabric           .id
selfDestructableA = PAccessor 9    $ PSelfDestructable .id
movingA           = PAccessor 10   $ PMoving           .id
layerA            = PAccessor 11   $ PLayer            .id
collisionA        = PAccessor 12   $ PCollision        .id

named            = property (key namedA)            _named
durability       = property (key durabilityA)       _durability
battery          = property (key batteryA)          _battery
ownership        = property (key ownershipA)        _ownership
passRestriction  = property (key passRestrictionA)  _passRestriction
dislocation      = property (key dislocationA)      _dislocation
age              = property (key ageA)              _age
directed         = property (key directedA)         _directed
fabric           = property (key fabricA)           _fabric
selfDestructable = property (key selfDestructableA) _selfDestructable
moving           = property (key movingA)           _moving
layer            = property (key layerA)            _layer
collision        = property (key collisionA)        _collision

passRestrictions = [NoFly, NoWalk, NoUndermine]

baseFabric :: Fabric
baseFabric = Fabric 0 def

selfDestructOnTarget = SelfDestructOnTarget

straightMoving = StraightMoving

underground = Underground
ground = Ground
sky = Sky
layers = [ underground, ground, sky ]

-- This should be used carefully.
instance Monoid Object where
    mempty  = empty
    mappend = merge

instance Default Object where
    def = empty

instance Default Fabric where
    def = baseFabric

instance Eq (PAccessor a) where
    acc1 == acc2 = key acc1 == key acc2

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