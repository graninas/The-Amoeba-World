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
import qualified Data.Set as Set
import qualified Data.List as L (nub)

import GameLogic.Types
import GameLogic.Geometry
import GameLogic.Player

instance Monoid r => Monoid (Accessor r a) where
  mempty = Accessor mempty
  mappend (Accessor a) (Accessor b) = Accessor $ a <> b

type Target = Point

data PassRestriction = NoFly | NoWalk | NoUndermine
  deriving (Show, Read, Eq, Ord)

data PlacementAlg = PlaceToNearestEmptyCell
                  | PlaceToPoint Point
  deriving (Show, Read, Eq)

data Fabric = Fabric { _energyCost :: Energy
                     , _scheme :: Object
                     , _producing :: Bool
                     , _placementAlg :: PlacementAlg }
  deriving (Show, Read, Eq)

data SelfDestructable = SelfDestructOnTarget TargetPoint
  deriving (Show, Read, Eq)

data Moving = StraightMoving { _speed :: Speed
                             , _dir :: Direction }
  deriving (Show, Read, Eq)

data Layer = Underground | Ground | Sky
  deriving (Show, Read, Eq)

data Resource a = Resource { _current :: a
                           , _capacity :: Maybe a }
  deriving (Show, Read, Eq)

data Collision = Collision { _collidings :: Objects }
  deriving (Show, Read, Eq)

data Property = PNamed { __named :: String }
              | PDurability { __durability :: Resource Durability }
              | PBattery { __battery :: Resource Energy }
              | POwnership { __ownership :: Player }
              | PDislocation { __dislocation :: Point }
              | PPassRestriction { __passRestriction :: Set.Set PassRestriction }
              | PAge { __age :: Resource Age }
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
                             , constr :: a -> Property }

insertProperty = Map.insert
emptyPropertyMap = Map.empty
empty = Object emptyPropertyMap
merge (Object pm1) (Object pm2) = Object $ Map.union pm1 pm2

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
makeLenses ''PlacementAlg
makeLenses ''Fabric
makeLenses ''SelfDestructable
makeLenses ''Moving
makeLenses ''Layer
makeLenses ''Collision
makeLenses ''Resource

property k l = propertyMap . at k . traverse . l

-- Properties itself

isResourceValid (Resource c (Just m)) = (c >= 0) && (c <= m)
isResourceValid (Resource c Nothing)  = c >= 0
resourceValidator r | isResourceValid r = r
                    | otherwise         = error $ "Invalid resource property: " ++ show r
toResource (c, mbM) = resourceValidator $ Resource c mbM

notNullValidator s | null s = error "This property can't be null."
                   | otherwise = s

toPassRestriction = Set.fromList

toCollision = Collision . L.nub

-- TODO: remove boilerplate with TH
namedA            = PAccessor 0    $ PNamed            .notNullValidator
durabilityA       = PAccessor 1    $ PDurability       .toResource
batteryA          = PAccessor 2    $ PBattery          .toResource
ownershipA        = PAccessor 3    $ POwnership        .id
passRestrictionA  = PAccessor 4    $ PPassRestriction  .toPassRestriction
dislocationA      = PAccessor 5    $ PDislocation      .id
ageA              = PAccessor 6    $ PAge              .toResource
directedA         = PAccessor 7    $ PDirected         .id
fabricA           = PAccessor 8    $ PFabric           .id
selfDestructableA = PAccessor 9    $ PSelfDestructable .id
movingA           = PAccessor 10   $ PMoving           .id
layerA            = PAccessor 11   $ PLayer            .id
collisionA        = PAccessor 12   $ PCollision        .toCollision

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

placeToNearestEmptyCell = PlaceToNearestEmptyCell
placeToPoint = PlaceToPoint

noFly = NoFly
noWalk = NoWalk
noUndermine = NoUndermine
passRestrictions = [noFly, noWalk, noUndermine]

selfDestructOnTarget = SelfDestructOnTarget

straightMoving = StraightMoving

underground = Underground
ground = Ground
sky = Sky
layers = [ underground, ground, sky ]

charged :: Resource Energy -> Bool
charged (Resource c _) = c > 0

batteryCharge = battery.current

-- Don't know how to do this using lenses.
resourced d (la, lb) = (d ^. la, d ^. lb)


baseFabric :: Fabric
baseFabric = Fabric 0 def True placeToNearestEmptyCell

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
