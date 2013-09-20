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

data SelfDestructable = SelfDestructOnTarget TargetPoint
  deriving (Show, Read, Eq)
  
data Moving = StraightMoving { _speed :: Speed
                             , _route :: Direction }
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
  deriving (Show, Read, Eq)

type PropertyKey = Int
type PropertyMap = Map.Map PropertyKey Property
data Properties = Properties { _propertyMap :: PropertyMap }
                | LayeredProperties { _overProperties :: Properties 
                                    , _underProperties :: Properties }
  deriving (Show, Read, Eq)

data PAccessor a = PAccessor { key :: PropertyKey
                             , constr :: a -> Property
                             }

insertProperty = Map.insert

emptyPropertiesMap = Map.empty
emptyProperties = Properties emptyPropertiesMap
isEmptyProperties (Properties m) = Map.null m
isEmptyProperties (LayeredProperties mp1 mp2) = isEmptyProperties mp1 && isEmptyProperties mp2

propsertiesOver mp1 mp2 | isEmptyProperties mp1 = mp2
             | isEmptyProperties mp2 = mp1
propsertiesOver mp1@(Properties _) mp2@(Properties _) = LayeredProperties mp1 mp2
propsertiesOver mp1@(LayeredProperties imp11 imp12) mp2@(Properties _) = LayeredProperties imp11 (imp12 `propsertiesOver` mp2)
propsertiesOver mp1@(Properties _) mp2 = LayeredProperties mp1 mp2
propsertiesOver mp1@(LayeredProperties imp11 imp12) mp2@(LayeredProperties _ _) = LayeredProperties imp11 (imp12 `propsertiesOver` mp2)


(|=) accessor v = do
    props <- get
    let oldPropMap = _propertyMap props
    let newPropMap = insertProperty (key accessor) (constr accessor v) oldPropMap
    put $ props { _propertyMap = newPropMap }

setProperty :: PAccessor a -> a -> State Properties ()
setProperty = (|=)

-- Lenses
makeLenses ''Properties
makeLenses ''Property
makeLenses ''Fabric
makeLenses ''SelfDestructable
makeLenses ''Moving

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

passRestrictions = [NoFly, NoWalk, NoUndermine]

baseFabric :: Fabric
baseFabric = Fabric 0 def

selfDestructOnTarget = SelfDestructOnTarget

straightMoving = StraightMoving
move (StraightMoving s r) p = movePoint s p r



instance Default Properties where
    def = emptyProperties

instance Default Fabric where
    def = baseFabric

instance Monoid Properties where
    mempty = emptyProperties
    mappend = propsertiesOver
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