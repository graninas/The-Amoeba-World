{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module World.Properties where

import Data.Monoid
import Data.Maybe
import Control.Lens
import Control.Monad.State
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import World.Geometry
import World.Player

type Target = Point
type Speed = Int
type Energy = Int
type Capacity = Energy
type Durability = Int
data Passable = Passable { __ableToFly :: Bool
                         , __ableToWalk :: Bool
                         , __ableToUndermine :: Bool }
  deriving (Show, Read, Eq)

data PassRestriction = NoFly | NoWalk | NoUndermine
  deriving (Show, Read, Eq)
  
data Property = PDurability { __durability :: (Durability, Durability) }
              | PPassability { __passability :: Passable }
              | PBattery { __battery :: (Capacity, Energy) }
              | POwnership { __ownership :: Player }
              | PDislocation { __dislocation :: Point }
              | PPassRestriction { __passRestriction :: Seq.Seq PassRestriction }
  deriving (Show, Read, Eq)

type PropertyKey = Int
type PropertyMap = Map.Map PropertyKey Property
data Properties = Properties { _propertyMap :: PropertyMap }
  deriving (Show, Read, Eq)

  
mergeProperties (Properties ps1) (Properties ps2) = Properties $ Map.union ps1 ps2
emptyProperties = Properties Map.empty



-- Lenses

data PAccessor a = PAccessor { key :: PropertyKey
                             , val :: a }

makeLenses ''Property
makeLenses ''Properties
makeLenses ''Passable

maxVal = _1
curVal = _2

property accessor = propertyMap . at (key accessor) . traverse . val accessor

durability  = property $ PAccessor 1 _durability
battery     = property $ PAccessor 2 _battery
ownership   = property $ PAccessor 3 _ownership
passability = property $ PAccessor 4 _passability
ableToFly = passability . _ableToFly
ableToWalk = passability . _ableToWalk
ableToUndermine = passability . _ableToUndermine

flyIndex = 1
walkIndex = 2
undermineIndex = 3
{-
passRestriction = property $ PAccessor 5 _passRestriction
noFly = passRestriction . ix flyIndex .~ NoFly
noWalk = passRestriction . ix walkIndex .~ NoWalk
noUndermine = passRestriction . ix undermineIndex .~ NoUndermine
-}
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