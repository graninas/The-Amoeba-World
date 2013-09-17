module World.Objects where

import Control.Monad.State
import Data.Maybe
import Data.Monoid
import Control.Lens

import World.Geometry
import World.Properties
import World.Player

plasma :: Point -> Player -> State Properties ()
plasma p pl = do
    dislocationA |= p
    durabilityA |= (10, 10)
    ownershipA |= pl

karyon :: Point -> Player -> State Properties ()
karyon p pl = do
    dislocationA |= p
    batteryA |= (50, 30)
    durabilityA |= (100, 10)
    ownershipA |= pl
