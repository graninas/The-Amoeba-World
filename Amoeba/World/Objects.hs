module World.Objects where

import Control.Monad.State
import Data.Maybe
import Data.Monoid
import Control.Lens

import World.Properties
import World.Player

instance Monoid r => Monoid (Accessor r a) where
  mempty = Accessor mempty
  mappend (Accessor a) (Accessor b) = Accessor $ a <> b

plasma :: Player -> State Properties ()
plasma pl = do
    durabilityA |= (10, 10)
    --fullPassable
    ownershipA |= pl

karyon :: Player -> State Properties ()
karyon pl = do
    batteryA |= (50, 30)
    durabilityA |= (10, 10)
    --ableToFly .= True
    --ableToUndermine .= True
    ownershipA |= pl
