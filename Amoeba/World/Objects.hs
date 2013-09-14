module World.Objects where

import Control.Lens
import Control.Monad.State
import Data.Maybe
import Data.Monoid

import World.Properties
import World.Player

instance Monoid r => Monoid (Accessor r a) where
  mempty = Accessor mempty
  mappend (Accessor a) (Accessor b) = Accessor $ a <> b

ableToFly = undefined
ableToCreep = undefined
ableToUndermine = undefined

fullPassable = ableToFly >> ableToCreep >> ableToUndermine

plasma :: Player -> State Properties ()
plasma pl = do
    durability .= (10, 10)
    fullPassable
    ownership .= pl

karyon :: Player -> State Properties ()
karyon pl = do
    battery .= (50, 30)
    battery.maxVal .= 100
    durability .= (10, 10)
    ableToFly >> ableToUndermine
    ownership .= pl