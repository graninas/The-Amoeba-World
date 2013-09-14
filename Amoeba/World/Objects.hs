{-# LANGUAGE NoMonomorphismRestriction #-}

module World.Objects where

import Control.Lens
import Control.Monad.State
import Data.Maybe
import Data.Monoid
import Prelude hiding (maximum)

import World.Properties
import World.Player

instance Monoid r => Monoid (Accessor r a) where
  mempty = Accessor mempty
  mappend (Accessor a) (Accessor b) = Accessor $ a <> b

maximum = _1
current = _2

ableToFly = undefined
ableToCreep = undefined
ableToUndermine = undefined
ownership = undefined
battery = undefined

fullPassable = ableToFly >> ableToCreep >> ableToUndermine

-- maybe :: b -> (a -> b) -> Maybe a -> b
-- at :: Index m -> IndexedLens' (Index m) m (Maybe (IxValue m))

durability :: Traversal' Properties (Durability, Durability)
durability = propertyMap . at kDurability . traverse . _durability


plasma :: Player -> State Properties ()
plasma pl = do
    durability .= (10, 10)
    fullPassable
    ownership .= pl

karyon :: Player -> State Properties ()
karyon pl = do
    battery .= (50, 30)
    ableToFly >> ableToUndermine
    ownership .= pl