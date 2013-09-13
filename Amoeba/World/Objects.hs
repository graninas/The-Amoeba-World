{-# LANGUAGE NoMonomorphismRestriction #-}

module World.Objects where

import World.Properties

import Control.Lens
import Prelude hiding (maximum)

maximum = _1
current = _2

ableToFly = undefined
ableToCreep = undefined
ableToUndermine = undefined
durability = at pDurability . _durabilitygt
ownership = at pOwnership . _ownership
battery = at pBattery . _battery

fullPassable = ableToFly >> ableToCreep >> ableToUndermine

setDurability = (durability.both .=)
setBattery (m, c) = do
    battery.maximum .= m
    battery.current .= c


plasma pl = do
    setDurability (10, 10)
    fullPassable
    ownership .= pl

karyon pl = do
    setDurability (100, 100)
    setBattery (50, 30)
    ableToFly >> ableToUndermine
    ownership .= pl