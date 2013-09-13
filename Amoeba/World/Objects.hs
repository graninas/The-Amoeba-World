{-# LANGUAGE NoMonomorphismRestriction #-}

module World.Objects where

-- import World.Properties

ableToFly = undefined
ableToCreep = undefined
ableToUndermine = undefined
durability = undefined
ownership = undefined

fullPassable = ableToFly >> ableToCreep >> ableToUndermine


plasma pl = durability 10 10 >> fullPassable >> ownership pl
