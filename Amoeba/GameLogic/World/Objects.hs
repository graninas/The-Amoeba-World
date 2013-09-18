module GameLogic.World.Objects where

import Control.Monad.State
import Data.Default
import Control.Lens

import GameLogic.World.Geometry
import GameLogic.World.Properties
import GameLogic.World.Player

object :: Default a => State a () -> a
object = flip execState def

plasma :: Player -> Point -> Properties
plasma pl p = object $ do
    dislocationA |= p
    durabilityA |= (40, 30)
    ownershipA |= pl

plasmaFabric :: Player -> Point -> Fabric
plasmaFabric pl p = object $ do
    energyCost .= 1
    production .= plasma pl p

karyon :: Player -> Point -> Properties
karyon pl p = object $ do
    dislocationA |= p
    batteryA |= (2000, 300)
    durabilityA |= (100, 100)
    ownershipA |= pl
    fabricA |= plasmaFabric pl p

soundWave :: Player -> Direction -> Point -> Properties
soundWave pl dir p = object $ do
    dislocationA |= p
    ownershipA |= pl
    -- move dir
    
soundWaveFabric :: Player -> Direction -> Point -> Fabric
soundWaveFabric pl dir p = object $ do
    energyCost .= 1
    production .= soundWave pl dir p

influencer :: Player -> Direction -> Point -> Properties
influencer pl dir p = object $ do
    dislocationA |= p
    durabilityA |= (30, 30)
    ownershipA |= pl
    fabricA |= soundWaveFabric pl dir p
    
    
    