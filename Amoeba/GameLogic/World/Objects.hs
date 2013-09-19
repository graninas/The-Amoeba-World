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
    durabilityA |= (30, Just 40)
    ownershipA |= pl

plasmaFabric :: Player -> Point -> Fabric
plasmaFabric pl p = object $ do
    energyCost .= 1
    production .= plasma pl p

karyon :: Player -> Point -> Properties
karyon pl p = object $ do
    dislocationA |= p
    batteryA |= (300, Just 2000)
    durabilityA |= (100, Just 100)
    ownershipA |= pl
    fabricA |= plasmaFabric pl p

soundWave :: Player -> Direction -> TargetPoint -> Point -> Properties
soundWave pl dir tp p = object $ do
    dislocationA |= p
    ownershipA |= pl
    directedA |= dir
    selfDestructableA |= selfDestructOnTarget tp
    -- move dir

soundWaveFabric :: Player -> Direction -> Point -> Fabric
soundWaveFabric pl dir p = object $ do
    let targetP = movePoint 10 p dir
    energyCost .= 1
    production .= soundWave pl dir targetP p

influencer :: Player -> Direction -> Point -> Properties
influencer pl dir p = object $ do
    dislocationA |= p
    durabilityA |= (30, Just 30)
    ownershipA |= pl
    fabricA |= soundWaveFabric pl dir p
    
    
    