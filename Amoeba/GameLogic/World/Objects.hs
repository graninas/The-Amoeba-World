module GameLogic.World.Objects where

import Control.Monad.State
import Data.Default
import Control.Lens

import GameLogic.World.Geometry
import GameLogic.World.Properties
import GameLogic.World.Player
import GameLogic.World.Types

object :: Default a => State a () -> a
object = flip execState def

plasma :: Player -> Point -> Properties
plasma pl p = object $ do
    namedA |= "Plasma"
    layerA |= ground
    dislocationA |= p
    durabilityA |= (30, Just 40)
    ownershipA |= pl

plasmaFabric :: Player -> Point -> Fabric
plasmaFabric pl p = object $ do
    energyCost .= 1
    production .= plasma pl p

karyon :: Player -> Point -> Properties
karyon pl p = object $ do
    namedA |= "Karyon"
    layerA |= ground
    dislocationA |= p
    batteryA |= (300, Just 2000)
    durabilityA |= (100, Just 100)
    ownershipA |= pl
    fabricA |= plasmaFabric pl p

bullet :: String -> Speed -> Player -> Direction -> Power -> Point -> Properties
bullet name speed pl dir power p = object $ do
    let targetP = moveStraight power p dir
    namedA |= name
    layerA |= sky
    dislocationA |= p
    ownershipA |= pl
    directedA |= dir
    selfDestructableA |= selfDestructOnTarget targetP
    movingA |= straightMoving speed dir

soundWave = bullet "SoundWave" 1
laserBeam = bullet "LaserBeam" 10

soundWaveFabric :: Player -> Direction -> Point -> Fabric
soundWaveFabric pl dir p = object $ do
    energyCost .= 1
    production .= soundWave pl dir 10 p

influencer :: Player -> Direction -> Point -> Properties
influencer pl dir p = object $ do
    namedA |= "Influencer"
    layerA |= ground
    dislocationA |= p
    durabilityA |= (30, Just 30)
    ownershipA |= pl
    fabricA |= soundWaveFabric pl dir p
    
    
    