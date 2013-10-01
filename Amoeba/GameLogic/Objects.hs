module GameLogic.Objects where

import Control.Monad.State
import Data.Default
import Control.Lens

import GameLogic.Geometry
import GameLogic.Object
import GameLogic.Player
import GameLogic.Types

makeObject :: Default a => State a () -> a
makeObject = flip execState def



plasma :: Player -> Point -> Object
plasma pl p = makeObject $ do
    namedA |= "Plasma"
    layerA |= ground
    dislocationA |= p
    durabilityA |= (30, Just 40)
    ownershipA |= pl

plasmaFabric :: Player -> Point -> Fabric
plasmaFabric pl p = makeObject $ do
    energyCost .= 1
    production .= plasma pl p

karyon :: Player -> Point -> Object
karyon pl p = makeObject $ do
    namedA |= "Karyon"
    layerA |= ground
    dislocationA |= p
    batteryA |= (300, Just 2000)
    durabilityA |= (100, Just 100)
    ownershipA |= pl
    fabricA |= plasmaFabric pl p

bullet :: String -> Speed -> Player -> Direction -> Power -> Point -> Object
bullet name speed pl dir power p = makeObject $ do
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
soundWaveFabric pl dir p = makeObject $ do
    energyCost .= 1
    production .= soundWave pl dir 10 p

influencer :: Player -> Direction -> Point -> Object
influencer pl dir p = makeObject $ do
    namedA |= "Influencer"
    layerA |= ground
    dislocationA |= p
    durabilityA |= (30, Just 30)
    ownershipA |= pl
    fabricA |= soundWaveFabric pl dir p
    
    
