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

plasmaFabric :: Player -> Point -> Fabric
plasmaFabric pl p = makeObject $ do
    energyCost .= 1
    scheme .= plasma pl p
    producing .= True
    placementAlg .= placeToNearestEmptyCell

soundWaveFabric :: Player -> Direction -> Point -> Fabric
soundWaveFabric pl dir p = makeObject $ do
    let placementPoint = advance p dir
    energyCost .= 1
    scheme .= soundWave pl dir 10 placementPoint
    producing .= True
    placementAlg .= placeToPoint placementPoint

plasma :: Player -> Point -> Object
plasma pl p = makeObject $ do
    namedA |= "Plasma"
    layerA |= ground
    dislocationA |= p
    durabilityA |= (30, Just 40)
    ownershipA |= pl

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

influencer :: Player -> Direction -> Point -> Object
influencer pl dir p = makeObject $ do
    namedA |= "Influencer"
    layerA |= ground
    dislocationA |= p
    durabilityA |= (30, Just 30)
    ownershipA |= pl
    fabricA |= soundWaveFabric pl dir p

dummyFabric :: Fabric
dummyFabric = makeObject $ do
    energyCost .= 1
    scheme .= dummyObject
    producing .= False
    placementAlg .= placeToPoint zeroPoint

dummyObject :: Object
dummyObject = makeObject $ do
    namedA |= "DummyObject"
    layerA |= ground
    dislocationA |= zeroPoint
    durabilityA |= (0, Nothing)
    ownershipA |= dummyPlayer
    fabricA |= dummyFabric
    directedA |= left
    selfDestructableA |= selfDestructOnTarget (point 1 1 1)
    movingA |= straightMoving 0 right
    batteryA |= (0, Nothing)
    passRestrictionA |= passRestrictions
    ageA |= (0, Nothing)
    collisionA |= []
