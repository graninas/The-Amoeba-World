module GameLogic.Assets.Objects where

{-
import Control.Monad.State
import Data.Default
import Control.Lens

import GameLogic.Base.Geometry
import GameLogic.Data.Object
import GameLogic.Data.Player
import Misc.Descriptions

plasmaFabric :: Player -> Point -> Fabric
plasmaFabric pl p = makeObject $ do
    energyCost   .= 1
    scheme       .= plasma pl p
    producing    .= True
    placementAlg .= placeToNearestEmptyCell

soundWaveFabric :: Player -> Direction -> Point -> Fabric
soundWaveFabric pl dir p = makeObject $ do
    let placementPoint = advance p dir
    energyCost   .= 1
    scheme       .= soundWave pl dir 10 placementPoint
    producing    .= True
    placementAlg .= placeToPoint placementPoint

plasma :: Player -> Point -> Object
plasma pl p = makeObject $ do
    namedA       |= plasmaName
    layerA       |= ground
    dislocationA |= p
    durabilityA  |= (30, Just 40)
    ownershipA   |= pl

karyon :: Player -> Point -> Object
karyon pl p = makeObject $ do
    namedA       |= karyonName
    layerA       |= ground
    dislocationA |= p
    batteryA     |= (300, Just 2000)
    durabilityA  |= (100, Just 100)
    ownershipA   |= pl
    fabricA      |= plasmaFabric pl p

bullet :: Named -> Speed -> Player -> Direction -> Power -> Point -> Object
bullet name speed pl dir power p = makeObject $ do
    let targetP = moveStraight power p dir
    namedA            |= name
    layerA            |= sky
    dislocationA      |= p
    ownershipA        |= pl
    directedA         |= dir
    selfDestructableA |= selfDestructOnTarget targetP
    movingA           |= straightMoving speed dir

soundWave = bullet soundWaveName 1
laserBeam = bullet laserBeamName 10

influencer :: Player -> Direction -> Point -> Object
influencer pl dir p = makeObject $ do
    namedA       |= influencerName
    layerA       |= ground
    dislocationA |= p
    durabilityA  |= (30, Just 30)
    ownershipA   |= pl
    fabricA      |= soundWaveFabric pl dir p

-}