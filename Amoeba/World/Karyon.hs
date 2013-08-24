module World.Karyon where

import Prelude hiding (Bounded)

import World.World
import World.Player
import World.Geometry
import World.Stochastic

import Data.Word
import System.Random
import qualified Data.List as L
import qualified Data.Either as E

type Energy = Int

data Karyon = Karyon { karyonId :: ItemId
                     , karyonPlayer :: Player
                     , karyonEnergy :: Energy
                     , karyonFillers :: [Karyon]
                     , karyonBound :: Bound }
            | KaryonFiller { karyonId :: ItemId
                           , karyonPlayer :: Player
                           , fillerRelativeShift :: Shift }

instance Id Karyon where
    getId = karyonId
  
decreaseEnergyModificatorId = 1 -- TODO: implement better mechanism
increaseEnergyModificatorId = 2

instance Active Karyon where
  activate = activateKaryon
  ownedBy = karyonPlayer
  
ordinalKaryonBound :: Point -> Bound
ordinalKaryonBound p = Circle p 10
  
karyon :: ItemId -> Player -> Int -> Point -> [(Point, Items)]
karyon kId pl e pos = map mkItems (kayronCell : pointedFillers)
  where
    kayronCell = ( pos, Karyon kId pl e fillers (ordinalKaryonBound pos) )
    pointedFillers = map makePointedFiller ringSquareShifts
    makePointedFiller sh = (sh pos, KaryonFiller kId pl sh)
    fillers = map snd pointedFillers
    mkItems (p, k) = (p, makeItems [k])

activateKaryon :: World -> Point -> Karyon -> WorldMutator -> WorldMutator
activateKaryon w p k@(KaryonFiller{}) mutator = inactive w p mutator
activateKaryon w p k@(Karyon kId pl e fillers bound) mutator = let
    e' = calculateEnergyConsumption kId e mutator
    activationFunc = activateKayronPiece bound pl mutator w e' p (fillerRelativeShift . head $ fillers)
    in case activationFunc of
        Right (e'', wm) -> wm -- TODO
        Left _ -> error "activateKaryon fail not implemented"

calculateEnergyConsumption :: Int -> Energy -> WorldMutator -> Energy
calculateEnergyConsumption kId e mutator = let
    decreased = getModificatorActions kId decreaseEnergyModificatorId mutator
    increased = getModificatorActions kId increaseEnergyModificatorId mutator
    in validateEnergy $ e - length increased + length decreased

activateKayronPiece :: Bound -> Player -> WorldMutator -> World -> Energy -> Point -> Shift -> Either (Energy, Energy, WorldMutator) (Energy, WorldMutator)
activateKayronPiece karyonBound pl wm w e kayronPoint pieceShift = if isCornerShift pieceShift
    then E.either (reservableGrow 2) (reservableGrow 1) grow1
    else E.either (reserveEnergy 1) return grow0
  where
      reserveEnergy cnt (e, wm) = Left (cnt, e, wm)
      growFunc subShiftFunc (e', wm') = activateKayronPiece' karyonBound pl wm' w e' kayronPoint (subShiftFunc pieceShift)
      grow0 = growFunc id (e, wm)
      grow1 = growFunc subShift1 (e, wm)
      grow2 = growFunc subShift2
      reservableGrow reserve d = E.either (reserveEnergy reserve) return (grow2 d)

decreaseEnergy e | e <= 0 = 0
                 | otherwise = e - 1
validateEnergy e | e <= 0 = 0
                 | otherwise = e
                 
activateKayronPiece' :: Bound -> Player -> WorldMutator -> World -> Energy -> Point -> Shift -> Either (Energy, WorldMutator) (Energy, WorldMutator)
activateKayronPiece' karyonBound pl wm w e kayronPoint subShift
        | e <= 0 = Left (e, wm) -- No energy
        | otherwise = do 
            let growPlasmaFunc = growPlasma karyonBound pl wm w (subShift kayronPoint) (direction subShift)
            let decreaseEnergyFunc wm' = Right (decreaseEnergy e, wm')
            let failActivation _ = Left (e, wm) -- Failed to grow 
            E.either failActivation decreaseEnergyFunc growPlasmaFunc

growProbabilities :: [(Direction, DirectionProbability)]
growProbabilities = [ (left,  DirectionProbability 50 25 0 25)
                    , (up,    DirectionProbability 25 50 25 0)
                    , (right, DirectionProbability 0 25 50 25)
                    , (down,  DirectionProbability 25 0 25 50) ]


chooseRandomDir :: StdGen -> Direction -> [Direction] -> Either String (StdGen, Direction)
chooseRandomDir g0 dir triedDirs = do
    triedDirsChecker triedDirs
    let (rndNum, g1) = randomProbabilityNum g0
    let dirProb = getDirectionProbability dir growProbabilities
    rndDir <- getSafeRandomDirection rndNum dirProb
    return (g1, rndDir)


data Plasma = Plasma { plasmaId :: ItemId
                     , plasmaPlayer :: Player }

instance Id Plasma where
  getId = plasmaId

instance Active Plasma where
  activate w p _ = inactive w p
  ownedBy = plasmaPlayer

plasma :: Int -> Player -> Plasma
plasma = Plasma

plasmaConstructor :: Player -> ItemId -> Plasma
plasmaConstructor pl pId = plasma pId pl

data GrowMode = GrowOver Point
              | StopGrow

grow' :: Bound
     -> Player
     -> Actions
     -> World
     -> Point
     -> Direction
     -> Either GrowMode Actions
grow' bound pl acts w toPoint dir
    | not $ inBounds toPoint bound = Left StopGrow
    | otherwise = case takeWorldItems toPoint w of
        NoItems -> let act = addSingleActive toPoint (plasmaConstructor pl)
                   in return (act : acts)
        items | isOnePlayerHere pl items -> Left (GrowOver toPoint)
              | isObstacle items -> Left StopGrow
              | otherwise -> let act = addSingleConflicted toPoint (plasmaConstructor pl) pl
                             in return (act : acts)

grow :: Bound
     -> Player
     -> Actions
     -> StdGen
     -> World
     -> Point
     -> Direction
     -> [Direction]
     -> Either GrowMode WorldMutator
grow bound pl acts g0 w fromPoint dir triedDirs = 
    case chooseRandomDir g0 dir triedDirs of
        Left _ -> Left StopGrow
        Right (g1, rndDir) -> do
            let growFunc = grow' bound pl acts w (movePoint fromPoint rndDir) dir
            let nextDir = nextDirection dir
            let tryNextDirectionFunc = grow bound pl acts g1 w fromPoint nextDir (dir : triedDirs)
            let successedGrowFunc acts = Right $ createWorldMutator g1 acts
            let tryGrowMode StopGrow = tryNextDirectionFunc
            let tryGrowMode (GrowOver p) = grow bound pl acts g1 w p dir []
            E.either tryGrowMode successedGrowFunc growFunc

growPlasma :: Bound -> Player -> WorldMutator -> World -> Point -> Direction -> Either String WorldMutator
growPlasma bound pl wm@(WorldMutator acts g) w piecePoint dir = do
    let growFunc = grow bound pl acts g w piecePoint dir []
    let failGrow _ = Left "No grow possible" 
    E.either failGrow return growFunc
    